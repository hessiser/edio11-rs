#![feature(fn_traits)]
mod backup;
mod errors;
pub mod input;

use std::{
    mem,
    sync::{Mutex, Once},
};

use arboard::{Clipboard, ImageData};
use backup::BackupState;
use egui::{Context, PlatformOutput};
use errors::OverlayError;
use input::{InputHandler, InputResult};
use retour::static_detour;
use windows::{
    core::HRESULT,
    Win32::{
        Foundation::{HWND, LPARAM, LRESULT, WPARAM},
        Graphics::{
            Direct3D11::{
                ID3D11Device, ID3D11DeviceContext, ID3D11InputLayout, ID3D11RenderTargetView, ID3D11Texture2D
            },
            Dxgi::{Common::DXGI_FORMAT, IDXGISwapChain, IDXGISwapChain_Vtbl, DXGI_PRESENT},
        },
        UI::
            WindowsAndMessaging::{SetWindowLongPtrW, GWLP_WNDPROC}
        ,
    },
};

static mut OVERLAY_HANDLER: Option<OverlayHandler<Box<dyn Overlay>>> = None;

static_detour! {
    pub static Present_Detour: unsafe extern "stdcall" fn(*const IDXGISwapChain_Vtbl, u32, DXGI_PRESENT) -> HRESULT;
    pub static Resize_Buffers_Detour: fn(
        *const IDXGISwapChain_Vtbl,
        u32,
        u32,
        u32,
        DXGI_FORMAT,
        u32
    ) -> HRESULT;
}

type WNDPROC = unsafe extern "system" fn(HWND, u32, WPARAM, LPARAM) -> LRESULT;
struct OverlayHandlerInner {
    pub device_context: ID3D11DeviceContext,
    pub render_target: Option<Mutex<ID3D11RenderTargetView>>,
    pub egui_renderer: egui_directx11::Renderer,
    pub input_handler: InputHandler,
    pub window_process_callback: WNDPROC,
}

struct OverlayHandler<T: Overlay + ?Sized> {
    inner: Option<OverlayHandlerInner>,
    egui_ctx: egui::Context,
    backup: BackupState,
    overlay: Box<T>,
}

#[derive(Default)]
pub struct WindowMessage {
    pub msg: u32,
    pub wparam: WPARAM,
    pub lparam: LPARAM,
}

#[derive(Default)]
pub struct WindowProcessOptions {
    /// Egui only steals the incoming input corresponding to either
    /// ``Context::wants_pointer_input`` or ``Context::wants_keyboard_input`` by default.
    /// This flag will allow capture of all input regardless of what type.
    pub should_capture_all_input: bool,
    /// Egui only steals the incoming input corresponding to either
    /// ``Context::wants_pointer_input`` or ``Context::wants_keyboard_input`` by default.
    /// This flag will allow the underlying window to capture the input as well.
    pub should_input_pass_through: bool,
    /// Optional ``WindowMessage`` that will be processed as well.
    pub window_message: Option<WindowMessage>,
    /// If ``Some(self.window_message)``, then ``self.window_message`` will only be processed and not the original window message by default.
    /// This flag will allow the processing of the original input and return the result to the original function.
    pub should_process_original_message: bool,
}

pub trait Overlay {
    fn update(&mut self, ctx: &egui::Context);
    fn resize_buffers(
        &mut self,
        swap_chain_vtbl: *const IDXGISwapChain_Vtbl,
        buffer_count: u32,
        width: u32,
        height: u32,
        new_format: DXGI_FORMAT,
        swap_chain_flags: u32,
    ) {}
    fn window_process(
        &mut self,
        input: &InputResult,
        input_events: &Vec<egui::Event>,
    ) -> Option<WindowProcessOptions> {
        None
    }
}

impl<T: Overlay + ?Sized> Overlay for Box<T> {
    #[inline]
    fn update(&mut self, ctx: &egui::Context) {
        (**self).update(ctx);
    }

    #[inline]
    fn resize_buffers(
        &mut self,
        swap_chain_vtbl: *const IDXGISwapChain_Vtbl,
        buffer_count: u32,
        width: u32,
        height: u32,
        new_format: DXGI_FORMAT,
        swap_chain_flags: u32,
    ) {
        (**self).resize_buffers(
            swap_chain_vtbl,
            buffer_count,
            width,
            height,
            new_format,
            swap_chain_flags,
        );
    }

    #[inline]
    fn window_process(
        &mut self,
        input: &InputResult,
        input_events: &Vec<egui::Event>,
    ) -> Option<WindowProcessOptions> {
        (**self).window_process(input, input_events)
    }
}

impl<T: Overlay + ?Sized> OverlayHandler<T> {
    // A convenience wrapper
    unsafe fn get_swapchain_from_vtbl(swap_chain_vtbl: &IDXGISwapChain_Vtbl) -> &IDXGISwapChain {
        // &&IDXGISwapChain_Vtbl
        std::mem::transmute(&swap_chain_vtbl)
    }

    #[inline]
    // lazy static constructor
    fn lazy_initialize(&mut self, swap_chain: &IDXGISwapChain) {
        static INIT: Once = Once::new();
        INIT.call_once(|| {
            let swap_desc = unsafe { swap_chain.GetDesc().unwrap() };
            let hwnd = swap_desc.OutputWindow;
            if hwnd.0.is_null() {
                panic!("Invalid output window descriptor");
            }

            let (device, device_context) =
                unsafe { Self::get_device_and_context(swap_chain) }.unwrap();
            let render_target =
                unsafe { Self::create_render_target_for_swap_chain(&device, swap_chain).unwrap() };

            let egui_renderer =
                egui_directx11::Renderer::new(&device).expect("Failed to create egui renderer");

            let window_process_target = unsafe {
                mem::transmute(SetWindowLongPtrW(
                    hwnd,
                    GWLP_WNDPROC,
                    Self::window_process_hook as usize as _,
                ))
            };

            let inner = OverlayHandlerInner {
                device_context,
                render_target: Some(render_target.into()),
                window_process_callback: window_process_target,
                egui_renderer,
                input_handler: InputHandler::new(hwnd),
            };

            self.inner = Some(inner);
        });
    }

    pub fn present(&mut self) {
        if let Some(inner) = &mut self.inner {
            if let Some(render_target) = &inner.render_target {
                let egui_output = self
                    .egui_ctx
                    .run(inner.input_handler.collect_input(), |ctx| {
                        self.overlay.update(ctx);
                    });

                self.backup.save(&inner.device_context);

                let (renderer_output, platform_output, _) =
                    egui_directx11::split_output(egui_output);
                Self::handle_platform_output(platform_output);
                // let scale_factor = match unsafe { Self::scale_factor(inner) } {
                //     Ok(v) => v.0 as f32,
                //     Err(e) => {
                //         let scale_factor = 1.0;
                //         log::error!("`OverlayHandler::scale_factor` Error: {}. Defaulting to scale factor {}", e, scale_factor);
                //         scale_factor
                //     }
                // };
                if let Ok(render_target_lock) = render_target.try_lock() {
                    let _ = inner.egui_renderer.render(
                        &inner.device_context,
                        &render_target_lock,
                        &self.egui_ctx,
                        renderer_output,
                        1.0,
                    );
                }


                self.backup.restore(&inner.device_context);
            } else {
                log::error!("OverylayHander::present unreachable");
                unreachable!()
            }
        } else {
            log::error!("OverylayHander::present unreachable");
            unreachable!();
        }
    }

    // unsafe fn scale_factor(
    //     inner: &OverlayHandlerInner,
    // ) -> windows::core::Result<Common::DEVICE_SCALE_FACTOR> {
    //     let hmon = MonitorFromWindow(inner.hwnd, MONITOR_DEFAULTTONEAREST);
    //     GetScaleFactorForMonitor(hmon)
    // }

    // Only supporting WindowsOS
    fn handle_platform_output(platform_output: PlatformOutput) {
        let mut clipboard = Clipboard::new().unwrap();

        for cmd in platform_output.commands {
            let error = match cmd {
                egui::OutputCommand::CopyText(text) => clipboard.set_text(text),
                egui::OutputCommand::CopyImage(color_image) => clipboard.set_image(ImageData {
                    width: color_image.width(),
                    height: color_image.height(),
                    bytes: color_image
                        .pixels
                        .iter()
                        .map(|pixel| pixel.to_array())
                        .flatten()
                        .collect(),
                }),
                egui::OutputCommand::OpenUrl(_open_url) => Err(arboard::Error::Unknown {
                    description: "egui::OutputCommand::OpenUrl` is not supported.".to_string(),
                }),
            };
            if let Err(error) = error {
                log::error!("{}", error);
            }
        }
    }

    unsafe fn create_render_target_for_swap_chain(
        device: &ID3D11Device,
        swap_chain: &IDXGISwapChain,
    ) -> windows::core::Result<ID3D11RenderTargetView> {
        let swap_chain_texture = unsafe { swap_chain.GetBuffer::<ID3D11Texture2D>(0) }?;
        let mut render_target = None;
        unsafe {
            device.CreateRenderTargetView(&swap_chain_texture, None, Some(&mut render_target))
        }?;
        Ok(render_target.unwrap())
    }

    unsafe fn get_device_and_context(
        swap_chain: &IDXGISwapChain,
    ) -> windows::core::Result<(ID3D11Device, ID3D11DeviceContext)> {
        match swap_chain.GetDevice::<ID3D11Device>() {
            Ok(device) => match device.GetImmediateContext() {
                Ok(device_ctx) => Ok((device, device_ctx)),
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        }
    }

    // Hooked function
    fn present_hook(
        swap_chain_vtbl: *const IDXGISwapChain_Vtbl,
        sync_interval: u32,
        flags: DXGI_PRESENT,
    ) -> HRESULT {
        if let Some(overlay_handler) = unsafe { &mut OVERLAY_HANDLER } {
            // let swap_chain = unsafe { Self::get_swapchain_from_vtbl(&*swap_chain_vtbl) };
            overlay_handler.lazy_initialize(unsafe { mem::transmute(&swap_chain_vtbl) });
            overlay_handler.present()
        } else {
            let error = "`OverlayHandler::present_hook` Error: OVERLAY_HANDLER was not initialized";
            log::error!("{}", error);
        }
        unsafe { Present_Detour.call(swap_chain_vtbl, sync_interval, flags) }
    }

    // Hooked function
    fn resize_buffers_hook(
        swap_chain_vtbl: *const IDXGISwapChain_Vtbl,
        buffer_count: u32,
        width: u32,
        height: u32,
        new_format: DXGI_FORMAT,
        swap_chain_flags: u32,
    ) -> HRESULT {
        if let Some(overlay_handler) = unsafe { &mut OVERLAY_HANDLER } {
            if let Some(inner) = &mut overlay_handler.inner.as_mut() {
                // let swap_chain = unsafe { Self::get_swapchain_from_vtbl(&*swap_chain_vtbl) };
                let x: &IDXGISwapChain = unsafe { mem::transmute(&swap_chain_vtbl) };

                let result = Resize_Buffers_Detour.call(
                    swap_chain_vtbl,
                    buffer_count,
                    width,
                    height,
                    new_format,
                    swap_chain_flags,
                );
                overlay_handler.overlay.resize_buffers(
                    swap_chain_vtbl,
                    buffer_count,
                    width,
                    height,
                    new_format,
                    swap_chain_flags,
                );

                // drop(inner.render_target.take());
                let device: ID3D11Device = unsafe { x.GetDevice().unwrap() };
                let render_target =
                    unsafe { Self::create_render_target_for_swap_chain(&device, x) }.unwrap();

                inner.render_target = Some(render_target.into());
                return result;
            }
        }
        HRESULT(0)
    }

    // Hooked function
    fn window_process_hook(hwnd: HWND, umsg: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
        if let Some(overlay_handler) = unsafe { &mut OVERLAY_HANDLER } {
            if let Some(ref mut inner) = &mut overlay_handler.inner {
                let input = inner.input_handler.process(umsg, wparam.0, lparam.0);

                if let Some(options) = overlay_handler.overlay
                    .window_process(&input, &inner.input_handler.events) {
                        // If user doesn't want to steal the input
                        if options.should_input_pass_through {
                            return unsafe {
                                (inner.window_process_callback)(hwnd, umsg, wparam, lparam)
                            };
                        }

                        // Let overlay capture input only if in focus
                        match input {
                            InputResult::MouseMove |
                            InputResult::MouseLeft |
                            InputResult::MouseRight |
                            InputResult::MouseMiddle |
                            InputResult::Zoom |
                            InputResult::Scroll => {
                                if options.should_capture_all_input
                                    || overlay_handler.egui_ctx.wants_pointer_input()
                                {
                                    return LRESULT(1);
                                }
                            }
                            InputResult::Character | InputResult::Key => {
                                if options.should_capture_all_input
                                    || overlay_handler.egui_ctx.wants_keyboard_input()
                                {
                                    return LRESULT(1);
                                }
                            }
                            _ => {}
                        }

                        if let Some(wnd_msg) = options.window_message {
                            let res = unsafe {
                                (inner.window_process_callback)(
                                    hwnd,
                                    wnd_msg.msg,
                                    wnd_msg.wparam,
                                    wnd_msg.lparam,
                                )
                            };

                            if options.should_process_original_message {
                                return unsafe {
                                    (inner.window_process_callback)(hwnd, umsg, wparam, lparam)
                                };
                            }
                            else {
                                return res;
                            }
                        }
                }
                return unsafe { (inner.window_process_callback)(hwnd, umsg, wparam, lparam) };
            }
        }
        LRESULT(0)
    }
}

pub fn set_overlay(
    overlay_creator: Box<dyn FnOnce(Context) -> Box<dyn Overlay>>,
    present_target: fn(*const IDXGISwapChain_Vtbl, u32, DXGI_PRESENT) -> HRESULT,
    resize_buffers_target: fn(
        *const IDXGISwapChain_Vtbl,
        u32,
        u32,
        u32,
        DXGI_FORMAT,
        u32,
    ) -> HRESULT,
) -> Result<(), OverlayError> {
    match unsafe { &OVERLAY_HANDLER } {
        Some(_) => Err(OverlayError::AlreadyInitialized),
        None => unsafe {
            let egui_ctx = Context::default();
            let overlay = overlay_creator.call_once((egui_ctx.clone(),));
            let overlay_handler = OverlayHandler {
                inner: None,
                egui_ctx,
                backup: BackupState::default(),
                overlay: Box::new(overlay),
            };
            OVERLAY_HANDLER = Some(overlay_handler);
            Present_Detour
                .initialize(
                    mem::transmute(present_target),
                    OverlayHandler::<dyn Overlay>::present_hook,
                )
                .unwrap();
            Resize_Buffers_Detour
                .initialize(
                    mem::transmute(resize_buffers_target),
                    OverlayHandler::<dyn Overlay>::resize_buffers_hook,
                )
                .unwrap();
            Present_Detour.enable().unwrap();
            Resize_Buffers_Detour.enable().unwrap();
            Ok(())
        },
    }
}
