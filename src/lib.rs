#![feature(fn_traits)]
mod backup;
mod errors;
pub mod input;

use std::{mem, sync::Once};

use arboard::{Clipboard, ImageData};
use backup::BackupState;
use egui::{Context, Memory, PlatformOutput, RawInput, Vec2, gui_zoom::kb_shortcuts};
use errors::OverlayError;
use input::{InputHandler, InputResult};
use retour::static_detour;
use windows::{
    Win32::{
        Foundation::{HWND, LPARAM, LRESULT, RECT, WPARAM},
        Graphics::{
            Direct3D11::{
                D3D11_TEXTURE2D_DESC, ID3D11Device, ID3D11DeviceContext, ID3D11RenderTargetView,
                ID3D11Texture2D,
            },
            Dxgi::{Common::DXGI_FORMAT, DXGI_PRESENT, IDXGISwapChain, IDXGISwapChain_Vtbl},
            Gdi::{GetMonitorInfoW, MONITOR_DEFAULTTONEAREST, MONITORINFO, MonitorFromWindow},
        },
        UI::{
            Input::Pointer::EnableMouseInPointer,
            Shell::GetScaleFactorForMonitor,
            WindowsAndMessaging::{GWLP_WNDPROC, GetClientRect, SetWindowLongPtrW, WM_CLOSE},
        },
    },
    core::HRESULT,
};
use windows::core::HSTRING;

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
    pub render_target: Option<ID3D11RenderTargetView>,
    pub egui_renderer: egui_directx11::Renderer,
    pub input_handler: InputHandler,
    pub window_process_callback: WNDPROC,
    hwnd: HWND,
}

struct OverlayHandler<T: Overlay + ?Sized> {
    inner: Option<OverlayHandlerInner>,
    egui_ctx: egui::Context,
    backup: BackupState,
    overlay: Box<T>,
    zoom_factor: f32,
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
    ) {
    }
    fn window_process(
        &mut self,
        input: &InputResult,
        input_events: &Vec<egui::Event>,
    ) -> Option<WindowProcessOptions> {
        None
    }
    fn save(&mut self, _storage: &mut Memory) {}
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
    fn save(&mut self, _storage: &mut Memory) {
        (**self).save(_storage);
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

const MIN_ZOOM_FACTOR: f32 = 0.2;
const MAX_ZOOM_FACTOR: f32 = 5.0;

impl<T: Overlay + ?Sized> OverlayHandler<T> {
    #[inline]
    // lazy static constructor
    fn lazy_initialize(&mut self, swap_chain: &IDXGISwapChain) {
        static INIT: Once = Once::new();
        INIT.call_once(|| {
            let swap_desc = match unsafe { swap_chain.GetDesc() } {
                Ok(sd) => sd,
                Err(e) => {
                    log::error!("GetDesc failed for swap chain: {:?}", e);
                    return;
                }
            };
            let hwnd = swap_desc.OutputWindow;
            if hwnd.0.is_null() {
                log::error!("Invalid output window descriptor");
                return;
            }

            let (device, device_context) = match unsafe { Self::get_device_and_context(swap_chain) } {
                Ok(v) => v,
                Err(e) => {
                    log::error!("get_device_and_context failed: {:?}", e);
                    return;
                }
            };
            let render_target = match unsafe { Self::create_render_target_for_swap_chain(&device, swap_chain) } {
                Ok(rt) => rt,
                Err(e) => {
                    log::error!("create_render_target_for_swap_chain failed: {:?}", e);
                    return;
                }
            };

            let egui_renderer = match egui_directx11::Renderer::new(&device) {
                Ok(r) => r,
                Err(e) => {
                    log::error!("Failed to create egui renderer: {:?}", e);
                    return;
                }
            };

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
                input_handler: InputHandler::new(hwnd, &self.egui_ctx),
                hwnd,
            };

            self.inner = Some(inner);
        });
    }

    pub fn present(&mut self) {
        if let Some(inner) = &mut self.inner {
            if let Some(render_target) = &inner.render_target {
                let pixels_per_point = unsafe {
                    let monitor = MonitorFromWindow(inner.hwnd, MONITOR_DEFAULTTONEAREST);

                    // Scale
                    let scale_factor = match GetScaleFactorForMonitor(monitor) {
                        Ok(s) => s.0 / 100,
                        Err(e) => {
                            log::warn!("GetScaleFactorForMonitor failed: {:?}. Defaulting to 100.", e);
                            100
                        }
                    };

                    // Screen Size
                    let mut monitor_info = MONITORINFO {
                        cbSize: size_of::<MONITORINFO>() as _,
                        ..Default::default()
                    };
                    if unsafe { GetMonitorInfoW(monitor, &mut monitor_info) }.as_bool() == false {
                        let e = windows::core::Error::from_win32();
                        log::warn!("GetMonitorInfoW failed: {:?}", e);
                    }
                    let width = monitor_info.rcMonitor.right - monitor_info.rcMonitor.left;
                    let height = monitor_info.rcMonitor.bottom - monitor_info.rcMonitor.top;
                    let screen_size = Vec2::new(width as f32, height as f32);

                    // Window Size
                    let mut rect = RECT::default();
                    if let Err(e) = unsafe { GetClientRect(inner.hwnd, &mut rect) } {
                        log::warn!("GetClientRect failed: {:?}", e);
                    }

                    let window_size = Vec2::new(
                        (rect.right - rect.left) as f32,
                        (rect.bottom - rect.top) as f32,
                    );

                    let res_scale = window_size.length() / screen_size.length();
                    scale_factor as f32 * res_scale
                };

                if pixels_per_point > 0.0 {
                    self.egui_ctx
                        .set_pixels_per_point(pixels_per_point * self.zoom_factor);
                }

                let egui_output = self
                    .egui_ctx
                    .run(inner.input_handler.collect_input(), |ctx| {
                        if ctx.input_mut(|i| i.consume_shortcut(&kb_shortcuts::ZOOM_RESET)) {
                            self.zoom_factor = 1.0;
                            self.egui_ctx.set_zoom_factor(self.zoom_factor);
                        } else {
                            if ctx.input_mut(|i| i.consume_shortcut(&kb_shortcuts::ZOOM_IN))
                                || ctx.input_mut(|i| {
                                    i.consume_shortcut(&kb_shortcuts::ZOOM_IN_SECONDARY)
                                })
                            {
                                self.zoom_factor += 0.1;
                                self.zoom_factor =
                                    self.zoom_factor.clamp(MIN_ZOOM_FACTOR, MAX_ZOOM_FACTOR);
                                self.zoom_factor = (self.zoom_factor * 10.).round() / 10.;
                                self.egui_ctx.set_zoom_factor(self.zoom_factor);
                            }
                            if ctx.input_mut(|i| i.consume_shortcut(&kb_shortcuts::ZOOM_OUT)) {
                                self.zoom_factor -= 0.1;
                                self.zoom_factor =
                                    self.zoom_factor.clamp(MIN_ZOOM_FACTOR, MAX_ZOOM_FACTOR);
                                self.zoom_factor = (self.zoom_factor * 10.).round() / 10.;
                                self.egui_ctx.set_zoom_factor(self.zoom_factor);
                            }
                        }

                        self.overlay.update(ctx);
                    });

                self.backup.save(&inner.device_context);

                let (renderer_output, platform_output, _) =
                    egui_directx11::split_output(egui_output);
                Self::handle_platform_output(&self.egui_ctx, platform_output);

                let _ = inner.egui_renderer.render(
                    &inner.device_context,
                    &render_target,
                    &self.egui_ctx,
                    renderer_output,
                    1.0,
                );

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

    // Only supporting WindowsOS
    fn handle_platform_output(ctx: &egui::Context, platform_output: PlatformOutput) {
                let mut clipboard = match Clipboard::new() {
                    Ok(cb) => Some(cb),
                    Err(e) => {
                        log::warn!("Clipboard not available: {:?}", e);
                        None
                    }
                };

        for cmd in platform_output.commands {
            match cmd {
                egui::OutputCommand::CopyText(text) => {
                    if let Some(cb) = clipboard.as_mut() {
                        if let Err(e) = cb.set_text(text) {
                            log::warn!("Failed to set clipboard text: {:?}", e);
                        }
                    } else {
                        log::debug!("Skipping clipboard set_text because clipboard is unavailable");
                    }
                }
                egui::OutputCommand::CopyImage(color_image) => {
                    if let Some(cb) = clipboard.as_mut() {
                        if let Err(e) = cb.set_image(ImageData {
                            width: color_image.width(),
                            height: color_image.height(),
                            bytes: color_image
                                .pixels
                                .iter()
                                .map(|pixel| pixel.to_array())
                                .flatten()
                                .collect(),
                        }) {
                            log::warn!("Failed to set clipboard image: {:?}", e);
                        }
                    } else {
                        log::debug!("Skipping clipboard set_image because clipboard is unavailable");
                    }
                }
                egui::OutputCommand::OpenUrl(open_url) => {
                    if let Err(e) = webbrowser::open(&open_url.url) {
                        log::warn!("Failed to open URL {}: {:?}", open_url.url, e);
                    }
                }
            };
        }
    }

    unsafe fn create_render_target_for_swap_chain(
        device: &ID3D11Device,
        swap_chain: &IDXGISwapChain,
    ) -> windows::core::Result<ID3D11RenderTargetView> {
        let mut desc = D3D11_TEXTURE2D_DESC::default();
        let swap_chain_texture = unsafe { swap_chain.GetBuffer::<ID3D11Texture2D>(0) }?;
        unsafe { swap_chain_texture.GetDesc(&mut desc) };
        let mut render_target = None;
        unsafe {
            device.CreateRenderTargetView(&swap_chain_texture, None, Some(&mut render_target))
        }?;
        match render_target {
            Some(rt) => Ok(rt),
            None => {
                log::error!("CreateRenderTargetView succeeded but returned no render target");
                Err(windows::core::Error::new(HRESULT(0), "Missing render target"))
            }
        }
    }

    unsafe fn get_device_and_context(
        swap_chain: &IDXGISwapChain,
    ) -> windows::core::Result<(ID3D11Device, ID3D11DeviceContext)> {
        unsafe {
            match swap_chain.GetDevice::<ID3D11Device>() {
                Ok(device) => match device.GetImmediateContext() {
                    Ok(device_ctx) => Ok((device, device_ctx)),
                    Err(e) => Err(e),
                },
                Err(e) => Err(e),
            }
        }
    }

    // Hooked function
    fn present_hook(
        swap_chain_vtbl: *const IDXGISwapChain_Vtbl,
        sync_interval: u32,
        flags: DXGI_PRESENT,
    ) -> HRESULT {
        let overlay_handler = &raw mut OVERLAY_HANDLER;
        if let Some(overlay_handler) = unsafe { &mut *overlay_handler } {
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
        let overlay_handler = &raw mut OVERLAY_HANDLER;
        if let Some(overlay_handler) = unsafe { &mut *overlay_handler } {
            if let Some(inner) = &mut overlay_handler.inner.as_mut() {
                inner.render_target = None;
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

                let swap_chain: &IDXGISwapChain = unsafe { mem::transmute(&swap_chain_vtbl) };
                let device = unsafe { inner.device_context.GetDevice().unwrap() };

                let render_target = unsafe {
                    Self::create_render_target_for_swap_chain(&device, swap_chain).unwrap()
                };

                inner.render_target = Some(render_target);
                return result;
            }
        }
        HRESULT(0)
    }

    // Hooked function
    fn window_process_hook(hwnd: HWND, umsg: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
        let overlay_handler = &raw mut OVERLAY_HANDLER;

        if let Some(overlay_handler) = unsafe { &mut *overlay_handler } {
            if let Some(inner) = &mut overlay_handler.inner {
                let input = inner.input_handler.process(umsg, wparam.0, lparam.0);

                if umsg == WM_CLOSE {
                    overlay_handler
                        .egui_ctx
                        .memory_mut(|writer| overlay_handler.overlay.save(writer));
                    return unsafe { (inner.window_process_callback)(hwnd, umsg, wparam, lparam) };
                }

                if let Some(options) = overlay_handler
                    .overlay
                    .window_process(&input, &inner.input_handler.events)
                {
                    // If user doesn't want to steal the input
                    if options.should_input_pass_through {
                        return unsafe {
                            (inner.window_process_callback)(hwnd, umsg, wparam, lparam)
                        };
                    }

                    // Let overlay capture input only if in focus
                    match input {
                        InputResult::MouseMove
                        | InputResult::MouseLeft
                        | InputResult::MouseRight
                        | InputResult::MouseMiddle
                        | InputResult::Zoom
                        | InputResult::Scroll => {
                            if options.should_capture_all_input
                                || overlay_handler.egui_ctx.wants_pointer_input()
                                || overlay_handler.egui_ctx.is_pointer_over_area()
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
                        } else {
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
    let overlay_handler = &raw mut OVERLAY_HANDLER;
    match unsafe { &*overlay_handler } {
        Some(_) => Err(OverlayError::AlreadyInitialized),
        None => unsafe {
            let egui_ctx = Context::default();
            // Initialize system theme
            egui_ctx.options_mut(|opts| {
                if let Some(theme) = InputHandler::get_system_theme() {
                    opts.fallback_theme = theme
                }
            });

            let overlay = overlay_creator.call_once((egui_ctx.clone(),));
            if let Err(e) = EnableMouseInPointer(true) {
                log::warn!("EnableMouseInPointer failed: {:?}", e);
            }

            let overlay_handler = OverlayHandler {
                inner: None,
                egui_ctx,
                backup: BackupState::default(),
                overlay: Box::new(overlay),
                zoom_factor: 1.0,
            };
            OVERLAY_HANDLER = Some(overlay_handler);
            if let Err(e) = Present_Detour.initialize(
                mem::transmute(present_target),
                OverlayHandler::<dyn Overlay>::present_hook,
            ) {
                log::error!("Failed to initialize Present_Detour: {:?}", e);
                return Err(OverlayError::InitializationFailed);
            }
            if let Err(e) = Resize_Buffers_Detour.initialize(
                mem::transmute(resize_buffers_target),
                OverlayHandler::<dyn Overlay>::resize_buffers_hook,
            ) {
                log::error!("Failed to initialize Resize_Buffers_Detour: {:?}", e);
                return Err(OverlayError::InitializationFailed);
            }
            if let Err(e) = Present_Detour.enable() {
                log::error!("Failed to enable Present_Detour: {:?}", e);
                return Err(OverlayError::InitializationFailed);
            }
            if let Err(e) = Resize_Buffers_Detour.enable() {
                log::error!("Failed to enable Resize_Buffers_Detour: {:?}", e);
                return Err(OverlayError::InitializationFailed);
            }
            Ok(())
        },
    }
}
