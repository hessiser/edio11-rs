use crate::errors::OverlayError;
use std::{
    ffi::c_void,
    mem::{self},
    ptr::null_mut,
};
use widestring::u16str;
use windows::{Win32::Graphics::Direct3D::{D3D_FEATURE_LEVEL_10_0, D3D_FEATURE_LEVEL_11_1}, core::PCWSTR};
use windows::{
    Win32::{
        Foundation::HMODULE,
        Graphics::{
            Direct3D::{
                D3D_DRIVER_TYPE_HARDWARE, D3D_DRIVER_TYPE_UNKNOWN, D3D_FEATURE_LEVEL_10_1,
                D3D_FEATURE_LEVEL_11_0,
            },
            Direct3D11::{
                D3D11_CREATE_DEVICE_FLAG, D3D11_SDK_VERSION, D3D11CreateDeviceAndSwapChain,
                ID3D11Device, ID3D11DeviceContext,
            },
            Dxgi::{
                Common::{
                    DXGI_FORMAT_R8G8B8A8_UNORM, DXGI_MODE_DESC, DXGI_MODE_SCALING_UNSPECIFIED,
                    DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED, DXGI_RATIONAL, DXGI_SAMPLE_DESC,
                },
                CreateDXGIFactory, DXGI_SWAP_CHAIN_DESC, DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH,
                DXGI_SWAP_EFFECT_DISCARD, DXGI_USAGE_RENDER_TARGET_OUTPUT, IDXGIFactory,
                IDXGISwapChain,
            },
        },
        UI::WindowsAndMessaging::{
            CS_HREDRAW, CS_VREDRAW, CreateWindowExW, DefWindowProcW, DestroyWindow,
            RegisterClassExW, UnregisterClassW, WINDOW_EX_STYLE, WNDCLASSEXW, WS_OVERLAPPEDWINDOW,
        },
    },
    core::{Interface, w},
};

// rdbo Kiero
// https://github.com/eugen15/directx-present-hook
pub fn get_vtable() -> Result<Box<[usize; 205]>, OverlayError> {
    // Initializes a dummy swapchain to get the vtable
    unsafe {
        let window_class = WNDCLASSEXW {
            cbSize: mem::size_of::<WNDCLASSEXW>() as _,
            style: CS_HREDRAW | CS_VREDRAW,
            // This is fine as the wrapper sig matches the imported sig
            lpfnWndProc: Some(mem::transmute(DefWindowProcW as *const c_void)),
            lpszClassName: PCWSTR::from_raw(u16str!(env!("CARGO_PKG_NAME")).as_ptr()),
            ..Default::default()
        };

        RegisterClassExW(&window_class);

        let window = CreateWindowExW(
            WINDOW_EX_STYLE::default(),
            window_class.lpszClassName,
            PCWSTR::from_raw(u16str!(env!("CARGO_PKG_NAME")).as_ptr()),
            WS_OVERLAPPEDWINDOW,
            0,
            0,
            100,
            100,
            None,
            None,
            Some(window_class.hInstance),
            None,
        )?;

        let mut feature_levels = [
            D3D_FEATURE_LEVEL_10_1,
            D3D_FEATURE_LEVEL_10_0,
            D3D_FEATURE_LEVEL_11_0,
            D3D_FEATURE_LEVEL_11_1,
        ];

        let refresh_rate = DXGI_RATIONAL {
            Numerator: 60,
            Denominator: 1,
        };

        let buffer_desc = DXGI_MODE_DESC {
            Width: 100,
            Height: 100,
            RefreshRate: refresh_rate,
            Format: DXGI_FORMAT_R8G8B8A8_UNORM,
            ScanlineOrdering: DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED,
            Scaling: DXGI_MODE_SCALING_UNSPECIFIED,
        };

        let sample_desc = DXGI_SAMPLE_DESC {
            Count: 1,
            Quality: 0,
        };

        let swap_chain_desc = DXGI_SWAP_CHAIN_DESC {
            BufferDesc: buffer_desc,
            SampleDesc: sample_desc,
            BufferUsage: DXGI_USAGE_RENDER_TARGET_OUTPUT,
            BufferCount: 1,
            OutputWindow: window,
            Windowed: true.into(),
            SwapEffect: DXGI_SWAP_EFFECT_DISCARD,
            Flags: DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH.0 as u32,
        };

        let mut swap_chain: Option<IDXGISwapChain> = None;
        let mut device: Option<ID3D11Device> = None;
        let mut context: Option<ID3D11DeviceContext> = None;

        let mut created = D3D11CreateDeviceAndSwapChain(
            None,
            D3D_DRIVER_TYPE_HARDWARE,
            HMODULE(null_mut()),
            D3D11_CREATE_DEVICE_FLAG(0),
            Some(&feature_levels.clone()),
            D3D11_SDK_VERSION,
            Some(&swap_chain_desc),
            Some(&mut swap_chain),
            Some(&mut device),
            Some(feature_levels.as_mut_ptr()),
            Some(&mut context),
        )
        .is_ok();

        if !created {
            let mut adapters = Vec::new();
            if let Ok(factory) = CreateDXGIFactory::<IDXGIFactory>() {
                let mut adapter_index = 0;
                loop {
                    match factory.EnumAdapters(adapter_index) {
                        Ok(_adapter) => {
                            adapters.push(_adapter);
                            adapter_index += 1;
                        }
                        Err(_) => break,
                    }
                }
            } else {
                log::error!("Failed to create DXGI Factory, cannot enumerate adapters");
            }

            for (i, adapter) in adapters.iter().enumerate() {
                let mut feature_levels = [
                    D3D_FEATURE_LEVEL_10_1,
                    D3D_FEATURE_LEVEL_10_0,
                    D3D_FEATURE_LEVEL_11_0,
                    D3D_FEATURE_LEVEL_11_1,
                ];

                match D3D11CreateDeviceAndSwapChain(
                    adapter,
                    D3D_DRIVER_TYPE_UNKNOWN,
                    HMODULE(null_mut()),
                    D3D11_CREATE_DEVICE_FLAG(0),
                    Some(&feature_levels.clone()),
                    D3D11_SDK_VERSION,
                    Some(&swap_chain_desc),
                    Some(&mut swap_chain),
                    Some(&mut device),
                    Some(feature_levels.as_mut_ptr()),
                    Some(&mut context),
                ) {
                    Ok(_) => {
                        created = true;
                        break;
                    }
                    Err(e) => {
                        if i == adapters.len() - 1 {
                            return Err(OverlayError::DeviceCreation(e));
                        }
                    }
                }
            }
        }

        if !created {
            return Err(OverlayError::DeviceCreationFailed);
        }

        let mut vtable = Box::new([0usize; 205]);

        let swap_chain_ptr = swap_chain
            .as_ref()
            .ok_or(OverlayError::SwapChainNotInitialized)?;
        let swap_chain_vtable = Interface::vtable(swap_chain_ptr);

        let device_ptr = device.as_ref().ok_or(OverlayError::DeviceNotInitialized)?;
        let device_vtable = Interface::vtable(device_ptr);

        let context_ptr = context
            .as_ref()
            .ok_or(OverlayError::DeviceContextNotInitialized)?;
        let context_vtable = Interface::vtable(context_ptr);

        const SWAP_CHAIN_VTABLE_LEN: usize = 18;
        const DEVICE_VTABLE_LEN: usize = 43;
        const CONTEXT_VTABLE_LEN: usize = 144;

        // const DEVICE_VTABLE_OFFSET: usize = SWAP_CHAIN_VTABLE_LEN;
        // const CONTEXT_VTABLE_OFFSET: usize =
        //     DEVICE_VTABLE_OFFSET + DEVICE_VTABLE_LEN;

        std::ptr::copy_nonoverlapping(
            mem::transmute(swap_chain_vtable),
            vtable.as_mut_ptr(),
            SWAP_CHAIN_VTABLE_LEN,
        );

        std::ptr::copy_nonoverlapping(
            mem::transmute(&device_vtable),
            vtable[SWAP_CHAIN_VTABLE_LEN..].as_mut_ptr(),
            DEVICE_VTABLE_LEN,
        );

        std::ptr::copy_nonoverlapping(
            mem::transmute(context_vtable),
            vtable[SWAP_CHAIN_VTABLE_LEN + DEVICE_VTABLE_LEN..].as_mut_ptr(),
            CONTEXT_VTABLE_LEN,
        );

        DestroyWindow(window)?;
        UnregisterClassW(window_class.lpszClassName, Some(window_class.hInstance))?;

        Ok(vtable)
    }
}
