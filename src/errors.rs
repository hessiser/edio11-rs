use thiserror::Error;

#[derive(Error, Debug)]
pub enum OverlayError {
    #[error("The overlay was already initialized")]
    AlreadyInitialized,
    #[error("Unrecoverable error occured {0}")]
    General(&'static str),
    #[error("Windows error {0}")]
    Win(#[from] windows::core::Error),
    #[error("Failed to create D3D11 device and swap chain: {0}")]
    DeviceCreation(#[source] windows::core::Error),
    #[error("Failed to create D3D11 device and swap chain")]
    DeviceCreationFailed,
    #[error("Failed to initialize D3D11 swap chain")]
    SwapChainNotInitialized,
    #[error("Failed to initialize D3D11 device")]
    DeviceNotInitialized,
    #[error("Failed to initialize D3D11 device context")]
    DeviceContextNotInitialized,
    #[error("Initialization failed")]
    InitializationFailed,
}
