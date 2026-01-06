use thiserror::Error;

#[derive(Error, Debug)]
pub enum OverlayError {
    #[error("The overlay was already initialized")]
    AlreadyInitialized,
    #[error("Unrecoverable error occured {0}")]
    General(&'static str),
    #[error("Windows error {0}")]
    Win(#[from] windows::core::Error),
    #[error("Initialization failed")]
    InitializationFailed,
}
