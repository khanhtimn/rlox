use thiserror::Error;

use crate::interpreter::RuntimeError;
use crate::parser::ParseErrors;
use crate::scanner::ScanErrors;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Scan(#[from] ScanErrors),
    #[error(transparent)]
    Parse(#[from] ParseErrors),
    #[error(transparent)]
    Runtime(#[from] RuntimeError),
}

pub type Result<T> = std::result::Result<T, Error>;
