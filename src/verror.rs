use crate::ini::Ini;
use std::{fmt, error};

#[derive(Debug)]
pub enum VError {
    FileNotFound(String),
    LoadCfg(ini::ini::Error),
    LackOfRequiredParameter(String, String),
    Other(String)
}

pub type OrError<T> = Result<T, VError>;

impl fmt::Display for VError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            VError::FileNotFound(ref err) => write!(f, "FileNotFound: {}", err),
            VError::LoadCfg(ref err) => write!(f, "LoadConfigError: {}", err),
            VError::LackOfRequiredParameter(ref param, ref filename) =>
                write!(f,
                       "Parameter {} doesn't exist in the given configuration file: {}",
                       param, filename),
            VError::Other(ref err) => write!(f, "OtherError: {}", err)
        }
    }
}

impl error::Error for VError {
    fn description(&self) -> &str {
        match *self {
            VError::FileNotFound(ref err) => &err,
            VError::LoadCfg(ref err) => err.description(),
            VError::LackOfRequiredParameter(ref param, ref filename) => &param,
            VError::Other(ref err) => &err,
        }
    }
    fn cause(&self) -> Option<&error::Error> {
        match *self {
            VError::FileNotFound(ref _err) => None,
            VError::LoadCfg(ref err) => Some(err),
            VError::LackOfRequiredParameter(ref _param, ref _filename) => None,
            VError::Other(ref _err) => None,
        }
    }
}

