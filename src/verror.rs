//use crate::ini::Ini;
use std::{fmt, error};

#[derive(Debug)]
pub enum VError {
    FileNotFound(String),
    LoadCfg(ini::ini::Error),
    LackOfRequiredParameter(String, String),
    VNGLRequiredAttribute(String, String),// (tag name, attribute name)
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
            VError::VNGLRequiredAttribute(ref tag_name, ref attr_name) =>
                write!(f,
                       "VNGLRequiredAttribute: A VNGL component {} requires the attribute {}",
                       tag_name, attr_name),
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
            VError::VNGLRequiredAttribute(_, _) => "Deprecated.",
            VError::Other(ref err) => &err,
        }
    }
    fn cause(&self) -> Option<&error::Error> {
        match *self {
            VError::FileNotFound(ref _err) => None,
            VError::LoadCfg(ref err) => Some(err),
            VError::LackOfRequiredParameter(ref _param, ref _filename) => None,
            VError::VNGLRequiredAttribute(_, _) => None,
            VError::Other(ref _err) => None,
        }
    }
}

pub trait MayNecessary<T> {
    fn required(self, tname: &str, aname: &str) -> OrError<T>;
}

impl<T> MayNecessary<T> for Option<T> {
    fn required(self, tname: &str, aname: &str) -> OrError<T> {
        self.ok_or(VError::VNGLRequiredAttribute(tname.to_string(),
                                                 aname.to_string()))
    }
}
