//use crate::ini::Ini;
use std::{fmt, error};

use crate::vm::memory;

type CombErr = String;
//    combine::stream::easy::Errors<char, String, combine::stream::PointerOffset>;

#[derive(Debug)]
pub enum VError {
    FileNotFound(String),
//    LoadCfg(ini::ini::Error),
    LackOfRequiredParameter(String, String),
    VNGLRequiredAttribute(String, String),// (tag name, attribute name)
    CfgRequiredSection(String, String), // (filename, section name)
    CfgRequiredAttribute(String, String, String), // (filename, section name, attribute name)
//    IniError(ini::ini::Error),
    TomlError(toml::de::Error),
    IOError(std::io::Error),
    CombError(CombErr),
    MemoryError(memory::Error),
    Unimplemented(&'static str),
    //CombError(combine::stream::easy::ParseError<String>),
    Other(String)
}

//pub type OrError<T> = Result<T, VError>;
pub type OrError<T> = Result<T, VError>;
//pub type OrError_<'a, T> = Result<T, VError<'a>>;

impl fmt::Display for VError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            VError::FileNotFound(ref err) => write!(f, "FileNotFound: {}", err),
//            VError::LoadCfg(ref err) => write!(f, "LoadConfigError: {}", err),
            VError::LackOfRequiredParameter(ref param, ref filename) =>
                write!(f,
                       "Parameter {} doesn't exist in the given configuration file: {}",
                       param, filename),
            VError::VNGLRequiredAttribute(ref tag_name, ref attr_name) =>
                write!(f,
                       "VNGLRequiredAttribute: A VNGL component {} requires the attribute {}",
                       tag_name, attr_name),
            VError::CfgRequiredSection(ref filename, ref section) =>
                write!(f,
                       "CfgRequiredSection: A project file {} requires a section {}", filename, section),
            VError::CfgRequiredAttribute(ref filename, ref section, ref aname) =>
                write!(f,
                       "CfgRequiredAttribute: A section {} in the project file {} requires the attribute {}", filename, section, aname),
//            VError::IniError(ref err) => write!(f, "IniError: {}", err),
            VError::TomlError(ref err) => write!(f, "TomlError: {}", err),
            VError::IOError(ref err) => write!(f, "IOError: {}", err),
            VError::CombError(ref err) => write!(f, "CombError: {}", err),
            VError::MemoryError(ref err) => write!(f, "MemoryError: {}", err),
            VError::Unimplemented(ref msg) => write!(f, "Unimplemented: {}", msg), 
            VError::Other(ref err) => write!(f, "OtherError: {}", err)
        }
    }
}

impl error::Error for VError {
    fn description(&self) -> &str {
        match *self {
            VError::FileNotFound(ref err) => &err,
//            VError::LoadCfg(ref err) => err.description(),
            VError::LackOfRequiredParameter(ref param, ref filename) => &param,
            VError::VNGLRequiredAttribute(_, _) => "Deprecated.",
//            VError::IniError(ref err) => err.description(),
            VError::TomlError(ref err) => err.description(),
            VError::IOError(ref err) => err.description(),
            VError::CombError(ref err) => "Deprecated.",
            VError::Other(ref err) => &err,
            _ => "Deprecated.",
        }
    }
    fn cause(&self) -> Option<&error::Error> {
        match *self {
            VError::FileNotFound(ref _err) => None,
//            VError::LoadCfg(ref err) => Some(err),
            VError::LackOfRequiredParameter(ref _param, ref _filename) => None,
            VError::VNGLRequiredAttribute(_, _) => None,
            VError::CfgRequiredSection(_, _) => None,
            VError::CfgRequiredAttribute(_, _, _) => None,
//            VError::IniError(ref err) => Some(err),
            VError::TomlError(ref err) => Some(err),
            VError::IOError(ref err) => Some(err),
            VError::CombError(ref err) => None,
            VError::MemoryError(ref err) => Some(err),
            VError::Unimplemented(ref _err) => None,
            VError::Other(ref _err) => None,
        }
    }
}
/*
impl<'a> From<ini::ini::Error> for VError {
    fn from(err: ini::ini::Error) -> VError {
        VError::IniError(err)
    }
}*/


impl From<toml::de::Error> for VError {
    fn from(err: toml::de::Error) -> VError {
        VError::TomlError(err)
    }
}

impl<'a> From<std::io::Error> for VError {
    fn from(err: std::io::Error) -> VError {
        VError::IOError(err)
    }
}

impl<'a> From<combine::stream::easy::Errors<char, &str, combine::stream::PointerOffset>> for VError {
    fn from(err: combine::stream::easy::Errors<char, &str, combine::stream::PointerOffset>) -> VError {
        let it = err.errors.iter();
        let errs = it.fold("".to_string(), |errs, err| errs + &format!("{}\n", err));
        VError::CombError(errs)
    }
}

pub trait MayNecessary<T> {
    fn required(self, tname: &str, aname: &str) -> OrError<T>;
    fn csrequired(self, filename: &str, sname: &str) -> OrError<T>;
    fn carequired(self, filename: &str, sname: &str, aname: &str) -> OrError<T>;
}

impl<T> MayNecessary<T> for Option<T> {
    fn required(self, tname: &str, aname: &str) -> OrError<T> {
        self.ok_or(VError::VNGLRequiredAttribute(tname.to_string(),
                                                 aname.to_string()))
    }
    fn csrequired(self, filename: &str, sname: &str) -> OrError<T> {
        self.ok_or(VError::CfgRequiredSection(filename.to_string(),
                                              sname.to_string()))
    }
    fn carequired(self, filename: &str, sname: &str, aname: &str) -> OrError<T> {
        self.ok_or(VError::CfgRequiredAttribute(filename.to_string(),
                                                sname.to_string(),
                                                aname.to_string()))
    }
}
