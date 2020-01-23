// memo: プロトタイプ版では、scriptの分割ロードみたいなことは保留しておいて、
// メモリ上に一括で読み込んでしまう. ただし、memory.rsの形で実装は遮蔽しておいて
// あとで余裕ができたら分割ロードに対応する.
// memo2: エラーをモジュールごとに分離してfromで対応

use std::{fmt, error};

use super::instr::Instruction;
use crate::verror::VError;

type OrError<T> = Result<T, Error>;

pub enum Memory {
    Memory(Vec<Instruction>),
}

impl Memory {
    pub fn new() -> Memory {
        Memory::Memory(Vec::new())
    }
    pub fn fetch(&self, ip: u64) -> OrError<&Instruction> {
        let Memory::Memory(v) = self;
        v.get(ip as usize)
            .ok_or(Error::OutOfMemory(ip))
    }
}

#[derive(Debug)]
pub enum Error {
    OutOfMemory(u64),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::OutOfMemory(mp) => write!(f, "OutOfMemory at the address {}", mp),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match self { Error::OutOfMemory(_mp) => "Deprecated.", }}
    fn cause(&self) -> Option<&error::Error> {
        match *self { Error::OutOfMemory(_mp) => None, }}
}

impl<'a> From<Error> for VError<'a> {
    fn from(err: Error) -> VError<'static> {
        VError::MemoryError(err)
    }
}
