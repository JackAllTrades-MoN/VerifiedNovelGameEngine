// memo: プロトタイプ版では、scriptの分割ロードみたいなことは保留しておいて、
// メモリ上に一括で読み込んでしまう. ただし、memory.rsの形で実装は遮蔽しておいて
// あとで余裕ができたら分割ロードに対応する.

use super::instr::Instruction;

pub enum Memory {
    Memory(Vec<Instruction>),
}


impl Memory {
    pub fn new() -> Memory {
        Memory::Memory(Vec::new())
    }
}
