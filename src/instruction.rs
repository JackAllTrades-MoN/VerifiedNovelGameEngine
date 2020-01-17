use crate::project::Scene;
use crate::verror::{OrError, OrError_, VError};

use combine::parser::char::{spaces, digit, char};
use combine::{many1, Parser, token, between};
//use combine::stream::easy;

#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    UpdateText(String),
    LoadLayout(String),
}


impl Instruction {
    fn build(name: &str, arg: &str) -> OrError<Instruction> {
        match name {
            "layout" => Ok(Instruction::LoadLayout(arg.to_string())),
            _ => Err(VError::Other("unimplemented operator".to_string()))
        }
    }
    fn parse_line(scene: &str) -> OrError_<(Instruction, String)> {
        let mut nonsymbol = ||
            combine::satisfy(|c| c != ':' && c != '\\' && c != '[' && c != ']');
        let mut opname =
            many1::<String, _>(nonsymbol())
            .skip(token(':'))
            .map(|op: String| if op == "layout" { Ok(op) }
                 else {Result::Err(VError::Other("invalid operation".to_string()))});
        let skip_spaces = || spaces().silent();
        let oparg = many1::<String, _>(nonsymbol())
            .skip(skip_spaces());
        let mut instr =
            between(token('['), token(']'),
                    opname.skip(skip_spaces())
                    .and(oparg)
                    .map(|(name, arg)| Instruction::build(&name?, &arg)));
        let (inst, rest) = instr.easy_parse(scene)?;
        Ok((inst?, rest.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test1() -> OrError<()> {
        let (inst, rest) = Instruction::parse_line("[layout: title.vngl]")?;
        assert_eq!(inst, Instruction::LoadLayout("title.vngl".to_string()));
        assert_eq!(rest, "".to_string());
        Ok(())
    }
    #[test]
    fn test2() -> OrError<()> {
        let (inst, rest) = Instruction::parse_line("[layout:title.vngl]")?;
        assert_eq!(inst, Instruction::LoadLayout("title.vngl".to_string()));
        assert_eq!(rest, "".to_string());
        Ok(())
    }
}
