
use combine::parser::char::{spaces, digit, char};
use combine::{many1, Parser, token, between, choice, parser};

use crate::verror::{VError, OrError, OrError_};
use crate::project::Component;
use crate::interpreter::script::Script;
use crate::interpreter::instr::Instruction;

pub fn op_builder(name: &str) -> OrError<fn(&str) -> Instruction> {
    match name {
        "quit" => Ok(move |_: &str| Instruction::Quit),
        _ => Err(VError::Unimplemented("operator")),
    }
}

pub fn update_text(_txt: &str) -> Instruction {
    Instruction::Quit
}

pub fn parse_line (src: &str) -> OrError_<(Instruction, String)> {
    let mut nonsymbol = ||
        combine::satisfy(|c| c != ':' && c != '\\' && c != '[' && c != ']');
    let mut opname =
        many1::<String, _>(nonsymbol())
        .skip(token(':'))
        .map(|op: String| { op_builder(&op) });
    let skip_spaces = || spaces().silent();
    let oparg = many1::<String, _>(nonsymbol()).skip(skip_spaces());
    let mut instr =
        between(token('['), token(']'),
                opname.skip(skip_spaces())
                .and(oparg)
                .map(|(builder, arg)| builder.map(|b| b(&arg))));
    let mut dialog = many1::<String, _>(nonsymbol())
        .map(|s| Ok(update_text(&s)));
    let mut line = spaces().with(choice((instr, dialog)));
    let (inst, rest) = line.easy_parse(src)?;
    Ok((inst?, rest.to_string()))
}

pub fn parse (src: &str) -> OrError_<Vec<Instruction>> {
    let mut buf = Vec::new();
    loop {
        let (instr, rest) = parse_line(src)?;
        buf.push(instr);
        if(rest.len() <= 0) { break; }
    };
    Ok(buf)
}
