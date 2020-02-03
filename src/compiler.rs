
use combine::parser::char::{spaces, digit, char};
use combine::{many1, Parser, token, between, choice, parser};

use crate::verror::{VError, OrError};
//use crate::project::Component;
//use crate::vm::script::Script;
use crate::vm::instr::Instruction;

pub fn op_builder(name: &str) -> OrError<fn(&str) -> Instruction> {
    match name {
        "quit" => Ok(move |_: &str| Instruction::Quit),
        _ => Err(VError::Unimplemented("operator")),
    }
}

pub fn update_text(_txt: &str) -> Instruction {
    Instruction::Quit
}

pub fn parse_line (src: &str) -> OrError<(Instruction, String)> {
    let mut nonsymbol = ||
        combine::satisfy(|c| c != ':' && c != '\\' && c != '[' && c != ']');
/*    let skip_spaces = || spaces().silent();
    let opname = || many1::<String, _>(nonsymbol());
    let oparg = many::<String, _>(nonsymbol()).skip(skip_spaces());
    let op = choice::<OrError<Instruction>>((
        opname()
            .skip::<String>(token(':'))
            .map(|op: String| {op_builder(&op) })
            .skip(skip_spaces())
            .and(oparg)
            .map(|(builder, arg)| builder.map(|b| b(&arg))),
        opname().map(|_op: String| Ok(Instruction::Quit))));
    let instr = between(token('['), token(']'), op);
    let mut dialog = many1::<String, _>(nonsymbol())
        .map(|s| Ok(update_text(&s)));
    let mut line = spaces().with(choice((instr, dialog)));*/
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

pub fn parse (src: &str) -> OrError<Vec<Instruction>> {
    let mut buf = Vec::new();
    loop {
        let (instr, rest) = parse_line(src)?;
        buf.push(instr);
        if rest.len() <= 0 { break; }
    };
    Ok(buf)
}
