mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

fn main() {
    repl::run_repl();
}
