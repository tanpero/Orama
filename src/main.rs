mod ast;
mod evaluator;
mod lexer;
mod parser;
mod repl;
mod runtime;
mod stdlib;
mod token;
mod typechecker;

use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        // 如果提供了文件名，则运行文件
        let filename = &args[1];
        run_file(filename);
    } else {
        // 否则启动 REPL
        repl::run_repl();
    }
}

fn run_file(filename: &str) {
    match fs::read_to_string(filename) {
        Ok(source) => {
            match lexer::lex(&source) {
                Ok(tokens) => {
                    match parser::parse(tokens) {
                        Ok(program) => {
                            // 创建标准库环境
                            let stdlib_env = stdlib::create_stdlib();
                            let mut evaluator =
                                evaluator::Evaluator::with_environment(stdlib_env);

                            // 暂时跳过类型检查，直接执行程序
                            match evaluator.evaluate(&program) {
                                Ok(_) => {}
                                Err(e) => {
                                    eprintln!("运行时错误: {}", e);
                                    process::exit(1);
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!("语法错误: {}", e);
                            process::exit(1);
                        }
                    }
                }
                Err(e) => {
                    eprintln!("词法错误: {}", e);
                    process::exit(1);
                }
            }
        }
        Err(e) => {
            eprintln!("无法读取文件 '{}': {}", filename, e);
            process::exit(1);
        }
    }
}

// 在文件顶部添加
#[cfg(test)]
mod tests;
