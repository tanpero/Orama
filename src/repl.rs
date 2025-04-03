use crate::lexer;
use rustyline::error::ReadlineError;
use rustyline::Editor;

pub fn run_repl() {
    println!("Orama 语言 REPL");
    println!("输入 '.exit' 退出");
    println!("输入行尾使用 '\\' 可以自动续行");

    let mut rl = Editor::<(), rustyline::history::FileHistory>::new().unwrap();
    let mut input_buffer = String::new();
    let mut continuation = false;

    loop {
        let prompt = if continuation { "... " } else { ">>> " };
        match rl.readline(prompt) {
            Ok(line) => {
                if let Err(e) = rl.add_history_entry(line.as_str()) {
                    println!("无法添加历史记录: {:?}", e);
                }

                if line.trim() == ".exit" {
                    break;
                }

                // 检查行尾是否有续行符 '\'
                if line.trim_end().ends_with('\\') {
                    input_buffer.push_str(&line[..line.len() - 1]);
                    continuation = true;
                    continue;
                } else {
                    input_buffer.push_str(&line);
                    continuation = false;
                }

                // 如果有完整的输入，则进行词法分析
                if !continuation {
                    match lexer::lex(&input_buffer) {
                        Ok(tokens) => {
                            println!("Tokens:");
                            for token in tokens {
                                println!("  {}", token);
                            }
                        }
                        Err(e) => {
                            println!("词法错误: {}", e);
                        }
                    }
                    input_buffer.clear();
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("错误: {:?}", err);
                break;
            }
        }
    }
}