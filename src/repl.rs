use crate::lexer;
use crate::parser;
use crate::ast::{Expr, Literal, Stmt, Program, BinaryOp, UnaryOp, TypeAnnotation, TypeDefinition};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use colored::*;

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

                // 如果有完整的输入，则进行词法分析和语法分析
                if !continuation {
                    match lexer::lex(&input_buffer) {
                        Ok(tokens) => {
                            println!("{}", "词法分析结果:".green().bold());
                            for token in &tokens {
                                println!("  {}", token);
                            }
                            
                            // 进行语法分析
                            match parser::parse(tokens) {
                                Ok(program) => {
                                    println!("\n{}", "语法分析结果 (AST):".green().bold());
                                    print_program(&program);
                                }
                                Err(e) => {
                                    println!("{}: {}", "语法错误".red().bold(), e);
                                }
                            }
                        }
                        Err(e) => {
                            println!("{}: {}", "词法错误".red().bold(), e);
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

// 格式化输出 Program
fn print_program(program: &Program) {
    for (i, stmt) in program.statements.iter().enumerate() {
        if i > 0 {
            println!();
        }
        print_stmt(stmt, 0);
    }
}

// 格式化输出 Stmt
fn print_stmt(stmt: &Stmt, indent: usize) {
    let indent_str = "  ".repeat(indent);
    match stmt {
        Stmt::VariableDecl(name, expr) => {
            println!("{}{}({}) = ", indent_str, "变量声明".blue().bold(), name.yellow());
            print_expr(expr, indent + 1);
        }
        Stmt::FunctionDecl(name, params, body) => {
            println!("{}{}({}) 参数: ", indent_str, "函数声明".blue().bold(), name.yellow());
            for param in params {
                println!("{}  {} {}", indent_str, "参数:".cyan(), param.name.yellow());
                if let Some(type_ann) = &param.type_annotation {
                    print!("{}    {}: ", indent_str, "类型".cyan());
                    print_type_annotation(type_ann, indent + 2);
                    println!();
                }
            }
            println!("{}  {}", indent_str, "函数体:".cyan());
            print_expr(body, indent + 2);
        }
        Stmt::EffectDecl(name, type_params, signatures) => {
            print!("{}{}({}) ", indent_str, "效应声明".blue().bold(), name.yellow());
            if let Some(params) = type_params {
                print!("类型参数: [{}] ", params.iter().map(|p| p.yellow().to_string()).collect::<Vec<_>>().join(", "));
            }
            println!();
            for sig in signatures {
                println!("{}  操作: {} 类型: ", indent_str, sig.name.yellow());
                print!("{}    ", indent_str);
                print_function_type(&sig.function_type, indent + 2);
                println!();
            }
        }
        Stmt::TypeDecl(name, type_params, type_def) => {
            print!("{}{}({}) ", indent_str, "类型声明".blue().bold(), name.yellow());
            if let Some(params) = type_params {
                print!("类型参数: [{}] ", params.iter().map(|p| p.yellow().to_string()).collect::<Vec<_>>().join(", "));
            }
            println!();
            match type_def {
                TypeDefinition::Union(variants) => {
                    println!("{}  {}", indent_str, "联合类型:".cyan());
                    for variant in variants {
                        print!("{}    变体: {}", indent_str, variant.name.yellow());
                        if let Some(params) = &variant.params {
                            println!(" 参数:");
                            for param in params {
                                println!("{}      {} {}", indent_str, "参数:".cyan(), param.name.yellow());
                                if let Some(type_ann) = &param.type_annotation {
                                    print!("{}        {}: ", indent_str, "类型".cyan());
                                    print_type_annotation(type_ann, indent + 4);
                                    println!();
                                }
                            }
                        } else {
                            println!();
                        }
                    }
                }
                TypeDefinition::Record(fields) => {
                    println!("{}  {}", indent_str, "记录类型:".cyan());
                    for field in fields {
                        print!("{}    字段: {} 类型: ", indent_str, field.name.yellow());
                        print_type_annotation(&field.type_annotation, indent + 3);
                        println!();
                    }
                }
            }
        }
        Stmt::Expression(expr) => {
            println!("{}{}", indent_str, "表达式:".blue().bold());
            print_expr(expr, indent + 1);
        }
    }
}

// 格式化输出 Expr
fn print_expr(expr: &Expr, indent: usize) {
    let indent_str = "  ".repeat(indent);
    match expr {
        Expr::Literal(lit) => {
            match lit {
                Literal::Number(n) => println!("{}{}: {}", indent_str, "数字".magenta(), n),
                Literal::String(s) => println!("{}{}: \"{}\"", indent_str, "字符串".magenta(), s),
                Literal::Boolean(b) => println!("{}{}: {}", indent_str, "布尔值".magenta(), b),
                Literal::Array(items) => {
                    println!("{}{}: [", indent_str, "数组".magenta());
                    for item in items {
                        print_expr(item, indent + 1);
                    }
                    println!("{}]", indent_str);
                }
                Literal::Object(props) => {
                    println!("{}{}: {{", indent_str, "对象".magenta());
                    for (key, value) in props {
                        println!("{}  {}: ", indent_str, key.yellow());
                        print_expr(value, indent + 2);
                    }
                    println!("{}}}", indent_str);
                }
                Literal::Null => println!("{}{}", indent_str, "null".magenta()),
            }
        }
        Expr::Variable(name) => {
            println!("{}{}: {}", indent_str, "变量".magenta(), name.yellow());
        }
        Expr::Function(params, body) => {
            println!("{}{}: ", indent_str, "函数表达式".magenta());
            for param in params {
                println!("{}  {}: {}", indent_str, "参数".cyan(), param.name.yellow());
                if let Some(type_ann) = &param.type_annotation {
                    print!("{}    {}: ", indent_str, "类型".cyan());
                    print_type_annotation(type_ann, indent + 2);
                    println!();
                }
            }
            println!("{}  {}: ", indent_str, "函数体".cyan());
            print_expr(body, indent + 2);
        }
        Expr::Call(callee, args) => {
            println!("{}{}: ", indent_str, "函数调用".magenta());
            println!("{}  {}: ", indent_str, "被调用者".cyan());
            print_expr(callee, indent + 2);
            println!("{}  {}: ", indent_str, "参数".cyan());
            for arg in args {
                print_expr(arg, indent + 2);
            }
        }
        Expr::Binary(left, op, right) => {
            println!("{}{}: ", indent_str, "二元操作".magenta());
            println!("{}  {}: {}", indent_str, "操作符".cyan(), format_binary_op(op));
            println!("{}  {}: ", indent_str, "左操作数".cyan());
            print_expr(left, indent + 2);
            println!("{}  {}: ", indent_str, "右操作数".cyan());
            print_expr(right, indent + 2);
        }
        Expr::Unary(op, operand) => {
            println!("{}{}: ", indent_str, "一元操作".magenta());
            println!("{}  {}: {}", indent_str, "操作符".cyan(), format_unary_op(op));
            println!("{}  {}: ", indent_str, "操作数".cyan());
            print_expr(operand, indent + 2);
        }
        Expr::If(condition, then_branch, else_branch) => {
            println!("{}{}: ", indent_str, "条件表达式".magenta());
            println!("{}  {}: ", indent_str, "条件".cyan());
            print_expr(condition, indent + 2);
            println!("{}  {}: ", indent_str, "then分支".cyan());
            print_expr(then_branch, indent + 2);
            if let Some(else_expr) = else_branch {
                println!("{}  {}: ", indent_str, "else分支".cyan());
                print_expr(else_expr, indent + 2);
            }
        }
        Expr::Block(stmts, final_expr) => {
            println!("{}{}: ", indent_str, "块表达式".magenta());
            if !stmts.is_empty() {
                println!("{}  {}: ", indent_str, "语句".cyan());
                for stmt in stmts {
                    print_stmt(stmt, indent + 2);
                }
            }
            if let Some(expr) = final_expr {
                println!("{}  {}: ", indent_str, "返回表达式".cyan());
                print_expr(expr, indent + 2);
            }
        }
        Expr::Match(expr, cases) => {
            println!("{}{}: ", indent_str, "匹配表达式".magenta());
            println!("{}  {}: ", indent_str, "匹配值".cyan());
            print_expr(expr, indent + 2);
            println!("{}  {}: ", indent_str, "分支".cyan());
            for case in cases {
                println!("{}    {}: {} ", indent_str, "模式".cyan(), case.pattern.name.yellow());
                if let Some(params) = &case.pattern.params {
                    print!("参数: [");
                    for (i, param) in params.iter().enumerate() {
                        if i > 0 {
                            print!(", ");
                        }
                        print!("{}", param.name.yellow());
                    }
                    println!("]");
                }
                println!("{}    {}: ", indent_str, "分支体".cyan());
                print_expr(&case.body, indent + 3);
            }
        }
        Expr::Perform(effect, operation, args) => {
            println!("{}{}: ", indent_str, "效应操作".magenta());
            println!("{}  效应: {} 操作: {}", indent_str, effect.yellow(), operation.yellow());
            if !args.is_empty() {
                println!("{}  {}: ", indent_str, "参数".cyan());
                for arg in args {
                    print_expr(arg, indent + 2);
                }
            }
        }
        Expr::Handle(expr, handlers, return_handler) => {
            println!("{}{}: ", indent_str, "效应处理".magenta());
            println!("{}  {}: ", indent_str, "表达式".cyan());
            print_expr(expr, indent + 2);
            println!("{}  {}: ", indent_str, "处理器".cyan());
            for handler in handlers {
                println!("{}    效应: {}", indent_str, handler.effect_name.yellow());
                for op in &handler.operations {
                    println!("{}      操作: {} 参数: [{}]", 
                        indent_str, 
                        op.name.yellow(),
                        op.params.iter().map(|p| p.name.yellow().to_string()).collect::<Vec<_>>().join(", ")
                    );
                    println!("{}        {}: ", indent_str, "操作体".cyan());
                    print_expr(&op.body, indent + 4);
                }
            }
            if let Some(ret_handler) = return_handler {
                println!("{}  {}: {} ", indent_str, "返回处理".cyan(), ret_handler.param.yellow());
                println!("{}    {}: ", indent_str, "处理体".cyan());
                print_expr(&ret_handler.body, indent + 2);
            }
        }
        Expr::Pipe(left, right) => {
            println!("{}{}: ", indent_str, "管道表达式".magenta());
            println!("{}  {}: ", indent_str, "左表达式".cyan());
            print_expr(left, indent + 2);
            println!("{}  {}: ", indent_str, "右表达式".cyan());
            print_expr(right, indent + 2);
        }
    }
}

// 格式化输出 TypeAnnotation
fn print_type_annotation(type_ann: &TypeAnnotation, indent: usize) {
    let indent_str = "  ".repeat(indent);
    match type_ann {
        TypeAnnotation::Simple(name, args) => {
            print!("{}", name.green());
            if !args.is_empty() {
                print!("<");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    print_type_annotation(arg, indent);
                }
                print!(">");
            }
        }
        TypeAnnotation::Function(func_type) => {
            print_function_type(func_type, indent);
        }
        TypeAnnotation::Effect(effects, return_type) => {
            print!("<");
            for (i, effect) in effects.iter().enumerate() {
                if i > 0 {
                    print!(", ");
                }
                print!("{}", effect.green());
            }
            print!("> ");
            print_type_annotation(return_type, indent);
        }
    }
}

// 格式化输出 FunctionType
fn print_function_type(func_type: &crate::ast::FunctionType, indent: usize) {
    print!("(");
    for (i, param) in func_type.params.iter().enumerate() {
        if i > 0 {
            print!(", ");
        }
        print_type_annotation(param, indent);
    }
    print!(") => ");
    print_type_annotation(&func_type.return_type, indent);
}

// 格式化二元操作符
fn format_binary_op(op: &BinaryOp) -> ColoredString {
    match op {
        BinaryOp::Add => "+".bright_cyan(),
        BinaryOp::Subtract => "-".bright_cyan(),
        BinaryOp::Multiply => "*".bright_cyan(),
        BinaryOp::Divide => "/".bright_cyan(),
        BinaryOp::Modulo => "%".bright_cyan(),
        BinaryOp::Equal => "==".bright_cyan(),
        BinaryOp::NotEqual => "!=".bright_cyan(),
        BinaryOp::Less => "<".bright_cyan(),
        BinaryOp::Greater => ">".bright_cyan(),
        BinaryOp::LessEqual => "<=".bright_cyan(),
        BinaryOp::GreaterEqual => ">=".bright_cyan(),
        BinaryOp::And => "and".bright_cyan(),
        BinaryOp::Or => "or".bright_cyan(),
    }
}

// 格式化一元操作符
fn format_unary_op(op: &UnaryOp) -> ColoredString {
    match op {
        UnaryOp::Negate => "-".bright_cyan(),
        UnaryOp::Not => "not".bright_cyan(),
    }
}