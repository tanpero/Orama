use crate::lexer;
use crate::parser;
use crate::evaluator::Evaluator;
use crate::ast::Program;
use crate::stdlib;
use colored::*;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use crate::ast::*;
use crate::typechecker::TypeChecker;
use crate::token::TokenType::{
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
};
use crate::token::Token;
use crate::token::TokenType;


pub fn run_repl() {
    println!("{}", "Orama 语言 REPL".bright_green().bold());
    println!("{}", "输入 '.exit' 退出".cyan());
    println!("{}", "输入行尾使用 '\\' 可以自动续行".cyan());
    println!("{}", "输入 '.ast' 切换 AST 显示模式".cyan());
    println!("{}", "输入 '.eval' 切换求值模式".cyan());

    let mut rl = Editor::<(), rustyline::history::FileHistory>::new().expect("Failed to create line editor");
    let mut input_buffer = String::new();
    let mut continuation = false;
    let mut show_ast = false;
    
    // 创建标准库环境
    let stdlib_env = stdlib::create_stdlib();
    let mut evaluator = Evaluator::with_environment(stdlib_env.clone());

    // 创建并初始化类型检查器，保持其状态
    let mut type_checker = TypeChecker::new();
    type_checker.init_builtins();
    
    // 将标准库函数添加到类型检查器中
    type_checker.init_stdlib(&stdlib_env);
    
    loop {
        let prompt = if continuation { 
            "... ".to_string() 
        } else { 
            ">>> ".to_string() 
        };
        
        // 使用 rustyline 的 colored prompt 支持
        let readline_result = rl.readline_with_initial(&prompt, ("", ""));
        
        // 处理读取输入的结果
        if let Err(e) = &readline_result {
            match e {
                ReadlineError::Interrupted => {
                    println!("CTRL-C");
                    break;
                }
                ReadlineError::Eof => {
                    println!("CTRL-D");
                    break;
                }
                err => {
                    println!("{}: {:?}", "错误".red().bold(), err);
                    break;
                }
            }
        }
        
        // 如果读取成功，处理输入
        let line = match readline_result {
            Ok(line) => line,
            Err(_) => continue, // 已经处理过错误，这里直接跳过
        };
        
        // 添加历史记录，处理可能的错误
        if let Err(e) = rl.add_history_entry(line.as_str()) {
            println!("{}: 无法添加历史记录: {:?}", "警告".yellow().bold(), e);
        }

        // 处理特殊命令
        if line.trim() == ".exit" {
            break;
        } else if line.trim() == ".ast" {
            show_ast = !show_ast;
            println!("AST 显示模式: {}", if show_ast { "开启".green() } else { "关闭".red() });
            continue;
        } else if line.trim() == ".eval" {
            show_ast = false;
            println!("求值模式: {}", "开启".green());
            continue;
        }

        // 处理续行
        if line.trim_end().ends_with('\\') {
            input_buffer.push_str(&line[..line.len() - 1]);
            input_buffer.push('\n'); // 保留换行符
            continuation = true;
            continue;
        } else {
            input_buffer.push_str(&line);
            
            // 检查括号是否匹配，如果不匹配则自动续行
            let tokens_result = lexer::lex(&input_buffer);
            if let Ok(tokens) = tokens_result {
                if needs_continuation(&tokens) {
                    input_buffer.push('\n'); // 保留换行符
                    continuation = true;
                    continue;
                }
            }
            
            continuation = false;
        }

        // 如果不是续行，则处理完整输入
        if continuation {
            continue;
        }
        
        // 检查输入是否为空或只包含空白字符
        if input_buffer.trim().is_empty() {
            input_buffer.clear();
            continue;
        }
        
        // 词法分析
        let tokens_result = lexer::lex(&input_buffer);
        if let Err(e) = &tokens_result {
            println!("{}: {}", "词法错误".red().bold(), e);
            input_buffer.clear();
            continue;
        }
        
        // 获取词法分析结果
        let tokens = tokens_result.unwrap();
        
        // 检查是否只有注释或空白
        if tokens.is_empty() {
            input_buffer.clear();
            continue;
        }
        
        // 语法分析
        let ast_result = parser::parse(tokens);
        if let Err(e) = &ast_result {
            println!("{}: {}", "解析错误".red().bold(), e);
            if let parser::ParseError::InvalidSyntax(msg, _, _) = e {
                if msg.contains("数组类型检查错误") {
                    println!("{}: 数组中的所有元素必须是同一类型", "提示".yellow().bold());
                }
            }
            input_buffer.clear();
            continue;
        }
        
        // 获取 AST
        let ast = ast_result.unwrap();
        
        // 根据模式显示 AST 或执行代码
        if show_ast {
            println!("{}", "AST:".yellow().bold());
            print_ast(&ast);
        } else {
            // 使用保持状态的类型检查器，而不是每次创建新的
            match type_checker.infer_program(&ast.statements) {
                Ok(_) => {
                    
                    // 执行程序
                    let eval_result = evaluator.evaluate(&ast);
                    match eval_result {
                        Ok(value) => {
                            if !matches!(value, crate::runtime::Value::Null) {
                                println!("{} {}", "=>".bright_blue().bold(), format!("{}", stdlib::format_value(&value)).bright_white());
                            }
                        },
                        Err(e) => {
                            println!("{}: {}", "运行时错误".red().bold(), e);
                        }
                    }
                },
                Err(type_error) => {
                    println!("{}: {}", "类型错误".red().bold(), type_error);
                }
            }
        }
        
        // 清空输入缓冲区，准备下一次输入
        input_buffer.clear();
    }
}

// 检查是否需要续行（括号不匹配）
fn needs_continuation(tokens: &[Token]) -> bool {
    let mut open_parens = 0;
    let mut open_braces = 0;
    let mut open_brackets = 0;
    
    for token in tokens {
        match token.token_type {
            TokenType::LeftParen => open_parens += 1,
            TokenType::RightParen => {
                if open_parens > 0 {
                    open_parens -= 1;
                }
            },
            TokenType::LeftBrace => open_braces += 1,
            TokenType::RightBrace => {
                if open_braces > 0 {
                    open_braces -= 1;
                }
            },
            TokenType::LeftBracket => open_brackets += 1,
            TokenType::RightBracket => {
                if open_brackets > 0 {
                    open_brackets -= 1;
                }
            },
            _ => {}
        }
    }
    
    // 如果有任何未闭合的括号，则需要续行
    open_parens > 0 || open_braces > 0 || open_brackets > 0
}

// 保留原有的 print_ast 函数和其他辅助函数
fn print_ast(program: &Program) {
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
        Stmt::VariableDecl(name, type_ann, expr) => {
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
                Literal::Unit => println!("{}{}", indent_str, "单元值".magenta()),
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
    match type_ann {
        TypeAnnotation::Array(elem_type) => {
            print!("[]");
            print_type_annotation(elem_type, indent);
        }
        TypeAnnotation::Simple(name, args) => {
            print!("{}", name.green());
            if let Some(args) = args {
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
// 在 format_binary_op 函数中添加对 Index 操作符的处理
fn format_binary_op(op: &BinaryOp) -> colored::ColoredString {
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
        BinaryOp::Access => ".".bright_cyan(),
        BinaryOp::Index => "[]".bright_cyan(), // 添加索引操作符
    }
}

// 格式化一元操作符
fn format_unary_op(op: &UnaryOp) -> ColoredString {
    match op {
        UnaryOp::Negate => "-".bright_cyan(),
        UnaryOp::Not => "not".bright_cyan(),
    }
}
