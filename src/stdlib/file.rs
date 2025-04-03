use crate::runtime::{Value, Environment, RuntimeError, RuntimeResult};
use std::rc::Rc;
use std::cell::RefCell;
use std::fs::{self, File, OpenOptions};
use std::io::{self, Read, Write, Seek, SeekFrom};
use std::collections::HashMap;
use std::path::Path;

// 文件句柄管理器
struct FileHandleManager {
    next_id: usize,
    handles: HashMap<usize, File>,
}

impl FileHandleManager {
    fn new() -> Self {
        FileHandleManager {
            next_id: 1,
            handles: HashMap::new(),
        }
    }

    fn register(&mut self, file: File) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        self.handles.insert(id, file);
        id
    }

    fn get(&mut self, id: usize) -> Option<&mut File> {
        self.handles.get_mut(&id)
    }

    fn close(&mut self, id: usize) -> bool {
        self.handles.remove(&id).is_some()
    }
}

// 全局文件句柄管理器
thread_local! {
    static FILE_MANAGER: RefCell<FileHandleManager> = RefCell::new(FileHandleManager::new());
}

pub fn register(env: &Rc<RefCell<Environment>>) {
    // 打开文件
    super::add_native_fn(
        env,
        "file_open",
        vec!["path", "mode"],
        |args, _| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            let path = match &args[0] {
                Value::String(s) => s,
                _ => return Err(RuntimeError::TypeError("file_open 函数的第一个参数必须是字符串路径".to_string())),
            };
            
            let mode = match &args[1] {
                Value::String(s) => s,
                _ => return Err(RuntimeError::TypeError("file_open 函数的第二个参数必须是字符串模式".to_string())),
            };
            
            let mut options = OpenOptions::new();
            
            match mode.as_str() {
                "r" => { options.read(true); },
                "w" => { options.write(true).create(true).truncate(true); },
                "a" => { options.write(true).create(true).append(true); },
                "r+" => { options.read(true).write(true); },
                "w+" => { options.read(true).write(true).create(true).truncate(true); },
                "a+" => { options.read(true).write(true).create(true).append(true); },
                _ => return Err(RuntimeError::TypeError(format!("不支持的文件模式: {}", mode))),
            }
            
            match options.open(path) {
                Ok(file) => {
                    let handle_id = FILE_MANAGER.with(|manager| {
                        manager.borrow_mut().register(file)
                    });
                    
                    // 创建一个对象来表示文件句柄
                    let mut fields = HashMap::new();
                    fields.insert("handle".to_string(), Value::Number(handle_id as f64));
                    fields.insert("path".to_string(), Value::String(path.clone()));
                    fields.insert("mode".to_string(), Value::String(mode.clone()));
                    
                    Ok(Value::Object(fields))
                },
                Err(e) => Err(RuntimeError::IOError(format!("无法打开文件 '{}': {}", path, e))),
            }
        },
    );
    
    // 读取文件内容
    super::add_native_fn(
        env,
        "file_read",
        vec!["file", "size"],
        |args, _| {
            if args.len() < 1 {
                return Err(RuntimeError::ArgumentMismatch { expected: 1, actual: args.len() });
            }
            
            let file_obj = match &args[0] {
                Value::Object(fields) => fields,
                _ => return Err(RuntimeError::TypeError("file_read 函数的第一个参数必须是文件对象".to_string())),
            };
            
            let handle_id = match file_obj.get("handle") {
                Some(Value::Number(id)) => *id as usize,
                _ => return Err(RuntimeError::TypeError("无效的文件句柄".to_string())),
            };
            
            // 确定要读取的字节数
            let size = if args.len() > 1 {
                match &args[1] {
                    Value::Number(n) if *n >= 0.0 => *n as usize,
                    _ => return Err(RuntimeError::TypeError("size 参数必须是非负数字".to_string())),
                }
            } else {
                // 默认读取整个文件
                usize::MAX
            };
            
            FILE_MANAGER.with(|manager| {
                let mut manager = manager.borrow_mut();
                let file = match manager.get(handle_id) {
                    Some(f) => f,
                    None => return Err(RuntimeError::IOError("无效的文件句柄".to_string())),
                };
                
                let mut buffer = if size == usize::MAX {
                    // 读取整个文件
                    let mut content = String::new();
                    match file.read_to_string(&mut content) {
                        Ok(_) => content,
                        Err(e) => return Err(RuntimeError::IOError(format!("读取文件失败: {}", e))),
                    }
                } else {
                    // 读取指定字节数
                    let mut buffer = vec![0; size];
                    match file.read(&mut buffer) {
                        Ok(bytes_read) => {
                            buffer.truncate(bytes_read);
                            String::from_utf8_lossy(&buffer).to_string()
                        },
                        Err(e) => return Err(RuntimeError::IOError(format!("读取文件失败: {}", e))),
                    }
                };
                
                Ok(Value::String(buffer))
            })
        },
    );
    
    // 写入文件
    super::add_native_fn(
        env,
        "file_write",
        vec!["file", "data"],
        |args, _| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            let file_obj = match &args[0] {
                Value::Object(fields) => fields,
                _ => return Err(RuntimeError::TypeError("file_write 函数的第一个参数必须是文件对象".to_string())),
            };
            
            let handle_id = match file_obj.get("handle") {
                Some(Value::Number(id)) => *id as usize,
                _ => return Err(RuntimeError::TypeError("无效的文件句柄".to_string())),
            };
            
            let data = match &args[1] {
                Value::String(s) => s.as_bytes(),
                _ => return Err(RuntimeError::TypeError("file_write 函数的第二个参数必须是字符串".to_string())),
            };
            
            FILE_MANAGER.with(|manager| {
                let mut manager = manager.borrow_mut();
                let file = match manager.get(handle_id) {
                    Some(f) => f,
                    None => return Err(RuntimeError::IOError("无效的文件句柄".to_string())),
                };
                
                match file.write_all(data) {
                    Ok(_) => Ok(Value::Number(data.len() as f64)),
                    Err(e) => Err(RuntimeError::IOError(format!("写入文件失败: {}", e))),
                }
            })
        },
    );
    
    // 关闭文件
    super::add_native_fn(
        env,
        "file_close",
        vec!["file"],
        |args, _| {
            if args.is_empty() {
                return Err(RuntimeError::ArgumentMismatch { expected: 1, actual: 0 });
            }
            
            let file_obj = match &args[0] {
                Value::Object(fields) => fields,
                _ => return Err(RuntimeError::TypeError("file_close 函数的参数必须是文件对象".to_string())),
            };
            
            let handle_id = match file_obj.get("handle") {
                Some(Value::Number(id)) => *id as usize,
                _ => return Err(RuntimeError::TypeError("无效的文件句柄".to_string())),
            };
            
            let result = FILE_MANAGER.with(|manager| {
                manager.borrow_mut().close(handle_id)
            });
            
            Ok(Value::Boolean(result))
        },
    );
    
    // 文件是否存在
    super::add_native_fn(
        env,
        "file_exists",
        vec!["path"],
        |args, _| {
            if args.is_empty() {
                return Err(RuntimeError::ArgumentMismatch { expected: 1, actual: 0 });
            }
            
            let path = match &args[0] {
                Value::String(s) => s,
                _ => return Err(RuntimeError::TypeError("file_exists 函数的参数必须是字符串路径".to_string())),
            };
            
            Ok(Value::Boolean(Path::new(path).exists()))
        },
    );
    
    // 删除文件
    super::add_native_fn(
        env,
        "file_delete",
        vec!["path"],
        |args, _| {
            if args.is_empty() {
                return Err(RuntimeError::ArgumentMismatch { expected: 1, actual: 0 });
            }
            
            let path = match &args[0] {
                Value::String(s) => s,
                _ => return Err(RuntimeError::TypeError("file_delete 函数的参数必须是字符串路径".to_string())),
            };
            
            match fs::remove_file(path) {
                Ok(_) => Ok(Value::Boolean(true)),
                Err(e) => Err(RuntimeError::IOError(format!("删除文件失败: {}", e))),
            }
        },
    );
    
    // 重命名文件
    super::add_native_fn(
        env,
        "file_rename",
        vec!["old_path", "new_path"],
        |args, _| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            let old_path = match &args[0] {
                Value::String(s) => s,
                _ => return Err(RuntimeError::TypeError("file_rename 函数的第一个参数必须是字符串路径".to_string())),
            };
            
            let new_path = match &args[1] {
                Value::String(s) => s,
                _ => return Err(RuntimeError::TypeError("file_rename 函数的第二个参数必须是字符串路径".to_string())),
            };
            
            match fs::rename(old_path, new_path) {
                Ok(_) => Ok(Value::Boolean(true)),
                Err(e) => Err(RuntimeError::IOError(format!("重命名文件失败: {}", e))),
            }
        },
    );
    
    // 获取文件大小
    super::add_native_fn(
        env,
        "file_size",
        vec!["path"],
        |args, _| {
            if args.is_empty() {
                return Err(RuntimeError::ArgumentMismatch { expected: 1, actual: 0 });
            }
            
            let path = match &args[0] {
                Value::String(s) => s,
                _ => return Err(RuntimeError::TypeError("file_size 函数的参数必须是字符串路径".to_string())),
            };
            
            match fs::metadata(path) {
                Ok(metadata) => Ok(Value::Number(metadata.len() as f64)),
                Err(e) => Err(RuntimeError::IOError(format!("获取文件大小失败: {}", e))),
            }
        },
    );
    
    // 读取整个文件内容（便捷函数）
    super::add_native_fn(
        env,
        "read_file",
        vec!["path"],
        |args, _| {
            if args.is_empty() {
                return Err(RuntimeError::ArgumentMismatch { expected: 1, actual: 0 });
            }
            
            let path = match &args[0] {
                Value::String(s) => s,
                _ => return Err(RuntimeError::TypeError("read_file 函数的参数必须是字符串路径".to_string())),
            };
            
            match fs::read_to_string(path) {
                Ok(content) => Ok(Value::String(content)),
                Err(e) => Err(RuntimeError::IOError(format!("读取文件失败: {}", e))),
            }
        },
    );
    
    // 写入整个文件内容（便捷函数）
    super::add_native_fn(
        env,
        "write_file",
        vec!["path", "content"],
        |args, _| {
            if args.len() < 2 {
                return Err(RuntimeError::ArgumentMismatch { expected: 2, actual: args.len() });
            }
            
            let path = match &args[0] {
                Value::String(s) => s,
                _ => return Err(RuntimeError::TypeError("write_file 函数的第一个参数必须是字符串路径".to_string())),
            };
            
            let content = match &args[1] {
                Value::String(s) => s,
                _ => return Err(RuntimeError::TypeError("write_file 函数的第二个参数必须是字符串内容".to_string())),
            };
            
            match fs::write(path, content) {
                Ok(_) => Ok(Value::Boolean(true)),
                Err(e) => Err(RuntimeError::IOError(format!("写入文件失败: {}", e))),
            }
        },
    );
    
    // 创建目录
    super::add_native_fn(
        env,
        "create_dir",
        vec!["path"],
        |args, _| {
            if args.is_empty() {
                return Err(RuntimeError::ArgumentMismatch { expected: 1, actual: 0 });
            }
            
            let path = match &args[0] {
                Value::String(s) => s,
                _ => return Err(RuntimeError::TypeError("create_dir 函数的参数必须是字符串路径".to_string())),
            };
            
            match fs::create_dir_all(path) {
                Ok(_) => Ok(Value::Boolean(true)),
                Err(e) => Err(RuntimeError::IOError(format!("创建目录失败: {}", e))),
            }
        },
    );
    
    // 删除目录
    super::add_native_fn(
        env,
        "remove_dir",
        vec!["path", "recursive"],
        |args, _| {
            if args.is_empty() {
                return Err(RuntimeError::ArgumentMismatch { expected: 1, actual: 0 });
            }
            
            let path = match &args[0] {
                Value::String(s) => s,
                _ => return Err(RuntimeError::TypeError("remove_dir 函数的第一个参数必须是字符串路径".to_string())),
            };
            
            let recursive = if args.len() > 1 {
                match &args[1] {
                    Value::Boolean(b) => *b,
                    _ => return Err(RuntimeError::TypeError("remove_dir 函数的第二个参数必须是布尔值".to_string())),
                }
            } else {
                false
            };
            
            let result = if recursive {
                fs::remove_dir_all(path)
            } else {
                fs::remove_dir(path)
            };
            
            match result {
                Ok(_) => Ok(Value::Boolean(true)),
                Err(e) => Err(RuntimeError::IOError(format!("删除目录失败: {}", e))),
            }
        },
    );
    
    // 列出目录内容
    super::add_native_fn(
        env,
        "list_dir",
        vec!["path"],
        |args, _| {
            if args.is_empty() {
                return Err(RuntimeError::ArgumentMismatch { expected: 1, actual: 0 });
            }
            
            let path = match &args[0] {
                Value::String(s) => s,
                _ => return Err(RuntimeError::TypeError("list_dir 函数的参数必须是字符串路径".to_string())),
            };
            
            match fs::read_dir(path) {
                Ok(entries) => {
                    let mut result = Vec::new();
                    
                    for entry in entries {
                        if let Ok(entry) = entry {
                            if let Ok(file_name) = entry.file_name().into_string() {
                                result.push(Value::String(file_name));
                            }
                        }
                    }
                    
                    Ok(Value::Array(result))
                },
                Err(e) => Err(RuntimeError::IOError(format!("读取目录失败: {}", e))),
            }
        },
    );
}