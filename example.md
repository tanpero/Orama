# Orama 语言

Orama 语言是一门代数效应纯函数式编程语言。

## 核心语法

### 基本语法

```plaintext:c:\msys64\home\camille\lang\orama\syntax.md
// 变量声明
let x = 5
let name = "Orama"

// 函数定义 (使用箭头函数)
let add = (a, b) => a + b

// 箭头函数自动返回最后一个表达式的值
let factorial = (n) => 
  if n <= 1 {
    1
  } else {
    n * factorial(n - 1)
  }

// 函数组合
let compose = (f, g) => (x) => f(g(x))
let double = (x) => x * 2
let increment = (x) => x + 1
let doubleAndIncrement = compose(increment, double)
```

### 代数效应核心概念

```plaintext:c:\msys64\home\camille\lang\orama\effects.md
// 效应声明
effect State<T> {
  get: () => T,
  set: (value: T) => ()
}

// 效应操作使用
let increment = () => {
  let current = perform State.get()
  perform State.set(current + 1)
  current + 1
}

// 效应处理器
let counter = () => 
  handle increment() {
    effect State {
      get: () => resume(0),
      set: (value) => resume(())
    }
  }
```

## 详细语法设计

### 1. 效应声明

```plaintext:c:\msys64\home\camille\lang\orama\effect_declaration.md
effect EffectName<T1, T2, ...> {
  operation1: (param1: Type1, param2: Type2, ...) => ReturnType,
  operation2: () => ReturnType,
  // ...
}
```

### 2. 效应操作

```plaintext:c:\msys64\home\camille\lang\orama\effect_operations.md
// 触发效应
perform EffectName.operation(args)
```

### 3. 效应处理器

```plaintext:c:\msys64\home\camille\lang\orama\effect_handlers.md
handle expression {
  effect EffectName {
    operation1: (params) => computation,
    operation2: () => computation
  }
  
  // 可以处理多个效应
  effect AnotherEffect {
    // ...
  }
  
  // 返回值转换（可选）
  return: (x) => transformation
}
```

## 函数式特性

### 高阶函数

```plaintext:c:\msys64\home\camille\lang\orama\higher_order.md
// map 函数
let map = (array, fn) => {
  let result = []
  for (let i = 0; i < array.length; i++) {
    result.push(fn(array[i]))
  }
  result
}

// filter 函数
let filter = (array, predicate) => {
  let result = []
  for (let i = 0; i < array.length; i++) {
    if (predicate(array[i])) {
      result.push(array[i])
    }
  }
  result
}

// reduce 函数
let reduce = (array, fn, initial) => {
  let acc = initial
  for (let i = 0; i < array.length; i++) {
    acc = fn(acc, array[i])
  }
  acc
}

// 使用示例
let numbers = [1, 2, 3, 4, 5]
let doubled = map(numbers, (x) => x * 2)
let evens = filter(numbers, (x) => x % 2 == 0)
let sum = reduce(numbers, (acc, x) => acc + x, 0)
```

### 柯里化和部分应用

```plaintext:c:\msys64\home\camille\lang\orama\currying.md
// 柯里化
let curry = (fn) => {
  let arity = fn.arity
  let curried = (args) => {
    if (args.length >= arity) {
      return fn(...args)
    }
    return (...more) => curried([...args, ...more])
  }
  return curried([])
}

// 柯里化的加法函数
let add = curry((a, b, c) => a + b + c)
let add5 = add(5)
let add5and10 = add5(10)
let result = add5and10(15) // 30

// 部分应用
let partial = (fn, ...args) => (...moreArgs) => fn(...args, ...moreArgs)
let greet = (greeting, name) => greeting + " " + name
let sayHello = partial(greet, "Hello")
let helloWorld = sayHello("World") // "Hello World"
```

## 实际示例

### 示例1：状态管理

```plaintext:c:\msys64\home\camille\lang\orama\state_example.md
effect State<S> {
  get: () => S,
  set: (s: S) => ()
}

let counter = (n) => {
  let count = perform State.get()
  if (count >= n) {
    count
  } else {
    perform State.set(count + 1)
    counter(n)
  }
}

let runCounter = (n) => 
  handle counter(n) {
    effect State {
      get: () => {
        let state = 0  // 初始状态
        resume(state)
      },
      set: (newState) => {
        resume((), newState)  // 更新状态并继续
      }
    }
  }
```

### 示例2：异步操作

```plaintext:c:\msys64\home\camille\lang\orama\async_example.md
effect Async {
  await: <T>(promise: Promise<T>) => T
}

let fetchData = (url) => {
  let response = perform Async.await(fetch(url))
  let data = perform Async.await(response.json())
  data
}

let main = () => 
  handle fetchData("https://api.example.com/data") {
    effect Async {
      await: (promise) => {
        promise.then(value => {
          resume(value)
        })
      }
    }
  }
```

### 示例3：异常处理

```plaintext:c:\msys64\home\camille\lang\orama\exception_example.md
effect Exception {
  throw: <T>(error: String) => T
}

let divide = (a, b) => {
  if (b == 0) {
    perform Exception.throw("Division by zero")
  } else {
    a / b
  }
}

let safeDivide = (a, b) => 
  handle divide(a, b) {
    effect Exception {
      throw: (msg) => {
        // 不恢复计算，而是返回一个默认值
        0
      }
    }
  }
```

## 函数式模式

### 函数组合与管道

```plaintext:c:\msys64\home\camille\lang\orama\composition.md
// 函数组合
let compose = (f, g) => (x) => f(g(x))
let composeMany = (...fns) => 
  fns.reduce((f, g) => (x) => f(g(x)))

// 管道操作符
let pipe = (x, ...fns) => 
  fns.reduce((acc, fn) => fn(acc), x)

// 使用示例
let double = (x) => x * 2
let increment = (x) => x + 1
let square = (x) => x * x

let transformed = pipe(
  5,
  double,    // 10
  increment, // 11
  square     // 121
)
```

### 不可变数据结构

```plaintext:c:\msys64\home\camille\lang\orama\immutable.md
// 不可变更新
let update = (obj, key, value) => ({
  ...obj,
  [key]: value
})

// 不可变数组操作
let append = (arr, item) => [...arr, item]
let prepend = (arr, item) => [item, ...arr]
let without = (arr, index) => [
  ...arr.slice(0, index),
  ...arr.slice(index + 1)
]

// 使用示例
let user = { name: "Alice", age: 30 }
let updatedUser = update(user, "age", 31)
// user 保持不变，updatedUser 是 { name: "Alice", age: 31 }
```

## 类型系统

```plaintext:c:\msys64\home\camille\lang\orama\types.md
// 函数类型包含效应信息
let readFile: (path: String) => <IO> String

// 效应多态
let map: <T, U, E>(list: List<T>, f: (T) => <E> U) => <E> List<U>

// 效应约束
let pureFunction: <T, U>(x: T) => <Pure> U

// 类型别名
type Parser<T> = (input: String) => <Parse> { value: T, rest: String }

// 代数数据类型
type Option<T> = 
  | Some(value: T)
  | None

// 模式匹配
let unwrapOr = <T>(option: Option<T>, defaultValue: T) => 
  match option {
    Some(value) => value,
    None => defaultValue
  }
```

这种函数式风格的 Orama 语言设计使函数成为一等公民，支持高阶函数、柯里化、部分应用和函数组合等函数式编程范式，同时保留了代数效应的强大能力，使副作用处理更加模块化和可组合。
