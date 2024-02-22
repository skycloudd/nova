# Hello World

As with any programming tutorial, lets start with a simple demonstration program.

```swift
proc main() do
    let my_number = 1 + 2 * 3;

    if my_number == 7 then
        action print: 3;
    end
end

run main;
```

Output:

```swift
3
```

## Explanation

### The `main` procedure

```swift
proc main() do
    // ...
end

run main;
```

Every program starts with a `main` procedure. This is the entry point of the program. Inside the `main` procedure we write the code that we want to run when the program starts.

`run main;` tells the game to start running the `main` procedure when the level starts.

Throughout this book, the `main` procedure will usually not be shown for demonstration purposes, but when writing real programs it is always required.

### `let` statement

```swift
let my_number = 1 + 2 * 3;
```

This line assigns the value `7` to a variable called `my_number`. That means we can simply use its name in other places in the code and get the number `7` back.

### `if` statement

```swift
if my_number == 7 then
    action print: 3;
end
```

This says that if the variable `my_number` is _equal_ to the number `7`, we print the number `3`.
