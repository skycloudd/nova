# Infinite Loops

Infinite loops are a type of loop that will run forever. They are created using the `loop` keyword.

```swift
loop do
    action print: 5;
end
```

This will print the number 5 forever.

## `break`

To stop an infinite loop, you can use the `break` keyword. This will immediately stop the loop and continue with the code after the loop.

```swift
let i = 0;

loop do
    action print: i;

    if i == 5 then
        break;
    end

    i = i + 1;
end
```

This will print the numbers 0 through 5, and then stop.

## `continue`

The `continue` keyword will immediately stop the current iteration of the loop and start the next one.

```swift
let i = 0;

loop do
    if i == 3 then
        continue;
    end

    action print: i;

    if i == 5 then
        break;
    end

    i = i + 1;
end
```

This will print the numbers 0, 1, 2, 4, and 5. The number 3 is skipped because of the `continue` keyword.
