# For Loops

For loops are a type of loop that will only run a specific number of times. They are created using the `for` keyword.

```swift
for i in 0..5 do
    action print: i;
end
```

This will print the numbers 0 through 4, but not 5.

`break` and `continue` work the same way in for loops as they do in infinite loops.

```swift
for i in 0..5 do
    if i == 3 then
        continue;
    end

    action print: i;
end
```

## Range syntax

The `..` syntax is used in for loops to specify the numbers to loop through. That means that `0..5` will loop through the numbers 0, 1, 2, 3, and 4.

`..=` can be used to include the last number in the range, so `0..=5` will loop through the numbers 0, 1, 2, 3, 4, **and 5**.
