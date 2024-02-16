# If-Else Statements

If-else statements are used to control the flow of a program. They run different code depending on whether a condition is true or false.

## Syntax

```swift
if <expression> then
    <statements>
else
    <statements>
end
```

The `else` block is optional.

## Example

```swift
if true then
    // ...
end
```

```swift
if 2 + 2 == 5 then
    action print: 4;
else
    action print: true;
end
```

You can also use `else if` to check multiple conditions.

```swift
if 2 + 2 == 5 then
    action print: 4;
else if 2 + 2 == 4 then
    action print: 5;
else
    action print: true;
end
```
