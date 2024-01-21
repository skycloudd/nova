# Variables

Variables are used to store values. They are used to assign a name to a value. They are useful for storing values that you want to use later in the program.

## Declaration

Variables are declared using the `let` keyword. The syntax is as follows:

```swift
let variable_name = value;
```

Variable names can contain letters, numbers, and underscores, can't start with a number, **and are case sensitive**.

Some examples of valid variable names are:

```swift
my_variable
_variable
aa1234
a_b_c
```

An invalid variable name is:

```swift
12variable_name
```

### Constants

Variables can also be declared as a constant. That means the value can't be changed after it has been assigned. This is done by using the `const` keyword instead of `let`.

```cs
const my_variable = 123;
```

That means the following code is invalid:

```cs
const my_variable = 123;
my_variable = 456; // Cannot assign to const `my_variable`
```

## Assignment

Non-constant variables can be set to a new value using the `=` operator.

```swift
let my_variable = 123;

print my_variable; // 123

my_variable = 456;

print my_variable; // 456
```

## Scopes

...

## Type Inference

...
