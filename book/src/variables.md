# Variables

Variables are used to store values. They are used to assign a name to a value. They are useful for storing values that you want to use later in the program.

## Declaration

Variables are declared using the `let` keyword. The syntax is as follows:

```ruby
let variable_name = value;
```

Variable names can contain letters, numbers, and underscores, can't start with a number, **and are case sensitive**.

Some examples of valid variable names are:

```ruby
my_variable
_variable
aa1234
a_b_c
```

An invalid variable name is:

```ruby
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

```ruby
let my_variable = 123;

action print: my_variable; // 123

my_variable = 456;

action print: my_variable; // 456
```

## Scopes

Scopes limit where in the code a variable can be accessed. A variable declared inside a scope can only be accessed inside that scope.

A scope is defined by the `do` and `end` keywords.

```ruby
let my_variable = 123;

do
    action print: my_variable; // 123

    let another_variable = 456;

    action print: another_variable; // 456
end

action print: my_variable; // 123

action print: another_variable; // Error: Undefined variable `another_variable`
```
