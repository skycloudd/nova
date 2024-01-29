# Actions

Actions are the primary way to interact with the level and the objects in it.

## Syntax

Actions are written in the following format:

```ruby
action <name>: <arg1>, <arg2>, ...;
```

## Action types

The following is a list of all the actions that can be used, together with the arguments they take. This list will be expanded as more actions are integrated with the language.

### `wait`

Wait for a number of seconds and then continue running the script.

Arguments:

-   `float` - The time to wait, in seconds.

### `waitframes`

Wait for a number of frames and then continue running the script.

Arguments:

-   `int` - The number of frames to wait.

### `move`

Move an object by a vector.

Arguments:

-   `object/objectset` - The object to move.
-   `vector` - The vector to move the object by.
-   `bool` - Move the object globally or not. `true` = global, `false` = relative.
-   `float` - The time to take to move the object.

#### `print`

Print a value through a "show game text" action in-game.

Arguments:

-   `bool/int/float` - A value to display.
