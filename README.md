# L3P

# Requirements

- zig 0.15.1 (master)
- fasm
- gcc

# Build and Compile l3p files

### Build the compiler

## To build the compiler

```
zig build
```

The compiler is built in the _zig-out/bin_ directory

To build a program

```
zig-out/bin/l3p ./tests/hello_world.l3p
```

The executable is built in the _l3p-out/hello_world_ directory

## To build the compiler and compile the hello_world.l3p file

```
zig build run -- ./tests/hello_world.l3p
```

## To execute all the tests and check the output

```
zig build test-snapshot -p "tests/snapshots" --summary all
```

```
IfStatement:

  IF <expression> THEN
    <statements>
  ELSEIF <expression> THEN
    <statements>
  ELSE
    <statements
  ENDIF

WhileStatement:

  WHILE <expression> DO
    <statements>
  ENDWHILE

```
