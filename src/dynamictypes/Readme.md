# Dynamic Types

These units provide some more dynamic types:

## TOptional\<T>
This is a special type that can hold a value of another type, or be empty:
```pascal
var opt: TOptional<Integer>; // Either empty or an optional
begin
  opt := EmptyOptional; // Empty, default initalized as such
  opt := None; // if NoneType is used, same as EmptyOptional just shorter
  opt := 42; // Assign a value to it
```
It allows for three ways of accessing, Pointer Access, Value Access or GetOrDefault Access:
```pascal
  WriteLn(Opt.Value); // Writes the value in Opt, throws exception if empty
  Opt.Mutable^ := 42; // Pointer access to the value in opt, throws exception is empty
  WriteLn(Opt.GetOrDefault(32)); // Gets the value of opt, or returns 32 if empty
  WriteLn(Opt.GetOrDefault); // Gets teh value of opt or returns Default(Integer) if empty
```
To easiely check if the Optional holds a value, boolean conversion and the not operator can be used:
```pascal
if opt then
  // Opt has value
if not opt then
  // Opt is empty
```

## TUnion\<T, U>
TUnion holds a value which can be one of two differnt types (or also empty like TOptional)
```pascal
var
  union: TUnion<String, Integer>;
begin
  union := EmptyUnion; // Empty
  union := None; // Same as EmptyUnion;
  union := 'Hello World'; // Set the value to being a string
  union := 42; // Set the value to being an integer
```
The values of the union can be accessed via First, Second:
```pascal
  if union.isFirst then
    WriteLn('Union is String: ', Union.First);
  if union.isSecond then
    WriteLn('Union is Integer: ', Union.Second);
```
It also has boolean conversion like Optional.

For further information see the `examples/dynamictypestest/*` examples.