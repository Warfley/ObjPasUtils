# Iterators
The iterators library is a container independent library for handling enumerations over data.
This is highly inspired by typically Functional Style List handling.

It provides a set of functions for modifying streams of data that can be iterated through.
These streams can also be infinite, through lazy evaluation, each element is only loaded when required, meaning an iterator can also produce an infinite stream of elements, which might only be considered for the first few elements.

Amongst others those functions are:
* Map: Applies a function to each element, creating a stream of the return values of that function
* Filter: Filters the elements with a custom predicate, creating a stream of elements where the predicate returned true
* Take, TakeWhile: Returns the stream of the first elements, either a fixed number, or until a condition is false
* Skip, SkipWhile skips the first elements of a stream, either a fixed number or until a condition is false
* Cast, CastObject: Map using typecasts, either explicit casts `TNewType(value)` or Object Casts: `obj as TNewClass`
* FilterClass: Filter for different c1lasses `obj is TSubClass`
* Index: Creates a stream where for each incoming element a Pair is returned which contains the index (starting with 0 for the first element) and the element itself
* Step: Skips elements in steps, i.e. Step(Iterator, 2) will only give every other element from Interator
* Fold/Reduce: Applies a function to each of the values and the result of the previous step to compute a final result over the whole stream (e.g. a sum over all elements). Not possible on infinite streams.
* Reverse: Reverses a stream from last element to first. First requires loading all elements into memory before repeating them in opposite order. Not possible on infinite streams
* Next: Get the next element from an Iterator
* Last: Get the last element from an Iterator, not possible on infinite streams
* Collect, CollectArray: Collects all the elements from the stream into a container (e.g. List, Array, TStringList, etc)

## IIterator\<T>
The core of this library forms the `IIterator<T>` COM interface.
Any class that implements this interface can be used with the functions described above.

Iterators also implement the required functions to be used in a For-In loop, so you can also iterate through iterators.
```pascal
var
  iter: IIterator<Integer>;
  i: Integer;
begin
  iter := Iterate<Integer>(arr); // create iterator
  iter := filter(iter, isEven); // filter out odd elements
  for i in iter do
    WriteLn(i);
```
As this is a COM interface, it is reference counted, so no manual memory management is required.
This allows easy chaining of iterators:
```pascal
var
  Data: Array of Byte;
  HexStr: String;
begin
  // Converts each of the bytes to the HEX representation and forms a string by concatinating all of them
  HexStr := Reduce<String>(Map<Byte, String>(Iterate<Byte>(arr), ByteToHex), ConcatStr);
end;
```

## Generating from Container
To create Iterators for different Container types, does not require rewriting the container to fit this library. All it requires is the implementation of the Iterate function.

The library already provides in the `iterators.base` unit a class to Implement an Iterator based on already existing enumerators. As an example see this implementation to make TStringList compatible:
```pascal
function Iterate(const AStrings: TStrings): specialize IIterator<String>;
begin
  Result := specialize TClassEnumeratorIterator<String, TStringsEnumerator>.Create(AStrings.GetEnumerator);
end; 
```
The `TClassEnumeratorIterator` can simply take an existing Enumerator class, `TStringsEnumerator` in this case, and wrap it into an IIterator.
This simple one-liner allows using TStringLists as part of this library:
```pascal
  // Iterate through StringList but in lower case
  for item in Map<String, String>(Iterate(MyStringList), LowerCase) do
    WriteLn(item);
```

This allows adding new Containers to this library without having to change anything about their implementation, with a simple 1 line function.

The Library already provides Iterate functions for the old Pointer style `TList`, pretty much all `Generics.Collections`, arrays and `TStrings`

## Collectors
To collect the streams, e.g. to create a new List, requires to implement a collect function.
The library already provides a collect function for general containers, assuming they have a .Add function:
```pascal
var
  lst: TList<Integer>; // Generics.Collections
begin
  // external creation (safer):
  lst := TList<Integer>.Create;
  try
    Collect<Integer, TList<Integer>>(iter, lst);
    ...
  finally
    lst.Free;
  end;
  // Create by collect (memory leaks if exceptions are raised)
  lst := Collect<Integer, TList<Integer>>(iter);
  try
    ...
  finally
    lst.Free;
  end;
  // Combination of the two custom constructors:
  lst := Collect<Integer, TList<Integer>>(iter, TList<Integer>.Create());
  try
    ...
  finally
    lst.Free;
  end;
end;
```
Also it allows collecting into arrays:
```pascal
var
  arr: Array of Integer;
begin
  arr := CollectArray<Integer>(iterator)
```
It can be tweaked with the option boolean argument, to either grow the array geometrically (growing by doubling in size), or linear (growing for each element individually). Geometric growth is usually recommended (the same growth behavior as implemented by TList) as it results usually in a much better performance.

## Implicit Specialization
A new feature currently in development is implicit specialization for generic functions. This would allow leaving out the generic typing for the functions.
This would massively improve the readability of these functions presented here:
```pascal
HexStr := Reduce<String>(Map<Byte, String>(Iterate<Byte>(arr), ByteToHex), ConcatStr);
// would become
HexStr := Reduce(Map(Iterate(arr), ByteToHex), ConcatStr);
```
But until then, it is still required to always add the appropriate types to the generic specialization.

To ease readability, I would also suggest using `{$Mode Delphi}` rather than objFPC, as it does not require the `specialize` keyword:
```pascal
// Mode Delphi:
HexStr := Reduce<String>(Map<Byte, String>(Iterate<Byte>(arr), ByteToHex), ConcatStr);
// Mode ObjFPC:
HexStr := specialize Reduce<String>(specialize Map<Byte, String>(specialize Iterate<Byte>(arr), @ByteToHex), @ConcatStr);
```

## Examples
See `examples/iteratortest` for examples covering all currently available functionalities