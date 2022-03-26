# Ranges
The `Ranges` unit contains types to iterate over ranges of numbers or elements.

## Types
This unit introduces 3 types:
* `TRange` represents a range `[Start, Stop)` (inclusive start, exclusive stop) with a given `Step` size. E.g. `Range(0, 10, 2)` is `[0, 2, 4, 6, 8]`
* `TArrayRange<T>` references a subset of an array, without copying any elements. E.g. `TArrayRange<Integer>.Create([1, 2, 3], 0, 2)` represents the values `1, 2` without having to copy that array
* `TListRange<T>` is the same as `TArrayRange` just for `TList<T>` objects

Each of these types is iterable and provides the `[]` operator to directly access an element in that range:
```pascal
for i in Range(5, 10) do
  WriteLn(i);
// same as
rng := Range(5, 10);
for i := 0 to rng.Count - 1 do
  WriteLn(rng[i]);
```
With `[]` objects can be referenced from the back, e.g. `Range(0, 10)[-1]` is the last element in that range, i.e. 9.

Also subranges can be taken, e.g. `Range(5, 10).SubRange(2, 3)` is equivalent to `Range(7, 8)`.
The step size is multiplied, e.g. `Range(0, 10, 2).SubRange(0, 10, 2)` is `[0, 4, 8]`
The constants `RangeBegin` and `RangeEnd` can be used to reference the start and Stop values for `SubRange` without needing to know the actual numbers.
Also relative ranges can be used here: `Range(0, 10).SubRange(-3)` will cover the last 3 elements of the range

### Array/List Ranges
`TArrayRange` and `TListRange` allow for accessing parts of an array/list without copying the data. The ranges also allow referencing relative to the array like subranges:
```pascal
arr := [1, 2, 3, 4, 5, 6];
// Last 3 elements of the array
for i in TArrayRange<Integer>.Create(arr, -3) do
  WriteLn(i);
```
To avoid having the lazy copy mechanism take place `TArrayRange` references the target array by pointer, i.e. if the array get's destroyed before the range does, the range will cause use after free errors.

Array- and List ranges can also be created throug an implicit cast, to cover the whole array/list:
```pascal
rng := [0, 1, 2, 3];
for i in rng.SubRange(RangeBegin, RangeEnd, -1) do
  ...
```

### Conversion to Lists and Arrays
`TRange` provides `ToArray` to create an array of all the numbers covered, `ToList` to create a `TList<SizeInt>` of them and `FillList(AList)` to add the numbers to a list.
`TArrayRange` and `TListRange` have a `Copy` function to create an array/list from that subrange.
