unit iterators;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, functypes, iterators.base, iterators.map, iterators.filter,
  iterators.take, iterators.skip, iterators.typing, iterators.ordering, iterators.helper,
  iterators.collector, Generics.Collections, TupleTypes, NoneType, DynamicTypes;

type
  EEndOfIterator = class(Exception);

// Create Iterators from base type
generic function Iterate<T>(const AArray: specialize TArray<T>): specialize IIterator<T>; overload; inline;
generic function Iterate<T>(const AEnumarble: specialize IEnumerable<T>): specialize IIterator<T>; overload; inline;
// Generics.collections support
generic function Iterate<T>(const AEnumarble: specialize TEnumerable<T>): specialize IIterator<T>; overload; inline;
// Classic pascal containers
function Iterate(const AStrings: TStrings): specialize IIterator<String>; overload; inline;
function Iterate(const AList: Classes.TList): specialize IIterator<Pointer>; overload; inline;

// Map functions
generic function Map<TFrom, TTo>(AIterator: specialize IIterator<TFrom>;
  AFunction: specialize TAnyUnaryFunction<TTo, TFrom>): specialize IIterator<TTo>; overload; inline;

// Filter functions
generic function Filter<T>(AIterator: specialize IIterator<T>;
  AFunction: specialize TAnyUnaryFunction<Boolean, T>): specialize IIterator<T>; overload; inline;


// Take and Takewhile
generic function Take<T>(AIterator: specialize IIterator<T>; ACount: SizeInt):
  specialize IIterator<T>; overload;
generic function TakeWhile<T>(AIterator: specialize IIterator<T>;
  AFunction: specialize TAnyUnaryFunction<Boolean, T>): specialize IIterator<T>; overload; inline;

// Skip and SkipWhile
generic function Skip<T>(AIterator: specialize IIterator<T>; ACount: SizeInt):
  specialize IIterator<T>; overload;
generic function SkipWhile<T>(AIterator: specialize IIterator<T>;
  AFunction: specialize TAnyUnaryFunction<Boolean, T>): specialize IIterator<T>; overload; inline;

// Typing functions
generic function Cast<TFrom, TTo>(AIterator: specialize IIterator<TFrom>): specialize IIterator<TTo>; overload; inline;
generic function CastObject<TFrom, TTo: TObject>(AIterator: specialize IIterator<TFrom>): specialize IIterator<TTo>; overload; inline;
generic function CastObject<TTo: TObject>(AIterator: specialize IIterator<TObject>): specialize IIterator<TTo>; overload; inline;
generic function CastObject<TTo: TObject>(AIterator: specialize IIterator<Pointer>): specialize IIterator<TTo>; overload; inline;
generic function FilterClass<TBaseClass, TFilterClass: TObject>(AIterator: specialize IIterator<TBaseClass>): specialize IIterator<TFilterClass>; overload; inline;
generic function FilterClass<TFilterClass: TObject>(AIterator: specialize IIterator<TObject>): specialize IIterator<TFilterClass>; overload; inline;
generic function ClassTypes<TBaseClass: TObject>(AIterator: specialize IIterator<TBaseClass>): specialize IIterator<TClass>; overload; inline;
function ClassTypes(AIterator: specialize IIterator<TObject>): specialize IIterator<TClass>; overload; inline;

// Fold/Reduce
generic function FoldL<TResult, TData>(AIterator: specialize IIterator<TData>;
  AFunction: specialize TAnyBinaryFunction<TResult, TResult, TData>; const InitialData: TResult): TResult; inline;
generic function FoldR<TResult, TData>(AIterator: specialize IIterator<TData>;
  AFunction: specialize TAnyBinaryFunction<TResult, TData, TResult>; const InitialData: TResult): TResult; inline;
generic function Reduce<T>(AIterator: specialize IIterator<T>;
  AFunction: specialize TAnyBinaryFunction<T, T, T>; const InitialData: T): T; overload; inline;
generic function Reduce<T>(AIterator: specialize IIterator<T>;
  AFunction: specialize TAnyBinaryFunction<T, T, T>): T; overload; inline;

// Collect
generic function CollectArray<T>(AIterator: specialize IIterator<T>; GeometricGrowth: Boolean = True): specialize TArray<T>; inline;
generic function Collect<T, TContainer>(AIterator: specialize IIterator<T>): TContainer; overload; inline;
generic function Collect<T, TContainer>(AIterator: specialize IIterator<T>; AContainer: TContainer): TContainer; overload; inline;

// Index
generic function Index<T>(AIterator: specialize IIterator<T>): specialize IIterator<specialize TPair<SizeInt, T>>; inline;

// Step
generic function Step<T>(AIterator: specialize IIterator<T>; StepSize: SizeInt): specialize IIterator<T>; inline;

// Ordering
generic function Reverse<T>(AIterator: specialize IIterator<T>): specialize IIterator<T>; inline;
// Sorting: Compare function
generic function Sorted<T>(AIterator: specialize IIterator<T>;
  AFunction: specialize TAnyBinaryFunction<Integer, T, T>): specialize IIterator<T>; overload; inline;
// Sorting: Less function
generic function Sorted<T>(AIterator: specialize IIterator<T>;
  AFunction: specialize TAnyBinaryFunction<Boolean, T, T>): specialize IIterator<T>; overload; inline;

// Get Next element from iterator
generic function Next<T>(AIterator: specialize IIterator<T>; out AValue: T): Boolean; overload; inline;
generic function Next<T>(AIterator: specialize IIterator<T>): T; overload; inline;
generic function Next<T>(AIterator: specialize IIterator<T>): specialize TOptional<T>; overload; inline;
// Get the Last element from iterator
generic function Last<T>(AIterator: specialize IIterator<T>; out AValue: T): Boolean; overload; inline;
generic function Last<T>(AIterator: specialize IIterator<T>): T; overload; inline;
generic function Last<T>(AIterator: specialize IIterator<T>): specialize TOptional<T>; overload; inline;
implementation

{ Iterate Functions }

generic function Iterate<T>(const AArray: specialize TArray<T>): specialize IIterator<T>;
begin
  Result := specialize TArrayIterator<T>.Create(AArray);
end;

generic function Iterate<T>(const AEnumarble: specialize IEnumerable<T>): specialize IIterator<T>;
begin
  Result := specialize TInteraceEnumeratorIterator<T, specialize IEnumerator<T>>.Create(AEnumarble.GetEnumerator);
end; 

generic function Iterate<T>(const AEnumarble: specialize TEnumerable<T>): specialize IIterator<T>;
begin
  Result := specialize TClassEnumeratorIterator<T, specialize TEnumerator<T>>.Create(AEnumarble.GetEnumerator);
end;

function Iterate(const AStrings: TStrings): specialize IIterator<String>;
begin
  Result := specialize TClassEnumeratorIterator<String, Classes.TStringsEnumerator>.Create(AStrings.GetEnumerator);
end;

function Iterate(const AList: Classes.TList): specialize IIterator<Pointer>;
begin
  Result := specialize TClassEnumeratorIterator<Pointer, Classes.TListEnumerator>.Create(AList.GetEnumerator);
end;

{ Map Functions }

generic function Map<TFrom, TTo>(AIterator: specialize IIterator<TFrom>;
  AFunction: specialize TAnyUnaryFunction<TTo, TFrom>): specialize IIterator<TTo>;
begin
  Result := specialize TMapIterator<TFrom, TTo>.Create(AIterator, AFunction);
end;

{ Filter Functions }

generic function Filter<T>(AIterator: specialize IIterator<T>;
  AFunction: specialize TAnyUnaryFunction<Boolean, T>): specialize IIterator<T>;
begin
  Result := specialize TFilterIterator<T>.Create(AIterator, AFunction);
end;
  
{ Take and TakeWhile Functions }

generic function Take<T>(AIterator: specialize IIterator<T>; ACount: SizeInt):
  specialize IIterator<T>;
begin
  Result := specialize TTakeIterator<T>.Create(AIterator, ACount);
end;

generic function TakeWhile<T>(AIterator: specialize IIterator<T>;
  AFunction: specialize TAnyUnaryFunction<Boolean, T>): specialize IIterator<T>;
begin
  Result := specialize TTakeWhileIterator<T>.Create(AIterator, AFunction);
end;

{ Skip and SkipWhile Functions }

generic function Skip<T>(AIterator: specialize IIterator<T>; ACount: SizeInt):
  specialize IIterator<T>;
begin
  Result := specialize TSkipIterator<T>.Create(AIterator, ACount);
end;

generic function SkipWhile<T>(AIterator: specialize IIterator<T>;
  AFunction: specialize TAnyUnaryFunction<Boolean, T>): specialize IIterator<T>;
begin
  Result := specialize TSkipWhileIterator<T>.Create(AIterator, AFunction);
end;

{ Typing functions }

generic function Cast<TFrom, TTo>(AIterator: specialize IIterator<TFrom>): specialize IIterator<TTo>;
begin
  Result := specialize TCastIterator<TFrom, TTo>.Create(AIterator);
end;

generic function CastObject<TFrom, TTo>(AIterator: specialize IIterator<TFrom>): specialize IIterator<TTo>;
begin
  Result := specialize TCastObjectIterator<TFrom, TTo>.Create(AIterator);
end;

generic function CastObject<TTo>(AIterator: specialize IIterator<TObject>): specialize IIterator<TTo>;
begin
  Result := specialize TCastObjectIterator<TObject, TTo>.Create(AIterator);
end;

generic function CastObject<TTo>(AIterator: specialize IIterator<Pointer>): specialize IIterator<TTo>;
begin
  Result := specialize TCastPointerObjectIterator<TTo>.Create(AIterator);
end;

generic function FilterClass<TBaseClass, TFilterClass>(AIterator: specialize IIterator<TBaseClass>): specialize IIterator<TFilterClass>;
begin
  Result := specialize TFilterTypeIterator<TBaseClass, TFilterClass>.Create(AIterator);
end; 

generic function FilterClass<TFilterClass>(AIterator: specialize IIterator<TObject>): specialize IIterator<TFilterClass>;
begin
  Result := specialize TFilterTypeIterator<TObject, TFilterClass>.Create(AIterator);
end;

generic function ClassTypes<TBaseClass>(AIterator: specialize IIterator<TBaseClass>): specialize IIterator<TClass>;
begin
  Result := specialize TClassTypesIterator<TBaseClass>.Create(AIterator);
end;

function ClassTypes(AIterator: specialize IIterator<TObject>): specialize IIterator<TClass>;
begin
  Result := specialize TClassTypesIterator<TObject>.Create(AIterator);
end;

{ Fold/Reduce }
generic function FoldL<TResult, TData>(AIterator: specialize IIterator<TData>;
  AFunction: specialize TAnyBinaryFunction<TResult, TResult, TData>; const InitialData: TResult): TResult;
begin
  Result := specialize FoldLCollect<TResult, TData>(AIterator, AFunction, InitialData);
end;

generic function FoldR<TResult, TData>(AIterator: specialize IIterator<TData>;
  AFunction: specialize TAnyBinaryFunction<TResult, TData, TResult>; const InitialData: TResult): TResult;
begin
  Result := specialize FoldRCollect<TResult, TData>(AIterator, AFunction, InitialData);
end;

generic function Reduce<T>(AIterator: specialize IIterator<T>;
  AFunction: specialize TAnyBinaryFunction<T, T, T>; const InitialData: T): T;
begin
  Result := specialize FoldLCollect<T, T>(AIterator, AFunction, InitialData);
end;

generic function Reduce<T>(AIterator: specialize IIterator<T>;
  AFunction: specialize TAnyBinaryFunction<T, T, T>): T;
begin
  Result := specialize FoldLCollect<T, T>(AIterator, AFunction, Default(T));
end;

{ Collectors }

generic function CollectArray<T>(AIterator: specialize IIterator<T>; GeometricGrowth: Boolean = True): specialize TArray<T>;
begin
  if GeometricGrowth then
    Result := specialize CollectArrayGeometric<T>(AIterator)
  else 
    Result := specialize CollectArrayLinear<T>(AIterator);
end;

generic function Collect<T, TContainer>(AIterator: specialize IIterator<T>): TContainer;
begin
  Result := specialize CollectContainerAdd<T, TContainer>(AIterator);
end;

generic function Collect<T, TContainer>(AIterator: specialize IIterator<T>; AContainer: TContainer): TContainer;
begin
  Result := specialize CollectContainerAdd<T, TContainer>(AIterator, AContainer);
end;

{ Index }

generic function Index<T>(AIterator: specialize IIterator<T>): specialize IIterator<specialize TPair<SizeInt, T>>;
begin
  Result := specialize TIndexIterator<T>.Create(AIterator);
end;

{ Step }
generic function Step<T>(AIterator: specialize IIterator<T>; StepSize: SizeInt): specialize IIterator<T>;
begin
  Result := specialize TStepIterator<T>.Create(AIterator, StepSize);
end;

{ Ordering }
generic function Reverse<T>(AIterator: specialize IIterator<T>): specialize IIterator<T>;
begin
  Result := specialize TReverseIterator<T>.Create(AIterator);
end;

generic function Sorted<T>(AIterator: specialize IIterator<T>;
  AFunction: specialize TAnyBinaryFunction<Integer, T, T>): specialize IIterator<T>;
begin
  Result := specialize TSortingIterator<T>.Create(AIterator, AFunction);
end;

// Sorting: Less function
generic function Sorted<T>(AIterator: specialize IIterator<T>;
  AFunction: specialize TAnyBinaryFunction<Boolean, T, T>): specialize IIterator<T>;
begin
  Result := specialize TSortingIterator<T>.Create(AIterator, AFunction);
end;

{ Next/Last Functions }

generic function Next<T>(AIterator: specialize IIterator<T>; out AValue: T): Boolean;
begin
  Result := AIterator.MoveNext;
  if Result then
    AValue := AIterator.Current;
end;

generic function Next<T>(AIterator: specialize IIterator<T>): T;
begin
  if not specialize Next<T>(AIterator, Result) then
    raise EEndOfIterator.Create('Iterator has no next element');
end;

generic function Next<T>(AIterator: specialize IIterator<T>): specialize TOptional<T>;
begin
  Result := None;
  if AIterator.MoveNext then
    Result := AIterator.Current;
end;

generic function Last<T>(AIterator: specialize IIterator<T>; out AValue: T): Boolean;
begin
  Result := AIterator.MoveNext;
  if Result then repeat
    AValue := AIterator.Current;
  until not AIterator.MoveNext;
end;

generic function Last<T>(AIterator: specialize IIterator<T>): T;
begin
  if not specialize Last<T>(AIterator, Result) then
    raise EEndOfIterator.Create('Iterator has no next element');
end;

generic function Last<T>(AIterator: specialize IIterator<T>): specialize TOptional<T>;
begin
  Result := None;
  While AIterator.MoveNext do
    Result := AIterator.Current;
end;

end.

