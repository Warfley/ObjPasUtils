unit iterators.collector;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, iterators.base, functypes;

type

  { TFoldLCollector }

  generic TFoldLCollector<TResult, TData> = record
  public type
    TOwnType = specialize TFoldLCollector<TResult, TData>;
    TIteratorType = specialize IIterator<TData>;
    TFoldFunction = specialize TBinaryFunction<TResult, TResult, TData>;
    TFoldMethod = specialize TBinaryMethodFunction<TResult, TResult, TData>;
    TConsTFoldFunction = specialize TConstBinaryFunction<TResult, TResult, TData>;
    TConsTFoldMethod = specialize TConstBinaryMethodFunction<TResult, TResult, TData>;
  public
    class operator :=(AFunction: TFoldFunction): TOwnType;
    class operator :=(AFunction: TFoldMethod): TOwnType;
    class operator :=(AFunction: TConsTFoldFunction): TOwnType;
    class operator :=(AFunction: TConsTFoldMethod): TOwnType;

    function Collect(AIterator: TIteratorType; const InitialValue: TResult): TResult; inline;
  public
    case FuncType: TFunctionType of
      ftFunction: (Func: TFoldFunction);
      ftMethod: (Method: TFoldMethod);
      ftConstFunction: (ConstFunc: TConsTFoldFunction);
      ftConstMethod: (ConstMethod: TConsTFoldMethod);
  end;

  { TFoldRCollector }

  generic TFoldRCollector<TResult, TData> = record
  public type
    TOwnType = specialize TFoldRCollector<TResult, TData>;  
    TIteratorType = specialize IIterator<TData>;
    TFoldFunction = specialize TBinaryFunction<TResult, TData, TResult>;
    TFoldMethod = specialize TBinaryMethodFunction<TResult, TData, TResult>;
    TConsTFoldFunction = specialize TConstBinaryFunction<TResult, TData, TResult>;
    TConsTFoldMethod = specialize TConstBinaryMethodFunction<TResult, TData, TResult>;
  public
    class operator :=(AFunction: TFoldFunction): TOwnType;
    class operator :=(AFunction: TFoldMethod): TOwnType;
    class operator :=(AFunction: TConsTFoldFunction): TOwnType;
    class operator :=(AFunction: TConsTFoldMethod): TOwnType;

    function Collect(AIterator: TIteratorType; const InitialValue: TResult): TResult; inline;
  public
    case FuncType: TFunctionType of
      ftFunction: (Func: TFoldFunction);
      ftMethod: (Method: TFoldMethod);
      ftConstFunction: (ConstFunc: TConsTFoldFunction);
      ftConstMethod: (ConstMethod: TConsTFoldMethod);
  end;

generic function CollectArrayGeometric<T>(AIterator: specialize IIterator<T>): specialize TArray<T>;
generic function CollectArrayLinear<T>(AIterator: specialize IIterator<T>): specialize TArray<T>;
generic function CollectContainerAdd<T, TContainer>(AIterator: specialize IIterator<T>; AContainer: TContainer): TContainer; overload;
generic function CollectContainerAdd<T, TContainer>(AIterator: specialize IIterator<T>): TContainer; overload; inline;

implementation

generic function CollectArrayGeometric<T>(AIterator: specialize IIterator<T>): specialize TArray<T>;
var
  Head, len: SizeInt;
begin
  Result := nil;
  Head := 0;
  len := 2048 div SizeOf(T); // half a page
  SetLength(Result, len);
  while AIterator.MoveNext do
  begin
    if head >= len then
    begin
      len *= 2;
      SetLength(Result, len);
    end;
    Result[Head] := AIterator.Current;
    Inc(Head);
  end;
  SetLength(Result, Head);
end;

generic function CollectArrayLinear<T>(AIterator: specialize IIterator<T>): specialize TArray<T>;
begin
  Result := nil;
  while AIterator.MoveNext do
    Insert(AIterator.Current, Result, Length(Result));
end;
                                                                                              
generic function CollectContainerAdd<T, TContainer>(AIterator: specialize IIterator<T>; AContainer: TContainer): TContainer;
begin
  Result := AContainer;
  while AIterator.MoveNext do
    Result.Add(AIterator.Current);
end;

generic function CollectContainerAdd<T, TContainer>(AIterator: specialize IIterator<T>): TContainer;
begin
  Result := specialize CollectContainerAdd<T, TContainer>(AIterator, TContainer.Create);
end;


{ TFoldLCollector }

class operator TFoldLCollector.:=(AFunction: TFoldFunction): TOwnType;
begin
  Result.FuncType := ftFunction;
  Result.Func := AFunction;
end;

class operator TFoldLCollector.:=(AFunction: TFoldMethod): TOwnType;
begin
  Result.FuncType := ftMethod;
  Result.Method := AFunction;
end;

class operator TFoldLCollector.:=(AFunction: TConsTFoldFunction): TOwnType;
begin
  Result.FuncType := ftConstFunction;
  Result.ConstFunc := AFunction;
end;

class operator TFoldLCollector.:=(AFunction: TConsTFoldMethod): TOwnType;
begin
  Result.FuncType := ftConstMethod;
  Result.ConstMethod := AFunction;
end;

function TFoldLCollector.Collect(AIterator: TIteratorType;
  const InitialValue: TResult): TResult;
begin
  Result := InitialValue;
  while AIterator.MoveNext do
    case FuncType of
    ftFunction: Result := Func(Result, AIterator.Current);
    ftMethod: Result := Method(Result, AIterator.Current);
    ftConstFunction: Result := ConstFunc(Result, AIterator.Current);
    ftConstMethod: Result := ConstMethod(Result, AIterator.Current);
    end;
end;

{ TFoldRCollector }

class operator TFoldRCollector.:=(AFunction: TFoldFunction): TOwnType;
begin
  Result.FuncType := ftFunction;
  Result.Func := AFunction;
end;

class operator TFoldRCollector.:=(AFunction: TFoldMethod): TOwnType;
begin
  Result.FuncType := ftMethod;
  Result.Method := AFunction;
end;

class operator TFoldRCollector.:=(AFunction: TConsTFoldFunction): TOwnType;
begin
  Result.FuncType := ftConstFunction;
  Result.ConstFunc := AFunction;
end;

class operator TFoldRCollector.:=(AFunction: TConsTFoldMethod): TOwnType;
begin
  Result.FuncType := ftConstMethod;
  Result.ConstMethod := AFunction;
end;

function TFoldRCollector.Collect(AIterator: TIteratorType;
  const InitialValue: TResult): TResult;
var
  arr: Array of TData;
  i: SizeInt;
begin
  Result := InitialValue;
  Arr := specialize CollectArrayGeometric<TData>(AIterator);
  for i := High(arr) downto Low(arr) do
    case FuncType of
    ftFunction: Result := Func(arr[i], Result);
    ftMethod: Result := Method(arr[i], Result);
    ftConstFunction: Result := ConstFunc(arr[i], Result);
    ftConstMethod: Result := ConstMethod(arr[i], Result);
    end;
end;

end.

