unit iterators.filter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, iterators.base, functypes;

type

  { TFilterIterator }

  generic TFilterIterator<T> = class(specialize TIteratorIterator<T, T>)
  public type
    TFilterFunction = specialize TUnaryFunction<Boolean, T>;
    TFilterMethod = specialize TUnaryMethodFunction<Boolean, T>;
    TConstFilterFunction = specialize TConstUnaryFunction<Boolean, T>;
    TConstFilterMethod = specialize TConstUnaryMethodFunction<Boolean, T>;
  private type
    TFilterFunctionData = record
      case FuncType: TFunctionType of
        ftFunction: (Func: TFilterFunction);
        ftMethod: (Method: TFilterMethod);
        ftConstFunction: (ConstFunc: TConstFilterFunction);
        ftConstMethod: (ConstMethod: TConstFilterMethod);
    end;
  private
    FFunction: TFilterFunctionData;
    FCurrent: T;

    function UpdateCurrentAndCheck: Boolean; inline;
  public
    constructor Create(AEnumerator: IIteratorType; AFilterFunction: TFilterFunctionData); overload;
    constructor Create(AEnumerator: IIteratorType; AFilterFunction: TFilterFunction); overload;
    constructor Create(AEnumerator: IIteratorType; AFilterFunction: TFilterMethod); overload;
    constructor Create(AEnumerator: IIteratorType; AFilterFunction: TConstFilterFunction); overload;
    constructor Create(AEnumerator: IIteratorType; AFilterFunction: TConstFilterMethod); overload;

    function GetCurrent: T; override;
    function MoveNext: Boolean; override;
  end;

implementation

{ TFilterIterator }

function TFilterIterator.UpdateCurrentAndCheck: Boolean;
begin
  FCurrent := IteratorCurrent;
case FFunction.FuncType of
  ftFunction: Result := FFunction.Func(FCurrent);
  ftMethod: Result := FFunction.Method(FCurrent);
  ftConstFunction: Result := FFunction.ConstFunc(FCurrent);
  ftConstMethod: Result := FFunction.ConstMethod(FCurrent);
end;
end;

constructor TFilterIterator.Create(AEnumerator: IIteratorType;
  AFilterFunction: TFilterFunctionData);
begin
  inherited Create(AEnumerator);
  FFunction := AFilterFunction;
end;

constructor TFilterIterator.Create(AEnumerator: IIteratorType;
  AFilterFunction: TFilterFunction);
var
  FilterFunction: TFilterFunctionData;
begin
  FilterFunction.FuncType := ftFunction;
  FilterFunction.Func := AFilterFunction;
  Create(AEnumerator, FilterFunction);
end;

constructor TFilterIterator.Create(AEnumerator: IIteratorType;
  AFilterFunction: TFilterMethod);
var
  FilterFunction: TFilterFunctionData;
begin
  FilterFunction.FuncType := ftMethod;
  FilterFunction.Method := AFilterFunction;
  Create(AEnumerator, FilterFunction);
end;

constructor TFilterIterator.Create(AEnumerator: IIteratorType;
  AFilterFunction: TConstFilterFunction);
var
  FilterFunction: TFilterFunctionData;
begin
  FilterFunction.FuncType := ftConstFunction;
  FilterFunction.ConstFunc := AFilterFunction;
  Create(AEnumerator, FilterFunction);
end;

constructor TFilterIterator.Create(AEnumerator: IIteratorType;
  AFilterFunction: TConstFilterMethod);
var
  FilterFunction: TFilterFunctionData;
begin
  FilterFunction.FuncType := ftConstMethod;
  FilterFunction.ConstMethod := AFilterFunction;
  Create(AEnumerator, FilterFunction);
end;

function TFilterIterator.GetCurrent: T;
begin 
  Result := FCurrent;
end;

function TFilterIterator.MoveNext: Boolean;
begin
  repeat
    Result := IteratorMoveNext;
  until Not Result or UpdateCurrentAndCheck;
end;

end.

