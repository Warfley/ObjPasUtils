unit iterators.take;

{$mode objfpc}{$H+}

interface

uses
  iterators.base, functypes;

type

  { TTakeIterator }

  generic TTakeIterator<T> = class(specialize TIteratorIterator<T, T>)
  private
    FCount: SizeInt;
  public
    constructor Create(AIterator: IIteratorType; ACount: SizeInt);

    function GetCurrent: T; override;
    function MoveNext: Boolean; override;
  end;

  generic TTakeWhileIterator<T> = class(specialize TIteratorIterator<T, T>)
  public type
    TWhileConditionFunction = specialize TUnaryFunction<Boolean, T>;
    TWhileConditionMethod = specialize TUnaryMethodFunction<Boolean, T>;
    TConstWhileConditionFunction = specialize TConstUnaryFunction<Boolean, T>;
    TConstWhileConditionMethod = specialize TConstUnaryMethodFunction<Boolean, T>;
  private type
    TWhileConditionFunctionData = record
      case FuncType: TFunctionType of
        ftFunction: (Func: TWhileConditionFunction);
        ftMethod: (Method: TWhileConditionMethod);
        ftConstFunction: (ConstFunc: TConstWhileConditionFunction);
        ftConstMethod: (ConstMethod: TConstWhileConditionMethod);
    end;
  private
    FFunction: TWhileConditionFunctionData;
    FCurrent: T;

    function UpdateCurrentAndCheck: Boolean; inline;
  public
    constructor Create(AEnumerator: IIteratorType; AWhileConditionFunction: TWhileConditionFunctionData); overload;
    constructor Create(AEnumerator: IIteratorType; AWhileConditionFunction: TWhileConditionFunction); overload;
    constructor Create(AEnumerator: IIteratorType; AWhileConditionFunction: TWhileConditionMethod); overload;
    constructor Create(AEnumerator: IIteratorType; AWhileConditionFunction: TConstWhileConditionFunction); overload;
    constructor Create(AEnumerator: IIteratorType; AWhileConditionFunction: TConstWhileConditionMethod); overload;

    function GetCurrent: T; override;
    function MoveNext: Boolean; override;
  end;


implementation

{ TTakeIterator }

constructor TTakeIterator.Create(AIterator: IIteratorType; ACount: SizeInt);
begin
  inherited Create(AIterator);
  // + 1 because MoveNext will be called before the first element
  // So the counting starts after the first decrement
  FCount := ACount + 1;
end;

function TTakeIterator.GetCurrent: T;
begin
  Result := IteratorCurrent;
end;

function TTakeIterator.MoveNext: Boolean;
begin
  Dec(FCount);
  result := FCount > 0;
  if Result then
    Result := IteratorMoveNext;
end;

{ TTakeWhileIterator }

function TTakeWhileIterator.UpdateCurrentAndCheck: Boolean;
begin
  FCurrent := IteratorCurrent;
case FFunction.FuncType of
  ftFunction: Result := FFunction.Func(FCurrent);
  ftMethod: Result := FFunction.Method(FCurrent);
  ftConstFunction: Result := FFunction.ConstFunc(FCurrent);
  ftConstMethod: Result := FFunction.ConstMethod(FCurrent);
end;
end;

constructor TTakeWhileIterator.Create(AEnumerator: IIteratorType;
  AWhileConditionFunction: TWhileConditionFunctionData);
begin
  inherited Create(AEnumerator);
  FFunction := AWhileConditionFunction;
end;

constructor TTakeWhileIterator.Create(AEnumerator: IIteratorType;
  AWhileConditionFunction: TWhileConditionFunction);
var
  WhileConditionFunction: TWhileConditionFunctionData;
begin
  WhileConditionFunction.FuncType := ftFunction;
  WhileConditionFunction.Func := AWhileConditionFunction;
  Create(AEnumerator, WhileConditionFunction);
end;

constructor TTakeWhileIterator.Create(AEnumerator: IIteratorType;
  AWhileConditionFunction: TWhileConditionMethod);
var
  WhileConditionFunction: TWhileConditionFunctionData;
begin
  WhileConditionFunction.FuncType := ftMethod;
  WhileConditionFunction.Method := AWhileConditionFunction;
  Create(AEnumerator, WhileConditionFunction);
end;

constructor TTakeWhileIterator.Create(AEnumerator: IIteratorType;
  AWhileConditionFunction: TConstWhileConditionFunction);
var
  WhileConditionFunction: TWhileConditionFunctionData;
begin
  WhileConditionFunction.FuncType := ftConstFunction;
  WhileConditionFunction.ConstFunc := AWhileConditionFunction;
  Create(AEnumerator, WhileConditionFunction);
end;

constructor TTakeWhileIterator.Create(AEnumerator: IIteratorType;
  AWhileConditionFunction: TConstWhileConditionMethod);
var
  WhileConditionFunction: TWhileConditionFunctionData;
begin
  WhileConditionFunction.FuncType := ftConstMethod;
  WhileConditionFunction.ConstMethod := AWhileConditionFunction;
  Create(AEnumerator, WhileConditionFunction);
end;

function TTakeWhileIterator.GetCurrent: T;
begin
  Result := FCurrent;
end;

function TTakeWhileIterator.MoveNext: Boolean;
begin
  Result := IteratorMoveNext And UpdateCurrentAndCheck;
end;

end.

