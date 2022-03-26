unit iterators.skip;

{$mode objfpc}{$H+}

interface

uses
  iterators.base, functypes;

type

  { TSkipIterator }

  generic TSkipIterator<T> = class(specialize TIteratorIterator<T, T>)
  private
    FCount: SizeInt;
  public
    constructor Create(AIterator: IIteratorType; ACount: SizeInt);

    function GetCurrent: T; override;
    function MoveNext: Boolean; override;
  end;

  generic TSkipWhileIterator<T> = class(specialize TIteratorIterator<T, T>)
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
    FirstMove: Boolean;

    function CheckCurrent: Boolean; inline;
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

{ TSkipIterator }

constructor TSkipIterator.Create(AIterator: IIteratorType; ACount: SizeInt);
begin
  inherited Create(AIterator);
  FCount := ACount;
end;

function TSkipIterator.GetCurrent: T;
begin
  Result := IteratorCurrent;
end;

function TSkipIterator.MoveNext: Boolean;
begin
  Result := True;
  // Increase FCount so this is executed at least once
  Inc(FCount);
  While Result and (FCount > 0) do
  begin
    Result := IteratorMoveNext;
    Dec(FCount);
  end;
end;

{ TSkipWhileIterator }

function TSkipWhileIterator.CheckCurrent: Boolean;
var
  Curr: T;
begin
  Curr := IteratorCurrent;
  case FFunction.FuncType of
    ftFunction: Result := FFunction.Func(Curr);
    ftMethod: Result := FFunction.Method(Curr);
    ftConstFunction: Result := FFunction.ConstFunc(Curr);
    ftConstMethod: Result := FFunction.ConstMethod(Curr);
  end;
end;

constructor TSkipWhileIterator.Create(AEnumerator: IIteratorType;
  AWhileConditionFunction: TWhileConditionFunctionData);
begin
  inherited Create(AEnumerator);
  FFunction := AWhileConditionFunction;
  FirstMove := True;
end;

constructor TSkipWhileIterator.Create(AEnumerator: IIteratorType;
  AWhileConditionFunction: TWhileConditionFunction);
var
  WhileConditionFunction: TWhileConditionFunctionData;
begin
  WhileConditionFunction.FuncType := ftFunction;
  WhileConditionFunction.Func := AWhileConditionFunction;
  Create(AEnumerator, WhileConditionFunction);
end;

constructor TSkipWhileIterator.Create(AEnumerator: IIteratorType;
  AWhileConditionFunction: TWhileConditionMethod);
var
  WhileConditionFunction: TWhileConditionFunctionData;
begin
  WhileConditionFunction.FuncType := ftMethod;
  WhileConditionFunction.Method := AWhileConditionFunction;
  Create(AEnumerator, WhileConditionFunction);
end;

constructor TSkipWhileIterator.Create(AEnumerator: IIteratorType;
  AWhileConditionFunction: TConstWhileConditionFunction);
var
  WhileConditionFunction: TWhileConditionFunctionData;
begin
  WhileConditionFunction.FuncType := ftConstFunction;
  WhileConditionFunction.ConstFunc := AWhileConditionFunction;
  Create(AEnumerator, WhileConditionFunction);
end;

constructor TSkipWhileIterator.Create(AEnumerator: IIteratorType;
  AWhileConditionFunction: TConstWhileConditionMethod);
var
  WhileConditionFunction: TWhileConditionFunctionData;
begin
  WhileConditionFunction.FuncType := ftConstMethod;
  WhileConditionFunction.ConstMethod := AWhileConditionFunction;
  Create(AEnumerator, WhileConditionFunction);
end;

function TSkipWhileIterator.GetCurrent: T;
begin
  Result := IteratorCurrent;
end;

function TSkipWhileIterator.MoveNext: Boolean;
begin
  Result := IteratorMoveNext;
  if FirstMove then
  begin
    while Result and CheckCurrent do
      Result := IteratorMoveNext;
    FirstMove := False;
  end;
end;
end.

