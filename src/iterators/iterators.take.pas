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
    TWhileConditionFunction = specialize TAnyUnaryFunction<Boolean, T>;
  private
    FFunction: TWhileConditionFunction;
    FCurrent: T;

    function UpdateCurrentAndCheck: Boolean; inline;
  public
    constructor Create(AEnumerator: IIteratorType; AWhileConditionFunction: TWhileConditionFunction);

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
  Result := FFunction.apply(FCurrent);
end;

constructor TTakeWhileIterator.Create(AEnumerator: IIteratorType;
  AWhileConditionFunction: TWhileConditionFunction);
begin
  inherited Create(AEnumerator);
  FFunction := AWhileConditionFunction;
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

