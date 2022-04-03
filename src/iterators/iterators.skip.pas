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

  { TSkipWhileIterator }

  generic TSkipWhileIterator<T> = class(specialize TIteratorIterator<T, T>)
  public type
    TWhileConditionFunction = specialize TAnyUnaryFunction<Boolean, T>;
  private
    FFunction: TWhileConditionFunction;
    FirstMove: Boolean;

  public
    constructor Create(AEnumerator: IIteratorType; AWhileConditionFunction: TWhileConditionFunction);

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

constructor TSkipWhileIterator.Create(AEnumerator: IIteratorType;
  AWhileConditionFunction: TWhileConditionFunction);
begin
  inherited Create(AEnumerator);
  FFunction := AWhileConditionFunction;
  FirstMove := True;
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
    while Result and FFunction.apply(IteratorCurrent) do
      Result := IteratorMoveNext;
    FirstMove := False;
  end;
end;
end.

