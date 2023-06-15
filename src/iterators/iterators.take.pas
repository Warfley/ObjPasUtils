unit iterators.take;

{$mode objfpc}{$H+}
{$Assertions On}

interface

uses
  SysUtils, iterators.base, functypes, Generics.Collections;

type
  EEmptySequence = class(Exception);

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

  { TTakeUntilIterator }

  generic TTakeUntilIterator<T> = class(specialize TIteratorIterator<T, T>)
  public type
    TTArray = array of T;
  private
    FSequence: Array of T;    
    FIncludeSequence: Boolean;

    FCurrent: T;
    // Simple ring queue
    FBacklog: TTArray;
    FBacklogHead: Integer;
    FBacklogLen: Integer;
    // When this flag is set, the remaining backlog will be dumped
    // no further analysis is needed
    FOnlyOutputRemainingBacklog: Boolean;

    procedure BacklogPush(const Element: T); {$IFDEF INLINING}inline;{$ENDIF}
    function BacklogPop: T; {$IFDEF INLINING}inline;{$ENDIF}
    function BacklogPeek(AIndex: Integer): T; {$IFDEF INLINING}inline;{$ENDIF}

    function GetNextElement(out NextElement: T): Boolean;
    function PeekNextElement(AIndex: Integer; out NextElement: T): Boolean;
  public
    constructor Create(AEnumerator: IIteratorType; const ASequence: TTArray; AIncludeSequence: Boolean);
    constructor Create(AEnumerator: IIteratorType; const ASequence: array of T; AIncludeSequence: Boolean);

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

{ TTakeUntilIterator }

procedure TTakeUntilIterator.BacklogPush(const Element: T);
begin
  Assert(FBacklogLen < Length(FBacklog), 'Backlog full');
  FBacklog[(FBacklogHead + FBacklogLen) mod Length(FBacklog)] := Element;
  Inc(FBacklogLen);
end;

function TTakeUntilIterator.BacklogPop: T;
begin
  Assert(FBacklogLen > 0, 'Backlog empty');
  Result := FBacklog[FBacklogHead];
  FBacklogHead := (FBacklogHead + 1) mod Length(FBacklog);
  Dec(FBacklogLen);
end;

function TTakeUntilIterator.BacklogPeek(AIndex: Integer): T;
begin
  Assert((AIndex >= 0) and (AIndex < FBacklogLen), 'Index out of bounds');
  Result := FBacklog[(FBacklogHead + AIndex) mod Length(FBacklog)];
end;

function TTakeUntilIterator.GetNextElement(out NextElement: T): Boolean;
begin
  Result := True;
  if FBacklogLen > 0 then
  begin
    NextElement := BacklogPop;
    Exit;
  end;

  if FOnlyOutputRemainingBacklog or not IteratorMoveNext then
    Exit(False);
  NextElement := IteratorCurrent;
end;

function TTakeUntilIterator.PeekNextElement(AIndex: Integer; out NextElement: T
  ): Boolean;
begin
  Result := False;
  while AIndex >= FBacklogLen do
  begin
    if not IteratorMoveNext then
      Exit;
    BacklogPush(IteratorCurrent);
  end;
  Result := True;
  NextElement := BacklogPeek(AIndex);
end;

constructor TTakeUntilIterator.Create(AEnumerator: IIteratorType;
  const ASequence: TTArray; AIncludeSequence: Boolean);
begin
  if Length(ASequence) = 0 then
    raise EEmptySequence.Create('The Until Sequence cannot be empty');

  inherited Create(AEnumerator);

  FSequence := ASequence;
  FIncludeSequence := AIncludeSequence;

  SetLength(FBacklog, Length(FSequence));
  FBacklogHead := 0;
  FBacklogLen := 0;

  FCurrent := Default(T);
  FOnlyOutputRemainingBacklog := False;
end;

constructor TTakeUntilIterator.Create(AEnumerator: IIteratorType;
  const ASequence: array of T; AIncludeSequence: Boolean);
var
  Seq: TTArray;
  i: Integer;
begin
  Seq := [];
  SetLength(Seq, Length(ASequence));
  for i:=0 to Length(ASequence) - 1 do
    Seq[i] := ASequence[i];
  Create(AEnumerator, Seq, AIncludeSequence);
end;

function TTakeUntilIterator.GetCurrent: T;
begin
  Result := FCurrent;
end;      

function TTakeUntilIterator.MoveNext: Boolean;
var
  NextElement: T;
  LookAheadPosition: Integer;
begin
  Result := GetNextElement(FCurrent);
  if not Result or FOnlyOutputRemainingBacklog or (FCurrent <> FSequence[0]) then
    Exit;

  LookAheadPosition := 1; // References the current index of Sequence
  // Read the rest of the sequence
  while LookAheadPosition < Length(FSequence) do
  begin
    if not PeekNextElement(LookAheadPosition - 1, NextElement) then // -1 because first element is FCurrent
    begin
      // There are now not enough left in the iterator to finish the sequence
      // So we just dump our backlog and are done
      FOnlyOutputRemainingBacklog := True;
      Exit;
    end;
    if NextElement <> FSequence[LookAheadPosition] then
      Break; // If we have a sequence break, no reason to continue
    Inc(LookAheadPosition);
  end;

  if LookAheadPosition >= Length(FSequence) then // the sequence was found
  begin      
    FOnlyOutputRemainingBacklog := True;
    if not FIncludeSequence then
    begin
      // Because we don't include the pattern, we are finished and can throw away the backlog
      FBacklogLen := 0;
      FBacklog := [];
      Exit(False);
    end;
  end;
end;

end.

