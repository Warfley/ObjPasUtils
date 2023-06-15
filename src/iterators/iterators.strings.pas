unit iterators.strings;

{$mode objfpc}{$H+}
{$ZeroBasedStrings On}

interface

uses
  SysUtils, iterators.base;

type
  EInvalidUTF8Char = class(Exception);
  EIncompleteUTF8Char = class(Exception);

  { TCharIterator }

  TCharIterator = class(specialize TIterator<Char>)
  private
    FDataString: String;
    FPosition: SizeInt;
  public
    constructor Create(const AData: String);

    function GetCurrent: Char; override;
    function MoveNext: Boolean; override;
  end;

  { TUTF8Iterator }

  TUTF8Iterator = class(specialize TIterator<String>)
  private
    FDataString: String;
    FPosition: SizeInt;
    FCurrent: String;
  public
    constructor Create(const AData: String);

    function GetCurrent: String; override;
    function MoveNext: Boolean; override;
  end;

  { TUTF8AggregateIterator }

  TUTF8AggregateIterator = class(specialize TIteratorIterator<String, Char>)
  private
    FCurrent: String;
  public
    constructor Create(AIterator: IIteratorType);

    function GetCurrent: String; override;
    function MoveNext: Boolean; override;
  end;

  { TStringSplitIterator }

  TStringSplitIterator = class(specialize TIterator<String>)
  private
    FDataString: String;
    FPosition: SizeInt;
    FSplitSequence: String;
    FCurrent: String;
  public
    constructor Create(const AData: String; const ADelimiter: String);

    function GetCurrent: String; override;
    function MoveNext: Boolean; override;
  end;

implementation

function ValidFirstUTF8Char(FirstChar: Char): Boolean; inline;
begin
  {$Push}
  {$B+} // Disable short circuit to avoid jump instructions
  Result := ((ord(FirstChar) And %10000000) = %00000000)  // One char sequence
         Or ((ord(FirstChar) And %11100000) = %11000000)  // Two char sequence
         Or ((ord(FirstChar) And %11110000) = %11100000)  // Three char sequence
         Or ((ord(FirstChar) And %11111000) = %11110000); // Four char sequence
  {$Pop}
end;

function ValidUTF8FollowChar(FollowChar: Char): Boolean; inline;
begin
  Result := (ord(FollowChar) And %11000000) = %10000000;
end;

function UTF8CodePointLen(FirstChar: Char): Integer; inline;
begin
  Result := 1 + ord((ord(FirstChar) And %11000000) = %11000000)
              + ord((ord(FirstChar) And %11100000) = %11100000)
              + ord((ord(FirstChar) And %11110000) = %11110000);
end;


{ TCharIterator }

constructor TCharIterator.Create(const AData: String);
begin
  inherited Create;
  FDataString := AData;
  FPosition := -1;
end;

function TCharIterator.GetCurrent: Char;
begin
  Result := FDataString[FPosition];
end;

function TCharIterator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < Length(FDataString);
end;

{ TUTF8Iterator }

constructor TUTF8Iterator.Create(const AData: String);
begin
  inherited Create;
  FDataString := AData;
  FPosition := 0;
  FCurrent := '';
end;

function TUTF8Iterator.GetCurrent: String;
begin
  Result := FCurrent;
end;

function TUTF8Iterator.MoveNext: Boolean;
var
  CodePointLen: Integer;
  FirstChar: Char;
  i: SizeInt;
begin
  if FPosition >= Length(FDataString) then
    Exit(False);

  FirstChar := FDataString[FPosition];
  if not ValidFirstUTF8Char(FirstChar) then
    raise EInvalidUTF8Char.CreateFmt('Invalid UTF8 char: %s', [ord(FirstChar)]);

  CodePointLen := UTF8CodePointLen(FirstChar);
  if FPosition + CodePointLen > Length(FDataString) then
    raise EIncompleteUTF8Char.CreateFmt('Incomplete UTF8 char at end of string [%d]', [FPosition]);

  for i := FPosition + 1 to FPosition + CodePointLen - 1 do
    if not ValidUTF8FollowChar(FDataString[i]) then
      raise EInvalidUTF8Char.CreateFmt('Invalid UTF8 char: %d', [ord(FDataString[i])]);

  Result := True;
  FCurrent := FDataString.Substring(FPosition, CodePointLen);
  Inc(FPosition, CodePointLen);
end; 

{ TUTF8AggregateIterator }

constructor TUTF8AggregateIterator.Create(AIterator: IIteratorType);
begin
  inherited Create(AIterator);
  FCurrent := '';
end;

function TUTF8AggregateIterator.GetCurrent: String;
begin
  Result := FCurrent;
end;

function TUTF8AggregateIterator.MoveNext: Boolean;
var
  FirstChar, NextChar: Char;
  CodePointLen, i: Integer;
begin
  Result := IteratorMoveNext;
  if not Result then
    Exit;
  FirstChar := IteratorCurrent;

  if not ValidFirstUTF8Char(FirstChar) then
    raise EInvalidUTF8Char.CreateFmt('Invalid UTF8 char: %s', [ord(FirstChar)]);

  CodePointLen := UTF8CodePointLen(FirstChar);
  SetLength(FCurrent, CodePointLen);
  FCurrent[0] := FirstChar;

  for i:=1 to CodePointLen - 1 do
    if not IteratorMoveNext then
      raise EIncompleteUTF8Char.Create(FCurrent.Substring(0, i))
    else
    begin
      NextChar := IteratorCurrent;
      if not ValidUTF8FollowChar(NextChar) then
        raise EInvalidUTF8Char.CreateFmt('Invalid UTF8 char: %s', [ord(NextChar)]);
      FCurrent[i] := NextChar;
    end;
end;    

{ TStringSplitIterator }

constructor TStringSplitIterator.Create(const AData: String; const ADelimiter: String);
begin
  inherited Create;
  FDataString := AData;
  FPosition := 0;
  FSplitSequence := ADelimiter;
  FCurrent := '';
end;

function TStringSplitIterator.GetCurrent: String;
begin
  Result := FCurrent;
end;

function TStringSplitIterator.MoveNext: Boolean;
var
  Len, PatternIndex: SizeInt;
begin
  if FPosition >= Length(FDataString) then
    Exit(False);
  Result := True;

  Len := 0;
  PatternIndex := 0;

  // Search first occurance
  while FPosition + Len < Length(FDataString) do
  begin
    if FDataString[FPosition + Len] = FSplitSequence[PatternIndex] then
    begin
      Inc(PatternIndex);
      if PatternIndex >= Length(FSplitSequence) then
      begin
        Inc(Len); // As we skip the Inc down below, we must take it here
        Break;  // End of pattern reached, split here
      end;
    end
    else if PatternIndex > 0 then // Pattern mismatch, backtrack to 2nd char of pattern match
    begin
      Len -= PatternIndex;
      PatternIndex := 0;
    end;

    Inc(Len);
  end;

  FCurrent := FDataString.Substring(FPosition, Len);
  // Strip Pattern from result
  if PatternIndex >= Length(FSplitSequence) then
    SetLength(FCurrent, Length(FCurrent) - PatternIndex);
  // Move Position
  FPosition += Len;
end;

end.

