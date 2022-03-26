unit ranges;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch typehelpers}

interface

uses
  SysUtils, Generics.Collections;

const
  RangeEnd = SizeInt.MaxValue;
  RangeBegin = 0;

type
  EOutOfRangeException = class(Exception);
  EInvalidRangeException = class(Exception);

  { TRange }

  TRange = record
  public type
    TSizeIntArray = array of SizeInt;
    TSizeIntList = specialize TList<SizeInt>;
  private
    FStart: SizeInt;
    FStop: SizeInt;
    FStep: SizeInt;
    FInitialStep: Boolean;
    function GetElement(AIndex: SizeInt): SizeInt;
  public
    function Start: SizeInt; {$IFDEF INLINING}inline;{$ENDIF}
    function Stop: SizeInt; {$IFDEF INLINING}inline;{$ENDIF}
    function Step: SizeInt; {$IFDEF INLINING}inline;{$ENDIF}
    function Count: SizeInt; {$IFDEF INLINING}inline;{$ENDIF}

    function MoveNext: Boolean;
    property Current: SizeInt read FStart;

    constructor Create(const AStart: SizeInt; const AStop: SizeInt; const AStep: SizeInt = 1);

    function GetEnumerator: TRange; {$IFDEF INLINING}inline;{$ENDIF}
    function ToArray: TSizeIntArray;
    function FillList(const AList: TSizeIntList): SizeInt;
    function ToList: TSizeIntList;

    function SubRange(AStart: SizeInt; AStop: SizeInt = RangeEnd;
                      AStep: SizeInt = 1): TRange;
    function SubRange(constref ARange: TRange): TRange; {$IFDEF INLINING}inline;{$ENDIF}

    property Elements[const AIndex: SizeInt]: SizeInt read GetElement; default;
  end;

  { TArrayRange }

  generic TArrayRange<T> = record
  public type
    TArrayType = array of T;
    PArrayType = ^TArrayType;
    TSpecializedArrayRange = specialize TArrayRange<T>;

    { TArrayHelper }

    //TArrayHelper = type helper for TArrayType
    //function Range(AStart: SizeInt= RangeBegin; AStop: SizeInt = RangeEnd;
    //                  AStep: SizeInt = 1): TSpecializedArrayRange; {$IFDEF INLINING}inline;{$ENDIF}
    //function Range(constref ARange: TRange): TSpecializedArrayRange; {$IFDEF INLINING}inline;{$ENDIF}
    //end;
  private
    FInternalRange: TRange;
    FArray: PArrayType;
    function GetCurrent: T; {$IFDEF INLINING}inline;{$ENDIF}
    function GetElement(const AIndex: SizeInt): T; {$IFDEF INLINING}inline;{$ENDIF}
  public
    function Start: SizeInt; {$IFDEF INLINING}inline;{$ENDIF}
    function Stop: SizeInt; {$IFDEF INLINING}inline;{$ENDIF}
    function Step: SizeInt; {$IFDEF INLINING}inline;{$ENDIF}
    function Count: SizeInt; {$IFDEF INLINING}inline;{$ENDIF}

    function MoveNext: Boolean; {$IFDEF INLINING}inline;{$ENDIF}
    property Current: T read GetCurrent;

    constructor Create(constref AArray: TArrayType; AStart: SizeInt = RangeBegin;
      AStop: SizeInt = RangeEnd; const AStep: SizeInt = 1);
    constructor Create(constref AArray: TArrayType; constref ARange: TRange);

    function GetEnumerator: TSpecializedArrayRange; {$IFDEF INLINING}inline;{$ENDIF}
    function Copy: TArrayType;

    function SubRange(AStart: SizeInt; AStop: SizeInt = RangeEnd;
                      AStep: SizeInt = 1): TSpecializedArrayRange;
    function SubRange(constref ARange: TRange): TSpecializedArrayRange;

    property Elements[const AIndex: SizeInt]: T read GetElement; default;

    class operator :=(constref AArray: TArrayType): TSpecializedArrayRange;
  end;

  { TListRange }

  generic TListRange<T> = record
  public type
    TListType = specialize TList<T>;
    TSpecializedListRange = specialize TListRange<T>;

    { TListHelper }

    //TListHelper = class Helper for TListType
    //function Range(AStart: SizeInt= RangeBegin; AStop: SizeInt = RangeEnd;
    //                  AStep: SizeInt = 1): TSpecializedListRange; {$IFDEF INLINING}inline;{$ENDIF}
    //function Range(constref ARange: TRange): TSpecializedListRange; {$IFDEF INLINING}inline;{$ENDIF}
    //end;
  private
    FInternalRange: TRange;
    FList: TListType;
    function GetCurrent: T; {$IFDEF INLINING}inline;{$ENDIF}
    function GetElement(const AIndex: SizeInt): T; {$IFDEF INLINING}inline;{$ENDIF}
  public
    function Start: SizeInt; {$IFDEF INLINING}inline;{$ENDIF}
    function Stop: SizeInt; {$IFDEF INLINING}inline;{$ENDIF}
    function Step: SizeInt; {$IFDEF INLINING}inline;{$ENDIF}
    function Count: SizeInt; {$IFDEF INLINING}inline;{$ENDIF}

    function MoveNext: Boolean; {$IFDEF INLINING}inline;{$ENDIF}
    property Current: T read GetCurrent;

    constructor Create(const AList: TListType; AStart: SizeInt = RangeBegin;
      AStop: SizeInt = RangeEnd; const AStep: SizeInt = 1);
    constructor Create(const AList: TListType; constref ARange: TRange);

    function GetEnumerator: TSpecializedListRange; {$IFDEF INLINING}inline;{$ENDIF}
    function FillList(const AList: TListType): SizeInt;
    function Copy: TListType;

    function SubRange(AStart: SizeInt; AStop: SizeInt = RangeEnd;
                      AStep: SizeInt = 1): TSpecializedListRange;
    function SubRange(constref ARange: TRange): TSpecializedListRange;

    property Elements[const AIndex: SizeInt]: T read GetElement; default;

    class operator:=(const AList: TListType): TSpecializedListRange;
  end;

function Range(const Start: SizeInt; const Stop: SizeInt; const Step: SizeInt = 1): TRange; {$IFDEF INLINING}inline;{$ENDIF}
implementation

function Range(const Start: SizeInt; const Stop: SizeInt; const Step: SizeInt
  ): TRange;
begin
  Result := TRange.Create(Start, Stop, Step);
end;

{ TListRange.TListHelper }

//function TListRange.TListHelper.Range(AStart: SizeInt; AStop: SizeInt;
//  AStep: SizeInt): TSpecializedListRange;
//begin
//  Result := TSpecializedListRange(self, AStart, AStop, AStep);
//end;
//
//function TListRange.TListHelper.Range(constref ARange: TRange
//  ): TSpecializedListRange;
//begin
//  Result := TSpecializedListRange(self, ARange);
//end;

//{ TArrayRange.TArrayHelper }
//
//function TArrayRange.TArrayHelper.Range(AStart: SizeInt; AStop: SizeInt;
//  AStep: SizeInt): TSpecializedArrayRange;
//begin
//  Result := TSpecializedArrayRange.Create(self, AStart, AStop, AStep);
//end;
//
//function TArrayRange.TArrayHelper.Range(constref ARange: TRange
//  ): TSpecializedArrayRange;
//begin
//  Result := TSpecializedArrayRange.Create(self, ARange);
//end;

{ TListRange }

function TListRange.GetCurrent: T;
begin
  Result := FList[FInternalRange.Start];
end;

function TListRange.GetElement(const AIndex: SizeInt): T;
begin
  Result := FList[FInternalRange[AIndex]];
end;

function TListRange.Start: SizeInt;
begin
  Result := FInternalRange.Start;
end;

function TListRange.Stop: SizeInt;
begin
  Result := FInternalRange.Stop;
end;

function TListRange.Step: SizeInt;
begin
  Result := FInternalRange.Step;
end;

function TListRange.Count: SizeInt;
begin
  Result := FInternalRange.Count;
end;

function TListRange.MoveNext: Boolean;
begin
  Result := FInternalRange.MoveNext;
end;

constructor TListRange.Create(const AList: TListType; AStart: SizeInt;
  AStop: SizeInt; const AStep: SizeInt);
begin
  if AStop > AList.Count then
    AStop := AList.Count;
  if AStart > AList.Count then
    AStart := AList.Count;
  while AStop < 0 do
    AStop += AList.Count;
  while AStart < 0 do
    AStart += AList.Count;
  FInternalRange := Range(AStart, AStop, AStep);
  FList := AList;
end;

constructor TListRange.Create(const AList: TListType; constref ARange: TRange);
begin
  Create(AList, ARange.Start, ARange.Stop, ARange.Step);
end;

function TListRange.GetEnumerator: TSpecializedListRange;
begin
  Result.FList := FList;
  Result.FInternalRange := FInternalRange.GetEnumerator;
end;

function TListRange.FillList(const AList: TListType): SizeInt;
var
  elem: T;
begin
  Result := 0;
  for elem in self do
  begin
    AList.Add(elem);
    Inc(Result);
  end;
end;

function TListRange.Copy: TListType;
begin
  Result := TListType.Create;
  FillList(Result);
end;

function TListRange.SubRange(AStart: SizeInt; AStop: SizeInt; AStep: SizeInt
  ): TSpecializedListRange;
begin
  Result.FInternalRange := FInternalRange.SubRange(AStart, AStop, AStep);
  Result.FList := FList;
end;

function TListRange.SubRange(constref ARange: TRange): TSpecializedListRange;
begin
  Result := SubRange(ARange.Start, ARange.Stop, ARange.Step);
end;

class operator TListRange.:=(const AList: TListType): TSpecializedListRange;
begin
  Result := TSpecializedListRange.Create(AList);
end;

{ TArrayRange }

function TArrayRange.GetCurrent: T;
begin
  Result := FArray^[FInternalRange.Current];
end;

function TArrayRange.GetElement(const AIndex: SizeInt): T;
begin
  Result := FArray^[FInternalRange[AIndex]];
end;

function TArrayRange.Start: SizeInt;
begin
  Result := FInternalRange.Start;
end;

function TArrayRange.Stop: SizeInt;
begin
  Result := FInternalRange.Stop;
end;

function TArrayRange.Step: SizeInt;
begin
  Result := FInternalRange.Step;
end;

function TArrayRange.Count: SizeInt;
begin
  Result := FInternalRange.Count;
end;

function TArrayRange.MoveNext: Boolean;
begin
  Result := FInternalRange.MoveNext;
end;

constructor TArrayRange.Create(constref AArray: TArrayType;
  AStart: SizeInt; AStop: SizeInt; const AStep: SizeInt);
begin
  if AStop > Length(AArray) then
    AStop := Length(AArray);
  if AStart > Length(AArray) then
    AStart := Length(AArray);
  while AStop < 0 do
    AStop += Length(AArray);
  while AStart < 0 do
    AStart += Length(AArray);
  FInternalRange := Range(AStart, AStop, AStep);
  FArray := @AArray;
end;

constructor TArrayRange.Create(constref AArray: TArrayType; constref ARange: TRange
  );
begin
  Create(AArray, ARange.Start, ARange.Stop, ARange.Step);
end;

function TArrayRange.GetEnumerator: TSpecializedArrayRange;
begin
  Result.FArray := FArray;
  Result.FInternalRange := FInternalRange.GetEnumerator;
end;

function TArrayRange.Copy: TArrayType;
var
  i: SizeInt;
begin
  Result := nil;
  SetLength(Result, FInternalRange.Count);
  for i:=0 to Length(Result) -1 do
    Result[i] := GetElement(i);
end;

function TArrayRange.SubRange(AStart: SizeInt; AStop: SizeInt; AStep: SizeInt
  ): TSpecializedArrayRange;
begin
  Result.FInternalRange := FInternalRange.SubRange(AStart, AStop, AStep);
  Result.FArray := FArray;
end;

function TArrayRange.SubRange(constref ARange: TRange): TSpecializedArrayRange;
begin
  Result := SubRange(ARange.Start, ARange.Stop, ARange.Step);
end;

class operator TArrayRange.:=(constref AArray: TArrayType
  ): TSpecializedArrayRange;
begin
  Result := TSpecializedArrayRange.Create(AArray);
end;

{ TRange }

function TRange.Count: SizeInt;
begin
  Result := (FStop - FStart) div FStep
end;

function TRange.GetElement(AIndex: SizeInt): SizeInt;
begin
  while AIndex < 0 do
    AIndex += Count;
  if AIndex >= Count then
    raise EOutOfRangeException.CreateFmt('Element %d not in range', [AIndex]);
  Result := FStart + FStep * AIndex;
end;

function TRange.Start: SizeInt;
begin
  Result := FStart;
end;

function TRange.Stop: SizeInt;
begin
  Result := FStop;
end;

function TRange.Step: SizeInt;
begin
  Result := FStep;
end;

function TRange.MoveNext: Boolean;
begin
  Result := Count > 1;
  if Result then
    if FInitialStep then
      FInitialStep := False
    else
      Inc(Fstart, FStep);
end;

constructor TRange.Create(const AStart: SizeInt; const AStop: SizeInt;
  const AStep: SizeInt);
begin
  if AStep = 0 then
    raise EInvalidRangeException.Create('Step size can''t be 0');
  if ((AStep < 0) and (AStart < AStop)) or ((AStep > 0) and (AStart > AStop)) then
    FStart := AStop
  else
    FStart := AStart;
  FStop := AStop;
  FStep := AStep;
end;

function TRange.GetEnumerator: TRange;
begin
  Result := self;
  Result.FInitialStep := True;
end;

function TRange.ToArray: TSizeIntArray;
var
  i: SizeInt;
begin
  Result := nil;
  SetLength(Result, Count);
  for i := 0 to Length(Result) - 1 do
    Result[i] := FStart + i * FStep;
end;

function TRange.FillList(const AList: TSizeIntList): SizeInt;
var
  elem: SizeInt;
begin
  Result := 0;
  for elem in self do
  begin
    AList.Add(elem);
    Inc(Result);
  end;
end;

function TRange.ToList: TSizeIntList;
begin
  Result := TSizeIntList.Create;
  FillList(Result);
end;

function TRange.SubRange(AStart: SizeInt; AStop: SizeInt; AStep: SizeInt
  ): TRange;
begin
  if Count = 0 then
  begin
    AStart := 0;
    AStop := 0;
  end;
  if AStart > Count then
    AStart := Count;
  While AStart < 0 do
    AStart += Count;
  if AStop > Count then
    AStop := Count;
  While AStop < 0 do
    AStop += Count;
  Result := Range(Start + AStart, AStop + Start, AStep * Step);
end;

function TRange.SubRange(constref ARange: TRange): TRange;
begin
  Result := SubRange(ARange.Start, ARange.Stop, ARange.Step);
end;

end.

