unit ranges;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$TypedAddress On}

interface

uses
  SysUtils, DynamicTypes, nonetype;

type
  TRangeComparison = (rcLower, rcLowOverlap, rcHigher, rcHighOverlap, rcSubSet, rcEqual, rcSuperSet);

  { TRange }

  generic TRange<T> = record
  public type
    TOptionalT = specialize TOptional<T>;
    TTRange = specialize TRange<T>;
  private
    FStart: TOptionalT;
    FEnd: TOptionalT;

    FOpenStart: Boolean;
    FOpenEnd: Boolean;
    function GetOpenStart: Boolean;
    function GetOpenEnd: Boolean;

    function CompareOptional(constref LHSValue, RHSValue: TOptionalT;
                             LHSInfPositive, RHSInfPositive: Boolean): Integer;
  public
    function Transform(const NewStart: TOptionalT;
                       const NewEnd: TOptionalT): TTRange;
    function IsEmpty: Boolean;
    function Compare(constref WithRange: TTRange): TRangeComparison;

    property RangeStart: TOptionalT read FStart;
    property RangeEnd: TOptionalT read FEnd;
    property OpenStart: Boolean read GetOpenStart;
    property OpenEnd: Boolean read GetOpenEnd;


    // Range Comparisons 
    class operator in(constref lhs: TTRange; constref rhs: TTRange): Boolean; inline;
    class operator =(constref lhs: TTRange; constref rhs: TTRange): Boolean; inline;
    class operator <>(constref lhs: TTRange; constref rhs: TTRange): Boolean; inline;
    class operator >(constref lhs: TTRange; constref rhs: TTRange): Boolean; inline;
    class operator >=(constref lhs: TTRange; constref rhs: TTRange): Boolean; inline;
    class operator <(constref lhs: TTRange; constref rhs: TTRange): Boolean; inline;
    class operator <=(constref lhs: TTRange; constref rhs: TTRange): Boolean; inline;

    // Element Comparisons
    class operator in(constref AValue: T; constref ARange: TTRange): Boolean; inline;
    class operator >(constref AValue: T; constref ARange: TTRange): Boolean; inline;
    class operator >=(constref AValue: T; constref ARange: TTRange): Boolean; inline;
    class operator <(constref AValue: T; constref ARange: TTRange): Boolean; inline;
    class operator <=(constref AValue: T; constref ARange: TTRange): Boolean; inline;

    // Assignment
    class operator :=(constref AValue: T): TTRange; inline;
  end;

  { TRangeSet }

  generic TRangeSet<T> = record
  public type
    TTArray = array of T;
    TTRange = specialize TRange<T>;
    TTRangeArray = array of TTRange;
    TTRangeSet = specialize TRangeSet<T>;
  private
    FRanges: TTRangeArray;
  public
    function Complement: TRangeSet;
    function Union(constref WithSet: TTRangeSet): TTRangeSet;
    function Union(constref WithRange: TTRange): TTRangeSet;
    function Intersect(constref WithSet: TTRangeSet): TTRangeSet;
    function Intersect(constref WithRange: TTRange): TTRangeSet;
    function Difference(constref WithSet: TTRangeSet): TTRangeSet;
    function Difference(constref WithRange: TTRange): TTRangeSet;

    class operator in(constref AValue: T; constref ARange: TTRangeSet): Boolean; inline;
    // Complement
    class operator -(constref ASet: TTRangeSet): TTRangeSet; inline;
    class operator not(constref ASet: TTRangeSet): TTRangeSet; inline;
    // Union
    class operator +(constref lhs: TTRangeSet; constref rhs: TTRangeSet): TTRangeSet; inline;
    class operator +(constref ASet: TTRangeSet; constref ARange: TTRange): TTRangeSet; inline;
    class operator +(constref ARange: TTRange; constref ASet: TTRangeSet): TTRangeSet; inline; 
    // Intersect
    class operator /(constref lhs: TTRangeSet; constref rhs: TTRangeSet): TTRangeSet; inline;
    class operator /(constref ASet: TTRangeSet; constref ARange: TTRange): TTRangeSet; inline;
    class operator /(constref ARange: TTRange; constref ASet: TTRangeSet): TTRangeSet; inline;
    // Difference
    class operator -(constref lhs: TTRangeSet; constref rhs: TTRangeSet): TTRangeSet; inline;
    class operator -(constref ASet: TTRangeSet; constref ARange: TTRange): TTRangeSet; inline;
    class operator -(constref ARange: TTRange; constref ASet: TTRangeSet): TTRangeSet; inline;
    // Assignment
    class operator :=(constref ARange: TTRange): TTRangeSet;
    class operator :=(constref ARanges: TTRangeArray): TTRangeSet;
    class operator :=(constref AValues: TTArray): TTRangeSet;
  end;


generic function Range<T>(AStart: specialize TOptional<T>;
                          AEnd: specialize TOptional<T>;
                          AOpenStart: Boolean = False;
                          AOpenEnd: Boolean = False): specialize TRange<T>;

implementation

generic function Range<T>(AStart: specialize TOptional<T>;
                          AEnd: specialize TOptional<T>;
                          AOpenStart: Boolean;
                          AOpenEnd: Boolean): specialize TRange<T>;
begin
  Result.FStart := AStart;
  Result.FEnd := AEnd;
  Result.FOpenStart := AOpenStart;
  Result.FOpenEnd := AOpenEnd;
end;

{ TRange }

function TRange.GetOpenEnd: Boolean;
begin
  Result := not FEnd.HasValue or FOpenEnd;
end;

function TRange.CompareOptional(constref LHSValue, RHSValue: TOptionalT;
                                LHSInfPositive, RHSInfPositive: Boolean): Integer;
begin
  if not LHSValue or not RHSValue then
  begin
    // Before we can do a value comparison we need to handle if any of the two is infinity
    // Because having multiple nested ifs is "ugly" we do a bit of arithmetic:
    //
    // For both LHS and RHS consider:
    //    xHSInfFactor := -1 + 2 * Ord(xHSInfPositive) |> -1 + 2*1 = 1  if PositiveInfinity is True
    //                                                 |> -1 + 2*0 = -1 if PositiveInfinity is False
    //
    //    xHSIsInf = Ord(not xHSValue.HasValue)        |> 1 if no finite Value present
    //                                                 |> 0 if finite value present
    //
    //    xHSInf = xHSInfFactor * xHSIsInf             |> 0 if finite value
    //                                                 |> 1 if positive infinity
    //                                                 |> -1 if negative infinity
    //
    // Result Matrix for: LHSInf - RHSInf
    //           (-1)  (0)     (1)
    // |  S/W | -inf | fin | + inf |
    // |---------------------------|
    // | -inf |   0  | -1  |  -2   |                   |> Diagonal LHS = RHS
    // |---------------------------|                   |> Above diagonal LHS < RHS
    // |  Fin |   1  |  0  |  -1   |                   |> Below diagonal: LHS > RHS
    // |---------------------------|
    // | +inf |   2  |  1  |   0   |
    // |---------------------------|
    //
    // If at least one is Infinite then it holds
    // => LHSInf - RHSInf < 0 => LHS < RHS
    // => LHSInf - RHSInf = 0 => LHS = RHS
    // => LHSInf - RHSInf > 0 => LHS > RHS
    //
    // Filling in variables:
    //    (LHSInf) - (RHSInf)
    //  = (LHSInfFactor * LHSIsInf) - (RHSInfFactor * RHSIsInf)
    //  = ( (-1 + 2 * Ord(LHSInfPositive)) * Ord(not LHSValue.HasValue) ) -
    //    ( (-1 + 2 * Ord(RHSInfPositive)) * Ord(not RHSValue.HasValue) )
    // RHS Ord(not Bool) = 1 - Ord(Bool)
    //  = ( (-1 + 2 * Ord(LHSInfPositive)) * (1 - Ord(LHSValue.HasValue)) ) -
    //    ( (-1 + 2 * Ord(RHSInfPositive)) * (1 - Ord(RHSValue.HasValue)) )

    Result := ( (-1 + 2 * Ord(LHSInfPositive)) * (1 - Ord(LHSValue.HasValue)) ) -
              ( (-1 + 2 * Ord(RHSInfPositive)) * (1 - Ord(RHSValue.HasValue)) );
    Exit;
  end;

  // If both values are finite a simple if then else is enough
  if LHSValue.Value < RHSValue.Value then
    Result := -1
  else if LHSValue.Value = RHSValue.Value then
    Result := 0
  else
    Result := 1;
end;

function TRange.GetOpenStart: Boolean;
begin
  Result := not FStart.HasValue or FOpenStart;
end;

function TRange.Transform(const NewStart: TOptionalT; const NewEnd: TOptionalT
  ): TTRange;
begin
  Result := Self;
  if NewStart then
    Result.FStart := NewStart;
  if NewEnd then
    Result.FEnd := NewEnd;
end;

function TRange.IsEmpty: Boolean;
var
  CompareVal: Integer;
begin
  CompareVal := CompareOptional(FStart, FEnd, False, True);
  Result := (CompareVal > 0)
         Or ((CompareVal = 0) and (FOpenEnd or FOpenStart));
end;

function TRange.Compare(constref WithRange: TTRange): TRangeComparison;
var
  CompareStartStart, CompareEndStart, CompareEndEnd,
    CompareStartEnd: Integer;
begin
  // Trivial cases
  if IsEmpty then
    if WithRange.IsEmpty then
      Exit(rcEqual)
    else
      Exit(rcSuperSet); // The empty set is a subset of any empty set
  if WithRange.IsEmpty then
    Exit(rcSubSet); // Any non empty set is a superset for the empty set

  // Find relative position
  CompareStartStart := CompareOptional(FStart, WithRange.FStart, False, False);
  CompareEndEnd := CompareOptional(FEnd, WithRange.FEnd, True, True);

  // Simplest case: Equality
  if (CompareStartStart = 0) and (CompareEndEnd = 0) and
     (OpenStart = WithRange.OpenStart) and (OpenEnd = WithRange.OpenEnd) then
    Exit(rcEqual);

  // Superset check i.e. SelfStart >= WithStart and SelfEnd <= WithEnd
  // Equality case is already checked above and would skip here
  // Caveat: OpenStart and OpenEnd. If the two starts are equal, but one is open the other isn't
  // Then the open start is effectively later than the closed, because the actual range starts behind it
  // For OpenEnd it's the other way around if two ends are the same, but one is Open, it's smaller
  // Because the actual end of the range is right before that open end
  // i.e. 5 and 10 are in [5..10] but neither is in (5..10) therefore it is effectively smaller
  if ((CompareStartStart > 0) Or (
         (CompareStartStart = 0) And // If it's the same we need to make sure that the start of the other aren't open when ours are closed
         (OpenStart or not WithRange.OpenStart) // When our end isn't open => WithRange cannot be open either
       )
     ) and ((CompareEndEnd < 0) Or (
         (CompareEndEnd = 0) And // As above, our end being open (and therefore smaller) is fine, but when we are closed the otherone should also be closed
         (OpenEnd or not WithRange.OpenEnd)
       )
     ) then
    Exit(rcSuperSet);

  // Subset check i.e. SelfStart <= WithStart and SelfEnd >= WithEnd
  // Basically the same as above just flipped
  if ((CompareStartStart < 0) Or (
         (CompareStartStart = 0) And
         (WithRange.OpenStart or not OpenStart) // As above, when the other start is closed, then ours cant be open
       )
     ) and ((CompareEndEnd > 0) Or (
         (CompareEndEnd = 0) And
         (WithRange.OpenEnd or not OpenEnd) // As above, when the other end is closed, then ours cant be open
       )
     ) then
    Exit(rcSubSet);

  // Now there are only two options left, either the other range starts before us and ends before we do
  if CompareStartStart >= 0 then // So we just check if there is any overlap at all
  begin
    CompareStartEnd := CompareOptional(FStart, WithRange.FEnd, False, True);
    // Similar to above we now check if SelfStart <= WithEnd to check for overlap
    // but now the only way to overlap when the value is equal is when
    // both are closed. When start is closed it is effectively greater than end
    // and when end is closed but start not, it is effectively smaller
    if (CompareStartEnd < 0) or
       ((CompareStartEnd = 0) and not WithRange.OpenEnd and not OpenEnd) then
       Exit(rcLowOverlap);
    Exit(rcLower);
  end;
  // Or it starts after us and ends after we do
  if CompareStartStart <= 0 then // Should not be necessary, but to detect errors we check anyway
  begin
    CompareEndStart := CompareOptional(FEnd, WithRange.FStart, True, False);
    // Same as above just swapped, we check if SelfEnd >= OtherStart
    if (CompareEndStart > 0) or
       ((CompareEndStart = 0) and not WithRange.OpenEnd and not OpenEnd) then
       Exit(rcHighOverlap);
    Exit(rcHigher);
  end;

  raise Exception.Create('This point should never be reached');
end;

class operator TRange.in(constref lhs: TTRange; constref rhs: TTRange): Boolean;
begin
  Result := lhs.Compare(rhs) = rcSuperSet;
end;

class operator TRange.=(constref lhs: TTRange; constref rhs: TTRange): Boolean;
begin
  Result := lhs.Compare(rhs) = rcEqual;
end;

class operator TRange.<>(constref lhs: TTRange; constref rhs: TTRange): Boolean;
begin
  Result := lhs.Compare(rhs) <> rcEqual;
end;

class operator TRange.>(constref lhs: TTRange; constref rhs: TTRange): Boolean;
begin
  Result := lhs.Compare(rhs) = rcLower;
end;

class operator TRange.>=(constref lhs: TTRange; constref rhs: TTRange): Boolean;
begin
  Result := lhs.Compare(rhs) = rcLowOverlap;
end;

class operator TRange.<(constref lhs: TTRange; constref rhs: TTRange): Boolean;
begin
  Result := lhs.Compare(rhs) = rcHigher;
end;

class operator TRange.<=(constref lhs: TTRange; constref rhs: TTRange): Boolean;
begin
  Result := lhs.Compare(rhs) = rcHighOverlap;
end;

class operator TRange.in(constref AValue: T; constref ARange: TTRange): Boolean;
begin
  Result := (
    not ARange.FStart.HasValue or (ARange.FStart.Value < AValue) or
    ((ARange.FStart.Value = AValue) and not ARange.FOpenStart)
  ) and (
    not ARange.FEnd.HasValue or (ARange.FEnd.Value > AValue) or
    ((ARange.FEnd.Value = AValue) and not ARange.FOpenEnd)
  );
end;

class operator TRange.>(constref AValue: T; constref ARange: TTRange): Boolean;
begin
  Result := not ARange.IsEmpty and ARange.FEnd.HasValue and (
    (ARange.FEnd.Value < AValue) or
    ((ARange.FEnd.Value = AValue) and ARange.FOpenEnd)
  );
end;

class operator TRange.>=(constref AValue: T; constref ARange: TTRange): Boolean;
begin
  Result := not ARange.IsEmpty and (
    not ARange.FStart.HasValue or (ARange.FStart.Value < AValue) or
    ((ARange.FStart.Value = AValue) and not ARange.FOpenStart)
  );
end;

class operator TRange.<(constref AValue: T; constref ARange: TTRange): Boolean;
begin
  Result := not ARange.IsEmpty and ARange.FStart.HasValue and (
    (ARange.FStart.Value > AValue) or
    ((ARange.FStart.Value = AValue) and ARange.OpenStart)
  );
end;

class operator TRange.<=(constref AValue: T; constref ARange: TTRange): Boolean;
begin
  Result := not ARange.IsEmpty and (
    not ARange.FEnd.HasValue or (ARange.FEnd.Value > AValue) or
    ((ARange.FEnd.Value = AValue) and not ARange.FOpenEnd)
  );
end;

class operator TRange.:=(constref AValue: T): TTRange;
begin
  Result := specialize Range<T>(AValue, AValue);
end;  

{ TRangeSet }

function TRangeSet.Complement: TRangeSet;
begin

end;

function TRangeSet.Union(constref WithSet: TTRangeSet): TTRangeSet;
begin

end;

function TRangeSet.Union(constref WithRange: TTRange): TTRangeSet;
begin

end;

function TRangeSet.Intersect(constref WithSet: TTRangeSet): TTRangeSet;
begin

end;

function TRangeSet.Intersect(constref WithRange: TTRange): TTRangeSet;
begin

end;

function TRangeSet.Difference(constref WithSet: TTRangeSet): TTRangeSet;
begin

end;

function TRangeSet.Difference(constref WithRange: TTRange): TTRangeSet;
begin

end;

class operator TRangeSet.in(constref AValue: T; constref ARange: TTRangeSet
  ): Boolean;
begin

end;

class operator TRangeSet.-(constref ASet: TTRangeSet): TTRangeSet;
begin

end;

class operator TRangeSet.not(constref ASet: TTRangeSet): TTRangeSet;
begin

end;

class operator TRangeSet.+(constref lhs: TTRangeSet; constref rhs: TTRangeSet
  ): TTRangeSet;
begin

end;

class operator TRangeSet.+(constref ASet: TTRangeSet; constref ARange: TTRange
  ): TTRangeSet;
begin

end;

class operator TRangeSet.+(constref ARange: TTRange; constref ASet: TTRangeSet
  ): TTRangeSet;
begin

end;

class operator TRangeSet./(constref lhs: TTRangeSet; constref rhs: TTRangeSet
  ): TTRangeSet;
begin

end;

class operator TRangeSet./(constref ASet: TTRangeSet; constref ARange: TTRange
  ): TTRangeSet;
begin

end;

class operator TRangeSet./(constref ARange: TTRange; constref ASet: TTRangeSet
  ): TTRangeSet;
begin

end;

class operator TRangeSet.-(constref lhs: TTRangeSet; constref rhs: TTRangeSet
  ): TTRangeSet;
begin

end;

class operator TRangeSet.-(constref ASet: TTRangeSet; constref ARange: TTRange
  ): TTRangeSet;
begin

end;

class operator TRangeSet.-(constref ARange: TTRange; constref ASet: TTRangeSet
  ): TTRangeSet;
begin

end;

class operator TRangeSet.:=(constref ARange: TTRange): TTRangeSet;
begin

end;

class operator TRangeSet.:=(constref ARanges: TTRangeArray): TTRangeSet;
begin

end;

class operator TRangeSet.:=(constref AValues: TTArray): TTRangeSet;
begin

end;

end.

