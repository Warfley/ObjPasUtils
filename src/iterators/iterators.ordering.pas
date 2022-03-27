unit iterators.ordering;

{$mode objfpc}{$H+}

interface

uses
  iterators.base, iterators.collector,  functypes;

type

  { TReverseIterator }

  generic TReverseIterator<T> = class(specialize TIterator<T>)
  private
    FIndex: SizeInt;
    FData: specialize TArray<T>;
  public
    constructor Create(AIterator: specialize IIterator<T>);

    function MoveNext: Boolean; override;
    function GetCurrent: T; override;
  end;

  { TSortingIterator }

  generic TSortingIterator<T> = class(specialize TIterator<T>)
  public type
    IIteratorType = specialize IIterator<T>;

    TCompareFunction = specialize TBinaryFunction<Integer, T, T>;
    TCompareMethod = specialize TBinaryMethodFunction<Integer, T, T>;
    TConstCompareFunction = specialize TConstBinaryFunction<Integer, T, T>;
    TConstCompareMethod = specialize TConstBinaryMethodFunction<Integer, T, T>;

    TLessFunction = specialize TBinaryFunction<Boolean, T, T>;
    TLessMethod = specialize TBinaryMethodFunction<Boolean, T, T>;
    TConstLessFunction = specialize TConstBinaryFunction<Boolean, T, T>;
    TConstLessMethod = specialize TConstBinaryMethodFunction<Boolean, T, T>;
  private type
    TCompareFunctionData = record
      case FuncType: TFunctionType of
        ftFunction: (Func: TCompareFunction);
        ftMethod: (Method: TCompareMethod);
        ftConstFunction: (ConstFunc: TConstCompareFunction);
        ftConstMethod: (ConstMethod: TConstCompareMethod);
    end;
    TLessFunctionData = record
      case FuncType: TFunctionType of
        ftFunction: (Func: TLessFunction);
        ftMethod: (Method: TLessMethod);
        ftConstFunction: (ConstFunc: TConstLessFunction);
        ftConstMethod: (ConstMethod: TConstLessMethod);
    end;
    TSortingFunctionData = record
      case isLess: Boolean of
      True: (LessFunctionData: TLessFunctionData);
      False: (CompareFunctionData: TCompareFunctionData);
    end;
  private
    // Heap functions
    function hasLChild(AIndex: SizeInt): Boolean; inline;
    function hasRChild(AIndex: SizeInt): Boolean; inline;
    class function isRoot(AIndex: SizeInt): Boolean; static; inline;
    class function getParent(AIndex: SizeInt): SizeInt; static inline;  
    class function getLChild(AIndex: SizeInt): SizeInt; static inline;
    class function getRChild(AIndex: SizeInt): SizeInt; static inline;
  private
    // Using a heap structure based on an array
    FData: Array of T;
    FHeapSize: SizeInt;
    FSortingFunctionData: TSortingFunctionData;
    FFirstMove: Boolean;

    function CompareLess(const LHS, RHS: T; const LessFunction: TLessFunctionData): Boolean;
    function CompareCompare(const LHS, RHS: T; const CompareFunctioN: TCompareFunctionData): Boolean;
    function Compare(LIndex, RIndex: SizeInt): Boolean; inline;

    procedure Swap(AIndex, BIndex: SizeInt);
    procedure Heapify(AIndex: SizeInt);
    function InsertData(constref AElem: T): SizeInt;
    procedure FillData(AIterator: IIteratorType);
  public
    constructor Create(AIterator: IIteratorType; ASortingFunction: TSortingFunctionData); overload;
    constructor Create(AIterator: IIteratorType; ACompareFunction: TCompareFunction); overload;
    constructor Create(AIterator: IIteratorType; ACompareFunction: TCompareMethod); overload;
    constructor Create(AIterator: IIteratorType; ACompareFunction: TConstCompareFunction); overload;
    constructor Create(AIterator: IIteratorType; ACompareFunction: TConstCompareMethod); overload;
    constructor Create(AIterator: IIteratorType; ALessFunction: TLessFunction); overload;
    constructor Create(AIterator: IIteratorType; ALessFunction: TLessMethod); overload;
    constructor Create(AIterator: IIteratorType; ALessFunction: TConstLessFunction); overload;
    constructor Create(AIterator: IIteratorType; ALessFunction: TConstLessMethod); overload;

    function GetCurrent: T; override;
    function MoveNext: Boolean; override;
  end;

implementation

{ TSortingIterator }

// Heap functions: See https://github.com/Warfley/STAX/blob/master/src/stax.helpertypes.pas#L272

function TSortingIterator.hasLChild(AIndex: SizeInt): Boolean;
begin
  Result := AIndex * 2 + 1 < FHeapSize;
end;

function TSortingIterator.hasRChild(AIndex: SizeInt): Boolean;
begin
  Result := AIndex * 2 + 2 < FHeapSize;
end;

class function TSortingIterator.isRoot(AIndex: SizeInt): Boolean;
begin
  Result := AIndex = 0;
end;

class function TSortingIterator.getParent(AIndex: SizeInt): SizeInt;
begin
  Result := (AIndex - 1) div 2;
end;

class function TSortingIterator.getLChild(AIndex: SizeInt): SizeInt;
begin
  Result := AIndex * 2 + 1;
end;

class function TSortingIterator.getRChild(AIndex: SizeInt): SizeInt;
begin
  Result := AIndex * 2 + 2;
end;

function TSortingIterator.CompareLess(const LHS, RHS: T;
  const LessFunction: TLessFunctionData): Boolean;
begin
  case LessFunction.FuncType of
  ftFunction: Result := LessFunction.Func(LHS, RHS);
  ftMethod: Result := LessFunction.Method(LHS, RHS);
  ftConstFunction: Result := LessFunction.ConstFunc(LHS, RHS);
  ftConstMethod: Result := LessFunction.ConstMethod(LHS, RHS);
  end;
end;

function TSortingIterator.CompareCompare(const LHS, RHS: T;
  const CompareFunctioN: TCompareFunctionData): Boolean;
begin
  case CompareFunctioN.FuncType of
  ftFunction: Result := CompareFunctioN.Func(LHS, RHS) < 0;
  ftMethod: Result := CompareFunctioN.Method(LHS, RHS) < 0;
  ftConstFunction: Result := CompareFunctioN.ConstFunc(LHS, RHS) < 0;
  ftConstMethod: Result := CompareFunctioN.ConstMethod(LHS, RHS) < 0;
  end;
end;

function TSortingIterator.Compare(LIndex, RIndex: SizeInt): Boolean;
begin
  if FSortingFunctionData.isLess then
    Result := CompareLess(FData[LIndex], FData[RIndex], FSortingFunctionData.LessFunctionData)
  else
    Result := CompareCompare(FData[LIndex], FData[RIndex], FSortingFunctionData.CompareFunctionData);
end;

procedure TSortingIterator.Swap(AIndex, BIndex: SizeInt);
var
  tmp: T;
begin
  // Use move to avoid overhead of managed types
  Move(FData[AIndex], tmp, SizeOf(T));
  Move(FData[BIndex], FData[AIndex], SizeOf(T));
  Move(tmp, FData[BIndex], SizeOf(T));
end;

procedure TSortingIterator.Heapify(AIndex: SizeInt);
var
  SwapChild: SizeInt;
begin
  while hasLChild(AIndex) do
  begin
    if not HasRChild(AIndex) or Compare(getLChild(AIndex), getRChild(AIndex)) then
      SwapChild := getLChild(AIndex)
    else
      SwapChild := getRChild(AIndex);
    // If current is smaller than the smalles child, it is in the right place
    if Compare(AIndex, SwapChild) then
      Break;
    // Otherwise swap with child
    Swap(AIndex, SwapChild);
    // and continue checking that new place
    AIndex := SwapChild;
  end;
end;

function TSortingIterator.InsertData(constref AElem: T): SizeInt;
begin
  if FHeapSize >= Length(FData) then
    SetLength(FData, Length(FData) * 2);
  Result := FHeapSize;
  inc(FHeapSize);
  FData[Result] := AElem;
  While not isRoot(Result) and Compare(Result, getParent(Result)) do
  begin
    Swap(Result, getParent(Result));
    Result := getParent(Result);
  end;
end;

procedure TSortingIterator.FillData(AIterator: IIteratorType);
var
  Elem: T;
begin
  SetLength(FData, 2048 div SizeOf(T)); // Half a page
  FHeapSize := 0; // Empty heap
  for elem in AIterator do
    InsertData(Elem);
  // Shrink to fit
  SetLength(FData, FHeapSize);
end;

constructor TSortingIterator.Create(AIterator: IIteratorType;
  ASortingFunction: TSortingFunctionData);
begin
  inherited Create;
  FSortingFunctionData := ASortingFunction;
  FillData(AIterator);
  FFirstMove := True;
end;

constructor TSortingIterator.Create(AIterator: IIteratorType;
  ACompareFunction: TCompareFunction);
var
  SortingData: TSortingFunctionData;
begin
  SortingData.isLess := False;
  SortingData.CompareFunctionData.FuncType := ftFunction;
  SortingData.CompareFunctionData.Func := ACompareFunction;
  Create(AIterator, SortingData);
end;

constructor TSortingIterator.Create(AIterator: IIteratorType;
  ACompareFunction: TCompareMethod);
var
  SortingData: TSortingFunctionData;
begin
  SortingData.isLess := False;
  SortingData.CompareFunctionData.FuncType := ftMethod;
  SortingData.CompareFunctionData.Method := ACompareFunction;
  Create(AIterator, SortingData);
end;

constructor TSortingIterator.Create(AIterator: IIteratorType;
  ACompareFunction: TConstCompareFunction);
var
  SortingData: TSortingFunctionData;
begin
  SortingData.isLess := False;
  SortingData.CompareFunctionData.FuncType := ftConstFunction;
  SortingData.CompareFunctionData.ConstFunc := ACompareFunction;
  Create(AIterator, SortingData);
end;

constructor TSortingIterator.Create(AIterator: IIteratorType;
  ACompareFunction: TConstCompareMethod);
var
  SortingData: TSortingFunctionData;
begin
  SortingData.isLess := False;
  SortingData.CompareFunctionData.FuncType := ftConstMethod;
  SortingData.CompareFunctionData.ConstMethod := ACompareFunction;
  Create(AIterator, SortingData);
end;

constructor TSortingIterator.Create(AIterator: IIteratorType;
  ALessFunction: TLessFunction);
var
  SortingData: TSortingFunctionData;
begin
  SortingData.isLess := True;
  SortingData.LessFunctionData.FuncType := ftFunction;
  SortingData.LessFunctionData.Func := ALessFunction;
  Create(AIterator, SortingData);
end;

constructor TSortingIterator.Create(AIterator: IIteratorType;
  ALessFunction: TLessMethod);
var
  SortingData: TSortingFunctionData;
begin
  SortingData.isLess := True;
  SortingData.LessFunctionData.FuncType := ftMethod;
  SortingData.LessFunctionData.Method:= ALessFunction;
  Create(AIterator, SortingData);
end;

constructor TSortingIterator.Create(AIterator: IIteratorType;
  ALessFunction: TConstLessFunction);
var
  SortingData: TSortingFunctionData;
begin
  SortingData.isLess := True;
  SortingData.LessFunctionData.FuncType := ftConstFunction;
  SortingData.LessFunctionData.ConstFunc := ALessFunction;
  Create(AIterator, SortingData);
end;

constructor TSortingIterator.Create(AIterator: IIteratorType;
  ALessFunction: TConstLessMethod);
var
  SortingData: TSortingFunctionData;
begin
  SortingData.isLess := True;
  SortingData.LessFunctionData.FuncType := ftConstMethod;
  SortingData.LessFunctionData.ConstMethod:= ALessFunction;
  Create(AIterator, SortingData);
end;

function TSortingIterator.GetCurrent: T;
begin
  Result := FData[0];
end;

function TSortingIterator.MoveNext: Boolean;
begin
  if FFirstMove then
  begin
    FFirstMove := False;
    Exit(FHeapSize > 0);
  end;
  Dec(FHeapSize);
  Result := FHeapSize > 0;
  if not Result then
  begin
    FData := nil;
    Exit;
  end;
  Swap(0, FHeapSize);
  Heapify(0);
end;

{ TReverseIterator }

constructor TReverseIterator.Create(AIterator: specialize IIterator<T>);
begin
  inherited Create;
  FData := specialize CollectArrayGeometric<T>(AIterator);
  FIndex := Length(FData);
end;

function TReverseIterator.MoveNext: Boolean;
begin
  Dec(FIndex);
  Result := FIndex >= 0;
end;

function TReverseIterator.GetCurrent: T;
begin
  Result := FData[FIndex]
end;

end.

