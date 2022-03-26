unit iterators.base;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, finalizers;

type
  // COM interface for reference counting
  generic IIterator<T> = interface
    function GetCurrent: T;
    function MoveNext: Boolean;

    function GetEnumerator: specialize IIterator<T>;
    property Current: T read GetCurrent;
  end;

  { TIterator }

  generic TIterator<T> = class(TInterfacedObject, specialize IIterator<T>)
  public
    function GetCurrent: T; virtual; abstract;
    function MoveNext: Boolean; virtual; abstract;

    function GetEnumerator: specialize IIterator<T>;
    property Current: T read GetCurrent;
  end;

  { TEnumeratorIterator }

  generic TEnumeratorIterator<T, TEnumerator, TFinalizer> = class(specialize TIterator<T>)
  private
    FEnumerator: TEnumerator; 
    FOwnsEnumerator: Boolean;
  public
    constructor Create(AEnumerator: TEnumerator; AOwnsEnumerator: Boolean = True);
    destructor Destroy; override;

    function GetCurrent: T; override;
    function MoveNext: Boolean; override;
  end;

  generic TClassEnumeratorIterator<T, TEnumerator> =
    class(specialize TEnumeratorIterator<T, TEnumerator, specialize TClassFinalizer<TEnumerator>>);
  generic TRecordEnumeratorIterator<T, TEnumerator> =
    class(specialize TEnumeratorIterator<T, TEnumerator, specialize TBaseFinalizer<TEnumerator>>);
  generic TInteraceEnumeratorIterator<T, TEnumerator> =
    class(specialize TEnumeratorIterator<T, TEnumerator, specialize TBaseFinalizer<TEnumerator>>);
  // To avoid confusion with TObject, and as Objects are probably rare anyway, this is ignored
  // Uncomment if needed
  //generic TObjectEnumeratorIterator<T, TEnumerator> =
  //  class(specialize TEnumeratorIterator<T, TEnumerator, specialize TObjectFinalizer<TEnumerator>>);

  { TIteratorIterator }

  generic TIteratorIterator<T, TSource> = class(specialize TIterator<T>)
  protected type
    IIteratorType = specialize IIterator<TSource>;
  private
    FIterator: IIteratorType;
  protected
    function IteratorMoveNext: Boolean; inline;
    function IteratorCurrent: TSource; inline;
  public
    constructor Create(AIterator: IIteratorType);
  end;

  { TArrayIterator }

  generic TArrayIterator<T> = class(specialize TIterator<T>)
  public type
    TArrayType = array of T;
  private
    FData: TArrayType;
    FHead: SizeInt;
  public
    constructor Create(const AArray: TArrayType);

    function GetCurrent: T; override;
    function MoveNext: Boolean; override;
  end;

implementation

{ TIteratorIterator }

function TIteratorIterator.IteratorMoveNext: Boolean;
begin
  Result := FIterator.MoveNext;
end;

function TIteratorIterator.IteratorCurrent: TSource;
begin
  Result := FIterator.GetCurrent;
end;

constructor TIteratorIterator.Create(AIterator: IIteratorType);
begin
  inherited Create;
  FIterator := AIterator;
end;

{ TArrayIterator }

constructor TArrayIterator.Create(const AArray: TArrayType);
begin
  inherited Create;
  FData := AArray;
  FHead := -1;
end;

function TArrayIterator.GetCurrent: T;
begin
  Result := FData[FHead];
end;

function TArrayIterator.MoveNext: Boolean;
begin
  Inc(FHead);
  Result := FHead <= High(FData);
end;

{ TIterator }

function TIterator.GetEnumerator: specialize IIterator<T>;
begin
  Result := Self;
end; 

{ TEnumeratorIterator }

constructor TEnumeratorIterator.Create(AEnumerator: TEnumerator;
  AOwnsEnumerator: Boolean);
begin
  inherited Create;
  FEnumerator := AEnumerator;
  FOwnsEnumerator := AOwnsEnumerator;
end;

destructor TEnumeratorIterator.Destroy;
begin
  if FOwnsEnumerator then
    TFinalizer.Finalize(FEnumerator);
  inherited Destroy;
end;

function TEnumeratorIterator.GetCurrent: T;
begin
  Result := FEnumerator.Current;
end;

function TEnumeratorIterator.MoveNext: Boolean;
begin
  Result := FEnumerator.MoveNext;
end;

end.

