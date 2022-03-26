unit iterators.map;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, iterators.base, functypes;

type

  { TMapIterator }

  generic TMapIterator<TFrom, TTo> = class(specialize TIteratorIterator<TTo, TFrom>)
  public type
    TMapFunction = specialize TUnaryFunction<TTo, TFrom>;
    TMapMethod = specialize TUnaryMethodFunction<TTo, TFrom>;
    TConsTMapFunction = specialize TConstUnaryFunction<TTo, TFrom>;
    TConsTMapMethod = specialize TConstUnaryMethodFunction<TTo, TFrom>;
  private type
    TMapFunctionData = record
      case FuncType: TFunctionType of
        ftFunction: (Func: TMapFunction);
        ftMethod: (Method: TMapMethod);
        ftConstFunction: (ConstFunc: TConsTMapFunction);
        ftConstMethod: (ConstMethod: TConsTMapMethod);
    end;
  private
    FFunction: TMapFunctionData;
  public
    constructor Create(AEnumerator: IIteratorType; AMapFunction: TMapFunctionData); overload;
    constructor Create(AEnumerator: IIteratorType; AMapFunction: TMapFunction); overload;
    constructor Create(AEnumerator: IIteratorType; AMapFunction: TMapMethod); overload;
    constructor Create(AEnumerator: IIteratorType; AMapFunction: TConsTMapFunction); overload;
    constructor Create(AEnumerator: IIteratorType; AMapFunction: TConsTMapMethod); overload;

    function GetCurrent: TTo; override;
    function MoveNext: Boolean; override;
  end;

implementation

{ TMapIterator }

constructor TMapIterator.Create(AEnumerator: IIteratorType;
  AMapFunction: TMapFunctionData);
begin
  inherited Create(AEnumerator);
  FFunction := AMapFunction;
end;

constructor TMapIterator.Create(AEnumerator: IIteratorType;
  AMapFunction: TMapFunction);
var
  MapFunction: TMapFunctionData;
begin
  MapFunction.FuncType := ftFunction;
  MapFunction.Func := AMapFunction;
  Create(AEnumerator, MapFunction);
end;

constructor TMapIterator.Create(AEnumerator: IIteratorType;
  AMapFunction: TMapMethod);
var
  MapFunction: TMapFunctionData;
begin
  MapFunction.FuncType := ftMethod;
  MapFunction.Method := AMapFunction;
  Create(AEnumerator, MapFunction);
end;

constructor TMapIterator.Create(AEnumerator: IIteratorType;
  AMapFunction: TConsTMapFunction);
var
  MapFunction: TMapFunctionData;
begin
  MapFunction.FuncType := ftConstFunction;
  MapFunction.ConstFunc := AMapFunction;
  Create(AEnumerator, MapFunction);
end;

constructor TMapIterator.Create(AEnumerator: IIteratorType;
  AMapFunction: TConsTMapMethod);
var
  MapFunction: TMapFunctionData;
begin
  MapFunction.FuncType := ftConstMethod;
  MapFunction.ConstMethod := AMapFunction;
  Create(AEnumerator, MapFunction);
end;

function TMapIterator.GetCurrent: TTo;
var
  SourceValue: TFrom;
begin
  SourceValue := IteratorCurrent;
  case FFunction.FuncType of
  ftFunction: Result := FFunction.Func(SourceValue);
  ftMethod: Result := FFunction.Method(SourceValue);
  ftConstFunction: Result := FFunction.ConstFunc(SourceValue);
  ftConstMethod: Result := FFunction.ConstMethod(SourceValue);
  end;
end;

function TMapIterator.MoveNext: Boolean;
begin
  Result := IteratorMoveNext;
end;

end.

