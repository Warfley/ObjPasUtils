unit functypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFunctionType = (ftFunction, ftMethod, ftConstFunction, ftConstMethod);

  generic TFunction<TResult> = function: TResult;
  generic TMethodFunction<TResult> = function: TResult of object;

  generic TUnaryFunction<TResult, TParam1> = function(AParam: TParam1): TResult;
  generic TUnaryMethodFunction<TResult, TParam1> =
    function(AParam: TParam1): TResult of object;
  generic TConstUnaryFunction<TResult, TParam1> =
    function(const AParam: TParam1): TResult;
  generic TConstUnaryMethodFunction<TResult, TParam1> =
    function(const AParam: TParam1): TResult of object;

  generic TBinaryFunction<TResult, TParam1, TParam2> =
    function(AParam1: TParam1; AParam2: TParam2): TResult;
  generic TBinaryMethodFunction<TResult, TParam1, TParam2> =
    function(AParam1: TParam1; AParam2: TParam2): TResult of object;
  generic TConstBinaryFunction<TResult, TParam1, TParam2> =
    function(const AParam1: TParam1; const AParam2: TParam2): TResult;
  generic TConstBinaryMethodFunction<TResult, TParam1, TParam2> =
    function(const AParam1: TParam1; const AParam2: TParam2): TResult of object;

  generic TTernaryFunction<TResult, TParam1, TParam2, TParam3> =
    function(AParam1: TParam1; AParam2: TParam2; AParam3: TParam3): TResult;
  generic TTernaryMethodFunction<TResult, TParam1, TParam2, TParam3> =
    function(AParam1: TParam1; AParam2: TParam2; AParam3: TParam3): TResult of object;
  generic TConstTernaryFunction<TResult, TParam1, TParam2, TParam3> =
    function(const AParam1: TParam1; const AParam2: TParam2; const AParam3: TParam3): TResult;
  generic TConstTernaryMethodFunction<TResult, TParam1, TParam2, TParam3> =
    function(const AParam1: TParam1; const AParam2: TParam2; const AParam3: TParam3): TResult of object;

            
  TProcedureType = (ptFunction, ptMethod, ptConstFunction, ptConstMethod);
  TProcedure = procedure;
  TMethodProcedure = procedure of object;

  generic TUnaryProcedure<TParam1> = procedure(AParam: TParam1);
  generic TUnaryMethodProcedure<TParam1> =
    procedure(AParam: TParam1) of object;
  generic TConstUnaryProcedure<TParam1> = procedure(const AParam: TParam1);
  generic TConstUnaryMethodProcedure<TParam1> =
    procedure(const AParam: TParam1) of object;

  generic TBinaryProcedure<TParam1, TParam2> =
    procedure(AParam1: TParam1; AParam2: TParam2);
  generic TBinaryMethodProcedure<TParam1, TParam2> =
    procedure(AParam1: TParam1; AParam2: TParam2) of object;
  generic TConstBinaryProcedure<TParam1, TParam2> =
    procedure(const AParam1: TParam1; const AParam2: TParam2);
  generic TConstBinaryMethodProcedure<TParam1, TParam2> =
    procedure(const AParam1: TParam1; const AParam2: TParam2) of object;

  generic TTernaryProcedure<TParam1, TParam2, TParam3> =
    procedure(AParam1: TParam1; AParam2: TParam2; AParam3: TParam3);
  generic TTernaryMethodProcedure<TParam1, TParam2, TParam3> =
    procedure(AParam1: TParam1; AParam2: TParam2; AParam3: TParam3) of object;
  generic TConstTernaryProcedure<TParam1, TParam2, TParam3> =
    procedure(const AParam1: TParam1; const AParam2: TParam2; const AParam3: TParam3);
  generic TConstTernaryMethodProcedure<TParam1, TParam2, TParam3> =
    procedure(const AParam1: TParam1; const AParam2: TParam2; const AParam3: TParam3) of object;

implementation

end.

