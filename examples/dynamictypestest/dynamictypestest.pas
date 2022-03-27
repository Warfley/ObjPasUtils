program dynamictypestest;

{$mode delphi}{$H+}

uses
  SysUtils, DynamicTypes;

procedure TestOptionalInitialization;
var
  opt: TOptional<Integer>;
begin
  Write('Testing Initialization: ');
  opt :=  EmptyOptional;
  WriteLn('Optional has Value: ', opt.HasValue);
end;

procedure TestOptionalBoolConversion;
var
  opt: TOptional<Integer>;
begin
  Write('Testing empty Optional bool test: ');
  opt := EmptyOptional;
  if opt then
    WriteLn('Opt has Value ', opt.Value);
  if not opt then
    WriteLn('Opt has no value');
  Write('Testing Optional 42 bool test: ');
  opt := 42;
  if opt then
    WriteLn('Opt has Value ', opt.Value);
  if not opt then
    WriteLn('Opt has no value');
end;

procedure TestOptionalGetOrDefault;
var
  opt: TOptional<Integer>;
begin
  Write('Testing GetOrDefault 42 on empty: ');
  opt := EmptyOptional;
  WriteLn(opt.GetOrDefault(42));
  Write('Testing GetOrDefault on 32: ');
  opt := 32;
  WriteLn(opt.GetOrDefault);
end;

procedure TestOptionalPointerAccess;
var
  opt: TOptional<Integer>;
begin
  Write('Testing Pointer access to change 42 to 32: ');
  opt := 42;
  opt.Mutable^ := 32;
  WriteLn(opt.Value);
end; 

procedure TestOptionalException;
var
  opt: TOptional<Integer>;
begin
  Write('Testing Exception accessing empty value: ');
  opt :=  EmptyOptional;
  try
    WriteLn(opt.Value);
  except on E: Exception do
    WriteLn('Exception: ', E.Message);
  end;
end;

begin
  TestOptionalInitialization;
  TestOptionalBoolConversion;
  TestOptionalGetOrDefault;
  TestOptionalPointerAccess;
  TestOptionalException;

  ReadLn;
end.

