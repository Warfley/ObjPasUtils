program optionaltest;

{$mode delphi}{$H+}

uses
  SysUtils, DynamicTypes, NoneType;

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

procedure TestOptionalMap;
var
  optInt: TOptional<Integer>;
  optStr: TOptional<String>;
begin
  Write('Testing map Int->Str on empty value: ');
  optInt := None;
  optStr := Map<Integer, String>(optInt, IntToHex);
  if OptStr then
    WriteLn('Opt str is: ', OptStr.Value)
  else
    WriteLn('OptStr has no Value');
  Write('Testing map Int->Str on 42: ');
  optInt := 42;
  optStr := Map<Integer, String>(optInt, IntToHex);
  if OptStr then
    WriteLn('Opt str is: ', OptStr.Value)
  else
    WriteLn('OptStr has no Value');
end;

begin
  TestOptionalInitialization;
  TestOptionalBoolConversion;
  TestOptionalGetOrDefault;
  TestOptionalPointerAccess;
  TestOptionalException;
  TestOptionalMap;

  ReadLn;
end.

