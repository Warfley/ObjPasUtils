program uniontest;

{$mode Delphi}{$H+}

uses
  SysUtils, nonetype, DynamicTypes;

procedure PrintUnionContent(const Union: TUnion<String, Integer>);
begin
  Write('Stored type is ', Union.GetType);
  if Union.isFirst then
    WriteLn('Value is String ', Union.First)
  else if Union.isSecond then
    WriteLn('Value is Integer ', Union.Second)
  else if not Union.HasValue then
    WriteLn('Union is Empty')
  else
    WriteLn('Union is broken');
end;

procedure TestUnionAssignment;
var
  union: TUnion<String, Integer>;
begin
  Write('Testing string assignemnt: ');
  union := 'Hello World';
  PrintUnionContent(union);
  Write('Testing integer assignemnt: ');
  union := 42;
  PrintUnionContent(union);
  Write('Testing Empty assignemnt: ');
  union := EmptyUnion;
  PrintUnionContent(union);
  Write('Testing None assignemnt: ');
  union := None;
  PrintUnionContent(union);
end;

procedure TestUnionGetOrDefault;
var
  union: TUnion<String, Integer>;
begin
  Write('Testing FirstOrDefault on String union: ');
  union := 'Hello World';
  WriteLn(union.SecondOrDefault);
  Write('Testing SecondOrDefault 42 on String union: ');
  WriteLn(union.SecondOrDefault(42));
end;

procedure TestUnionBoolConversion;
var
  union: TUnion<String, Integer>;
begin
  Write('Testing empty union bool test: ');
  union := EmptyUnion;
  if union then
    WriteLn('union has value');
  if not union then
    WriteLn('union has no value');
  Write('Testing Union 42 bool test: ');
  union := 42;
  if union then
    WriteLn('union has value');
  if not union then
    WriteLn('union has no value');
end;

procedure TestUnionOperators;
var
  union: TUnion<String, Integer>;
begin
  Write('Testing empty union = utNone: ');
  union := EmptyUnion;
  if union = utNone then
    WriteLN('union is None'); 
  Write('Testing empty union <> utFirst: ');
  if union <> utFirst then
    WriteLN('union is not First');
end;

begin
  TestUnionAssignment;
  TestUnionGetOrDefault;
  TestUnionBoolConversion;
  TestUnionOperators;

  ReadLn;
end.

