{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ObjPasUtils;

{$warn 5023 off : no warning about unused units}
interface

uses
  TupleTypes, DynamicTypes, finalizers, functypes, iterators, iterators.base, 
  iterators.map, iterators.filter, iterators.take, iterators.skip, 
  iterators.typing, iterators.helper, nonetype, iterators.collector, 
  iterators.ordering, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('ObjPasUtils', @Register);
end.
