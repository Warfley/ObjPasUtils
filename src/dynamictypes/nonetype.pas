unit nonetype;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type
  TNoneType = record
  end;

var
  None: TNoneType;

operator :=(None: TNoneType): Pointer; inline;
operator :=(None: TNoneType): TObject; inline;
operator :=(None: TNoneType): String; inline;
implementation

operator:=(None: TNoneType): Pointer;
begin
  Result := nil;
end;

operator:=(None: TNoneType): TObject;
begin
  Result := nil;
end;

operator:=(None: TNoneType): String;
begin
  Result := String.Empty;
end;

end.

