unit nonetype;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type
  TNoneType = record
  end;

const
  None: TNoneType = ();

operator :=(None: TNoneType): Pointer; inline;
operator :=(None: TNoneType): TObject; inline;
operator :=(None: TNoneType): String; inline;

operator =(LHS: TNoneType; RHS: Pointer): Boolean; inline;
operator =(LHS: TNoneType; RHS: TObject): Boolean; inline;
operator =(LHS: TNoneType; RHS: String): Boolean; inline;
operator =(LHS: Pointer; RHS: TNoneType): Boolean; inline;
operator =(LHS: TObject; RHS: TNoneType): Boolean; inline;
operator =(LHS: String; RHS: TNoneType): Boolean; inline;
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

operator=(LHS: TNoneType; RHS: Pointer): Boolean;
begin
  Result := Assigned(RHS);
end;

operator=(LHS: TNoneType; RHS: TObject): Boolean;
begin
  Result := Assigned(RHS);
end;

operator=(LHS: TNoneType; RHS: String): Boolean;
begin
  Result := RHS = '';
end;

operator=(LHS: Pointer; RHS: TNoneType): Boolean;
begin
  Result := Assigned(LHS);
end;

operator=(LHS: TObject; RHS: TNoneType): Boolean;
begin
  Result := Assigned(LHS);
end;

operator=(LHS: String; RHS: TNoneType): Boolean;
begin
  Result := LHS = '';
end;

end.

