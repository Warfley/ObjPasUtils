unit CustomPath;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils;

type
  EInvalidPathException = class(Exception);
  EPathNotRelativeException = class(Exception);

  { TCustomPath }

  generic TCustomPath<Params> = record
  public type
    TPath = specialize TCustomPath<Params>;
  private
    FPath: String;

    class function IncludeDelimiter(const Path: String): String; static; {$IFDEF inlining}inline;{$ENDIF}
    class function ExcludeDelimiter(const Path: String): String; static; {$IFDEF inlining}inline;{$ENDIF}

    function isRoot: Boolean; {$IFDEF inlining}inline;{$ENDIF}
    function SharedPrefixLength(Path1, Path2: String): Integer;
  public
    function AsString: String; {$IFDEF inlining}inline;{$ENDIF}

    function isAbsolute: Boolean; {$IFDEF inlining}inline;{$ENDIF}
    function isRelative: Boolean; {$IFDEF inlining}inline;{$ENDIF}

    function Normalize: TPath;
    function Concat(WithPath: TPath): TPath; {$IFDEF inlining}inline;{$ENDIF}
    function Parent: TPath; {$IFDEF inlining}inline;{$ENDIF}
    function Name: String; {$IFDEF inlining}inline;{$ENDIF}

    function RelativeTo(const ToPath: TPath): TPath;

    function Equals(const ToPath: TPath): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    function isPrefixTo(ToPath: TPath): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    function isRelativeTo(ToPath: TPath): Boolean; {$IFDEF inlining}inline;{$ENDIF}

    constructor Create(const APath: String);

    class operator :=(const APath: String): TPath; {$IFDEF inlining}inline;{$ENDIF}
    class operator :=(const APath: TPath): String; {$IFDEF inlining}inline;{$ENDIF}
    class operator :=(const APath: RawByteString): TPath; {$IFDEF inlining}inline;{$ENDIF}
    class operator :=(const APath: TPath): RawByteString; {$IFDEF inlining}inline;{$ENDIF}

    class operator /(const RHS: TPath; const LHS: TPath): TPath; {$IFDEF inlining}inline;{$ENDIF}
    class operator /(const RHS: TPath; const LHS: String): TPath; {$IFDEF inlining}inline;{$ENDIF}
    class operator mod(const RHS: TPath; const LHS: TPath): TPath; {$IFDEF inlining}inline;{$ENDIF}
    class operator mod(const RHS: TPath; const LHS: String): TPath; {$IFDEF inlining}inline;{$ENDIF}
    class operator shl(const RHS: TPath; const LHS: Integer): TPath; {$IFDEF inlining}inline;{$ENDIF}
    class operator +(const RHS: TPath; const LHS: String): TPath; {$IFDEF inlining}inline;{$ENDIF}

    class operator =(const RHS: TPath; const LHS: TPath): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator =(const RHS: TPath; const LHS: String): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator =(const RHS: String; const LHS: TPath): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator >=(const RHS: TPath; const LHS: TPath): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator >=(const RHS: TPath; const LHS: String): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator >=(const RHS: String; const LHS: TPath): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator <=(const RHS: TPath; const LHS: TPath): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator <=(const RHS: TPath; const LHS: String): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator <=(const RHS: String; const LHS: TPath): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator >(const RHS: TPath; const LHS: TPath): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator >(const RHS: TPath; const LHS: String): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator >(const RHS: String; const LHS: TPath): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator <(const RHS: TPath; const LHS: TPath): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator <(const RHS: TPath; const LHS: String): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator <(const RHS: String; const LHS: TPath): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator <>(const RHS: TPath; const LHS: TPath): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator <>(const RHS: TPath; const LHS: String): Boolean; {$IFDEF inlining}inline;{$ENDIF}
    class operator <>(const RHS: String; const LHS: TPath): Boolean; {$IFDEF inlining}inline;{$ENDIF}
  end;

implementation

{ TCustomPath }

class function TCustomPath.IncludeDelimiter(const Path: String): String;
begin
  if (Path.Length > 0) and (Path[Path.Length] = Params.PathDelim) then
    Result := Path
  else
    Result := Path + Params.PathDelim;
end;

class function TCustomPath.ExcludeDelimiter(const Path: String): String;
begin
  if (Path.Length > 0) and (Path[Path.Length] = Params.PathDelim) then
    Result := Path.Substring(0, Path.Length-1)
  else
    Result := Path;
end;

function TCustomPath.isRoot: Boolean;
begin
  Result := Params.isRoot(FPath);
end;

function TCustomPath.SharedPrefixLength(Path1, Path2: String): Integer;
var
  min, i: SizeInt;
begin
  if not Params.CaseSensitive then
  begin
    Path1 := Path1.ToLower;
    Path2 := Path2.ToLower;
  end;
  if Path1.Length < Path2.Length then
    min := Path1.Length
  else
    min := Path2.Length;

  for i := 0 to min - 1 do
    if Path1.Chars[i] <> Path2.Chars[i] then
      Exit(i);
  Result := min;
end;

function TCustomPath.AsString: String;
begin
  Result := FPath;
end;

function TCustomPath.isAbsolute: Boolean;
begin
  Result := Params.isAbsolute(FPath);
end;

function TCustomPath.isRelative: Boolean;
begin
  Result := not Params.isAbsolute(FPath);
end;

function TCustomPath.Normalize: TPath;
function handlePart(const part: String; parts: TStringList): SizeInt;
begin
  Result := 0;
  if part.IsEmpty and (parts.Count > 0) then
    raise EInvalidPathException.CreateFmt('Path %s contains empty parts', [FPath])
  // if we have .. we remove the last part
  else if (part = '..') and
    // but only if this is not the beginning in a relative path
    (((parts.Count > 1) and (parts[parts.Count-1] <> '..')) or not isAbsolute) then
  begin
    Result := -parts[parts.Count-1].length - 1;
    parts.pop;
  end
  // If we add .. we are at the beginning, so if this is an absolute path
  // We produce an invalid path
  else if (part = '..') and isAbsolute then
    raise EInvalidPathException.Create('Root does not have a parent')
  // if we don't have .
  else if (part <> '.') or (parts.Count = 0) then
  begin
    parts.Add(part);
    Result := part.Length + 1;
  end;
end;

var
  parts: TStringList;
  part: string;
  start, length, i, totalLength: SizeInt;
  ResultPath: String;
begin
  parts := TStringList.Create;
  try
    if FPath = '' then
      Exit(TPath.Create('.'));
    if isRoot then
      Exit(self);
    start := 0;
    length := 0;
    totalLength := 0;
    if isRelative then
      totalLength := handlePart('.', parts);
    while start + length <= FPath.Length do
    begin
      if (start+length = FPath.Length) or (FPath.Chars[start+length] = Params.PathDelim) then
      begin
        part := FPath.Substring(start, length);
        totalLength += handlePart(part, parts);
        start := start + length + 1;
        length := 0;
      end
      else
        Inc(Length);
    end;
    ResultPath := '';
    SetLength(ResultPath, totalLength - 1);
    start := 1;
    for i := 0 to parts.Count -1 do
    begin
      part := parts[i];
      length := part.length;
      if Length > 0 then
        Move(part[1], ResultPath[start], length);
      if i < parts.Count - 1 then
        ResultPath[start + length] := Params.PathDelim;
      Inc(start, length + 1);
    end;
  finally
    parts.Free;
  end;
  Result := TPath.Create(ResultPath);
end;

function TCustomPath.Concat(WithPath: TPath): TPath;
begin
  if (WithPath.FPath = '') or (WithPath.FPath = '.') then
    Exit(self);
  if (FPath = '') or (FPath = '.') then
    Exit(withPath);

  if WithPath.isAbsolute then
    raise EPathNotRelativeException.Create('Path to concat must be relative');
  Result := TPath.Create(IncludeDelimiter(FPath) + WithPath.FPath);
end;

function TCustomPath.Parent: TPath;
begin
  Result := Concat(TPath.Create('..'));
end;

function TCustomPath.Name: String;
var
  tmp: TPath;
begin
  tmp := Normalize;
  if tmp.isRoot then
    Exit(tmp.FPath);
  Result := tmp.FPath.Substring(tmp.FPath.LastIndexOf(Params.PathDelim) + 1);
end;

function TCustomPath.RelativeTo(const ToPath: TPath): TPath;

function CharCount(const str: String; c: Char; const StartPos: SizeInt): SizeInt; {$IFDEF inlining}inline;{$ENDIF}
var
  i: SizeInt;
begin
  Result := 0;
  for i := StartPos to str.Length do
    if str[i] = c then
      Inc(Result);
end;

var
  prefLen, diverganceCount: SizeInt;
  Path1, Path2: String;
  parenting: String;
  i: Integer;
begin
  Path1 := ToPath.Normalize.FPath;
  if not Params.isRoot(Path1) then
    Path1 += Params.PathDelim;
  Path2 := Normalize.FPath;
  prefLen := SharedPrefixLength(Path1, Path2);
  if (prefLen = Path1.Length) or (prefLen = Path2.Length) then
    Exit(TPath.Create(Path2.Substring(Path1.Length)));
  while (prefLen > 0) and (Path1[prefLen] <> Params.PathDelim) do
    Dec(prefLen);
  diverganceCount := CharCount(Path1, Params.PathDelim, prefLen + 1);
  parenting := '';
  SetLength(parenting, diverganceCount*3 -1);
  for i:=1 to parenting.Length do
    if i mod 3 = 0 then
      parenting[i] := Params.PathDelim
    else
      parenting[i] := '.';
  Result := TPath.Create(parenting).Concat(TPath.Create(Path2.Substring(prefLen)));
end;

function TCustomPath.Equals(const ToPath: TPath): Boolean;
var
  Path1, Path2: String;
begin
  Path1 := Normalize.FPath;
  Path2 := ToPath.Normalize.FPath;
  if Params.CaseSensitive then
  begin
    Path1 := Path1.ToLower;
    Path2 := Path2.ToLower;
  end;
  Result := Path1 = Path2;
end;

function TCustomPath.isPrefixTo(ToPath: TPath): Boolean;
var
  tmp: TPath;
begin
  if FPath.IsEmpty then
    Exit(False);
  tmp := Normalize;
  ToPath := ToPath.Normalize;
  Result := SharedPrefixLength(ToPath.FPath, IncludeDelimiter(tmp.FPath)) >= tmp.FPath.Length;
end;

function TCustomPath.isRelativeTo(ToPath: TPath): Boolean;
begin
  Result := ToPath.isPrefixTo(Self);
end;

constructor TCustomPath.Create(const APath: String);
begin
  if APath = '' then
    FPath := '.'
  else
    FPath := IncludeDelimiter(APath);
  if not isRoot then
    FPath := ExcludeDelimiter(FPath);
end;

class operator TCustomPath.:=(const APath: String): TPath;
begin
  Result := TPath.Create(APath);
end;

class operator TCustomPath.:=(const APath: TPath): String;
begin
  Result := APath.FPath;
end;

class operator TCustomPath.:=(const APath: RawByteString): TPath;
begin
  Result := TPath.Create(APath);
end;

class operator TCustomPath.:=(const APath: TPath): RawByteString;
begin
  Result := APath.FPath;
end;

class operator TCustomPath./(const RHS: TPath; const LHS: TPath): TPath;
begin
  Result := RHS.Concat(LHS);
end;

class operator TCustomPath./(const RHS: TPath; const LHS: String): TPath;
begin
  Result := RHS.Concat(LHS);
end;

class operator TCustomPath.mod(const RHS: TPath; const LHS: TPath): TPath;
begin
  Result := RHS.RelativeTo(LHS);
end;

class operator TCustomPath.mod(const RHS: TPath; const LHS: String): TPath;
begin
  Result := RHS.RelativeTo(LHS);
end;

class operator TCustomPath.shl(const RHS: TPath; const LHS: Integer): TPath;
var
  i: Integer;
  Path: String;
begin
  Path := RHS.FPath;
  for i:=0 to LHS-1 do
    Path := Path.Substring(0, Path.LastIndexOf(Params.PathDelim));
  Result := TPath.Create(Path);
end;

class operator TCustomPath.+(const RHS: TPath; const LHS: String): TPath;
begin
  Result := TPath.Create(RHS.FPath + LHS);
end;

class operator TCustomPath.=(const RHS: TPath; const LHS: TPath): Boolean;
begin
  Result := RHS.Equals(LHS);
end;

class operator TCustomPath.=(const RHS: TPath; const LHS: String): Boolean;
begin
    Result := RHS.Equals(LHS);
end;

class operator TCustomPath.=(const RHS: String; const LHS: TPath): Boolean;
begin
  Result := LHS.Equals(RHS);
end;

class operator TCustomPath.>=(const RHS: TPath; const LHS: TPath): Boolean;
begin
  Result := RHS.isRelativeTo(LHS);
end;

class operator TCustomPath.>=(const RHS: TPath; const LHS: String): Boolean;
begin
  Result := RHS.isRelativeTo(LHS);
end;

class operator TCustomPath.>=(const RHS: String; const LHS: TPath): Boolean;
begin
  Result := LHS.isPrefixTo(RHS);
end;

class operator TCustomPath.<=(const RHS: TPath; const LHS: TPath): Boolean;
begin
  Result := RHS.isPrefixTo(LHS);
end;

class operator TCustomPath.<=(const RHS: TPath; const LHS: String): Boolean;
begin
  Result := RHS.isPrefixTo(LHS);
end;

class operator TCustomPath.<=(const RHS: String; const LHS: TPath): Boolean;
begin
  Result := LHS.isRelativeTo(RHS);
end;

class operator TCustomPath.>(const RHS: TPath; const LHS: TPath): Boolean;
begin
  Result := (RHS >= LHS) and not (LHS = RHS);
end;

class operator TCustomPath.>(const RHS: TPath; const LHS: String): Boolean;
begin
  Result := (RHS >= LHS) and not (LHS = RHS);
end;

class operator TCustomPath.>(const RHS: String; const LHS: TPath): Boolean;
begin
  Result := (RHS >= LHS) and not (LHS = RHS);
end;

class operator TCustomPath.<(const RHS: TPath; const LHS: TPath): Boolean;
begin
  Result := (RHS <= LHS) and not (LHS = RHS);
end;

class operator TCustomPath.<(const RHS: TPath; const LHS: String): Boolean;
begin
  Result := (RHS <= LHS) and not (LHS = RHS);
end;

class operator TCustomPath.<(const RHS: String; const LHS: TPath): Boolean;
begin
  Result := (RHS <= LHS) and not (LHS = RHS);
end;

class operator TCustomPath.<>(const RHS: TPath; const LHS: TPath): Boolean;
begin
  Result := not (RHS = LHS);
end;

class operator TCustomPath.<>(const RHS: TPath; const LHS: String): Boolean;
begin
  Result := not (RHS = LHS);
end;

class operator TCustomPath.<>(const RHS: String; const LHS: TPath): Boolean;
begin
  Result := not (RHS = LHS);
end;

end.

