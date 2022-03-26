unit pathlib;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, CustomPath;

type
  EInvalidPathException = CustomPath.EInvalidPathException;
  EPathNotRelativeException = CustomPath.EPathNotRelativeException;
  EPathNotExistsException = class(Exception);
  EPathAlreadyExistsException = class(Exception);
  ENotADirectoryException = class(Exception);

  { TWindowsPathParams }

  TWindowsPathParams = record
  const PathDelim = '\';
  const CaseSensitive: Boolean = False;
  class function isAbsolute(const Path: String): Boolean; static; {$IFDEF inlining}inline;{$ENDIF}
  class function isRoot(const Path: String): Boolean; static; {$IFDEF inlining}inline;{$ENDIF}
  end;

  { TUnixPathParams }

  TUnixPathParams = record
  const PathDelim = '/';
  const CaseSensitive: Boolean = True;
  class function isAbsolute(const Path: String): Boolean; static; {$IFDEF inlining}inline;{$ENDIF}
  class function isRoot(const Path: String): Boolean; static; {$IFDEF inlining}inline;{$ENDIF}
  end;

  TWindowsPath = specialize TCustomPath<TWindowsPathParams>;
  TUnixPath = specialize TCustomPath<TUnixPathParams>;

  {$IfDef WINDOWS}
  TPath = TWindowsPath;
  {$Else}
  TPath = TUnixPath;
  {$EndIf}

  TSearchMode = (smAll, smFiles, smDirectories);

  // Would use managed record but they are currently broken
  // See https://bugs.freepascal.org/view.php?id=37164

  { TPathEnumerator }

  TPathEnumerator = class
  private
    FPaths: TStringList;
    FPattern: String;
    FSearchRec: TSearchRec;
    FMode: TSearchMode;
    FRecursive: Boolean;
    FInSearch: Boolean;
    FCurrentPath: TPath;
    function GetCurrent: TPath;
    function ValidResult: Boolean; {$IFDEF inlining}inline;{$ENDIF}
    function FindNextValid: Boolean; {$IFDEF inlining}inline;{$ENDIF}
    procedure AddSubdirectories; {$IFDEF inlining}inline;{$ENDIF}

  public                         
    property Current: TPath read GetCurrent;
    function MoveNext: Boolean;

    constructor Create(const Path: TPath; Pattern: String; Mode: TSearchMode; Recursive: Boolean);
    destructor Destroy; override;
  end;

  { TPathIterator }

  TPathIterator = record
  private
    FPath: TPath;
    FPattern: String;
    FMode: TSearchMode;
    FRecursive: Boolean;
  public
    constructor Create(const Path: TPath; const Pattern: String; Mode: TSearchMode; Recursive: Boolean);
    function GetEnumerator: TPathEnumerator; {$IFDEF inlining}inline;{$ENDIF}
    function AddToList(const List: TStrings): Integer;
    function ToList: TStringList; {$IFDEF inlining}inline;{$ENDIF}
  end;

  { TPathHelper }

  TPathHelper = record helper for TPath
  function isFile: Boolean; {$IFDEF inlining}inline;{$ENDIF}
  function isDirectory: Boolean; {$IFDEF inlining}inline;{$ENDIF}
  function Exists: Boolean; {$IFDEF inlining}inline;{$ENDIF}

  function AbsolutePath: TPath; {$IFDEF inlining}inline;{$ENDIF}
  function RelativePath: TPath; {$IFDEF inlining}inline;{$ENDIF}

  function Delete(Recursive: Boolean = True): Boolean;
  procedure Copy(const Target: TPath; CreateParents: Boolean = True; Replace: Boolean = False);
  function Rename(const Target: TPath; CreateParents: Boolean = True): Boolean;

  // Directory Specifics
  function MakeDir(CreateParents: Boolean = True; AllowExist: Boolean = True): Boolean;
  function Children(const Pattern: String = '*'; Recursive: Boolean = True): TPathIterator; {$IFDEF inlining}inline;{$ENDIF}
  function Files(const Pattern: String = '*'; Recursive: Boolean = True): TPathIterator; {$IFDEF inlining}inline;{$ENDIF}
  function Directories(const Pattern: String = '*'; Recursive: Boolean = True): TPathIterator; {$IFDEF inlining}inline;{$ENDIF}
  function GetEnumerator: TPathEnumerator; {$IFDEF inlining}inline;{$ENDIF}

  // File specifics
  function Extension: String; {$IFDEF inlining}inline;{$ENDIF}
  function ChangeExt(const NewExt: String): TPath; {$IFDEF inlining}inline;{$ENDIF}
  function WithoutExt: TPath; {$IFDEF inlining}inline;{$ENDIF}
  end;

  { TUnixPathHelper }

  TUnixPathHelper = record Helper{$IfDef Unix}(TPathHelper){$EndIf} for TUnixPath
  function ToWindowsPath(const Root: String = 'C:\'): TWindowsPath;
  end;

  { TWindowsPathHelper }

  TWindowsPathHelper = record Helper{$IfDef Windows}(TPathHelper){$EndIf} for TWindowsPath
  function ToUnixPath(const Root: String = '/'): TUnixPath;
  end;

operator *(const RHS: TPath; const LHS: String): TPathIterator; {$IFDEF inlining}inline;{$ENDIF}
operator **(const RHS: TPath; const LHS: String): TPathIterator; {$IFDEF inlining}inline;{$ENDIF}

operator /(const RHS: TWindowsPath; const LHS: TUnixPath): TWindowsPath; {$IFDEF inlining}inline;{$ENDIF}
operator /(const RHS: TUnixPath; const LHS: TWindowsPath): TUnixPath; {$IFDEF inlining}inline;{$ENDIF}
operator :=(const APath: TWindowsPath): TUnixPath; {$IFDEF inlining}inline;{$ENDIF}
operator :=(const APath: TUnixPath): TWindowsPath; {$IFDEF inlining}inline;{$ENDIF}

function CurrentDirectory: TPath;
implementation

operator*(const RHS: TPath; const LHS: String): TPathIterator;
begin
  Result := RHS.Children('*' + LHS, False);
end;

operator**(const RHS: TPath; const LHS: String): TPathIterator;
begin
  Result := RHS.Children('*' + LHS);
end;

operator/(const RHS: TWindowsPath; const LHS: TUnixPath): TWindowsPath;
begin
  Result := RHS/LHS.ToWindowsPath;
end;

operator/(const RHS: TUnixPath; const LHS: TWindowsPath): TUnixPath;
begin
  Result := RHS/LHS.ToUnixPath;
end;

operator:=(const APath: TWindowsPath): TUnixPath;
begin
  Result := APath.ToUnixPath;
end;

operator:=(const APath: TUnixPath): TWindowsPath;
begin
  Result := APath.ToWindowsPath;
end;

function CurrentDirectory: TPath;
begin
  Result := GetCurrentDir;
end;

{ TWindowsPathHelper }

function TWindowsPathHelper.ToUnixPath(const Root: String): TUnixPath;
var
  parts: TStringList;
  path: TWindowsPath;
  part: String;
begin
  path := Normalize;
  if path.isAbsolute then
  begin
    path := path.RelativeTo(path.AsString.Substring(0, 3));
    Result := Root
  end
  else
    Result := '';
  parts := TStringList.Create;
  try
    parts.Delimiter := TWindowsPathParams.PathDelim;
    parts.StrictDelimiter := True;
    parts.DelimitedText := path;
    for part in parts do
      Result := Result/part;
  finally
    parts.Free;
  end;
end;

{ TUnixPathHelper }

function TUnixPathHelper.ToWindowsPath(const Root: String): TWindowsPath;
var
  parts: TStringList;
  path: TUnixPath;
  part: String;
begin
  path := Normalize;
  if path.isAbsolute then
  begin
    path := path.RelativeTo('/');
    Result := Root
  end
  else
    Result := '';
  parts := TStringList.Create;
  try
    parts.Delimiter := TUnixPathParams.PathDelim;
    parts.StrictDelimiter := True;
    parts.DelimitedText := path;
    for part in parts do
      Result := Result/part;
  finally
    parts.Free;
  end;
end;

{ TPathIterator }

constructor TPathIterator.Create(const Path: TPath; const Pattern: String;
  Mode: TSearchMode; Recursive: Boolean);
begin
  FPath := Path;
  FPattern := Pattern;
  FMode := Mode;
  FRecursive := Recursive;
end;

function TPathIterator.GetEnumerator: TPathEnumerator;
begin
  Result := TPathEnumerator.Create(FPath, FPattern, FMode, FRecursive);
end;

function TPathIterator.AddToList(const List: TStrings): Integer;
var
  path: TPath;
begin
  Result := 0;
  for Path in Self do
  begin
    List.Add(path.AsString);
    Inc(Result);
  end;
end;

function TPathIterator.ToList: TStringList;
begin
  Result := TStringList.Create;
  AddToList(Result);
end;

{ TPathEnumerator }

function TPathEnumerator.GetCurrent: TPath;
begin
  Result := FCurrentPath/FSearchRec.Name;
end;

function TPathEnumerator.ValidResult: Boolean;
begin
  if (FSearchRec.Attr and faDirectory = faDirectory) then
    Result := (FMode <> smFiles) and (FSearchRec.Name <> '.') and (FSearchRec.Name <> '..')
  else
    Result := (FMode <> smDirectories);
end;

function TPathEnumerator.FindNextValid: Boolean;
begin
  while FindNext(FSearchRec) = 0 do
    if ValidResult then
      Exit(True);
  // no valid result found
  FindClose(FSearchRec);
  FInSearch := False;
  Result := False;
end;

procedure TPathEnumerator.AddSubdirectories;
var
  sr: TSearchRec;
begin
  if FindFirst(FCurrentPath/'*', faDirectory, sr) = 0 then
  try
    repeat
      if (sr.Attr and faDirectory = faDirectory)
         and (sr.Name <> '.') and (sr.Name <> '..') then
        FPaths.Add(FCurrentPath/sr.Name);
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
end;

function TPathEnumerator.MoveNext: Boolean;
begin
  if FInSearch then
    // Open search: find next
    // If returns false findclose was already called
    if FindNextValid then
      Exit(True); // Found a valid result -> Return true

  // Either not in search or no valid result found
  While FPaths.Count > 0 do // Check if we have another path to check (recursive descent)
  begin
    FCurrentPath := FPaths.Pop;
    // Do recursive descent into subdirectories
    if FRecursive then
      AddSubdirectories;
    // Search in subdirectory
    if FindFirst(FCurrentPath/FPattern, faAnyFile, FSearchRec) = 0 then
    begin
      FInSearch:=True;
      // If there is at least one match in subdirectory
      if FindNextValid then
        Exit(True); // return true
      // otherwise findclose will already have been called
    end;
  end;
  // Only if not a single path was left with a valid result
  Result := False;
end;

constructor TPathEnumerator.Create(const Path: TPath; Pattern: String;
  Mode: TSearchMode; Recursive: Boolean);
begin
  FPaths := TStringList.Create;
  FPaths.Add(Path);
  FPattern := Pattern;
  FMode := Mode;
  FRecursive := Recursive;
end;

destructor TPathEnumerator.Destroy;
begin
  FPaths.Free;
  if FInSearch then
    FindClose(FSearchRec);
  inherited Destroy;
end;

{ TPathHelper }

function TPathHelper.isFile: Boolean;
begin
  Result := FileExists(self.AsString);
end;

function TPathHelper.isDirectory: Boolean;
begin
  Result := DirectoryExists(self.AsString);
end;

function TPathHelper.Exists: Boolean;
begin
  Result := isFile or isDirectory;
end;

function TPathHelper.AbsolutePath: TPath;
begin
  if isAbsolute then
    Exit(Self);
  Result := CurrentDirectory/Self;
end;

function TPathHelper.RelativePath: TPath;
begin
  Result := RelativeTo(CurrentDirectory);
end;

function TPathHelper.Delete(Recursive: Boolean): Boolean;
var
  Child: TPath;
  path: TPath;
begin
  path := self.Normalize;
  if not path.Exists then
    raise EPathNotExistsException.CreateFmt('No such file or directory: %s', [path.AsString]);
  if path.isFile then
    Result := DeleteFile(path.AsString)
  else
  begin
    Result := True;
    if Recursive then
      for Child in path.Children do
        Result := Result And Child.Delete(Recursive);
    Result := Result and RemoveDir(path.AsString);
  end;
end;

procedure TPathHelper.Copy(const Target: TPath; CreateParents: Boolean; Replace: Boolean);
procedure FileCopy(const Source: TPath; const Dest: TPath);
var
  src, dst: TFileStream;
  buffer: Array[0..1023] of Byte;
  len: Integer;
begin
  if Dest.Exists and not Replace then
    raise EPathAlreadyExistsException.CreateFmt('Can''t create path: "%s". Already exists', [Dest.AsString]);
  src := TFileStream.Create(Source, fmOpenRead);
  try
    dst := TFileStream.Create(Dest, fmCreate);
    try
      repeat
        len := src.Read(buffer, 1024);
        dst.Write(buffer, len);
      until len < 1024;
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure DirectoryCopy(const Source: TPath; const Dest: TPath);
var
  child: TPath;
begin
  Dest.MakeDir(CreateParents, Replace);
  for Child in Source.Children do
    Child.Copy(dest/child.Name, CreateParents);
end;

var
  src, dst: TPath;
begin
  src := self.Normalize;
  dst := Target.Normalize;
  if not src.Exists then
    raise EPathNotFoundException.CreateFmt('No such file or directory: %s', [src.AsString]);
  if CreateParents then
    dst.Parent.MakeDir;
  if isFile then
    FileCopy(src, dst)
  else
    DirectoryCopy(src, dst);
end;

function TPathHelper.Rename(const Target: TPath; CreateParents: Boolean
  ): Boolean;
var
  src, dst: TPath;
begin
  src := self.Normalize;
  dst := Target.Normalize;
  if not src.Exists then
    raise EPathNotExistsException.CreateFmt('No such file or directory: %s', [src.AsString]);
  if dst.Exists then
    raise EPathAlreadyExistsException.CreateFmt('Can''t create path: "%s". Already exists', [dst.AsString]);
  if CreateParents then
    dst.Parent.MakeDir;
  Result := RenameFile(src.AsString, dst.AsString);
end;

function TPathHelper.MakeDir(CreateParents: Boolean; AllowExist: Boolean
  ): Boolean;
var
  path: TPath;
begin
  path := self.Normalize;
  if path.Exists and not AllowExist then
    raise EPathAlreadyExistsException.CreateFmt('Can''t create directory: "%s". Already exists', [path.AsString]);
  if path.isFile then
    raise EPathAlreadyExistsException.CreateFmt('Can''t create directory: "%s". It''s a file', [path.AsString]);
  if path.Exists then
    Exit(True);
  if CreateParents then
    Result := ForceDirectories(path.AsString)
  else
    Result := CreateDir(path.AsString);
end;

function TPathHelper.Children(const Pattern: String; Recursive: Boolean
  ): TPathIterator;
var
  path: TPath;
begin
  path := Normalize;
  if not isDirectory then
    raise ENotADirectoryException.CreateFmt('Path is not a directory: %s', [path.AsString]);
  Result := TPathIterator.Create(path, Pattern, smAll, Recursive);
end;

function TPathHelper.Files(const Pattern: String; Recursive: Boolean
  ): TPathIterator;
var
  path: TPath;
begin
  path := Normalize;
  if not isDirectory then
    raise ENotADirectoryException.CreateFmt('Path is not a directory: %s', [path.AsString]);
  Result := TPathIterator.Create(path, Pattern, smFiles, Recursive);
end;

function TPathHelper.Directories(const Pattern: String; Recursive: Boolean
  ): TPathIterator;
var
  path: TPath;
begin
  path := Normalize;
  if not isDirectory then
    raise ENotADirectoryException.CreateFmt('Path is not a directory: %s', [path.AsString]);
  Result := TPathIterator.Create(path, Pattern, smDirectories, Recursive);
end;

function TPathHelper.GetEnumerator: TPathEnumerator;
begin
  Result := Children.GetEnumerator;
end;

function TPathHelper.Extension: String;
var
  NameWithExt: String;
begin
  NameWithExt := Name;
  Result := NameWithExt.Substring(NameWithExt.LastIndexOf('.') + 1);
end;

function TPathHelper.ChangeExt(const NewExt: String): TPath;
var
  path: TPath;
begin
  path := Normalize;
  Result := TPath.Create(ChangeFileExt(path.AsString, NewExt));
end;

function TPathHelper.WithoutExt: TPath;
begin
  Result := ChangeExt('');
end;

{ TUnixPathParams }

class function TUnixPathParams.isAbsolute(const Path: String): Boolean;
begin
  Result := (Length(Path) > 0) and (Path[1] = '/');
end;

class function TUnixPathParams.isRoot(const Path: String): Boolean;
begin
  Result := Path = '/';
end;

{ TWindowsPathParams }

class function TWindowsPathParams.isAbsolute(const Path: String): Boolean;
begin
 Result := (Path.Length >= 3)
       and (Path[1] in ['A'..'Z', 'a'..'z'])
       and (Path[2] = ':')
       and (Path[3] = '\');
end;

class function TWindowsPathParams.isRoot(const Path: String): Boolean;
begin
  Result := (Path.Length = 3) and TWindowsPathParams.isAbsolute(Path);
end;

end.

