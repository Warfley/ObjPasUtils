# Pathlib
This unit provides an OOP-style path library that allows for managing Windows and Unix Paths.

## Features
The unit `pathlib` already defines everything that is required to use the functionality.
While it uses `custompath` internally, there is no need to include this one in the uses clause.

`pathlib` declares 3 main types:
* `TUnixPath` representing unix paths
* `TWindowsPath` representing windows paths
* `TPath` the native path representation being either `TUnixPath` or `TWindowsPath` depending on the OS

All of these types are a simple wrapper around a string field, containing the actual path and provide functionality around it.
These types are designed to be read only, all modulations create a new instance.

### General Functionality

The functionality provided by all classes is:
* `AsString` return the string representation of the path.
* `isAbsolute` returns true if the path is absolute
* `isRelative` returns true if the path is relative
* `Normalize` returns a normalized form of the path with no `.` parts (except for relative paths at the beginning) and minimal number of `..`. For example `foo/.././bar` becomes simply `./bar`
* `Concat(WithPath)` returns a path that is the combination of the two paths. E.g. `TPath('foo').Concat('bar')` becomes `foo/bar`
* `parent` returns a non normalized path pointing to the parent. E.g. `TPath('foo/bar').parent` is `foo/bar/..` and normalized this becomes `foo`
* `Name` returns the name of the end of the normalized path. E.g. `TPath('foo/bar/..').Name` is `foo` while `TPath('..').Name` is `..` as the real name can not be resolved from that path.
* `RelativeTo(ToPath)` returns a path relative to another path. E.g. `TPath('/foo/bar').RelativeTo('/foo')` returns `bar`. This also can use `..`. E.g. `TPath('/foo/bar').RelativeTo('/foo/baz')` becomes `../bar`
* `Equals(ToPath)` returns true if the normalized representation of both paths is equal
* `isPrefixTo(ToPath)` returns true if the path is the prefix of another path
* `isRelativeTo(ToPath)` returns true if the other path is a refix to this path

A path can be created by the constructor `Create(String)` or through the implicit and explicit conversion:
```pascal
var p: TPath;
begin
  p := 'foo/bar';
  p := TPath('foo/bar');
  p := TPath.Create('foo/bar')
end;
```
It also supports implicit conversions to String:
```pascal
  ShowMessage(TPath('foo/bar'))
```
Using this paths can be used transparently with other string based components:
```pascal
var p: TPath;
begin
  if OpenDialog1.Execute then
    p := OpenDialog1.FileName;
  ...
  With TFileStream.Create(p, fmOpenRead) do
  try
     ...
  finally
    Free;
```

Paths are always represented such that there is no trailing delimiter. E.g. `TPath('foo/')` is the same as `TPath('foo')`.
This gets rid of all the `ExcludeTrailingPathDelimiter` and `IncludeTrailingPathDelimiter` calls often found in programs handling paths.

Besides this, to ease usage, these types also provide some operators:
* `/` to concat paths. E.g. `TPath('foo')/'bar'/'baz'` is the same as `TPath('foo').Concat('bar').Concat('baz')`
* `mod` for creating relative paths. E.g. `TPath('/foo/bar') mod '/foo'` is the same as `TPath('/foo/bar').RelativeTo('/foo')`
* `shl` removes the last parts of the path. E.g. `TPath('foo/bar/baz') shl 1` becomes `foo/bar`. Note this is not the same as `.parent` as it will also remove `..` without resolving it.
* `+` appends a string to the last part, e.g. `TPath('foo/bar') + '.txt'` becomes `foo/bar.txt`
* `>`, `>=`, `<=`, `<` implement prefix/relative relations, e.g. `TPath(/foo/bar)` > `/foo` is true because `/foo` is relative to `/foo/bar`
* `=` and `<>` alternatively to `.Equals` and its negation

All the operators support strings to be used on the LHS, e.g. `TPath('foo')/'bar'` instead of `TPath('foo')/TPath('bar')`. The comparison operators also support strings on the rhs, but the modifying operators (like `/`) don't. So while `'foo' = TPath('foo')` works, `'foo'/TPath('bar')` doesn't

As this functionality is provided for all paths, this library can be used to handle non native paths, e.g. on a remote system, server or in a simulation.

### Native functionality
Through helper types, pathlib also extends the functionality of the native path type (i.e. the one that `TPath` represents).
The general functionality is:
* `isFile` returns True if the path exists and points to a file
* `isDirectory` returns True if the path exists and points to a directory
* `Exists` returns True if the path points to either a file or directory
* `Delete(Recursive)` deletes the file or directory. If it is a directory, `Recursive` (default: True) controls if the contents is also deleted. Otherwise it will fail if the directory is not empty.
* `Copy(Target, CreateParents, Replace)` copies the file or directory (recursively) to the `Target` path. If `CreateParents` (default: True) is set, all missing directories to `Target` will be created. If `Replace` is true (default: False) already existing files will be overwritten.
* `Copy(Target, CreateParents)` renames/moves the file or directory to the `Target` path. If `CreateParents` (default: True) is set, all missing directories to `Target` will be created.

If the path represents a directory, the following functions can be used:
* `MakeDir(CreateParents, AllowExist)` creates the directory. If `CreateParents` (default: True) is set, all directories leading up to the path will be created. If `AllowExist` (default: True) is False and the target does already exist, it will throw an `EPathAlreadyExistsException`.
* `Children(Pattern, Recursive)` returns an iterator that iterates through all children of that directory that match the `Pattern` (default: *). If `Recursive` (default: True) is set, it will also iterate through subdirectories.
* `Files(Pattern, Recursive)` like `Children` but will only iterate through files
* `Directories(Pattern, Recursive)` like `Children` but will only iterate through directories

Besides this, it provides a `GetEnumerator` function, which allows for for-in loops. `for child in path` is equivalent to `for child in path.children`
Also two operators are provided, `*` and `**` for searching for `*XXX` patterns. `*` searches non recursively and `**` searches recursively. 
```pascal
var
  child, path: TPath;
...
  for child in path*'.pas' do
    ...
  // equivalent to
  for child in path.children('*.pas', False) do
    ...
  // and
  for child in path**'.pas' do
    ...
  // equivalent to
  for child in path.children('*.pas', True) do
    ...
```

For paths representing files, the following functions are provided:
* `Extension` returns the extension of a file
* `ChangeExt(NewExt)` returns a path with a different extension
* `WithoutExt` returns a path that does not have an extension

### Other functionality
Besides this, the library supports conversion between unix and windows paths.
For example if you wanted to upload files from a windows computer to a unix server you could do the following:
```pascal
var
  local_dir, local_path: TPath;
  remote_id, remote_path: TUnixPath;
begin
  local_dir := SelectDirectory()
  remote_dir := TUnixPath('/target')/local_dir.name;
  server.CreateDirectory(remote_dir);
  for local_path in local_dir do
  begin
    // Automatic conversion between paths when concatenating
    remote_path = remote_dir/local_path.RelativeTo(local_dir);
    // alternative, explicit conversion
    remote_path = remote_dir/local_path.RelativeTo(local_dir).ToUnixPath;
    if local_path.isDirectory then
      server.CreateDirectory(remote_path)
    else
      server.UploadFile(local_path, remote_path);
  end;    
end;
```