from pathlib import Path
from typing import List, Set, TextIO, Tuple

FUNC_TYPE_SUFFIXES = {
    "": "",
    "Method": "of object",
    "Nested": "is nested"
}

def write_header(file: TextIO) -> None:
    file.write("unit FuncTypes;\n\n")
    file.write("{$Mode ObjFPC}{$H+}\n")
    file.write("{$ModeSwitch AdvancedRecords}\n")
    file.write("{$ModeSwitch NestedProcvars}\n\n")
    file.write("interface\n\n")
    file.write("type\n")

def build_param_types(num_args: int, is_function: bool) -> List[str]:
    return [f"TParam{i+1}" for i in range(num_args)]

def param_list(param_types: List[str], is_const: bool) -> str:
    param_elems = [f"{'const ' if is_const else ''}{param_type[1:]}: {param_type}" for param_type in param_types]
    return "; ".join(param_elems)

def param_call_list(param_types: List[str]) -> str:
    param_elems = [f"{param_type[1:]}" for param_type in param_types]
    return ", ".join(param_elems)

def func_sig(param_types: List[str], is_func: bool, is_const: bool) -> str:
    return f"{'function' if is_func else 'procedure'}({param_list(param_types, is_const)}){': TResult' if is_func else ''}"

def build_name_type(prefix: str, suffix: str, is_func: bool, is_const: bool) -> Tuple[str, str]:
    const_str = "Const" if is_const else ""
    func_or_proc = "Function" if is_func else "Procedure"
    func_type_name = f"T{const_str}{prefix}{func_or_proc}{suffix}"
    type_name = f"{'f' if is_func else 'p'}t{const_str}{func_or_proc}{suffix}"
    return func_type_name, type_name

def build_generic_params(param_types: List[str], is_func: bool) -> str:
    generic_param_types = ["TResult"] if is_func else []
    generic_param_types.extend(param_types)
    return f"<{', '.join(generic_param_types)}>" if generic_param_types else ""


def func_type(prefix: str, suffix: str, type_suffix: str, param_types: List[str], is_func: bool, is_const: bool) -> Tuple[str]:
    generic_params = build_generic_params(param_types, is_func)
    func_type_name, _ = build_name_type(prefix, suffix, is_func, is_const)
    
    return f"{'generic ' if generic_params else ''}{func_type_name}{generic_params} = {func_sig(param_types, is_func, is_const)}{' ' if type_suffix else ''}{type_suffix}"

def write_func_types(file: TextIO, num_args: int, prefix: str, is_function: bool) -> None:
    param_types = build_param_types(num_args, is_function)
    for name_suffix, type_suffix in FUNC_TYPE_SUFFIXES.items():
        type_decl = func_type(prefix, name_suffix, type_suffix, param_types, is_function, False)
        file.write(f"  {type_decl};\n")

        if param_types:
            const_type_decl = func_type(prefix, name_suffix, type_suffix, param_types, is_function, True)
            file.write(f"  {const_type_decl};\n")

def write_union_type_header(file: TextIO, num_args: int, prefix: str, is_function: bool) -> str:
    param_types = build_param_types(num_args, is_function)
    type_name = f"TAny{prefix}{'Function' if is_function else 'Procedure'}"
    generic_params = build_generic_params(param_types, is_function)
    my_type_name = "TMyType" if generic_params else type_name
    
    file.write(f"  {'generic ' if generic_params else ''}{type_name}{generic_params} = record\n")
    func_type_names: List[Tuple[str, str]] = []
    if generic_params:
        file.write("  public type\n")
        file.write(f"    TMyType = specialize {type_name}{generic_params};\n")
        for name_suffix in FUNC_TYPE_SUFFIXES:
            func_type_name, enum_type = build_name_type(prefix, name_suffix, is_function, False)
            file.write(f"    TMy{func_type_name[1:]} = specialize {func_type_name}{generic_params};\n")
            func_type_names.append((f"TMy{func_type_name[1:]}", enum_type))

            if param_types:
                func_type_name, enum_type = build_name_type(prefix, name_suffix, is_function, True)
                file.write(f"    TMy{func_type_name[1:]} = specialize {func_type_name}{generic_params};\n")
                func_type_names.append((f"TMy{func_type_name[1:]}", enum_type))
    else:
        func_type_names = [build_name_type(prefix, suffix, is_function, False) for suffix in FUNC_TYPE_SUFFIXES]
    file.write("\n")
    file.write("  public\n")
    for func_type_name, _ in func_type_names:
        file.write(f"    class operator :=(AFunc: {func_type_name}): {my_type_name}; inline; overload;\n")
    file.write("\n")
    file.write("  public\n")
    file.write(f"    {'function' if is_function else 'procedure'} apply({param_list(param_types, False)}){': TResult' if is_function else ''}; inline;\n")
    file.write("\n")
    file.write("  private\n")
    func_or_proc = "Function" if is_function else "Procedure"
    file.write(f"    case {func_or_proc}Type: T{func_or_proc}Type of\n")
    for func_type, enum_type in func_type_names:
        file.write(f"      {enum_type}: (F{enum_type[2:]}: {func_type});\n")
    file.write("  end;\n")

    return type_name

def write_union_type_body(file: TextIO, num_args: int, prefix: str, is_function: bool) -> None:
    param_types = build_param_types(num_args, is_function)
    type_name = f"TAny{prefix}{'Function' if is_function else 'Procedure'}"
    generic_params = build_generic_params(param_types, is_function)
    my_type_name = "TMyType" if generic_params else type_name

    file.write("{ " + type_name + " }\n\n")
    func_type_names: List[Tuple[str, str]] = []
    if generic_params:
        for name_suffix in FUNC_TYPE_SUFFIXES:
            func_type_name, enum_type = build_name_type(prefix, name_suffix, is_function, False)
            func_type_names.append((f"TMy{func_type_name[1:]}", enum_type))

            if param_types:
                func_type_name, enum_type = build_name_type(prefix, name_suffix, is_function, True)
                func_type_names.append((f"TMy{func_type_name[1:]}", enum_type))
    else:
        func_type_names = [build_name_type(prefix, suffix, is_function, False) for suffix in FUNC_TYPE_SUFFIXES]

    func_or_proc = "Function" if is_function else "Procedure"
    for func_type_name, enum_type in func_type_names:
        file.write(f"class operator {type_name}.:=(AFunc: {func_type_name}): {my_type_name};\n")
        file.write("begin\n")
        file.write(f"  Result.{func_or_proc}Type := {enum_type};\n")
        file.write(f"  Result.F{enum_type[2:]} := AFunc;\n")
        file.write("end;\n\n")
    
    file.write(f"{'function' if is_function else 'procedure'} {type_name}.apply({param_list(param_types, False)}){': TResult' if is_function else ''};\n")
    file.write("begin\n")
    file.write(f"  case {func_or_proc}Type of\n")
    for func_type_name, enum_type in func_type_names:
        file.write(f"  {enum_type}: {'Result := ' if is_function else ''}F{enum_type[2:]}({param_call_list(param_types)});\n")
    file.write("  end;\n")
    file.write("end;\n\n")
    


def build_enum_types(is_function: bool) -> List[str]:
    result = []
    for name_suffix in FUNC_TYPE_SUFFIXES:
        result.append(build_name_type("", name_suffix, is_function, False)[1])
        result.append(build_name_type("", name_suffix, is_function, True)[1])
    return result


def main() -> None:
    func_path = Path(__file__).parent/"functypes.pas"
    file: TextIO = open(func_path, "w")
    write_header(file)
    prefixes = ["", "Unary", "Binary", "Ternary"]
    file.write("  { Function Types }\n")

    enum_types = ", ".join(build_enum_types(True))
    file.write(f"  TFunctionType = ({enum_types});\n\n")
    for i, prefix in enumerate(prefixes):
        write_func_types(file, i, prefix, True)
        file.write("\n")
        write_union_type_header(file, i, prefix, True)
        file.write("\n")

    file.write("  { Procedure Types }\n")
    enum_types = ", ".join(build_enum_types(False))
    file.write(f"  TProcedureType = ({enum_types});\n\n")
    for i, prefix in enumerate(prefixes):
        write_func_types(file, i, prefix, False)
        file.write("\n")
        write_union_type_header(file, i, prefix, False)
        file.write("\n")
    
    file.write("implementation\n\n")

    for i, prefix in enumerate(prefixes):
        write_union_type_body(file, i, prefix, True)
        file.write("\n")

    for i, prefix in enumerate(prefixes):
        write_union_type_body(file, i, prefix, False)
        file.write("\n")

    file.write("end.\n")

if __name__=="__main__":
    main()