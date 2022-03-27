# ObjPasUtils

This Package aims at providing a set of useful functions, and types for FreePascal.

It is based on and extends RecUtils (https://github.com/Warfley/Recutils) in order to provide a consistent package.

While for RecUtils all the units are independent, this provides all the units as one package, allowing the units to use each other and provide their full potential in combination.
Also unlike RecUtils, this repository provides more than just record types, but also classes and functions.

This is a collection of small interconnected libraries found in the `src` directory:
* DynamicTypes: Custom types for dynamically managing data of different types
* Iterators: Collection independent library for iterating over streams of data (e.g. the elements of an array) and modifying such streams (map, filter, reduce, etc.)
* Pathlib: Custom Path type containing path management, resolution and simple file management functions. Automatically handles Windows and Unix paths
* Range: Views on Subsets over types and data, e.g. only considering a subset of array data without having to copy the data into a new array
* Tuple: Implementation of tuples, Types that contain multiple fields of different types, up to 5 elements

Further explainations can be found in the Readme of the different directories in `src`. See the `examples` directory for examples on how to use the units provided by this package.