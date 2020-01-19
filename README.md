# PCL - Pcl CompiLer --- Still under construction --- This document is incomplete

Implementation of a compiler for the PCL toy language in F# using [FsLexYacc](https://fsprojects.github.io/FsLexYacc/) and [LLVMSharp](https://github.com/microsoft/LLVMSharp).

You can find the language specification in pcl2019.pdf (in Greek).

## How to install

Install [.Net Core](https://dotnet.microsoft.com/download). We are currently using version 3.0.

Then clone the project and execute:
```
cd compiler
./publish-linux.sh
```

This will install the necessary Nuget packages and build the project in release mode for linux x64.

## How to run

After building the executable image, you should find in the compiler directory an executable file name 'compiler'.

To execute it run

```
./compiler /path/to/pcl_file.pcl
```

You can also pass any of the following flags to alter the execution:
```
-O: Enable optimizations at the intermediate code level.
-f: Dump final code to Standard Output.
-i: Dump intermediate code to Standard Output.

-l: Treat this file as a library. The resulting code does not have an entry point and all functions are marked visible to external modules.
-p: Enable pointer arithmetic. This flag allows some basic pointer arithmetic (eg. addition) to happen.
``` 

After compiling your file, you should see two newly created files (unless you passed the -i or -f flag), namely /path/to/pcl_file.imm and /path/to/pcl_file.asm. The former contains the intermediate representation of the program in LLVM code, while the latter contains x86-64 assembly.

## How to create an executable

Before creating the final executable from the generated x86-64 assembly you should make sure that you have built the two dependencies.

lib.a: This is the generic library that provides I/O and math routines. You can find the code [here](https://github.com/abenetopoulos/edsger_lib). Compile it to get lib.a.

allocator.a: This is the custom dynamic memory allocator written in PCL. You can find the PCL code in the directory examples/dyn_alloc/allocator.pcl.
Compile the PCL code by running ??????

After that execute 
```
clang /path/to/pcl_file.asm lib.a allocator.a -o a.out
```

You should now see the final executable a.out.