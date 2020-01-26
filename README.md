# PCL (PCL's a Compiled Language)

Implementation of a compiler for the PCL compiled language in F# using [FsLexYacc](https://fsprojects.github.io/FsLexYacc/) and [LLVMSharp](https://github.com/microsoft/LLVMSharp).

You can find the language specification in pcl2019.pdf (in Greek).

## How to install

Install [.Net Core](https://dotnet.microsoft.com/download). We are currently using version 3.0.

Clone the project and run:
```
cd compiler
dotnet publish -c Release -r linux-x64 /p:PublishSingleFile=true
cp bin/Release/netcoreapp3.0/linux-x64/publish/compiler .
```

This will install the necessary Nuget packages and build the project in release mode for linux x64. If you need to build for another platform, find the appropriate runtime identifier (RID) in the [docs](https://docs.microsoft.com/en-us/dotnet/core/rid-catalog) and change the -r flag and the 'cp' path accordingly.

## How to run

After building the project, you should find an executable file named 'compiler' in the current directory (the compiler directory).

In order to compile your pcl file, run

```
./compiler /path/to/pcl_file.pcl
```

You can also pass any of the following flags to alter the execution:
```
-O: Enable optimizations.
-f: Dump final code to Standard Output.
-i: Dump intermediate code to Standard Output.

-l: Treat this file as a library. The resulting code does not have an entry point and all functions are marked visible to external modules.
-p: Enable pointer arithmetic. This flag allows some basic pointer arithmetic to happen (eg. addition of pointers and integers).
``` 

After compiling your pcl program, you should see two newly created files (unless you passed the -i or -f flag), namely /path/to/pcl_file.imm and /path/to/pcl_file.asm. The former contains the intermediate representation of the program in LLVM code, while the latter contains x86-64 assembly.

## How to build the runtime libraries

This implementation of PCL comes with two external runtime dependencies.

lib.a: This is the generic library that provides I/O and math routines. You can find the code [here](https://github.com/abenetopoulos/edsger_lib). Compile it to get lib.a.

allocator.a: This is the dynamic memory allocator written in PCL. You can find the PCL code in the directory examples/dyn_alloc/allocator.pcl. To generate this archive, execute the folowing instructions from the 'compiler' directory.

```
./compiler ../examples/dyn_alloc/allocator.pcl -l -p -O
clang -c -o ../examples/dyn_alloc/allocator.o ../examples/dyn_alloc/allocator.asm
llvm-ar rc ../examples/dyn_alloc/allocator.a ../examples/dyn_alloc/allocator.o
cp ../examples/dyn_alloc/allocator.a .
rm ../examples/dyn_alloc/allocator.o
```

## How to create an executable

After generating the dependency archives, you are ready to perform the final compilation of the x86-64 assembly.

Simply run 
```
clang /path/to/pcl_file.asm lib.a allocator.a
```

and you should now be able to run the final executable, 'a.out'.