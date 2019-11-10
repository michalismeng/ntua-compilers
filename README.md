# PCL - Pcl CompiLer

Implementation of a compiler for the PCL toy language in F# using [FsLexYacc](https://fsprojects.github.io/FsLexYacc/).

You can find the language specification in pcl2019.pdf (in Greek).

## How to install

Install [.Net Core](https://dotnet.microsoft.com/download). We are currently using version 3.0.

Then clone the project and execute:
```
cd ntua-compiler
dotnet build
```

The above will install the Nuget packages required and will build the project.

To run the compiler on the default PCL file, execute:
```
cd compiler
dotnet run
```

If you want to provide your own PCL file to be compiled, execute:
```
dotnet run path/to/test-file
```