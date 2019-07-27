# ntua-compiler

Implementation of a compiler for the PCL toy language in F# using [FsLexYacc](https://fsprojects.github.io/FsLexYacc/).

## How to install

Install [.Net Core](https://dotnet.microsoft.com/download). We are currently using version 2.2.

Then clone the project and execute:
```
cd ntua-compiler
dotnet build
```

The above will install the necessary Nuget packages and build the project.

To run the compiler on the default PCL file, execute:
```
cd compiler
dotnet run
```

If you want to provide your own PCL file, execute:
```
dotnet run path/to/test-file
```