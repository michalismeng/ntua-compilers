FROM mcr.microsoft.com/dotnet/core/runtime:3.0

COPY compiler/bin/Release/netcoreapp3.0/publish/ compiler/

ENTRYPOINT ["dotnet", "compiler/compiler.dll"]