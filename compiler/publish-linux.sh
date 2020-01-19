dotnet publish -c Release -r linux-x64 /p:PublishSingleFile=true
cp bin/Release/netcoreapp3.0/linux-x64/publish/compiler .