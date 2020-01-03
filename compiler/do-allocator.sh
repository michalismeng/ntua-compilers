dotnet run -- ../examples/dyn_alloc/allocator.pcl -l
clang -c -o allocator.o test.asm
llvm-ar-9 rc allocator.a allocator.o