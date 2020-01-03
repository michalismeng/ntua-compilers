llc test.txt -o test.asm
clang test.asm lib.a allocator.a -o a.out
./a.out