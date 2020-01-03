llvm-link-9 allocator.imm test.txt -o result.imm -S
llc result.imm -o result.asm
clang result.asm lib.a -o a.out
./a.out