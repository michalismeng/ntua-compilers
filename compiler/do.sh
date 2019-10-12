llc --x86-asm-syntax=intel test.txt -o test.asm
clang test.asm lib.a -o a.out
./a.out