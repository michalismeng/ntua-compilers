#!/usr/bin/python3

# Script that runs the test suite for our compiler implementation.
# PCL source files are located in the programs folder while standard input and output files are located in the corresponding folders.
# Eg. the file programs/program.pcl will be compiled and then run two times, once with input inputs/program.1.in and the second with inputs/program.2.in
# the outputs will be checked against outputs/program.{1,2}.out respectively.

import os.path
import subprocess
import glob

compilerPath = "./compiler"
compilerOutputPath = "test.txt"
llcOutputPath = "test.asm"
libPath = "lib.a"
clangOutputPath = "a.out"

if os.path.exists(compilerPath) == False:
    print ('The compiler under test does not exist. Please add a ./compiler executable in the current directory')
    exit (1)

if os.path.exists(libPath) == False:
    print ('The compiler library does not exist. Please add a ./lib.a archive in the current directory')
    exit (1)

# iterate over all PCL source files
for fprog in glob.glob("programs/*.pcl"):
    progName = fprog[fprog.find('/') + 1:fprog.find(".")]

    print("testing %s" % progName)

    compileProc = subprocess.Popen([compilerPath, fprog], stdout=subprocess.PIPE)
    compileProc.wait()

    compileResult = compileProc.returncode
    compileOutput = compileProc.stdout.read()

    if compileResult != 0:
        print ("Error on compilation of %s" % progName)
        exit (1)
        
    # LLVM output of the compiler for the current PCL source file
    # with open(compilerOutputPath, 'r') as f:
    #     llvmProrgam = f.read()

    subprocess.call("llc %s -o %s" % (compilerOutputPath, llcOutputPath), shell=True)
    subprocess.call("clang %s %s -o %s" % (llcOutputPath, libPath, clangOutputPath), shell=True)

    errors = 0
    # iterate over all input files for the currently compiled PCL program
    for fin in glob.glob("inputs/%s.*.in" % progName):
        fout = "outputs/%s.out" % fin[fin.find('/') + 1:fin.rfind('.')]
        with open(fin) as ffin:
            output = subprocess.Popen("./a.out", stdin=ffin, stdout=subprocess.PIPE).stdout.read().decode('utf-8')
            # check against the desired output, which is found in the outputs folder
            with open(fout) as ffout:
                expected = ffout.read()
                if expected != output:
                    errors = errors + 1
                    print ("Error on " + fout)
                    print ("Got '%s'. Expected '%s'" % (output, expected))

    if errors == 0:
        print("Found no errors")
    else:
        print("Found %d errors" % errors)

# remove test.txt, test.asm and a.out which were created while compiling the test programs
subprocess.call("rm %s %s %s" % (compilerOutputPath, llcOutputPath, clangOutputPath), shell=True)
