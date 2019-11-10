#!/usr/bin/python3

import subprocess
import glob

compilerPath = "./compiler"
compilerOutputPath = "test.txt"
llcOutputPath = "test.asm"
libPath = "lib.a"
clangOutputPath = "a.out"

for fprog in glob.glob("*.pcl"):
    progName = fprog[0:fprog.find(".")]

    print("testing %s" % progName)

    compileProc = subprocess.Popen([compilerPath, fprog], stdout=subprocess.PIPE)
    compileProc.wait()

    compileResult = compileProc.returncode
    compileOutput = compileProc.stdout.read()

    if compileResult != 0:
        print ("Error on compilation")
        exit (1)
        
    with open(compilerOutputPath, 'r') as f:
        llvmProrgam = f.read()

    subprocess.call("llc %s -o %s" % (compilerOutputPath, llcOutputPath), shell=True)
    subprocess.call("clang %s %s -o %s" % (llcOutputPath, libPath, clangOutputPath), shell=True)

    errors = 0
    for fin in glob.glob("%s.*.in" % progName):
        fout = "%s.out" % fin[0:fin.rfind('.')]
        with open(fin) as ffin:
            output = subprocess.Popen("./a.out", stdin=ffin, stdout=subprocess.PIPE).stdout.read().decode('utf-8')
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

subprocess.call("rm %s %s %s" % (compilerOutputPath, llcOutputPath, clangOutputPath), shell=True)
