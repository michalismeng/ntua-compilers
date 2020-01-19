namespace Compiler.Helpers

open Compiler.Base

module ExternalFunctions =
  let private stringType = IArray Character

  let private allocatorParentParams = [Ptr <| Ptr Integer; Integer]
  let private mallocParams = Ptr Integer :: List.replicate 4 Integer

  let private freeParams = Integer :: List.replicate 2 (Ptr Integer)


  let private externalIO = [
     ProcessHeader ("writeInteger", [("n", Integer, ByValue)], Unit);
     ProcessHeader ("writeBoolean", [("b", Boolean, ByValue)], Unit);
     ProcessHeader ("writeChar", [("c", Character, ByValue)], Unit);
     ProcessHeader ("writeReal", [("r", Real, ByValue)], Unit);
     ProcessHeader ("writeString", [("s", stringType, ByRef)], Unit);

     ProcessHeader("readInteger", [], Integer);
     ProcessHeader("readBoolean", [], Boolean);
     ProcessHeader("readChar",    [], Character);
     ProcessHeader("readReal",    [], Real);
     ProcessHeader("readString",  [("size", Integer, ByValue); ("s", stringType, ByRef)], Unit)
    ]

  let private externalMath = [
    ProcessHeader ("abs", [("n", Integer, ByValue)], Integer);
    ProcessHeader ("fabs", [("r", Real, ByValue)], Real);
    ProcessHeader ("sqrt", [("r", Real, ByValue)], Real);
    ProcessHeader ("sin", [("r", Real, ByValue)], Real);
    ProcessHeader ("cos", [("r", Real, ByValue)], Real);
    ProcessHeader ("tan", [("r", Real, ByValue)], Real);
    ProcessHeader ("atan", [("r", Real, ByValue)], Real);   // changed name from 'arctan' to 'atan' to match library function.
    ProcessHeader ("exp", [("r", Real, ByValue)], Real);
    ProcessHeader ("ln", [("r", Real, ByValue)], Real);
    ProcessHeader ("pi", [], Real)
  ] 

  let private externalConvert = [
    ProcessHeader ("trunc", [("r", Real, ByValue)], Integer);
    ProcessHeader ("round", [("r", Real, ByValue)], Integer);
    ProcessHeader ("ord", [("c", Character, ByValue)], Integer);
    ProcessHeader ("chr", [("n", Integer, ByValue)], Character);
  ] 

  let ExternalAllocator = [
    ("allocator.mymalloc", allocatorParentParams, mallocParams)
    ("allocator.myfree", allocatorParentParams, freeParams)
  ]

  let Exetrnals = externalIO @ externalMath @ externalConvert
