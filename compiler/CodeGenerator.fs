namespace Compiler

open Compiler.Base
open Compiler.Helpers
open LLVMSharp
open System

module CodeModule =
  let mutable theModule = Unchecked.defaultof<LLVMModuleRef>
  let mutable theBuilder = Unchecked.defaultof<LLVMBuilderRef>
  let mutable theFPM = Unchecked.defaultof<LLVMPassManagerRef>

module rec CodeGenerator =

  open CodeModule
  open Utils

  module Utils =
    let ToLLVM t =
      match t with
      | Unit            -> LLVM.VoidType ()
      | Integer         -> LLVM.IntType (uint32(Environment.VariableSize.IntegerSizeBits))
      | Boolean         -> LLVM.IntType (uint32(Environment.VariableSize.BooleanSizeBits))
      | Character       -> LLVM.IntType (uint32(Environment.VariableSize.CharacterSizeBits))
      | Real            -> LLVM.X86FP80Type ()
      | Array (t, i)    -> LLVM.ArrayType (ToLLVM t, uint32(i))
      | Ptr t           -> LLVM.PointerType (ToLLVM t, 0u)
      | IArray t        -> LLVM.ArrayType (ToLLVM t, 0u)
      | _               -> raise <| Error.InternalException "Invalid conversion to LLVM type"

    let LLVMTypeInitializer t =
      match t with
      | Integer         -> LLVM.ConstInt (ToLLVM t, 0UL, LowLevel.theTrue)
      | Boolean         -> LLVM.ConstInt (ToLLVM t, 0UL, LowLevel.theTrue)
      | Character       -> LLVM.ConstInt (ToLLVM t, 0UL, LowLevel.theTrue)
      | Real            -> LLVM.ConstReal (ToLLVM t, 0.0)
      | Array (t, i)    -> LLVM.ConstArray (ToLLVM t, LLVMTypeInitializer t |> List.replicate i |> Array.ofList)
      | Ptr t           -> LLVM.ConstPointerNull (LLVM.PointerType(ToLLVM t, 0u))
      | IArray t        -> LLVM.ConstArray (ToLLVM t, LLVMTypeInitializer t |> List.replicate 0 |> Array.ofList)
      | _               -> raise <| Error.InternalException "Cannot get LLVM initializer for this type"

    let strChrToChr s =
      let escapes = ['n'; 't'; 'r'; '0'; '\\'; '\''; '"']
      let escaped = ['\n'; '\t'; '\r'; '\x00'; '\\'; '\''; '\"']

      match String.length s with
      | 1 -> s.[0]
      | 2 when List.contains s.[1] escapes -> escaped.[List.findIndex (fun i -> i = s.[1]) escapes]
      | _ -> raise <| Helpers.Error.InternalException "String as character cannot contain more than 3 characters"

  module LowLevel =

    let theTrue  = LLVMBool 1
    let theFalse = LLVMBool 0
    let theZero s = LLVM.ConstInt (LLVM.IntType(uint32(s)), 0UL, theTrue)

    let GenerateUnop op p isFloat =
      match op with
      | Positive          -> p
      | Not               -> let neg = LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntEQ, p, LLVM.ConstInt(p.TypeOf (), 0UL, LLVMBool 1), "tncmp")
                             LLVM.BuildZExt (theBuilder, neg, Utils.ToLLVM Base.Boolean, "tneg")
      | Negative          -> if isFloat then LLVM.BuildFNeg (theBuilder, p, "tfneg") else LLVM.BuildNeg (theBuilder, p, "tneg")

    let GenerateBinop op lhs rhs isFloat =
      match op with
      | Add           -> if isFloat then LLVM.BuildFAdd (theBuilder, lhs, rhs, "tfadd") 
                                    else LLVM.BuildAdd (theBuilder, lhs, rhs, "tadd") 
      | Sub           -> if isFloat then LLVM.BuildFSub (theBuilder, lhs, rhs, "tfsub") 
                                    else LLVM.BuildSub (theBuilder, lhs, rhs, "tsub")
      | Mult          -> if isFloat then LLVM.BuildFMul (theBuilder, lhs, rhs, "tfmult") 
                                    else LLVM.BuildMul (theBuilder, lhs, rhs, "tmult")
      | Div           -> if isFloat then LLVM.BuildFDiv (theBuilder, lhs, rhs, "tfdiv")
                                    else raise <| Error.InternalException "Operator Div requires float"
      | Divi          -> if not(isFloat) then LLVM.BuildSDiv (theBuilder, lhs, rhs, "tdivi") 
                                         else raise <| Error.InternalException "Operatir Divi requires integer"
      | Modi          -> if not(isFloat) then LLVM.BuildSRem (theBuilder, lhs, rhs, "tmodi") 
                                         else raise <| Error.InternalException "Operatir Modi requires integer"
      | Equals        -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOEQ, lhs, rhs, "tfeq") 
                                    else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntEQ, lhs, rhs, "teq")
      | NotEquals     -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealONE, lhs, rhs, "tfeq") 
                                    else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntNE, lhs, rhs, "teq")
      | Less          -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOLT, lhs, rhs, "tfeq") 
                                    else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSLT, lhs, rhs, "teq")
      | LessEquals    -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOLE, lhs, rhs, "tfeq") 
                                    else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSLE, lhs, rhs, "teq")
      | Greater       -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOGT, lhs, rhs, "tfeq") 
                                    else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSGT, lhs, rhs, "teq")
      | GreaterEquals -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOGE, lhs, rhs, "tfeq") 
                                    else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSGE, lhs, rhs, "teq")
      | Or            -> LLVM.BuildOr  (theBuilder, lhs, rhs, "tor")
      | And           -> LLVM.BuildAnd (theBuilder, lhs, rhs, "tand")

    let GenerateConstant inst =
      match inst with
      | SemInt i                    -> LLVM.ConstInt (ToLLVM Integer, uint64(i), theTrue)
      | SemReal r                   -> LLVM.ConstReal (ToLLVM Real, float(r))
      | SemChar c                   -> LLVM.ConstInt (ToLLVM Character, uint64 <| strChrToChr c, theTrue)
      | SemBool b                   -> LLVM.ConstInt (ToLLVM Base.Boolean, Convert.ToUInt64 b, theTrue)
      | _                           -> raise <| Helpers.Error.InternalException "Expected constant but didn't find one"
    
    let GenerateString str needPtr =
      if needPtr then LLVM.BuildGlobalStringPtr (theBuilder, str, "str")
                 else LLVM.ConstString (str, uint32(str.Length), theTrue)


    let GenerateStructType parent types =
      let toLLVM (t: Base.Type) = if t = Unit then ToLLVM Integer else ToLLVM t
      let structTypes = parent :: (types |> List.map toLLVM)
      LLVM.StructType (Array.ofList structTypes, false)

    let GenerateStructAccess structPtr fieldIndex =
      LLVM.BuildGEP (theBuilder, structPtr, [theZero 32; LLVM.ConstInt (LLVM.Int32Type (), uint64(fieldIndex), LLVMBool 0)] |> Array.ofList, "tempptr")

    let GenerateStructStore structPtr fieldIndex stroreValue =
      let gep = GenerateStructAccess structPtr fieldIndex
      LLVM.BuildStore (theBuilder, stroreValue, gep)   

    let GenerateStructLoad structPtr fieldIndex =
      let gep = GenerateStructAccess structPtr fieldIndex
      LLVM.BuildLoad (theBuilder, gep, "tempload")

    let GenerateLoad p = if (LLVM.IsConstant p) = LLVMBool 0 then p else LLVM.BuildLoad (theBuilder, p, "tempload")  

    let GenerateAlloca typ =
      LLVM.BuildAlloca (theBuilder, typ, "templocal")

    let GenerateCall (func: LLVMValueRef) parameters =
      let retType = LLVM.GetReturnType (func.TypeOf ())
      // since the result is stored in the activation record, all functions return void
      // but this is a nice LLVM line of code so I will not erase it
      if retType.GetElementType().TypeKind = LLVMTypeKind.LLVMVoidTypeKind then 
        LLVM.BuildCall (theBuilder, func, parameters, "")
      else
        LLVM.BuildCall (theBuilder, func, parameters, "tempcall")

    let GenerateArrayAccess array index =
      LLVM.BuildGEP (theBuilder, array, [theZero 32; index] |> Array.ofList, "tempptr")

  
  (* Generates an activation record type for the given function *)
  let GenerateARType parent (header: Base.ProcessHeader) (body: Base.Body) =
    let _, _params, retType = header
    let declarations, _ = body

    let mapVariableDeclaration decl = 
      match decl with
      | Variable (_, t) -> Some t
      | _               -> None

    let paramTypes = _params |> 
                     List.map (fun (_, t, s) -> if s = ByRef then Ptr t else t)

    let declTypes = declarations 
                    |> List.choose mapVariableDeclaration 
                    
    let structTypes = retType :: paramTypes @ declTypes
    LowLevel.GenerateStructType parent structTypes

  (* Generate space for hosting a local variable *)
  let GenerateLocal localType =
    LowLevel.GenerateAlloca localType

  let GenerateGlobalVariable name (typ: Base.Type) =
    let theGlobal = LLVM.AddGlobal (theModule, ToLLVM typ, name)
    LLVM.SetLinkage (theGlobal, LLVMLinkage.LLVMPrivateLinkage)
    LLVM.SetInitializer (theGlobal, LLVMTypeInitializer typ)
    LLVM.SetAlignment (theGlobal, 32u)

  let GenerateFunctionRogue name parameters retType attrs =
    let parameters = parameters 
                     |> List.map ((fun (t, s) -> if s = ByRef then Ptr t else t) >> ToLLVM)
                     |> Array.ofList

    let theFunction = LLVM.AddFunction (theModule, name, LLVM.FunctionType (ToLLVM retType, parameters, false))
    LLVM.SetLinkage (theFunction, LLVMLinkage.LLVMPrivateLinkage)
    List.iter (addAttributeToValue theFunction) attrs
    theFunction

  let private addAttributeToValue value (attr: obj) =
    match attr with
    | :? LLVMLinkage as l -> LLVM.SetLinkage (value, l)
    | _                   -> ()

  let GenerateFunctionPrototype arStructs (func: SemanticFunction) (retType: Base.Type) attrs =
    let name, _ = func
    let arStruct = Map.find name arStructs
    let arStructPtr = LLVM.PointerType (arStruct, 0u)
    let theFunction = LLVM.AddFunction (theModule, name, LLVM.FunctionType (ToLLVM retType, [|arStructPtr|], false))

    if Environment.CLI.IsLibrary then LLVM.SetLinkage (theFunction, LLVMLinkage.LLVMExternalLinkage)
                                 else LLVM.SetLinkage (theFunction, LLVMLinkage.LLVMLinkerPrivateLinkage)

    List.iter (addAttributeToValue theFunction) attrs
    theFunction

  let GenerateBasicBlock functionValue name =
    let theBasicBlock = LLVM.AppendBasicBlock (functionValue, name)
    LLVM.PositionBuilderAtEnd (theBuilder, theBasicBlock)
    theBasicBlock

  (* Generate a call to the given function -- activation record is NOT setup here *)
  let GenerateFunctionCall name parameters =
    let theFunction = LLVM.GetNamedFunction (theModule, name)
    let theParameters = Array.ofList parameters

    if theFunction.Pointer = IntPtr.Zero then
      raise <| Error.InternalException (sprintf "Could not find function '%s' by name in LLVM ledger" name)

    LowLevel.GenerateCall theFunction theParameters

  let GenerateLabelledNames (program: Program) =
    let rec generate statements =
      let mapToLblStmt i =
        match i with
        | LabeledStatement (s, _, _) -> Some s
        | _                          -> None

      let names = List.choose mapToLblStmt statements
      let namesNested i = 
        match i with
        | Block is        -> generate is
        | While (_, s, _) -> generate [s]
        | If (_, i, e, _) -> generate [i] @ generate [e]
        | _               -> []

      names @ List.fold (fun acc s -> acc @ namesNested s) [] statements

    let _, body = program
    let _, statements = body
    
    generate statements

  let private generateARType parentName parent header body =
      let mapFunction decl =
        match decl with
        | Process (hdr, body) -> Some (hdr, body)
        | _                   -> None

      let funcName, _, _ = header
      let declarations, _ = body
      let structName = (if parentName = "" then "" else parentName + ".") + funcName

      let _struct = GenerateARType (LLVM.PointerType (parent, 0u)) header body

      let functions = List.choose mapFunction declarations

      if not(List.isEmpty functions) then
        let x = functions |> 
                List.fold (fun acc f -> generateARType structName _struct (fst f) (snd f) :: acc ) [] |>
                List.collect id
        (structName, _struct) :: x
      else
        [(structName, _struct)]

  let GenerateARTypes program =
    let name, body = program
    let globalDecls, statements = body

    let mapToGlobalVariable g =
      match g with
      | Variable (s, t) -> Some (s, t)
      | _               -> None

    let mapToFunction g =
      match g with
      | Process _ -> Some g
      | _               -> None

    let globalInstructions = globalDecls
                            |> List.choose mapToGlobalVariable
                            |> List.map SemanticGlobalVariable

    // remove variable declarations of the first scope, since they will be generated as global variables
    // only process functions 
    let functionsOfGlobalScope = List.choose mapToFunction globalDecls

    let arTypes = generateARType "" (ToLLVM <| Ptr Integer) (name, [], Integer) (functionsOfGlobalScope, statements) 
                  |> Map.ofList

    (arTypes, globalInstructions)

  let navigateToAR curAR timesUp = List.fold (fun acc _ -> LowLevel.GenerateStructLoad acc 0) curAR [1..timesUp]

  let getAccessLink curAR timesUp =
    if timesUp = -1 then curAR
    else LowLevel.GenerateStructLoad (navigateToAR curAR timesUp) 0

  let getCurrentBB () = LLVM.GetInsertBlock theBuilder

  let GenerateBasicBlockPlain func name = LLVM.AppendBasicBlock (func, name)

  let GenerateBasicBlockAfterCurrent func name =
    let block = GenerateBasicBlockPlain func name
    LLVM.MoveBasicBlockAfter (block, getCurrentBB ())
    block

  let GenerateBBTriplet func names =
    let blockNames =
      match names with
      | Some ns -> ns
      | None -> [".firstBB"; ".secondBB"; ".thirdBB"]
    GenerateBasicBlockPlain func blockNames.[0], GenerateBasicBlockPlain func blockNames.[1], GenerateBasicBlockPlain func blockNames.[2]

  let GenerateBrAndLink targetBB newBB =
    let br = LLVM.BuildBr (theBuilder, targetBB)
    LLVM.PositionBuilderAtEnd (theBuilder, newBB) 
    br

  let GenerateBrAndLinkSame targetBB = GenerateBrAndLink targetBB targetBB

  let GeneratePtrArithmeticIndex idx isSubtract =
    if isSubtract then LLVM.BuildNeg (theBuilder, idx, "parithmneg")
    else               idx



  let GenerateInstruction allBBs ars curAR func inst =
    let rec generateInstruction curAR needPtr inst =
      let generateInCurContext = generateInstruction curAR
      let generateSubInstructions = List.iter (generateInCurContext true >> ignore)
      match inst with
      | SemInt _  | SemReal _  
      | SemBool _ | SemChar _       -> LowLevel.GenerateConstant inst            
      | SemString s                 -> LowLevel.GenerateString s needPtr                 

      | SemNil t                    -> LLVM.ConstPointerNull <| ToLLVM t

      | SemBinop (e1, e2, op, typ)  -> let lhs = generateInCurContext false e1 
                                       let rhs = generateInCurContext false e2
                                       match typ with
                                       | Ptr _ -> let index = GeneratePtrArithmeticIndex rhs (op = Sub)
                                                  LLVM.BuildGEP (theBuilder, lhs, [|index|], "tempgep")
                                       |_      -> LowLevel.GenerateBinop op lhs rhs (typ = Real)
      | SemUnop (e, op, typ)        -> LowLevel.GenerateUnop op (generateInCurContext false e) (typ = Real)
      | SemAddress s                -> generateInCurContext true s
      | SemIdentifier (u, i)        -> let ar = navigateToAR curAR u
                                       let fIndex = i + Environment.ActivationRecord.NumberOfNonParameters
                                       if needPtr then LowLevel.GenerateStructAccess ar fIndex 
                                       else            LowLevel.GenerateStructLoad ar fIndex       
      | SemGlobalIdentifier s       -> let ptr = LLVM.GetNamedGlobal (theModule, s)  
                                       if needPtr then ptr else LowLevel.GenerateLoad ptr
      | SemAssign (l, r)            -> LLVM.BuildStore (theBuilder, generateInCurContext false r, generateInCurContext true l)

      | SemReturn                   -> let theContBB = GenerateBasicBlockAfterCurrent func ".return"
                                       let theRet = LLVM.BuildRetVoid (theBuilder)
                                       LLVM.PositionBuilderAtEnd (theBuilder, theContBB)
                                       theRet

      | SemWhile (c, s)             -> let bbLoop, bbBody, bbAfter = GenerateBBTriplet func (Some [".looppart"; ".bodypart"; "afterpart"])

                                       GenerateBrAndLinkSame bbLoop |> ignore

                                       let condition = generateInCurContext false c

                                       LLVM.BuildCondBr (theBuilder, condition, bbBody, bbAfter) |> ignore
                                       LLVM.PositionBuilderAtEnd (theBuilder, bbBody)

                                       generateSubInstructions s

                                       GenerateBrAndLink bbLoop bbAfter |> ignore

                                       condition

      | SemIf (c, i, e)             -> let bbif, bbelse, bbendif = GenerateBBTriplet func (Some [".ifpart"; ".elsepart"; ".endifpart"])
                                       let condition = generateInCurContext false c

                                       LLVM.BuildCondBr (theBuilder, condition, bbif, bbelse) |> ignore
                                       LLVM.PositionBuilderAtEnd (theBuilder, bbif)

                                       generateSubInstructions i

                                       GenerateBrAndLink bbendif bbelse |> ignore

                                       generateSubInstructions e

                                       GenerateBrAndLinkSame bbendif |> ignore
                                       condition

      | SemGoto s                   -> let theContBB = GenerateBasicBlockAfterCurrent func ".cont"
                                       let theTargetBB = Map.find s allBBs
                                       LLVM.MoveBasicBlockAfter (theTargetBB, theContBB)

                                       GenerateBrAndLink theTargetBB theContBB 

      | SemLblStmt (l, s)           -> let theLabeledBB = Map.find l allBBs
                                       let x = LLVM.BuildBr (theBuilder, theLabeledBB)
                                       LLVM.PositionBuilderAtEnd (theBuilder, theLabeledBB)
                                       generateSubInstructions s
                                       x

      | SemResult                   -> if needPtr then LowLevel.GenerateStructAccess curAR Environment.ActivationRecord.ReturnFieldIndex
                                       else LowLevel.GenerateStructLoad curAR Environment.ActivationRecord.ReturnFieldIndex
      | SemFunctionCall(e, n, d, ps)-> let llvmParams = List.map (fun (p, ref) -> generateInCurContext ref p) ps  // if byReference is set => needPtr = true
                                       if not(e) then     // not external => uses AR mechanism
                                         let targetAR, accessLink = 
                                          match d with
                                            // this is a call to the allocator library
                                          | System.Int32.MinValue -> let allocatorARType, parentARType = GenerateAllocatorAR n
                                                                     (GenerateLocal allocatorARType, LLVM.ConstPointerNull <| LLVM.PointerType(parentARType, 0u))
                                          | _                     -> let ar = Map.find n ars
                                                                     (GenerateLocal ar, getAccessLink curAR d)

                                         LowLevel.GenerateStructStore targetAR 0 accessLink |> ignore    // setup access link
                                         List.iteri (fun i p -> LowLevel.GenerateStructStore targetAR (i + 2) p |> ignore) llvmParams  // setup all other parameters
                                         GenerateFunctionCall n [targetAR] |> ignore
                                         LowLevel.GenerateStructLoad targetAR 1
                                       else
                                        GenerateFunctionCall n.[1..] llvmParams
      | SemToFloat s                -> let value = generateInCurContext false s
                                       LLVM.BuildSIToFP (theBuilder, value, ToLLVM Real, "tempcast")
      | SemToZeroArray (s, t)       -> let value = generateInCurContext true s
                                       LLVM.BuildBitCast (theBuilder, value, LLVM.PointerType(LLVM.ArrayType(ToLLVM t, 0u), 0u), "tempcast")
      | SemDeref s                  -> let x = generateInCurContext false s
                                       if needPtr then x else LLVM.BuildLoad (theBuilder, x, "tempload")
      | SemDerefArray (s, i)        -> let array = generateInCurContext true s
                                       let index = generateInCurContext false i
                                       let x = LowLevel.GenerateArrayAccess array index
                                       if needPtr then x
                                                  else LLVM.BuildLoad (theBuilder, x, "tempload")

      // | SemNew (t : Base.Type)      -> let targetARType, mallocParent = GenerateAllocatorAR "allocator.mymalloc"
      //                                  let targetAR = GenerateLocal targetARType
      //                                  let accessLink = LLVM.ConstPointerNull <| LLVM.PointerType(mallocParent, 0u)
      //                                  let param = LLVM.ConstInt (Integer |> ToLLVM, uint64(t.Size), LLVMBool 0)
      //                                  LowLevel.GenerateStructStore targetAR 0 accessLink |> ignore    // setup access link
      //                                  LowLevel.GenerateStructStore targetAR 2 param |> ignore    // setup access link
      //                                  GenerateFunctionCall "allocator.mymalloc" [targetAR] |> ignore
      //                                  let v = LowLevel.GenerateStructLoad targetAR 1
      //                                  let cast = LLVM.BuildBitCast(theBuilder, v, ToLLVM (Ptr t), "tempcast")
      //                                  cast

      // | SemNewArray (e, t)          -> let cnt = generateInCurContext false e
      //                                  LLVM.BuildArrayMalloc(theBuilder, ToLLVM t, cnt, "mallocarray")

      // | SemDispose i                -> let targetARType, freeParent = GenerateAllocatorAR "allocator.myfree"
      //                                  let targetAR = GenerateLocal targetARType
      //                                  let accessLink = LLVM.ConstPointerNull <| LLVM.PointerType(freeParent, 0u)
      //                                  let v = generateInCurContext false i
      //                                  let cast = LLVM.BuildBitCast(theBuilder, v, ToLLVM (Ptr Integer), "tempcast")
      //                                  LowLevel.GenerateStructStore targetAR 0 accessLink |> ignore    // setup access link
      //                                  LowLevel.GenerateStructStore targetAR 2 cast |> ignore    // setup access link
      //                                  GenerateFunctionCall "allocator.myfree" [targetAR]

      // | SemDisposeArray i           -> let lval = generateInCurContext true i
      //                                  let ld = LowLevel.GenerateLoad lval
      //                                  LLVM.BuildFree(theBuilder, ld) |> ignore
      //                                  LLVM.BuildStore (theBuilder, LLVM.ConstPointerNull <| ld.TypeOf(), lval)
      | SemBitcast (t, l)           -> LLVM.BuildBitCast(theBuilder, generateInCurContext false l , ToLLVM t, "tempcast")
      | SemDeclFunction _           -> raise <| Error.InternalException "Cannot generate function in this context"

    generateInstruction curAR false inst

  let GenerateAllocatorAR name =
    let name, parentRecord, paramsRecord = List.find (fun (n, _, _) -> n = name) ExternalFunctions.ExternalAllocator
    let allocatorParent = LLVM.StructType (Array.ofList <| List.map ToLLVM parentRecord, false)
    let allocatorParams = List.map ToLLVM paramsRecord
    LLVM.StructType (Array.ofList (LLVM.PointerType(allocatorParent, 0u) :: allocatorParams), false), allocatorParent

  let GenerateAllocatorPrototypes () =
    for alloc in ExternalFunctions.ExternalAllocator do
      let name, _, _ = alloc
      let allocatorARType, _ = GenerateAllocatorAR name
      LLVM.AddFunction (theModule, name, LLVM.FunctionType (ToLLVM Unit, [|LLVM.PointerType(allocatorARType, 0u)|], false)) |> ignore
    
  let GenerateFunctionCode arTypes labelNames func =
    let funcName, instructions = func
    let theFunction = LLVM.GetNamedFunction (theModule, funcName)

    GenerateBasicBlock theFunction ".entry" |> ignore

    let appendLabelToMap curMap label = Map.add label (LLVM.AppendBasicBlock (theFunction, label)) curMap
    let allBBs = List.fold appendLabelToMap Map.empty labelNames

    let currentAR = theFunction.GetFirstParam ()
    List.iter (fun instruction -> (GenerateInstruction allBBs arTypes currentAR theFunction instruction) |> ignore) instructions
    LLVM.BuildRetVoid (theBuilder)

  let InitializeFPM () =
    let theFPM = LLVM.CreatePassManager ()
    LLVM.AddBasicAliasAnalysisPass theFPM
    LLVM.AddPromoteMemoryToRegisterPass theFPM
    LLVM.AddInstructionCombiningPass theFPM
    LLVM.AddReassociatePass theFPM
    LLVM.AddGVNPass theFPM
    LLVM.AddCFGSimplificationPass theFPM
    CodeModule.theFPM <- theFPM

  let GenerateLLVMModule () =
    theModule <- LLVM.ModuleCreateWithName (Array.last <| Environment.CLI.FileName.Split "/")
    theBuilder <- LLVM.CreateBuilder ()

  let GenerateMain topLevelFunctionName =

    // find the Activation Record of the program's top level functions -- only needed to contruct a null pointer to it
    let topLevelFunctionARType = LLVM.TypeOf <| 
                                 LLVM.GetFirstParam (LLVM.GetNamedFunction (theModule, topLevelFunctionName))
    
    // generate the main function and call the program's top level function
    let theMain = GenerateFunctionRogue "main" [] Integer [LLVMLinkage.LLVMExternalLinkage]
    GenerateBasicBlock theMain "entry" |> ignore
    GenerateFunctionCall topLevelFunctionName [LLVM.ConstNull (topLevelFunctionARType)] |> ignore
    LLVM.BuildRet (theBuilder, LowLevel.theZero (Environment.VariableSize.IntegerSizeBits)) |> ignore

    theMain