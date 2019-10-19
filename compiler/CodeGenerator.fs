namespace Compiler

open Compiler.Base
open Compiler.Helpers
open LLVMSharp
open System

module CodeModule =
  let mutable theModule = Unchecked.defaultof<LLVMModuleRef>
  let mutable theBuilder = Unchecked.defaultof<LLVMBuilderRef>

module rec CodeGenerator =

  open CodeModule
  open Utils

  module private Utils =
    let ToLLVM t =
      match t with
      | Unit            -> LLVM.VoidType ()
      | Integer         -> LLVM.IntType (uint32(Environment.VariableSize.IntegerSizeBits))
      | Boolean         -> LLVM.IntType (uint32(Environment.VariableSize.BooleanSizeBits))
      | Character       -> LLVM.IntType (uint32(Environment.VariableSize.CharacterSizeBits))
      | Real            -> LLVM.X86FP80Type ()
      | Array (t, i)    -> LLVM.ArrayType (ToLLVM t, uint32(i))
      | Ptr t           -> LLVM.PointerType (ToLLVM t, 0u)
      | _               -> raise <| Error.InternalException "Invalid conversion to LLVM type"

    let LLVMTypeInitializer t =
      match t with
      | Integer         -> LLVM.ConstInt (ToLLVM t, 0UL, LowLevel.theTrue)
      | Boolean         -> LLVM.ConstInt (ToLLVM t, 0UL, LowLevel.theTrue)
      | Character       -> LLVM.ConstInt (ToLLVM t, 0UL, LowLevel.theTrue)
      | Real            -> LLVM.ConstReal (ToLLVM t, 0.0)
      | Array (t, i)    -> LLVM.ConstArray (ToLLVM t, LLVMTypeInitializer t |> List.replicate i |> Array.ofList)
      | Ptr t           -> LLVM.ConstPointerNull (LLVM.PointerType(ToLLVM t, 0u))
      | _               -> raise <| Error.InternalException "Cannot get LLVM initializer for this type"

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
                                    else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSLE, lhs, rhs, "teq")
      | LessEquals    -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOLE, lhs, rhs, "tfeq") 
                                    else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSLT, lhs, rhs, "teq")
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
      | SemChar c                   -> LLVM.ConstInt (ToLLVM Character, uint64(c.[0]), theTrue)      // TODO: 'c' is a string that maps to a character (may have escape sequences)
      | SemBool b                   -> LLVM.ConstInt (ToLLVM Base.Boolean, Convert.ToUInt64 b, theTrue)
      | SemString s                 -> LLVM.ConstString (s, uint32(s.Length), theTrue)
      | _                           -> raise <| Helpers.Error.InternalException "Expected constant but didn't find one"

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

  
  (* Generates an activation record llvm type for the given function *)
  let GenerateARType parent (header: Base.ProcessHeader) (body: Base.Body) =
    let _, _params, retType = header
    let declarations, _ = body

    let mapVariableDeclaration decl = 
      match decl with
      | Variable (_, t) -> Some t
      | _               -> None

    let paramTypes = _params |> 
                     List.map (fun (_, t, _) -> t)

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

  let GenerateFunctionRogue name parameters retType  attrs =
    let parameters = parameters |>
                     List.map ToLLVM |>
                     Array.ofList
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
    LLVM.SetLinkage (theFunction, LLVMLinkage.LLVMLinkerPrivateLinkage)
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

  let private generateARType parentName parent header body =
      let mapFunction decl =
        match decl with
        | Process (hdr, body) -> Some (hdr, body)
        | _                         -> None

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

  let GenerateARTypes (program: Program) =
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

  let findOrCreateBB func allBBs label =
    match Map.tryFind label allBBs with
    | Some bb -> (bb, allBBs)
    | None    -> let newBB = LLVM.AppendBasicBlock (func, label)
                 (newBB, Map.add label newBB allBBs)

  let GenerateInstruction ars curAR func inst =
    let theBB = LLVM.GetFirstBasicBlock func
    let basicBlocks = Map.add "entry" theBB Map.empty

    let rec generateInstruction allBBs curBB curAR needPtr inst =
      let generateInCurContext = generateInstruction allBBs curBB curAR
      match inst with
      | SemInt _  | SemReal _  
      | SemBool _ | SemChar _                 
      | SemString _                 -> LowLevel.GenerateConstant inst

      | SemNil                      -> LLVM.ConstNull (LLVM.Int32Type ())

      | SemBinop (e1, e2, op, typ)  -> let lhs = generateInCurContext false e1 
                                       let rhs = generateInCurContext false e2 
                                       LowLevel.GenerateBinop op lhs rhs (typ = Real)
      | SemUnop (e, op, typ)        -> LowLevel.GenerateUnop op (generateInCurContext false e) (typ = Real)
      | SemAddress l                -> LowLevel.theZero 0
      | SemIdentifier (u, i)        -> let ar = navigateToAR curAR u
                                       let fIndex = i + Environment.ActivationRecord.NumberOfNonParameters
                                       if needPtr then LowLevel.GenerateStructAccess ar fIndex 
                                       else            LowLevel.GenerateStructLoad ar fIndex       
      | SemGlobalIdentifier s       -> let ptr = LLVM.GetNamedGlobal (theModule, s)  
                                       if needPtr then ptr else LowLevel.GenerateLoad ptr
      | SemAssign (l, r)            -> LLVM.BuildStore (theBuilder, generateInCurContext false r, generateInCurContext true l)
      | SemNone                     -> LowLevel.theZero 8
      | SemGoto s                   -> let theBB, newBBs = findOrCreateBB func allBBs s
                                       LLVM.BuildBr (theBuilder, theBB)
                                       
      | SemLblStmt (l, s)           -> LowLevel.theZero 0
      | SemResult                   -> if needPtr then LowLevel.GenerateStructAccess curAR Environment.ActivationRecord.ReturnFieldIndex
                                       else LowLevel.GenerateStructLoad curAR Environment.ActivationRecord.ReturnFieldIndex
      | SemAllocAR s                -> let ar = Map.tryFind s ars
                                       match ar with
                                       | Some a       -> GenerateLocal a
                                       | None         -> raise <| Error.InternalException "No activation record for that name is registered"
      | SemFunctionCall (n, d, ps)  -> let llvmParams = List.map (generateInCurContext false) ps
                                       let targetARType = Map.find n ars
                                       let targetAR = GenerateLocal targetARType
                                       let accessLink = getAccessLink curAR d
                                       LowLevel.GenerateStructStore targetAR 0 accessLink |> ignore    // setup access link
                                       List.iteri (fun i p -> LowLevel.GenerateStructStore targetAR (i + 2) p |> ignore) llvmParams  // setup all other parameters
                                       GenerateFunctionCall n [targetAR] |> ignore
                                       LowLevel.GenerateStructLoad targetAR 1 
      | SemDeclFunction _           -> raise <| Error.InternalException "Cannot generate function in this context"

    generateInstruction basicBlocks theBB curAR false inst

  let GenerateFunctionCode arTypes (func: SemanticFunction) =
    let funcName, instructions = func
    let theFunction = LLVM.GetNamedFunction (theModule, funcName)

    GenerateBasicBlock theFunction "entry" |> ignore
    let retInstr = LLVM.BuildRetVoid theBuilder

    LLVM.PositionBuilderBefore (theBuilder, retInstr)

    let currentAR = theFunction.GetFirstParam ()
    List.iter (fun instruction -> (GenerateInstruction arTypes currentAR theFunction instruction) |> ignore) instructions
    // GenerateInstruction arTypes currentAR theFunction instructions

  let GenerateLLVMModule () =
    theModule <- LLVM.ModuleCreateWithName "PCL Compiler"
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