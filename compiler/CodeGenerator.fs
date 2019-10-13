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

  let private theTrue  = LLVMBool 1
  let private theFalse = LLVMBool 0
  let private theZero s = LLVM.ConstInt (LLVM.IntType(uint32(s)), 0UL, theTrue)

  type Base.Type with
    member this.LLVMInitializer =
      match this with
      | Integer         -> LLVM.ConstInt (this.ToLLVM (), 0UL, theTrue)
      | Boolean         -> LLVM.ConstInt (this.ToLLVM (), 0UL, theTrue)
      | Character       -> LLVM.ConstInt (this.ToLLVM (), 0UL, theTrue)
      | Real            -> LLVM.ConstReal (this.ToLLVM (), 0.0)
      | Array (t, i)    -> LLVM.ConstArray (t.ToLLVM (), t.LLVMInitializer |> List.replicate i |> Array.ofList)
      | Ptr t           -> LLVM.ConstPointerNull (LLVM.PointerType(t.ToLLVM(), 0u))
      | _               -> raise <| Error.InternalException "Cannot get LLVM initializer for this type"

    member this.ToLLVM () =
      match this with
      | Unit            -> LLVM.VoidType ()
      | Integer         -> LLVM.IntType (uint32(Environment.VariableSize.IntegerSizeBits))
      | Boolean         -> LLVM.IntType (uint32(Environment.VariableSize.BooleanSizeBits))
      | Character       -> LLVM.IntType (uint32(Environment.VariableSize.CharacterSizeBits))
      | Real            -> LLVM.X86FP80Type ()
      | Array (t, i)    -> LLVM.ArrayType (t.ToLLVM (), uint32(i))
      | Ptr t           -> LLVM.PointerType (t.ToLLVM (), 0u)
      | _               -> raise <| Error.InternalException "Invalid conversion to LLVM type"

  let generateBinop op lhs rhs isFloat =
    match op with
    | Add           -> if isFloat then LLVM.BuildFAdd (theBuilder, lhs, rhs, "tfadd") else LLVM.BuildAdd (theBuilder, lhs, rhs, "tadd") 
    | Sub           -> if isFloat then LLVM.BuildFSub (theBuilder, lhs, rhs, "tfsub") else LLVM.BuildSub (theBuilder, lhs, rhs, "tsub")
    | Mult          -> if isFloat then LLVM.BuildFMul (theBuilder, lhs, rhs, "tfmult") else LLVM.BuildMul (theBuilder, lhs, rhs, "tmult")
    | Div           -> if isFloat then LLVM.BuildFDiv (theBuilder, lhs, rhs, "tfdiv") else raise <| Error.InternalException "Operator Div requires float"
    | Divi          -> if not(isFloat) then LLVM.BuildSDiv (theBuilder, lhs, rhs, "tdivi") else raise <| Error.InternalException "Operatir Divi requires integer"
    | Modi          -> if not(isFloat) then LLVM.BuildSRem (theBuilder, lhs, rhs, "tmodi") else raise <| Error.InternalException "Operatir Modi requires integer"
    | Equals        -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOEQ, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntEQ, lhs, rhs, "teq")
    | NotEquals     -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealONE, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntNE, lhs, rhs, "teq")
    | Less          -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOLT, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSLE, lhs, rhs, "teq")
    | LessEquals    -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOLE, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSLT, lhs, rhs, "teq")
    | Greater       -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOGT, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSGT, lhs, rhs, "teq")
    | GreaterEquals -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOGE, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSGE, lhs, rhs, "teq")
    | Or            -> LLVM.BuildOr  (theBuilder, lhs, rhs, "tor")
    | And           -> LLVM.BuildAnd (theBuilder, lhs, rhs, "tand")

  let generateUnop op p isFloat =
    match op with
    | Positive          -> p
    | Not               -> let neg = LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntEQ, p, LLVM.ConstInt(p.TypeOf (), 0UL, LLVMBool 1), "tncmp")
                           LLVM.BuildZExt (theBuilder, neg, Base.Boolean.ToLLVM (), "tneg")
    | Negative          -> if isFloat then LLVM.BuildFNeg (theBuilder, p, "tfneg") else LLVM.BuildNeg (theBuilder, p, "tneg")

  let GenerateStructType' parent (header: Base.ProcessHeader) (body: Base.Body) =
    let f, _params, retType = header
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
    GenerateStructType parent structTypes

  let GenerateStructType (parent: LLVMTypeRef) types =
    let toLLVM (t: Base.Type) = if t = Unit then Integer.ToLLVM () else t.ToLLVM ()
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

  let GenerateLocal localType =
    LLVM.BuildAlloca (theBuilder, localType, "templocal")

  let GenerateGlobalVariable name (typ: Base.Type) =
    let theGlobal = LLVM.AddGlobal (theModule, typ.ToLLVM (), name)
    LLVM.SetLinkage (theGlobal, LLVMLinkage.LLVMPrivateLinkage)
    LLVM.SetInitializer (theGlobal, typ.LLVMInitializer)
    LLVM.SetAlignment (theGlobal, 32u)

  let GenerateFunctionRogue name (parameters: Base.Type list) (retType: Base.Type) attrs =
    let parameters = parameters |>
                     List.map (fun p -> p.ToLLVM ()) |>
                     Array.ofList
    let theFunction = LLVM.AddFunction (theModule, name, LLVM.FunctionType (retType.ToLLVM (), parameters, false))
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
    let theFunction = LLVM.AddFunction (theModule, name, LLVM.FunctionType (retType.ToLLVM (), [|arStructPtr|], false))
    LLVM.SetLinkage (theFunction, LLVMLinkage.LLVMLinkerPrivateLinkage)
    List.iter (addAttributeToValue theFunction) attrs
    theFunction

  let GenerateBasicBlock functionValue name =
    let theBasicBlock = LLVM.AppendBasicBlock (functionValue, name)
    LLVM.PositionBuilderAtEnd (theBuilder, theBasicBlock)
    theBasicBlock

  let GenerateFunctionCall name parameters =
    let theCall = LLVM.GetNamedFunction (theModule, name)
    let theParameters = Array.ofList parameters

    if theCall.Pointer = IntPtr.Zero then
      raise <| Error.InternalException (sprintf "Could not find function '%s' by name in LLVM ledger" name)

    let retType = LLVM.GetReturnType (theCall.TypeOf ())

    // since the result is stored in the activation record, all functions return void
    // but this is a nice LLVM line of code so I will not erase it
    if retType.GetElementType().TypeKind = LLVMTypeKind.LLVMVoidTypeKind then 
      LLVM.BuildCall (theBuilder, theCall, theParameters, "")
    else
      LLVM.BuildCall (theBuilder, theCall, theParameters, "tempcall")

  let private generateARType parentName parent header body =
      let mapFunction decl =
        match decl with
        | Process (hdr, body) -> Some (hdr, body)
        | _                         -> None

      let funcName, _, _ = header
      let declarations, _ = body
      let structName = (if parentName = "" then "" else parentName + ".") + funcName

      let _struct = GenerateStructType' (LLVM.PointerType (parent, 0u)) header body

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

    let arTypes = generateARType "" ((Ptr Integer).ToLLVM ()) (name, [], Integer) (functionsOfGlobalScope, statements) 
                  |> Map.ofList

    (arTypes, globalInstructions)

  let navigateToAR curAR timesUp = List.fold (fun acc _ -> GenerateStructLoad acc 0) curAR [1..timesUp]

  let GenerateLoad p = if (LLVM.IsConstant p) = LLVMBool 0 then p else LLVM.BuildLoad (theBuilder, p, "tempload")

  let GenerateInstruction ars curAR inst =
    let rec generateInstruction curAR needPtr inst =
      match inst with
      | SemInt i                    -> LLVM.ConstInt (Integer.ToLLVM (), uint64(i), theTrue)
      | SemReal r                   -> LLVM.ConstReal (Real.ToLLVM (), float(r))
      | SemChar c                   -> LLVM.ConstInt (Character.ToLLVM (), uint64(c), theTrue)      // TODO: 'c' is a string that maps to a character (may have escape sequences)
      | SemBool b                   -> LLVM.ConstInt (Base.Boolean.ToLLVM (), Convert.ToUInt64 b, theTrue)
      | SemString s                 -> LLVM.ConstString (s, uint32(s.Length), theTrue)

      | SemNil                      -> LLVM.ConstNull (LLVM.Int32Type ())

      | SemBinop (e1, e2, op, typ)  -> let lhs = generateInstruction curAR false e1 
                                       let rhs = generateInstruction curAR false e2 
                                       generateBinop op lhs rhs (typ = Real)
      | SemUnop (e, op, typ)        -> generateUnop op (generateInstruction curAR false e) (typ = Real)
      | SemAddress l                -> theZero 0
      | SemIdentifier (u, i)        -> let ar = navigateToAR curAR u
                                       if needPtr then GenerateStructAccess ar (i + Environment.ActivationRecord.NumberOfNonParameters) 
                                       else            GenerateStructLoad ar (i + Environment.ActivationRecord.NumberOfNonParameters)
      | SemGlobalIdentifier s       -> let ptr = LLVM.GetNamedGlobal (theModule, s)
                                       if needPtr then ptr else GenerateLoad ptr
      | SemAssign (l, r)            -> LLVM.BuildStore (theBuilder, generateInstruction curAR false r, generateInstruction curAR true l)
      | SemNone                     -> theZero 8
      | SemGoto _                   -> theZero 0
      | SemResult                   -> if needPtr then GenerateStructAccess curAR Environment.ActivationRecord.ReturnFieldIndex
                                       else GenerateStructLoad curAR Environment.ActivationRecord.ReturnFieldIndex
      | SemAllocAR s                -> let ar = Map.tryFind s ars
                                       match ar with
                                       | Some a       -> GenerateLocal a
                                       | None         -> raise <| Error.InternalException "No activation record for that name is registered"
      | SemFunctionCall (n, ps)     -> let llvmParams = List.map (generateInstruction curAR false) ps
                                       let targetARType = Map.find n ars
                                       let targetAR = GenerateLocal targetARType
                                       GenerateStructStore targetAR 0 curAR |> ignore   // TODO: This is not the correct access link
                                       List.iteri (fun i p -> GenerateStructStore targetAR (i + 2) p |> ignore) llvmParams  // setup all other parameters
                                       GenerateFunctionCall n [targetAR] |> ignore
                                       GenerateStructLoad targetAR 1 
      | SemDeclFunction _           -> raise <| Error.InternalException "Cannot generate function in this context"

    generateInstruction curAR false inst 

  let GenerateFunctionCode arTypes (func: SemanticFunction) =
    let funcName, instructions = func
    let theFunction = LLVM.GetNamedFunction (theModule, funcName)

    GenerateBasicBlock theFunction "entry" |> ignore
    let retInstr = LLVM.BuildRetVoid theBuilder

    LLVM.PositionBuilderBefore (theBuilder, retInstr)

    let currentAR = theFunction.GetFirstParam ()
    List.iter (fun instruction -> (GenerateInstruction arTypes currentAR instruction) |> ignore) instructions

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
    LLVM.BuildRet (theBuilder, theZero (Environment.VariableSize.IntegerSizeBits)) |> ignore

    theMain