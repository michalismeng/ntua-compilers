namespace Compiler

open Compiler.Base
open LLVMSharp
open System

module Module =
  let mutable theModule = Unchecked.defaultof<LLVMModuleRef>
  let mutable theBuilder = Unchecked.defaultof<LLVMBuilderRef>

module rec CodeGenerator =

  open Module

  let private theTrue  = LLVMBool 1
  let private theFalse = LLVMBool 0

  let generateBinop op lhs rhs isFloat =
    match op with
    | Add           -> if isFloat then LLVM.BuildFAdd (theBuilder, lhs, rhs, "tfadd") else LLVM.BuildAdd (theBuilder, lhs, rhs, "tadd") 
    | Sub           -> if isFloat then LLVM.BuildFSub (theBuilder, lhs, rhs, "tfsub") else LLVM.BuildSub (theBuilder, lhs, rhs, "tsub")
    | Mult          -> if isFloat then LLVM.BuildFMul (theBuilder, lhs, rhs, "tfmult") else LLVM.BuildMul (theBuilder, lhs, rhs, "tmult")
    | Div           -> if isFloat then LLVM.BuildFDiv (theBuilder, lhs, rhs, "tfdiv") else raise <| Helpers.Error.InternalException "Operator Div requires float"
    | Divi          -> if not(isFloat) then LLVM.BuildSDiv (theBuilder, lhs, rhs, "tdivi") else raise <| Helpers.Error.InternalException "Operatir Divi requires integer"
    | Modi          -> if not(isFloat) then LLVM.BuildSRem (theBuilder, lhs, rhs, "tmodi") else raise <| Helpers.Error.InternalException "Operatir Modi requires integer"
    | Equals        -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOEQ, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntEQ, lhs, rhs, "teq")
    | NotEquals     -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealONE, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntNE, lhs, rhs, "teq")
    | Less          -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOLT, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSLE, lhs, rhs, "teq")
    | LessEquals    -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOLE, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSLT, lhs, rhs, "teq")
    | Greater       -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOGT, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSGT, lhs, rhs, "teq")
    | GreaterEquals -> if isFloat then LLVM.BuildFCmp (theBuilder, LLVMRealPredicate.LLVMRealOGE, lhs, rhs, "tfeq") else LLVM.BuildICmp (theBuilder, LLVMIntPredicate.LLVMIntSGE, lhs, rhs, "teq")
    | Or            -> LLVM.BuildOr  (theBuilder, lhs, rhs, "tor")
    | And           -> LLVM.BuildAnd (theBuilder, lhs, rhs, "tand")

  type Base.Type with
    member this.LLVMInitializer =
      match this with
      | Integer         -> LLVM.ConstInt (this.ToLLVM (), 0UL, theTrue)
      | Boolean         -> LLVM.ConstInt (this.ToLLVM (), 0UL, theTrue)
      | Character       -> LLVM.ConstInt (this.ToLLVM (), 0UL, theTrue)
      | Real            -> LLVM.ConstReal (this.ToLLVM (), 0.0)
      | Array (t, i)    -> LLVM.ConstArray (t.ToLLVM (), t.LLVMInitializer |> List.replicate i |> Array.ofList)
      | Ptr t           -> LLVM.ConstPointerNull (LLVM.PointerType(t.ToLLVM(), 0u))
      | _               -> raise <| Helpers.Error.InternalException "Cannot get LLVM initializer for this type"

    member this.ToLLVM () =
      match this with
      | Unit            -> LLVM.VoidType ()
      | Integer         -> LLVM.Int32Type ()
      | Boolean         -> LLVM.Int8Type ()
      | Character       -> LLVM.Int8Type ()
      | Real            -> LLVM.X86FP80Type ()
      | Array (t, i)    -> LLVM.ArrayType (t.ToLLVM (), uint32(i))
      | Ptr t           -> LLVM.PointerType (t.ToLLVM (), 0u)
      | _               -> raise <| Helpers.Error.InternalException "Invalid conversion to LLVM type"


  let GenerateStructType' parent (header: Base.ProcessHeader) (body: Base.Body) =
    let _, _params, _ = header
    let declarations, _ = body

    let mapVariableDeclaration decl = 
      match decl with
      | Variable (_, t) -> Some t
      | _               -> None

    let paramTypes = _params |> 
                     List.map (fun (_, t, _) -> t)

    let declTypes = declarations 
                    |> List.choose mapVariableDeclaration 
                    
    let structTypes = paramTypes @ declTypes
    GenerateStructType parent structTypes

  let GenerateStructType (parent: LLVMTypeRef) types =
    let toLLVM (t: Base.Type) = t.ToLLVM ()
    let structTypes = parent :: (types |> List.map toLLVM)
    LLVM.StructType (Array.ofList structTypes, false)

  let GenerateStructAccess structPtr fieldIndex =
    LLVM.BuildGEP (theBuilder, structPtr, [LLVM.ConstInt (LLVM.Int32Type (), 0UL, LLVMBool 0); LLVM.ConstInt (LLVM.Int32Type (), uint64(fieldIndex), LLVMBool 0)] |> Array.ofList, "tempptr")

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

  let GenerateFunction name arStruct (retType: Base.Type) attrs =
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

    if theCall.Pointer = IntPtr.Zero then
      raise <| Helpers.Error.InternalException "Could not find function by name in LLVM ledger"

    let retType = LLVM.GetReturnType (theCall.TypeOf ())

    if retType.GetElementType().TypeKind = LLVMTypeKind.LLVMVoidTypeKind then 
      LLVM.BuildCall (theBuilder, theCall, parameters, "")
    else
      LLVM.BuildCall (theBuilder, theCall, parameters, "tempcall")

  // let GenerateExpression symTable theModule theBuilder expression =
  //   match expression with
  //   | LExpression l -> generateLValue symTable theModule theBuilder l
  //   | RExpression r -> generateRValue r

  // let GenerateStatement symTable (theModule: LLVMModuleRef) (theBuilder: LLVMBuilderRef) (statement: Statement) =
  //   match statement with
  //   | Empty               -> (theModule, theBuilder)
  //   | Error _             -> (theModule, theBuilder)
  //   | Assign (lval, expr, _) -> let lhs = GenerateExpression symTable theModule theBuilder (LExpression lval)
  //                               let rhs = GenerateExpression symTable theModule theBuilder expr
  //                               LLVM.BuildStore (theBuilder, lhs, rhs) |> ignore
  //                               (theModule, theBuilder)
  //   | Block stmts         -> List.fold (fun (tmod, tbuil) s -> GenerateStatement symTable tmod tbuil s) (theModule, theBuilder) stmts
  //   | _ -> raise <| Helpers.Error.InternalException "error"

  let GenerateMain () =
    theModule <- LLVM.ModuleCreateWithName "PCL Compiler"
    theBuilder <- LLVM.CreateBuilder ()

    let theMain = GenerateFunctionRogue "main" [] Integer [LLVMLinkage.LLVMExternalLinkage]

    GenerateBasicBlock theMain "entry" |> ignore
    LLVM.BuildRet (theBuilder, LLVM.ConstInt (LLVM.Int32Type (), 0UL, theFalse)) |> ignore
    (theModule, theBuilder)

  let private generateARType parentName parent header body =
      let mapFunction decl =
        match decl with
        | Process (hdr, body) -> Some (hdr, body)
        | _                         -> None

      let funcName, _, _ = header
      let declarations, _ = body
      let structName = parentName + "." + funcName

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
    generateARType "" (LLVM.IntType (0u)) (name, [], Unit) body |>
    Map.ofList

  let navigateToAR curAR timesUp = 
      List.fold (fun acc _ -> GenerateStructLoad acc 0) curAR [1..timesUp]

  let GenerateInstruction curAR inst =
    match inst with
    | SemInt i                    -> LLVM.ConstInt (LLVM.Int32Type (), uint64(i), theTrue)
    | SemReal r                   -> LLVM.ConstReal (LLVM.X86FP80Type (), float(r))
    | SemChar c                   -> LLVM.ConstInt (LLVM.Int8Type  (), uint64(c), theTrue)      // TODO: 'c' is a string that maps to a character (may have escape sequences)
    | SemBool b                   -> LLVM.ConstInt (LLVM.Int1Type  (), Convert.ToUInt64 b, theTrue)
    | SemString s                 -> LLVM.ConstString (s, uint32(s.Length), theTrue)
    | SemNil                      -> LLVM.ConstNull (LLVM.Int32Type ())
    | SemBinop (e1, e2, op, typ)  -> generateBinop op (GenerateInstruction curAR e1) (GenerateInstruction curAR e2) (typ = Real)
    | SemAddress l                -> raise <| Helpers.Error.InternalException "akffkgjdfg"
    | SemIdentifier (u, i)        -> let ar = navigateToAR curAR u
                                     GenerateStructLoad ar (i + 1)    // increment intra motion since entry 0 is the access link