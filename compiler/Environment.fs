namespace Compiler.Helpers

module Environment =
  module VariableSize =
    let IntegerSize = 2
    let RealSize = 10
    let CharacterSize = 1
    let BooleanSize = 1
    let PointerSize = 2
    let ErroneousSize = 0

    let IntegerSizeBits = IntegerSize * 8
    let RealSizeBits = RealSize * 8
    let CharacterSizeBits = CharacterSize * 8
    let BooleanSizeBits = 1                   // TODO: We need this to be 8 bits - but then comparisons in llvm cry
    let PointerSizeBits = PointerSize * 8

  module ActivationRecord =
    let AccessLinkIndex = 0
    let ReturnFieldIndex = 1
    let NumberOfNonParameters = 2

  let GlobalScopeNesting = 0
  let ExternalsScopeName = "Guard"

  module CLI =
    open CommandLine;

    type Options = {
      [<Option('f',HelpText = "Output final code to standard output")>] finalStdout : bool;
      [<Option('i',HelpText = "Output intermediate code to standard output")>] interimStdout : bool;
      [<Option('O',HelpText = "Optimize code")>] shouldOptimize : bool;
      [<Option('l', "library", HelpText = "Is this a library file")>] isLibrary : bool;
      [<Value(0, Required=true, MetaName="inputFile", HelpText = "Input file")>] input : string;
    }

    // Name of the file being processed by the compiler
    let mutable FileName = "" 
    let mutable ShouldOptimize = false
    let mutable FinalCodeToStdout = false
    let mutable InterimCodeToStdout = false
    let mutable IsLibrary = false

    let private setCLIOptions (options: Options) =
      FileName <- options.input
      ShouldOptimize <- options.shouldOptimize
      FinalCodeToStdout <- options.finalStdout
      InterimCodeToStdout <- options.interimStdout
      IsLibrary <- options.isLibrary

    let parseCLIArguments argv =
      match CommandLine.Parser.Default.ParseArguments<Options>(argv) with
      | :? Parsed<Options> as parsed -> setCLIOptions parsed.Value; true
      | :? NotParsed<Options> -> false
      | _ -> false






