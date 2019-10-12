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
    let BooleanSizeBits = BooleanSize * 8
    let PointerSizeBits = PointerSize * 8

  module ActivationRecord =
    let AccessLinkIndex = 0
    let ReturnFieldIndex = 1
    let NumberOfNonParameters = 2

  let GlobalScopeNesting = 0
