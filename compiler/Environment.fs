namespace Compiler.Helpers

module Environment =
  module VariableSize =
    let IntegerSize = 2
    let RealSize = 10
    let CharacterSize = 1
    let BooleanSize = 1
    let PointerSize = 2
    let ErroneousSize = 0

  let GlobalScopeNesting = 0