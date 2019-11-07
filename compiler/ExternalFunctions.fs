namespace Compiler.Helpers

open Compiler

module ExternalFunctions =
  let private stringType = Base.IArray Base.Character

  let ExternalIO = [
     Base.ProcessHeader ("writeInteger", [("n", Base.Integer, Base.ProcessParamSpecies.ByValue)], Base.Unit);
     Base.ProcessHeader ("writeBoolean", [("b", Base.Boolean, Base.ProcessParamSpecies.ByValue)], Base.Unit);
     Base.ProcessHeader ("writeChar", [("c", Base.Character, Base.ProcessParamSpecies.ByValue)], Base.Unit);
     Base.ProcessHeader ("writeReal", [("r", Base.Real, Base.ProcessParamSpecies.ByValue)], Base.Unit);
     Base.ProcessHeader ("writeString", [("s", stringType, Base.ProcessParamSpecies.ByRef)], Base.Unit);

     Base.ProcessHeader("readInteger", [], Base.Integer);
    //  Base.ProcessHeader("readBoolean", [], Base.Boolean);
    //  Base.ProcessHeader("readChar",    [], Base.Character);
    //  Base.ProcessHeader("readReal",    [], Base.Real);
    //  Base.ProcessHeader("readString",  [("size", Base.Integer, Base.ProcessParamSpecies.ByValue); ("s", stringType, Base.ProcessParamSpecies.ByRef)], Base.Unit)
    ]