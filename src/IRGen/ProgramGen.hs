module IRGen.ProgramGen where

import IRGen.GlobalGen
import Grammar
import IRGen.IR

programGen :: Program -> IR ()
programGen (Program gus) = globalGen gus