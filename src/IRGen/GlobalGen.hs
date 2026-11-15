module IRGen.GlobalGen where

import IRGen.IR
import Grammar
import IRGen.GlobalVarDeclarationGen
import IRGen.FunctionGen

globalGen :: GlobalUnits -> IR ()
globalGen (GlobalUnitsEnd) = pure ()

globalGen (GlobalUnitVarDeclr gvd gus) = do
    globalVarDeclrGen gvd
    globalGen gus

globalGen (GlobalUnitFuncDfn gfd gus) = do
    functionDfnGen gfd
    globalGen gus