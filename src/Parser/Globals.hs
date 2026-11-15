module Parser.Globals where

import Parser.Parser
import Grammar
import Lexer.Token
import Parser.Function
import Parser.GlobalVarDeclaration

global :: Parser GlobalUnits
global = do
    (Token _ k _) <- peek 0

    case k of
        EOF -> pure GlobalUnitsEnd

        _ -> do
            (Token _ k' _) <- peek 1

            case k' of
                OPEN_ROUND -> do
                    fnDfn <- functionDfn
                    GlobalUnitFuncDfn fnDfn <$> global

                _ -> do
                    gv <- globalVarDeclrs
                    GlobalUnitVarDeclr gv <$> global