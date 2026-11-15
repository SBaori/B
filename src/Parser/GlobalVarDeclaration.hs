module Parser.GlobalVarDeclaration where

import Parser.Parser
import Grammar
import Lexer.Token
import Parser.Expression
import Parser.VarInitList
import Control.Monad.Error.Class (MonadError(throwError))
import Error

globalVarDeclrs :: Parser GlobalVarDeclrs
globalVarDeclrs = do
    (Token l _ p) <- peek 0
    expectKind IDENTIFIER
    (Token _ k _) <- peek 0

    case k of
        EQUALS -> do
            expectKind EQUALS
            e <- expr
            expectKind UNIT_END

            pure $ GlobalVarDeclrs (Node l p) e
        
        OPEN_SQUARE -> do
            expectKind OPEN_SQUARE
            e <- expr
            expectKind CLOSE_SQUARE
            expectKind EQUALS
            initList <- varInitList
            expectKind UNIT_END

            pure $ GlobalVarDeclrsArr (Node l p) e initList
                
        _ -> throwError $ Error p "Global variables are required to be explicitly initalized!"

