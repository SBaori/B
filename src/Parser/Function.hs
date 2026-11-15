module Parser.Function where

import Parser.Parser
import Grammar
import Lexer.Token
import Parser.Scope

functionDfn :: Parser FuncDfn
functionDfn = do
    (Token l _ p) <- peek 0

    expectKind IDENTIFIER
    expectKind OPEN_ROUND

    params <- getParams

    expectKind CLOSE_ROUND

    FuncDfn (Node l p) params <$> scope

    where
        getParams :: Parser [(Node String)]
        getParams = do
            (Token l k p) <- peek 0

            case k of
                CLOSE_ROUND -> pure []

                COMMA -> do
                    expectKind COMMA
                    getParams

                _ -> do
                    expectKind IDENTIFIER
                    ((Node l p):) <$> getParams

