module Parser.VarInitList where

import Parser.Parser
import Grammar
import Lexer.Token
import Parser.Expression

varInitList :: Parser [Expr]
varInitList = do
    expectKind OPEN_CURLY
    initList <- helper
    expectKind CLOSE_CURLY
    pure initList

    where

    helper = do
        (Token _ k _) <- peek 0

        case k of
            CLOSE_CURLY -> pure []

            COMMA -> expectKind COMMA >> helper 

            _ -> do
                e <- expr
                (e:) <$> helper