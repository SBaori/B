module Parser.ExternVarDeclaration where

import Parser.Parser
import Grammar
import Lexer.Token

externVarDeclarations :: Parser ExternVarDeclrs
externVarDeclarations = do
    expectKind EXTERN
    vars

    where
        vars :: Parser ExternVarDeclrs
        vars = do
            (Token l _ p) <- peek 0
            
            expectKind IDENTIFIER
            
            (Token _ k' _) <- peek 0
            
            case k' of
                COMMA -> do
                    expectKind COMMA
                    ExternVarDeclrs (Node l p) <$> vars
                
                _ -> pure $ ExternVarDeclrsEnd $ Node l p

