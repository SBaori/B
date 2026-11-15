module Parser.AutoVarDeclaration where

import Parser.Parser
import Grammar
import Lexer.Token
import Parser.Expression
import Parser.VarInitList

autoVarDeclarations :: Parser ScopedVarDeclrs
autoVarDeclarations = do
    expectKind VAR_KEYWORD
    helper

    where
        helper :: Parser ScopedVarDeclrs
        helper = do
            (Token l _ p) <- peek 0
            expectKind IDENTIFIER
            (Token _ k _) <- peek 0

            case k of
                COMMA -> do
                    expectKind COMMA
                    ScopedVarDeclrs (Node l p) Nothing <$> helper
                
                EQUALS -> do
                    expectKind EQUALS
                    e <- expr

                    (Token _ k' _) <- peek 0

                    case k' of
                        COMMA -> do
                            expectKind COMMA
                            ScopedVarDeclrs (Node l p) (Just e) <$> helper

                        _ -> pure $ ScopedVarDeclrsEnd (Node l p) (Just e)

                OPEN_SQUARE -> do
                    expectKind OPEN_SQUARE
                    e <- expr
                    expectKind CLOSE_SQUARE

                    (Token _ k' _) <- peek 0

                    case k' of
                        COMMA -> do
                            expectKind COMMA
                            ScopedVarDeclrsArr (Node l p) e Nothing <$> helper
                        
                        EQUALS -> do
                            expectKind EQUALS
                            initList <- varInitList

                            (Token _ k'' _) <- peek 0

                            case k'' of
                                COMMA -> do
                                    expectKind COMMA
                                    ScopedVarDeclrsArr (Node l p) e (Just initList) <$> helper
                                
                                _ -> pure $ ScopedVarDeclrsArrEnd (Node l p) e (Just initList)
                            
                        _ -> pure $ ScopedVarDeclrsArrEnd (Node l p) e Nothing

                _ -> pure $ ScopedVarDeclrsEnd (Node l p) Nothing

