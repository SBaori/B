module Parser.Scope 
    (scope)
where

import Lexer.Token
import Grammar
import Control.Monad.Error.Class (MonadError(throwError))
import Error
import Parser.Parser
import Parser.Expression
import Parser.AutoVarDeclaration
import Parser.ExternVarDeclaration

scope :: Parser ScopedUnits
scope = do
    expectKind OPEN_CURLY
    units <- parseUnits
    expectKind CLOSE_CURLY

    pure units

    where
        parseUnits :: Parser ScopedUnits
        parseUnits = do
            (Token _ k _) <- peek 0

            case k of
                CLOSE_CURLY -> pure ScopedUnitsEnd
                _ -> do
                    u <- scopedUnit
                    ScopedUnit u <$> parseUnits

scopedUnit :: Parser ScopedUnit
scopedUnit = do
    (Token l k p) <- peek 0

    case k of
        VAR_KEYWORD -> do
            d <- autoVarDeclarations
            expectKind UNIT_END
            pure $ ScopedUnitVarDeclr d
        
        EXTERN -> do
            ed <- externVarDeclarations
            expectKind UNIT_END
            pure $ ScopedUnitExternDeclr ed

        IF -> (ScopedUnitControl . ControlIf) <$> ifElse

        WHILE -> (ScopedUnitControl . ControlLoop) <$> loop

        RETURN -> do
            expectKind RETURN

            (Token _ k' _) <- peek 0

            case k' of
                UNIT_END -> do
                    expectKind UNIT_END
                    pure $ ScopedUnitReturn (Node l p) Nothing
                
                _ -> do
                    e <- expr
                    expectKind UNIT_END
                    pure $ ScopedUnitReturn (Node l p) (Just e)

        CONTINUE -> do
            expectKind CONTINUE
            expectKind UNIT_END
            pure $ ScopedUnitContinue (Node l p)
        
        BREAK -> do
            expectKind BREAK
            expectKind UNIT_END
            pure $ ScopedUnitBreak (Node l p)

        _ -> do
            e <- expr
            expectKind UNIT_END
            pure $ ScopedUnitExpr e

ifElse :: Parser IfElse
ifElse = do
    ifp <- ifParse
    IfElse ifp <$> elseIf

    where
        elseIf :: Parser ElseIf
        elseIf = do
            (Token _ k _) <- peek 0

            case k of
                ELSE -> do
                    expectKind ELSE

                    (Token _ k' _) <- peek 0

                    case k' of
                        IF -> do
                            ifp <- ifParse
                            ElseIf ifp <$> elseIf
                        OPEN_CURLY -> ElseScoped <$> scope
                        _ -> ElseUnScoped <$> scopedUnit

                _ -> pure Fi

        ifParse :: Parser If
        ifParse = do
            expectKind IF
            expectKind OPEN_ROUND
            e <- expr
            expectKind CLOSE_ROUND

            (Token _ k _) <- peek 0

            case k of
                OPEN_CURLY -> IfScoped e <$> scope
                _ -> IfUnScoped e <$> scopedUnit


loop :: Parser Loop
loop = do
    (Token _ k p) <- peek 0

    case k of
        WHILE -> do
            expectKind WHILE
            expectKind OPEN_ROUND
            e <- expr
            expectKind CLOSE_ROUND

            (Token _ k' _) <- peek 0

            case k' of
                OPEN_CURLY -> WhileScoped e <$> scope
                _ -> WhileUnscoped e <$> scopedUnit
        
        _ -> throwError $ Error p "Only While loops are supported!"