module Parser.Expression where

import Parser.Parser
import Grammar
import Lexer.Token
import Control.Monad.Error.Class (MonadError(throwError))
import Error
import qualified Data.Map.Strict as M

element :: Parser Element
element = do
    (Token l k p) <- peek 0

    case k of
        NUM -> do
            expectKind NUM
            pure $ Num (Node l p)
        IDENTIFIER -> do
            expectKind IDENTIFIER
            pure $ Var (Node l p)
        STRING -> do
            expectKind STRING
            pure $ Str (Node l p)
        OPEN_ROUND -> do
            expectKind OPEN_ROUND
            e <- expr
            expectKind CLOSE_ROUND
            pure $ Expr e

        _ -> throwError $ Error p ("expected an expression, but got: " ++ (tokenKindToString M.! k))

postFix :: Parser PostFix
postFix = do
    el <- element

    (Token _ k _) <- peek 0

    case k of
        OPEN_ROUND -> do
            expectKind OPEN_ROUND
            args <- getArgs
            expectKind CLOSE_ROUND

            Opr el <$> (opr (FuncCallEnd args))

        OPEN_SQUARE -> do
            expectKind OPEN_SQUARE
            e <- expr
            expectKind CLOSE_SQUARE

            Opr el <$> (opr (OffsetEnd e))
        
        PLUS_PLUS -> do
            expectKind PLUS_PLUS
            Opr el <$> (opr PostIncrEnd)
        
        MINUS_MINUS -> do
            expectKind MINUS_MINUS
            Opr el <$> (opr PostDecrEnd)

        _ -> pure $ Element el

    where
        opr :: Opr -> Parser Opr
        opr acc = do
            (Token _ k _) <- peek 0

            case k of
                OPEN_ROUND -> do
                    expectKind OPEN_ROUND
                    args <- getArgs
                    expectKind CLOSE_ROUND
                    
                    opr (FuncCall args acc)
                OPEN_SQUARE -> do
                    expectKind OPEN_SQUARE
                    e <- expr
                    expectKind CLOSE_SQUARE

                    opr (Offset e acc)
                
                PLUS_PLUS -> do
                    expectKind PLUS_PLUS
                    opr (PostIncr acc)
                
                MINUS_MINUS -> do
                    expectKind MINUS_MINUS
                    opr (PostDecr acc)

                _ -> pure $ acc

        getArgs :: Parser [Expr]
        getArgs = do
            (Token _ k _) <- peek 0

            case k of
                COMMA -> do
                    expectKind COMMA
                    getArgs
                CLOSE_ROUND -> pure []
                _ -> do
                    e <- expr
                    (e:) <$> getArgs

preFix :: Parser PreFix
preFix = do
    (Token _ k _) <- peek 0

    case k of
        NEGATION -> do
            expectKind NEGATION
            Negate <$> preFix

        MUL -> do
            expectKind MUL
            DeRef <$> preFix

        ANP -> do
            expectKind ANP
            Addr <$> preFix

        MINUS -> do
            expectKind MINUS
            UnarySub <$> preFix
        
        PLUS_PLUS -> do
            expectKind PLUS_PLUS
            PreIncr <$> preFix
        
        MINUS_MINUS -> do
            expectKind MINUS_MINUS
            PreDecr <$> preFix

        _ -> PostFix <$> postFix

mulDivMod :: Parser MulDivMod
mulDivMod = do
    pre <- preFix
    leftFold (PreFix pre)
    
    where
        leftFold :: MulDivMod -> Parser MulDivMod
        leftFold acc = do
            (Token _ k _) <- peek 0

            case k of
                MUL -> do
                    expectKind MUL
                    pre <- preFix

                    leftFold (Mul acc pre)
                
                DIV -> do
                    expectKind DIV
                    pre <- preFix

                    leftFold (Div acc pre)
                
                MOD -> do
                    expectKind MOD
                    pre <- preFix

                    leftFold (Mod acc pre)
                
                _ -> pure acc

addSub :: Parser AddSub
addSub = do
    mdm <- mulDivMod
    leftFold (MulDivMod mdm)

    where
        leftFold :: AddSub -> Parser AddSub
        leftFold acc = do
            (Token _ k _) <- peek 0

            case k of
                PLUS -> do
                    expectKind PLUS
                    mdm <- mulDivMod

                    leftFold (Add acc mdm)
                
                MINUS -> do
                    expectKind MINUS
                    mdm <- mulDivMod

                    leftFold (Sub acc mdm)
                
                _ -> pure acc


bitShifts :: Parser BitShifts
bitShifts = do
    as <- addSub
    leftFold (AddSub as)

    where
        leftFold :: BitShifts -> Parser BitShifts
        leftFold acc = do
            (Token _ k _) <- peek 0

            case k of
                BIT_LEFT_SHIFT -> do
                    expectKind BIT_LEFT_SHIFT
                    as <- addSub

                    leftFold (LeftShift acc as)
    
                BIT_RIGHT_SHIFT -> do
                    expectKind BIT_RIGHT_SHIFT
                    as <- addSub

                    leftFold (RightShift acc as)

                _ -> pure acc

relationExpr :: Parser RelationExpr
relationExpr = do
    bs <- bitShifts
    leftFold (BitShifts bs)

    where
        leftFold :: RelationExpr -> Parser RelationExpr
        leftFold acc = do
            (Token _ k _) <- peek 0

            case k of
                LESS -> do
                    expectKind LESS
                    bs <- bitShifts

                    leftFold (Less acc bs)
                
                ULESS -> do
                    expectKind ULESS
                    bs <- bitShifts

                    leftFold (ULess acc bs)
    
                GREATER -> do 
                    expectKind GREATER
                    bs <- bitShifts

                    leftFold (Greater acc bs)
                
                UGREATER -> do
                    expectKind UGREATER
                    bs <- bitShifts

                    leftFold (UGreater acc bs)

                LESS_EQUAL -> do
                    expectKind LESS_EQUAL
                    bs <- bitShifts

                    leftFold (LessEq acc bs)
                
                ULESS_EQUAL -> do
                    expectKind ULESS_EQUAL
                    bs <- bitShifts

                    leftFold (ULessEq acc bs)

                GREATER_EQUAL -> do
                    expectKind GREATER_EQUAL
                    bs <- bitShifts

                    leftFold (GreaterEq acc bs)

                UGREATER_EQUAL -> do
                    expectKind UGREATER_EQUAL
                    bs <- bitShifts

                    leftFold (UGreaterEq acc bs)

                _ -> pure acc

equalityExpr :: Parser EqualityExpr
equalityExpr = do
    re <- relationExpr
    leftFold (RelationExpr re)
    
    where
        leftFold :: EqualityExpr -> Parser EqualityExpr
        leftFold acc = do
            (Token _ k _) <- peek 0

            case k of
                EQUALITY -> do
                    expectKind EQUALITY
                    re <- relationExpr

                    leftFold (Equal acc re)

                NOT_EQUALITY -> do
                    expectKind NOT_EQUALITY
                    re <- relationExpr

                    leftFold (NotEqual acc re)

                _ -> pure acc


bitAnd :: Parser BitAnd
bitAnd = do
    ee <- equalityExpr
    leftFold (EqualityExpr ee)

    where
        leftFold :: BitAnd -> Parser BitAnd
        leftFold acc = do
            (Token _ k _) <- peek 0

            case k of
                ANP -> do
                    expectKind ANP
                    ee <- equalityExpr

                    leftFold (BitAnd acc ee)

                _ -> pure acc

bitXor :: Parser BitXor
bitXor = do
    ba <- bitAnd
    leftFold (BitAnd' ba)

    where
        leftFold :: BitXor -> Parser BitXor
        leftFold acc = do
            (Token _ k _) <- peek 0

            case k of
                XOR -> do
                    expectKind XOR
                    ba <- bitAnd

                    leftFold (BitXor acc ba)

                _ -> pure acc

bitOr :: Parser BitOr
bitOr = do
    bx <- bitXor
    leftFold (BitXor' bx)

    where
        leftFold :: BitOr -> Parser BitOr
        leftFold acc = do
            (Token _ k _) <- peek 0

            case k of
                BAR -> do
                    expectKind BAR
                    bx <- bitXor

                    leftFold (BitOr acc bx)

                _ -> pure acc

logicalAnd :: Parser LogicalAnd
logicalAnd = do
    bo <- bitOr
    leftFold (BitOr' bo)
    
    where
        leftFold :: LogicalAnd -> Parser LogicalAnd
        leftFold acc = do
            (Token _ k _) <- peek 0

            case k of
                AND -> do
                    expectKind AND
                    bo <- bitOr

                    leftFold (And acc bo)

                _ -> pure acc

logicalOr :: Parser LogicalOr
logicalOr = do
    la <- logicalAnd 
    leftFold (LogicalAnd la)
    
    where
        leftFold :: LogicalOr -> Parser LogicalOr
        leftFold acc = do
            (Token _ k _) <- peek 0

            case k of
                OR -> do
                    expectKind OR
                    la <- logicalAnd

                    leftFold (Or acc la)

                _ -> pure acc

ternary :: Parser Ternary
ternary = do
    lo <- logicalOr

-- TODO: Implement ternary

    pure $ LogicalOr lo

expr :: Parser Expr
expr = do
    t <- ternary

    (Token _ k _) <- peek 0

    case k of
        EQUALS -> do
            expectKind EQUALS
            Assign t <$> expr
        
        PLUS_ASSIGN -> do
            expectKind PLUS_ASSIGN
            AssignAdd t <$> expr
        
        MINUS_ASSIGN -> do
            expectKind MINUS_ASSIGN
            AssignSub t <$> expr
        
        MUL_ASSIGN -> do
            expectKind MUL_ASSIGN
            AssignMul t <$> expr
        
        DIV_ASSIGN -> do
            expectKind DIV_ASSIGN
            AssignDiv t <$> expr
        
        MOD_ASSIGN -> do
            expectKind MOD_ASSIGN
            AssignMod t <$> expr
        
        BITOR_ASSIGN -> do
            expectKind BITOR_ASSIGN
            AssignBitOr t <$> expr
        
        BITAND_ASSIGN -> do
            expectKind BITAND_ASSIGN
            AssignBitAnd t <$> expr
        
        BITXOR_ASSIGN -> do
            expectKind BITXOR_ASSIGN
            AssignBitXor t <$> expr
        
        BLS_ASSIGN -> do
            expectKind BLS_ASSIGN
            AssignBitLShift t <$> expr
        
        BRS_ASSIGN -> do
            expectKind BRS_ASSIGN
            AssignBitRShift t <$> expr
        
        _ -> pure $ Ternary' t

