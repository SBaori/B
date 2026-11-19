module IRGen.ExprGen 
    (exprGen)
where

import IRGen.IR
import Grammar
import Control.Monad.Error.Class (MonadError(throwError))
import IRGen.IRGenUtils
import Data.Char (ord)
import Data.List (intercalate)
import Data.Bool (bool)
import Error
import Data.Bits (xor, (.|.), (.&.), (.<<.), (.>>.))

elementGen :: Scopes -> Element -> IR ExprRes
elementGen _ (Num (Node n p)) = do
    -- @todo: Add overflow warnings.
    let num = read n :: Word

    -- addIR $ currReg ++ " =l add 0, " ++ show num
    pure (Literal (Node num p))

elementGen scopes (Var node) = either throwError pure (getScopeVariable scopes node)

elementGen scopes (Expr e) = exprGen scopes e

elementGen _ (Str (Node str p)) = do
    uc <- newUniqCounter

    addIRBuffer $ "data $.str" ++ uc ++ " = { "
    helper str
    addIRBuffer "}"
    pure $ Temporary (Node ("$.str" ++ uc) p)

    where
        helper [] = addIRBuffer $ "l 0"
        helper (c:cs) = do
            addIRBuffer $ "l " ++ show (ord c) ++ ","
            helper cs

postFixGen :: Scopes -> PostFix -> IR ExprRes
postFixGen scopes (Element el) = elementGen scopes el
postFixGen scopes (Opr el opr) = elementGen scopes el >>= oprGen opr
    where
        oprGen :: Opr -> ExprRes -> IR ExprRes
        oprGen (FuncCall args opr') exprRes = 
            oprGen opr' exprRes >>= oprGen (FuncCallEnd args)

        oprGen (FuncCallEnd args) exprRes =
            case exprRes of
                Literal (Node num p) -> do
                    argList <- intercalate "," 
                                <$> map (("l " ++))
                                <$> argsGen scopes args

                    reg1 <- newReg
                    addIR $ reg1 ++ " =l copy " ++ show num

                    reg2 <- newReg
                    addIR $ reg2 ++ " =l call " ++ reg1 ++ "(" ++ argList ++ ")"
                    pure $ Temporary (Node reg2 p)
                
                nonLiteral -> do
                    let (Node nid npos) = name nonLiteral

                    argList <- intercalate "," 
                                <$> map (("l " ++))
                                <$> argsGen scopes args

                    reg <- newReg
                    addIR $ reg ++ " =l call " ++ nid ++ "(" ++ argList ++ ")"

                    pure $ Temporary (Node reg npos)

        oprGen (PostIncr opr') exprRes = 
            oprGen  opr' exprRes >>= oprGen PostIncrEnd 

        oprGen PostIncrEnd exprRes =
            case exprRes of
                Literal (Node _ p) -> throwError $ Error p "lvalue required for post-increment!"
                Temporary (Node _ p) -> throwError $ Error p "lvalue required for post-increment!"

                Storage (Node name p) -> do
                    val <- newReg
                    addIR $ val ++ " =l loadl " ++ name

                    newVal <- newReg
                    addIR $ newVal ++ " =l add " ++ val ++ ", 1"
                    addIR $ "storel " ++ newVal ++ ", " ++ name

                    pure $ Temporary (Node val p)

        oprGen (PostDecr opr') exprRes = 
            oprGen opr' exprRes >>= oprGen PostDecrEnd

        oprGen PostDecrEnd exprRes = 
            case exprRes of
                Literal (Node _ p) -> throwError $ Error p "lvalue required for post-decrement!"
                Temporary (Node _ p) -> throwError $ Error p "lvalue required for post-decrement!"

                Storage (Node name p) -> do
                    val <- newReg
                    addIR $ val ++ " =l loadl " ++ name

                    newVal <- newReg
                    addIR $ newVal ++ " =l sub " ++ val ++ ", 1"
                    addIR $ "storel " ++ newVal ++ ", " ++ name

                    pure $ Temporary (Node val p)

        oprGen (OffsetEnd e) exprRes = do
            res <- exprGen scopes e
            adjustedOffset <- evalBinOp res (Literal (Node (fromIntegral wordSize) (-1,-1))) (*) "l mul"
            storage <- evalBinOp exprRes adjustedOffset (+) "l add"

            case storage of
                Literal (Node num p) -> pure $ Storage (Node (show num) p)
                nonLiteral -> pure $ Storage (name nonLiteral)

            -- case exprRes of
            --     Literal (Node num p) -> do
            --         storage <- evalBinOp (Temporary (Node (show num) p)) adjustedOffset (+) "l add"

            --         case storage of
            --             Temporary (Node name p') -> pure $ Storage (Node name p')
            --             _ -> undefined              -- unreachable
                
            --     nonLiteral -> do
            --         let nid = nodeId $ name nonLiteral
            --         let p = nodePos $ name nonLiteral

            --         storage <- evalBinOp (Temporary (Node nid p)) adjustedOffset (+) "l add"

            --         case storage of
            --             Temporary (Node name p') -> pure $ Storage (Node name p')
            --             _ -> undefined              -- unreachable

        oprGen (Offset e opr') exprRes = oprGen opr' exprRes >>= oprGen (OffsetEnd e)

        argsGen :: Scopes -> [Expr] -> IR [String]
        argsGen _ [] = pure []
        argsGen scopes (e:es) = do
            reg <- exprGen scopes e

            case reg of
                (Literal (Node num _)) -> (show num:) <$> argsGen scopes es
                (Temporary (Node name _)) -> (name:) <$> argsGen scopes es
                (Storage (Node addr _)) -> do
                    reg <- newReg
                    addIR $ reg ++ " =l loadl " ++ addr
                    (reg:) <$> argsGen scopes es

preFixGen :: Scopes -> PreFix -> IR ExprRes
preFixGen scopes (PostFix postf) = postFixGen scopes postf
preFixGen scopes (Negate pref) = do
    res <- preFixGen scopes pref

    case res of
        Literal (Node num p) -> pure $ Literal (Node (bool 1 0 (num /= 0)) p)
        
        Storage (Node addr p) -> do
            val <- newReg
            addIR $ val ++ " =l loadl " ++ addr

            reg <- newReg
            addIR $ reg ++ " =l ceql " ++ val ++ ", 0"

            pure $ Temporary (Node reg p)
        
        Temporary (Node name p) -> do
            reg <- newReg
            addIR $ reg ++ " =l ceql " ++ name ++ ", 0"

            pure $ Temporary (Node reg p)
 
preFixGen scopes (DeRef pref) = do
    res <- preFixGen scopes pref

    case res of
        Literal (Node num p) -> pure $ Storage (Node (show num) p)
        Storage (Node name p) -> do
            val <- newReg
            addIR $ val ++ " =l loadl " ++ name

            pure $ Storage (Node val p)
        
        Temporary node -> pure $ Storage node

preFixGen scopes (Addr pref) = do
    res <- preFixGen scopes pref

    case res of
        Storage node -> pure $ Temporary node

        Temporary (Node _ p) -> 
            throwError $ Error p "lvalue required to take the address!"

        Literal (Node _ p) ->
            throwError $ Error p "lvalue required to take the address!"


preFixGen scopes (UnarySub pref) = do
    res <- preFixGen scopes pref

    case res of
        Literal (Node num p) -> pure $ Literal (Node (negate num) p)

        Temporary (Node name p) -> do
            reg <- newReg

            addIR $ reg ++ " =l neg " ++ name
            pure $ Temporary (Node reg p)

        Storage (Node addr p) -> do
            val <- newReg
            addIR $ val ++ " =l loadl " ++ addr

            reg <- newReg
            addIR $ reg ++ " =l neg " ++ val

            pure $ Temporary (Node reg p)

preFixGen scopes (PreIncr pref) = do
    res <- preFixGen scopes pref

    case res of
        Storage (Node addr p) -> do
            val <- newReg
            addIR $ val ++ " =l loadl " ++ addr

            newVal <- newReg
            addIR $ newVal ++ " =l add " ++ val ++ ", 1"
            addIR $ "storel " ++ newVal ++ ", " ++ addr

            pure $ Temporary (Node newVal p)

        Literal (Node _ p) ->
            throwError $ Error p "lvalue required for pre-increment!"

        Temporary (Node _ p) ->
            throwError $ Error p "lvalure required for pre-increment"

preFixGen scopes (PreDecr pref) = do
    res <- preFixGen scopes pref

    case res of
        Storage (Node addr p) -> do
            val <- newReg
            addIR $ val ++ " =l loadl " ++ addr

            newVal <- newReg
            addIR $ newVal ++ " =l sub " ++ val ++ ", 1"
            addIR $ "storel " ++ newVal ++ ", " ++ addr

            pure $ Temporary (Node newVal p)

        Literal (Node _ p) ->
            throwError $ Error p "lvalue required for pre-decrement!"

        Temporary (Node _ p) ->
            throwError $ Error p "lvalue required for pre-decrement!"


mulDivModGen :: Scopes -> MulDivMod -> IR ExprRes
mulDivModGen scopes (PreFix pref) = preFixGen scopes pref
mulDivModGen scopes (Mul mdm pref) = do
    left <- mulDivModGen scopes mdm
    right <- preFixGen scopes pref

    evalBinOp left right (*) "l mul"

mulDivModGen scopes (Div mdm pref) = do
    left <- mulDivModGen scopes mdm
    right <- preFixGen scopes pref

    evalBinOp left right div "l div"

mulDivModGen scopes (Mod mdm pref) = do
    left <- mulDivModGen scopes mdm
    right <- preFixGen scopes pref

    evalBinOp left right mod "l urem" 

addSubGen :: Scopes -> AddSub -> IR ExprRes
addSubGen scopes (MulDivMod mdm) = mulDivModGen scopes mdm
addSubGen scopes (Add as mdm) = do
    left <- addSubGen scopes as
    right <- mulDivModGen scopes mdm

    evalBinOp left right (+) "l add"

addSubGen scopes (Sub as mdm) = do
    left <- addSubGen scopes as
    right <- mulDivModGen scopes mdm

    evalBinOp left right (-) "l sub"

bitShiftsGen :: Scopes -> BitShifts -> IR ExprRes
bitShiftsGen scopes (AddSub as) = addSubGen scopes as
bitShiftsGen scopes (LeftShift bs as) = do
    left <- bitShiftsGen scopes bs
    right <- addSubGen scopes as
    
    evalBinOp left right ((\x y -> x .<<. (fromIntegral y))) "l shl"

bitShiftsGen scopes (RightShift bs as) = do
    left <- bitShiftsGen scopes bs
    right <- addSubGen scopes as

    evalBinOp left right ((\x y -> x .>>. (fromIntegral y))) "l shr"

relationExprGen :: Scopes -> RelationExpr -> IR ExprRes
relationExprGen scopes (BitShifts bs) = bitShiftsGen scopes bs
relationExprGen scopes (Less re bs) = do
    left <- relationExprGen scopes re
    right <- bitShiftsGen scopes bs

    evalBinOp left right (\x y -> if ((fromIntegral x) :: Int) < ((fromIntegral y) :: Int) then 1 else 0) "l csltl"

relationExprGen scopes (ULess re bs) = do
    left <- relationExprGen scopes re
    right <- bitShiftsGen scopes bs

    evalBinOp left right (\x y -> if x < y then 1 else 0) "l cultl"

relationExprGen scopes (LessEq re bs) = do
    left <- relationExprGen scopes re
    right <- bitShiftsGen scopes bs

    evalBinOp left right (\x y -> if ((fromIntegral x) :: Int) <= ((fromIntegral y) :: Int) then 1 else 0) "l cslel"

relationExprGen scopes (ULessEq re bs) = do
    left <- relationExprGen scopes re
    right <- bitShiftsGen scopes bs

    evalBinOp left right (\x y -> if x <= y then 1 else 0) "l culel"

relationExprGen scopes (Greater re bs) = do
    left <- relationExprGen scopes re
    right <- bitShiftsGen scopes bs

    evalBinOp left right (\x y -> if ((fromIntegral x) :: Int) > ((fromIntegral y) :: Int) then 1 else 0) "l csgtl"

relationExprGen scopes (UGreater re bs) = do
    left <- relationExprGen scopes re
    right <- bitShiftsGen scopes bs

    evalBinOp left right (\x y -> if x > y then 1 else 0) "l cugtl"

relationExprGen scopes (GreaterEq re bs) = do
    left <- relationExprGen scopes re
    right <- bitShiftsGen scopes bs

    evalBinOp left right (\x y -> if ((fromIntegral x) :: Int) >= ((fromIntegral y) :: Int) then 1 else 0) "l csgel"

relationExprGen scopes (UGreaterEq re bs) = do
    left <- relationExprGen scopes re
    right <- bitShiftsGen scopes bs

    evalBinOp left right (\x y -> if x >= y then 1 else 0) "l cugel"

equalityExprGen :: Scopes -> EqualityExpr -> IR ExprRes
equalityExprGen scopes (RelationExpr re) = relationExprGen scopes re
equalityExprGen scopes (Equal eq re) = do
    left <- equalityExprGen scopes eq
    right <- relationExprGen scopes re

    evalBinOp left right (\x y -> if x == y then 1 else 0) "l ceql"

equalityExprGen scopes (NotEqual eq re) = do
    left <- equalityExprGen scopes eq
    right <- relationExprGen scopes re

    evalBinOp left right (\x y -> if x /= y then 1 else 0) "l cnel"

bitAndGen :: Scopes -> BitAnd -> IR ExprRes
bitAndGen scopes (EqualityExpr eq) = equalityExprGen scopes eq
bitAndGen scopes (BitAnd ba eq) = do
    left <- bitAndGen scopes ba
    right <- equalityExprGen scopes eq

    evalBinOp left right (.&.) "l and"

bitXorGen :: Scopes -> BitXor -> IR ExprRes
bitXorGen scopes (BitAnd' ba) = bitAndGen scopes ba
bitXorGen scopes (BitXor bx ba) = do
    left <- bitXorGen scopes bx
    right <- bitAndGen scopes ba

    evalBinOp left right (xor) "l xor"

bitOrGen :: Scopes -> BitOr -> IR ExprRes
bitOrGen scopes (BitXor' bx) = bitXorGen scopes bx
bitOrGen scopes (BitOr bo bx) = do
    left <- bitOrGen scopes bo
    right <- bitXorGen scopes bx

    evalBinOp left right (.|.) "l or"

logicalAndGen :: Scopes -> LogicalAnd -> IR ExprRes
logicalAndGen scopes (BitOr' bo) = bitOrGen scopes bo
logicalAndGen scopes (And la bo) = do
    left <- logicalAndGen scopes la
    right <- bitOrGen scopes bo

    evalBinOp left right (\x y -> if x /= 0 && y /= 0 then 1 else 0) "l and"

logicalOrGen :: Scopes -> LogicalOr -> IR ExprRes
logicalOrGen scopes (LogicalAnd la) = logicalAndGen scopes la
logicalOrGen scopes (Or lo la) = do
    left <- logicalOrGen scopes lo
    right <- logicalAndGen scopes la

    evalBinOp left right (\x y -> if x /= 0 || y /= 0 then 1 else 0) "l or"

ternaryGen :: Scopes -> Ternary -> IR ExprRes
ternaryGen scopes (LogicalOr lo) = logicalOrGen scopes lo
ternaryGen scopes _ = throwError $
    Error (-1,-1) "Ternary operator not implemented!"

exprGen :: Scopes -> Expr -> IR ExprRes
exprGen scopes (Ternary' t) = ternaryGen scopes t
exprGen scopes (Assign t e) = do
    left <- ternaryGen scopes t

    case left of
        Storage (Node addr p) -> do
            right <- exprGen scopes e

            case right of
                Literal (Node num _) -> do
                    addIR $ "storel " ++ show num ++ ", " ++ addr
                    pure $ Literal (Node num p)

                Temporary (Node name _) -> do
                    addIR $ "storel " ++ name ++ ", " ++ addr
                    pure $ Temporary (Node name p)

                Storage (Node addr' _) -> do
                    val <- newReg
                    addIR $ val ++ " =l loadl " ++ addr'

                    addIR $ "storel " ++ val ++ ", " ++ addr

                    pure $ Temporary (Node val p)

        Literal (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"

        Temporary (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"

exprGen scopes (AssignAdd t e) = do
    left <- ternaryGen scopes t

    case left of
        Storage (Node addr p) -> do
            right <- exprGen scopes e
            temp <- evalBinOp left right (+) "l add"

            case temp of
                Temporary (Node name _) -> do
                    addIR $ "storel " ++ name ++ ", " ++ addr
                    pure $ Temporary (Node name p)

                _ -> undefined                                          -- Uncreachable

        Literal (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"

        Temporary (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"

exprGen scopes (AssignSub t e) = do
    left <- ternaryGen scopes t

    case left of
        Storage (Node addr p) -> do
            right <- exprGen scopes e
            temp <- evalBinOp left right (-) "l sub"

            case temp of
                Temporary (Node name _) -> do
                    addIR $ "storel " ++ name ++ ", " ++ addr
                    pure $ Temporary (Node name p)

                _ -> undefined                                          -- Uncreachable

        Literal (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"
        
        Temporary (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"

exprGen scopes (AssignMul t e) = do
    left <- ternaryGen scopes t

    case left of
        Storage (Node addr p) -> do
            right <- exprGen scopes e
            temp <- evalBinOp left right (*) "l mul"

            case temp of
                Temporary (Node name _) -> do
                    addIR $ "storel " ++ name ++ ", " ++ addr
                    pure $ Temporary (Node name p)

                _ -> undefined                                          -- Uncreachable

        Literal (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"
        
        Temporary (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"

exprGen scopes (AssignDiv t e) = do
    left <- ternaryGen scopes t

    case left of
        Storage (Node addr p) -> do
            right <- exprGen scopes e
            temp <- evalBinOp left right (div) "l div"

            case temp of
                Temporary (Node name _) -> do
                    addIR $ "storel " ++ name ++ ", " ++ addr
                    pure $ Temporary (Node name p)

                _ -> undefined                                          -- Uncreachable

        Literal (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"
        
        Temporary (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"

exprGen scopes (AssignMod t e) = do
    left <- ternaryGen scopes t

    case left of
        Storage (Node addr p) -> do
            right <- exprGen scopes e
            temp <- evalBinOp left right (mod) "l mod"

            case temp of
                Temporary (Node name _) -> do
                    addIR $ "storel " ++ name ++ ", " ++ addr
                    pure $ Temporary (Node name p)

                _ -> undefined                                          -- Uncreachable

        Literal (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"
        
        Temporary (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"

exprGen scopes (AssignBitOr t e) = do
    left <- ternaryGen scopes t

    case left of
        Storage (Node addr p) -> do
            right <- exprGen scopes e
            temp <- evalBinOp left right (.|.) "l or"

            case temp of
                Temporary (Node name _) -> do
                    addIR $ "storel " ++ name ++ ", " ++ addr
                    pure $ Temporary (Node name p)

                _ -> undefined                                          -- Uncreachable

        Literal (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"
        
        Temporary (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"

exprGen scopes (AssignBitAnd t e) = do
    left <- ternaryGen scopes t

    case left of
        Storage (Node addr p) -> do
            right <- exprGen scopes e
            temp <- evalBinOp left right (.&.) "l and"

            case temp of
                Temporary (Node name _) -> do
                    addIR $ "storel " ++ name ++ ", " ++ addr
                    pure $ Temporary (Node name p)

                _ -> undefined                                          -- Uncreachable

        Literal (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"
        
        Temporary (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"

exprGen scopes (AssignBitXor t e) = do
    left <- ternaryGen scopes t

    case left of
        Storage (Node addr p) -> do
            right <- exprGen scopes e
            temp <- evalBinOp left right (xor) "l xor"

            case temp of
                Temporary (Node name _) -> do
                    addIR $ "storel " ++ name ++ ", " ++ addr
                    pure $ Temporary (Node name p)

                _ -> undefined                                          -- Uncreachable

        Literal (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"
        
        Temporary (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"

exprGen scopes (AssignBitLShift t e) = do
    left <- ternaryGen scopes t

    case left of
        Storage (Node addr p) -> do
            right <- exprGen scopes e
            temp <- evalBinOp left right (\x y -> x .<<. (fromIntegral y)) "l shl"

            case temp of
                Temporary (Node name _) -> do
                    addIR $ "storel " ++ name ++ ", " ++ addr
                    pure $ Temporary (Node name p)

                _ -> undefined                                          -- Uncreachable

        Literal (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"
        
        Temporary (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"

exprGen scopes (AssignBitRShift t e) = do
    left <- ternaryGen scopes t

    case left of
        Storage (Node addr p) -> do
            right <- exprGen scopes e
            temp <- evalBinOp left right (\x y -> x .>>. (fromIntegral y)) "l shr"

            case temp of
                Temporary (Node name _) -> do
                    addIR $ "storel " ++ name ++ ", " ++ addr
                    pure $ Temporary (Node name p)

                _ -> undefined                                          -- Uncreachable

        Literal (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"
        
        Temporary (Node _ p) -> throwError $
            Error p "Can only assign to lvalues!"

