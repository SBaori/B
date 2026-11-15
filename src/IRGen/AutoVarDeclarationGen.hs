module IRGen.AutoVarDeclarationGen 
    (autoVarDeclrGen)
where

import IRGen.IR
import Grammar
import Error
import Control.Monad.Error.Class (MonadError(throwError))
import IRGen.IRGenUtils
import IRGen.ExprGen
import Control.Monad (when)

autoVarDeclrGen :: Scopes -> ScopedVarDeclrs -> IR Scopes
autoVarDeclrGen scopes (ScopedVarDeclrsEnd node@(Node _ p) maybeExpr) = do
    case scopes of
        [] -> throwError $ Error p "Cannot declare variable in an empty scope!"

        (sc:scs) -> do
            (Node mangledName _, sc') <- addScopeVariable sc node True

            case maybeExpr of
                Nothing -> pure (sc':scs)

                Just e -> do
                    let scopes' = sc':scs

                    res <- exprGen scopes' e

                    case res of
                        Literal (Node num _) -> do
                            addIR $ "storel " ++ show num ++ ", " ++ mangledName
                            pure scopes'

                        Storage (Node name _) -> do
                            val <- newReg
                            addIR $ val ++ " =l loadl " ++ name

                            addIR $ "storel " ++ val ++ ", " ++ mangledName
                            pure scopes'

                        Temporary (Node name _) -> do
                            addIR $ "storel " ++ name ++ ", " ++ mangledName
                            pure scopes'

autoVarDeclrGen scopes (ScopedVarDeclrs node@(Node _ p) maybeExpr declrs) = do
    case scopes of
        [] -> throwError $ Error p "Cannot declare variable in an empty scope!"

        (sc:scs) -> do
            (Node mangledName _, sc') <- addScopeVariable sc node True

            case maybeExpr of
                Nothing -> autoVarDeclrGen (sc':scs) declrs

                Just e -> do
                    let scopes' = sc':scs

                    res <- exprGen scopes' e

                    case res of
                        Literal (Node num _) -> do
                            addIR $ "storel " ++ show num ++ ", " ++ mangledName
                            autoVarDeclrGen scopes' declrs

                        Storage (Node name _) -> do
                            val <- newReg
                            addIR $ val ++ " =l loadl " ++ name

                            addIR $ "storel " ++ val ++ ", " ++ mangledName
                            autoVarDeclrGen scopes' declrs

                        Temporary (Node name _) -> do
                            addIR $ "storel " ++ name ++ ", " ++ mangledName
                            autoVarDeclrGen scopes' declrs

autoVarDeclrGen scopes (ScopedVarDeclrsArrEnd node@(Node _ p) e maybeInitList) = do
    case scopes of
        [] -> throwError $ Error p "Cannot declare variable in an empty scope!"

        (sc:scs) -> do
            res <- exprGen scopes e
            size <- evalBinOp res (Literal (Node (1*(fromIntegral wordSize)) (-1,-1))) (*) "l mul"

            reg <- newReg 

            case size of
                Literal (Node num _) -> addIR $ reg ++ " =l alloc" ++ show wordSize ++ " " ++ show num
                Temporary (Node name _) -> addIR $ reg ++ " =l alloc" ++ show wordSize ++ " " ++ name
                Storage (Node name _) -> do
                    val <- newReg
                    addIR $ val ++ " =l loadl " ++ name
                    addIR $ reg ++ " =l alloc" ++ show wordSize ++ " " ++ val

            (mnode@(Node mangledVarName _), sc') <- addScopeVariable sc node True
            addIR $ "storel " ++ reg ++ ", " ++ mangledVarName

            case maybeInitList of
                Nothing -> pure (sc':scs)

                Just initList -> do
                    case res of
                        Literal (Node num _) -> do
                            when ((fromIntegral num) < length initList) 
                                (throwError $ Error p "Initializer length is greater than array length!")

                            initAutoArr scopes (Storage mnode) num initList
                            pure (sc':scs)

                        _ -> throwError $ Error p "Array initalization is only allowed for static arrays!"

autoVarDeclrGen scopes (ScopedVarDeclrsArr node@(Node _ p) e maybeInitList declrs) = do
    case scopes of
        [] -> throwError $ Error p "Cannot declare variable in an empty scope!"

        (sc:scs) -> do
            res <- exprGen scopes e
            size <- evalBinOp res (Literal (Node (1*(fromIntegral wordSize)) (-1,-1))) (*) "l mul"

            reg <- newReg 

            case size of
                Literal (Node num _) -> addIR $ reg ++ " =l alloc" ++ show wordSize ++ " " ++ show num
                Temporary (Node name _) -> addIR $ reg ++ " =l alloc" ++ show wordSize ++ " " ++ name
                Storage (Node name _) -> do
                    val <- newReg
                    addIR $ val ++ " =l loadl " ++ name
                    addIR $ reg ++ " =l alloc" ++ show wordSize ++ " " ++ val

            (mnode@(Node mangledVarName _), sc') <- addScopeVariable sc node True
            addIR $ "storel " ++ reg ++ ", " ++ mangledVarName

            case maybeInitList of
                Nothing -> pure (sc':scs)

                Just initList -> do
                    case res of
                        Literal (Node num _) -> do
                            when ((fromIntegral num) < length initList) 
                                (throwError $ Error p "Initializer length is greater than array length!")

                            initAutoArr scopes (Temporary mnode) num initList
                            autoVarDeclrGen (sc':scs) declrs

                        _ -> throwError $ Error p "Array initalization is only allowed for static arrays!"

initAutoArr :: (Num t, Eq t) => Scopes -> ExprRes -> t -> [Expr] -> IR ()
initAutoArr _ _ 0 _ = pure ()
initAutoArr scopes basePtr@(Storage (Node name _)) len list =
    case list of
        [] -> do
            baseVal <- newReg
            addIR $ baseVal ++ " =l loadl " ++ name
            addIR $ "storel 0, " ++ baseVal
            baseOffset <- evalBinOp basePtr (Literal (Node (fromIntegral wordSize) (-1,-1))) (+) "l add"

            initAutoArr scopes baseOffset (len-1) []

        (e:es) -> do
            res <- exprGen scopes e
            baseVal <- newReg
            addIR $ baseVal ++ " =l loadl " ++ name

            case res of
                Literal (Node num _) -> do
                    addIR $ "storel " ++ show num ++ ", " ++ baseVal
                
                Temporary (Node tempName _) -> do
                    addIR $ "storel " ++ tempName ++ ", " ++ baseVal 
                
                Storage (Node strName _) -> do
                    val <- newReg
                    addIR $ val ++ " =l loadl " ++ strName

                    addIR $ "storel " ++ val ++ ", " ++ baseVal

            baseOffset <- evalBinOp basePtr (Literal (Node (fromIntegral wordSize) (-1,-1))) (+) "l add"
            initAutoArr scopes baseOffset (len-1) es

initAutoArr scopes basePtr@(Literal (Node num _)) len list =
    case list of
        [] -> do
            addIR $ "storel 0, " ++ show num
            baseOffset <- evalBinOp basePtr (Literal (Node (fromIntegral wordSize) (-1,-1))) (+) "l add"
            initAutoArr scopes baseOffset (len-1) []

        (e:es) -> do
            res <- exprGen scopes e

            case res of
                Literal (Node num' _) -> do
                    addIR $ "storel " ++ (show num) ++ ", " ++ show num'

                Temporary (Node tempName _) -> do
                    addIR $ "storel " ++ (show num) ++ ", " ++ tempName

                Storage (Node strName _) -> do
                    val <- newReg
                    addIR $ val ++ " =l loadl " ++ strName

                    addIR $ "storel " ++ (show num) ++ ", " ++ val

            baseOffset <- evalBinOp basePtr (Literal (Node (fromIntegral wordSize) (-1,-1))) (+) "l add"
            initAutoArr scopes baseOffset (len-1) es

initAutoArr scopes basePtr@(Temporary (Node name _)) len list =
    case list of
        [] -> do
            addIR $ "storel 0, " ++ name
            baseOffset <- evalBinOp basePtr (Literal (Node (fromIntegral wordSize) (-1,-1))) (+) "l add"
            initAutoArr scopes baseOffset (len-1) []

        (e:es) -> do
            res <- exprGen scopes e

            case res of
                Literal (Node num _) -> do
                    addIR $ "storel " ++ show num ++ ", " ++ name
        
                Temporary (Node tempName _) -> do
                    addIR $ "storel " ++ tempName ++ ", " ++ name
        
                Storage (Node strName _) -> do
                    val <- newReg
                    addIR $ val ++ " =l loadl " ++ strName

                    addIR $ "storel " ++ val ++ ", " ++ name

            baseOffset <- evalBinOp basePtr (Literal (Node (fromIntegral wordSize) (-1,-1))) (+) "l add"
            initAutoArr scopes baseOffset (len-1) es

