module IRGen.GlobalVarDeclarationGen where

import IRGen.IR
import Grammar
import Control.Monad.State
import Control.Monad (when)
import qualified Data.Map.Strict as M
import Error
import Control.Monad.Error.Class (MonadError(throwError))
import IRGen.IRGenUtils
import IRGen.ExprGen

globalVarDeclrGen :: GlobalVarDeclrs -> IR ()
globalVarDeclrGen (GlobalVarDeclrs node@(Node name pos) e) = do
    globalScope <- globalSymbols <$> get

    when(M.member name globalScope)
        (throwError $ Error pos ("Redefinition of global symbol: " ++ name ++ ". First defined at: " ++ show (globalScope M.! name)))

    let scope = Scope Anon 0 globalScope M.empty

    res <- exprGen [scope] e

    case res of
        Literal (Node num _) -> do
            Node gv _ <- addGlobalVariable node
            addIRBuffer $ "export data " ++ gv ++ " = { l " ++ show num ++ " }"

        _ -> throwError $
            Error pos "Global variables can only be assigned with literals for now!"

globalVarDeclrGen (GlobalVarDeclrsArr node@(Node name pos) e initList) = do
    globalScope <- globalSymbols <$> get

    when(M.member name globalScope)
        (throwError $ Error pos ("Redefinition of global symbol: " ++ name ++ ". First defined at: " ++ show (globalScope M.! name)))

    let scope = Scope Anon 0 globalScope M.empty

    size <- exprGen [scope] e

    adjustedsize <- evalBinOp size (Literal (Node (1*(fromIntegral wordSize)) (-1,-1))) (*) "l mul"

    case adjustedsize of
        Literal (Node num _) -> do
            Node gv _ <- addGlobalVariable node

            when ((fromIntegral num) < (fromIntegral wordSize)*length initList) 
                (throwError $ Error pos "Initializer length is greater than array length!")

            uc <- newUniqCounter

            addIRBuffer $ "export data " ++ gv ++ " = { l $.arr" ++ uc ++ " }"

            addIRBuffer $ "data $.arr" ++ uc ++ " = { "
            initialize num initList
            addIRBuffer $ "}"

        _ -> throwError $ Error pos "Global arrays are not allowed to have variable length!"

    where
        initialize len [] = addIRBuffer $ "z " ++ show len
        initialize len (e:es) = do
            globalScope <- globalSymbols <$> get

            let scope = Scope Anon 0 globalScope M.empty
            res <- exprGen [scope] e

            case res of
                Literal (Node num _) -> do
                    addIRBuffer $ "l " ++ show num ++ ","
                    initialize (len - (fromIntegral wordSize)) es
                
                _ -> throwError $ Error pos "Global arrays can only be initialized with literals for now!"

