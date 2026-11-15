module IRGen.FunctionGen where

import IRGen.IR
import Grammar
import Control.Monad.State
import Control.Monad (when, unless)
import qualified Data.Map.Strict as M
import Error
import Control.Monad.Error.Class (MonadError(throwError))
import Data.List (intercalate)
import IRGen.IRGenUtils
import IRGen.ScopeGen

functionDfnGen :: FuncDfn -> IR ()
functionDfnGen (FuncDfn node@(Node name pos) params sUnits) = do
    globalScope <- globalSymbols <$> get

    when (M.member name globalScope)
        (throwError $ Error pos ("Redefinition of global symbol: " ++ name ++ ". First defined at: " ++ show (globalScope M.! name)))

    let paramsIR = intercalate "," $ map (\(Node n _) -> "l %" ++ n) params

    Node mangledName _ <- addGlobalVariable node

    modify (\(SemState ir irb gs _ _ uc) -> SemState ir irb gs 0 0 uc)

    addIR $ "export function l " ++ mangledName ++ "(" ++ paramsIR ++ ") {"
    addIR "@start"

    scopes <- allocate (newScope 0 Func []) params

    fnScope <- scopeGen scopes sUnits Nothing

    unless (null fnScope) (addIR "ret 0")
    addIR "}"

    where
        allocate [] _ = throwError $ Error pos "Cannot initialise function scope!"
        allocate scopes [] = pure scopes
        allocate (scope:scopes) (node@(Node n _):nodes) = do
            (Node mangledName _, scope') <- addScopeVariable scope node True
            addIR $ "storel %" ++ n ++ ", " ++ mangledName
            allocate (scope':scopes) nodes

