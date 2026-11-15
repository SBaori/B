module IRGen.ExternVarDeclarationGen where

import IRGen.IR
import Grammar
import IRGen.IRGenUtils
import Error
import Control.Monad.Error.Class (MonadError(throwError))

externVarDeclrGen :: Scopes -> ExternVarDeclrs -> IR Scopes
externVarDeclrGen scopes (ExternVarDeclrsEnd node@(Node _ p)) = do
    case scopes of
        [] -> throwError $ Error p "Cannot declare external variable in empty scope!"

        (sc:scs) -> do
            (_, sc') <- addScopeVariable sc node False
            pure (sc':scs)

externVarDeclrGen scopes (ExternVarDeclrs node@(Node _ p) evd) = do
    case scopes of
        [] -> throwError $ Error p "Cannot declare external variable in empty scope!"

        (sc:scs) -> do
            (_, sc') <- addScopeVariable sc node False
            externVarDeclrGen (sc':scs) evd