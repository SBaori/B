module IRGen.ScopeGen 
    (scopeGen) 
where

import IRGen.IR
import Grammar
import IRGen.ExprGen
import IRGen.IRGenUtils
import Control.Monad (unless)
import IRGen.AutoVarDeclarationGen
import IRGen.ExternVarDeclarationGen
import Error
import Control.Monad.Error.Class (MonadError(throwError))
import Data.Bool (bool)

scopeGen :: Scopes -> ScopedUnits -> Maybe ScopeType -> IR Scopes
scopeGen scopes sUnits = 
    maybe (driver scopes sUnits) 
        (\scopeType -> driver (newScope 0 scopeType scopes) sUnits)

    where
       driver :: Scopes -> ScopedUnits -> IR Scopes
       driver scopes ScopedUnitsEnd = pure scopes
       driver scopes (ScopedUnit su s) = do
            scopes' <- scopedUnitGen scopes su
            bool (driver scopes' s) (pure []) (null scopes')

scopedUnitGen :: Scopes -> ScopedUnit -> IR Scopes
scopedUnitGen scopes (ScopedUnitExpr e) = do
    _ <- exprGen scopes e
    pure scopes

scopedUnitGen scopes (ScopedUnitVarDeclr vd) = do
    scopes' <- autoVarDeclrGen scopes vd
    pure scopes'

scopedUnitGen scopes (ScopedUnitExternDeclr ed) = do
    scopes' <- externVarDeclrGen scopes ed
    pure scopes'

scopedUnitGen scopes (ScopedUnitControl c) = do
    controlGen scopes c
    pure scopes 

scopedUnitGen scopes (ScopedUnitReturn (Node _ p) maybeExpr) = do
    unless (isFuncScope scopes) (throwError $ Error p "return used in a non-function context!")

    case maybeExpr of
        Nothing -> addIR $ "ret 0"
        
        Just e -> do
            res <- exprGen scopes e

            case res of
                Literal (Node num _) -> addIR $ "ret " ++ show num

                Storage (Node name _) -> do
                    val <- newReg
                    addIR $ val ++ " =l loadl " ++ name
                    addIR $ "ret " ++ val

                Temporary (Node name _) -> addIR $ "ret " ++ name
    
    pure []

scopedUnitGen scopes (ScopedUnitContinue (Node _ p)) =
    maybe (throwError $ Error p "continue used in a non non-loop context!")
        (\(start, _) -> do
            addIR $ "jmp " ++ start
            temp <- newLabel
            addIR temp

            pure scopes
        ) $ getLoopLabels scopes

scopedUnitGen scopes (ScopedUnitBreak (Node _ p)) =
    maybe (throwError $ Error p "break used in a non non-loop context!")
        (\(_, exit) -> do
            addIR $ "jmp " ++ exit
            temp <- newLabel
            addIR temp

            pure scopes
        ) $ getLoopLabels scopes

controlGen :: Scopes -> Control -> IR ()
controlGen sfs (ControlIf ifelse) = ifElseGen sfs ifelse
controlGen sfs (ControlLoop l) = loopGen sfs l

ifGen :: Scopes -> String -> If -> IR ()
ifGen sfs exitLabel (IfUnScoped e su) = do
    res <- exprGen sfs e
    begin <- newLabel
    end <- newLabel
    bypass <- newLabel

    case res of
        Literal (Node num _) ->
            unless (num == 0) $ do
                _ <- scopedUnitGen sfs su
                pure ()

        Storage (Node name _) -> do
            val <- newReg
            addIR $ val ++ " =l loadl " ++ name

            addIR $ "jnz " ++ val ++ ", " ++ begin ++ ", " ++ bypass
            addIR begin

            _ <- scopedUnitGen sfs su

            addIR end
            addIR $ "jmp " ++ exitLabel
            addIR bypass

        Temporary (Node name _) -> do
            addIR $ "jnz " ++ name ++ ", " ++ begin ++ ", " ++ bypass
            addIR begin

            _ <- scopedUnitGen sfs su

            addIR end
            addIR $ "jmp " ++ exitLabel
            addIR bypass

ifGen sfs exitLabel (IfScoped e s) = do
    res <- exprGen sfs e
    begin <- newLabel
    end <- newLabel
    bypass <- newLabel

    case res of
        Literal (Node num _) -> do
            unless (num == 0) $ do
                _ <- scopeGen sfs s (Just Branch)
                pure ()

        Storage (Node name _) -> do
            val <- newReg
            addIR $ val ++ " =l loadl " ++ name

            addIR $ "jnz " ++ val ++ ", " ++ begin ++ ", " ++ bypass
            addIR begin

            _ <- scopeGen sfs s (Just Branch)

            addIR end
            addIR $ "jmp " ++ exitLabel
            addIR bypass
        
        Temporary (Node name _) -> do
            addIR $ "jnz " ++ name ++ ", " ++ begin ++ ", " ++ bypass
            addIR begin

            _ <- scopeGen sfs s (Just Branch)

            addIR end
            addIR $ "jmp " ++ exitLabel
            addIR bypass

ifElseGen :: Scopes -> IfElse -> IR ()
ifElseGen sfs (IfElse if' elif) = do
    exitLabel <- newLabel
    ifGen sfs exitLabel if'
    elseIfGen sfs exitLabel elif

elseIfGen :: Scopes -> String -> ElseIf -> IR ()
elseIfGen _ exitLabel Fi = addIR exitLabel

elseIfGen sfs exitLabel (ElseUnScoped su) = do
    _ <- scopedUnitGen sfs su
    addIR exitLabel

elseIfGen sfs exitLabel (ElseScoped s) = do
    _ <- scopeGen sfs s (Just Branch)
    addIR exitLabel

elseIfGen sfs exitLabel (ElseIf if' elif) = do
    ifGen sfs exitLabel if'
    elseIfGen sfs exitLabel elif

loopGen :: Scopes -> Loop -> IR ()
loopGen sfs (WhileUnscoped e su) = do
    labels <- loopInitGen sfs e

    maybe (pure ())
        (\(start, exit) -> do
            _ <- scopedUnitGen sfs su

            addIR $ "jmp " ++ start
            addIR exit
        ) labels

loopGen sfs (WhileScoped e s) = do
    labels <- loopInitGen sfs e

    maybe (pure ())
        (\(start, exit) -> do
            _ <- scopeGen sfs s (Just (Loop start exit))

            addIR $ "jmp " ++ start
            addIR exit       
        ) labels

loopInitGen :: Scopes -> Expr -> IR (Maybe (String, String)) 
loopInitGen sfs e = do
    start <- newLabel

    addIR start

    body <- newLabel
    exit <- newLabel

    res <- exprGen sfs e

    case res of
        Literal (Node num _) ->
            if num == 0 then
                pure Nothing
            else
                pure $ Just (start, exit)

        Storage (Node name _) -> do
            val <- newReg
            addIR $ val ++ " =l loadl " ++ name

            addIR $ "jnz " ++ val ++ ", " ++ body ++ ", " ++ exit
            addIR body

            pure $ Just (start, exit)

        Temporary (Node name _) -> do
            addIR $ "jnz " ++ name ++ ", " ++ body ++ ", " ++ exit
            addIR body

            pure $ Just (start, exit)