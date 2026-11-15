module IRGen.IRGenUtils where

import IRGen.IR
import Foreign.C (CUChar)
import Error
import Grammar
import Data.Bits (FiniteBits(finiteBitSize))
import Control.Monad.State (modify, get)
import qualified Data.Map.Strict as M
import Control.Monad.Error.Class (MonadError(throwError))

wordSize :: Int
wordSize = finiteBitSize (0 :: Word) `div` finiteBitSize (0 :: CUChar)

addIR :: String -> IR ()
addIR inst = modify $ (\(SemState ir irb gv regNum lNum uc) 
                        -> SemState (inst:ir) irb gv regNum lNum uc)

addIRBuffer :: String -> IR ()
addIRBuffer inst = modify $ (\(SemState ir irb gv regNum lNum uc) 
                        -> SemState ir (inst:irb) gv regNum lNum uc)

newReg :: IR String
newReg = do
    modify $ (\(SemState ir irb gvs regnum lNum uc) 
                -> SemState ir irb gvs (regnum+1) lNum uc)
    (("%t" ++) . show . regNum) <$> get

newUniqCounter :: IR String
newUniqCounter = do
    modify $ (\(SemState ir irb gvs regNum lNum uc)
                    -> SemState ir irb gvs regNum lNum (uc+1))
    
    show <$> uniqCounter <$> get

newLabel :: IR String
newLabel = do
    modify $ (\(SemState ir irb gvs regNum lNum uc) 
                -> SemState ir irb gvs regNum (lNum + 1) uc)
    (("@label" ++) . show . labelNum) <$> get

getLoopLabels :: Scopes -> Maybe (String, String)
getLoopLabels [] = Nothing
getLoopLabels (scope:scopes) = do
    case scopeType scope of
        Loop start exit -> Just (start, exit)
        _ -> getLoopLabels scopes

isFuncScope :: Scopes -> Bool
isFuncScope [] = False
isFuncScope (scope:scopes) =
    case scopeType scope of
        Func -> True
        _ -> isFuncScope scopes

newScope :: Int -> ScopeType -> Scopes -> Scopes
newScope base stype [] = [Scope stype base M.empty M.empty]
newScope _ stype scopes@(sc:_) = (Scope stype (sDepth + 1) M.empty M.empty):scopes
    where
        sDepth = scopeDepth sc

getGlobalSymbol :: Node String -> Node String
getGlobalSymbol (Node n p) = Node ("$" ++ n) p

getMangledAutoSymbol :: Int -> Node String -> Node String
getMangledAutoSymbol sDepth (Node n p) = Node ("%_" ++ show sDepth ++ "_" ++ n) p

getScopeVariable :: Scopes -> (Node String) -> Either Error ExprRes
getScopeVariable [] (Node n p) = throwError $ Error {messg="Use of undeclared variable " ++ show n, posError=p}
getScopeVariable (scope:scopes) node@(Node n _)
    | M.member n as = pure $ Storage $ getMangledAutoSymbol sDepth node
    | M.member n es = pure $ Storage $ getGlobalSymbol node
    | otherwise = getScopeVariable scopes node
    where
        sDepth = scopeDepth scope
        as = autoSymbols scope
        es = externSymbols scope

addScopeVariable :: Scope -> (Node String) -> Bool -> IR (Node String, Scope)
addScopeVariable scope node@(Node name p) False
    | M.member name as =
        throwError $ Error p ("Redeclaration of " ++ show name ++ "; previously defined at: " ++ show (as M.! name))

    | M.member name es =
        throwError $ Error p ("Redeclaration of " ++ show name ++ "; previously defined at: " ++ show (es M.! name))

    | otherwise = pure (getGlobalSymbol node, scope')
    where
        Scope st sd as es = scope
        scope' = Scope st sd as (M.insert name p es)

addScopeVariable scope node@(Node name p) True
    | M.member name as =
        throwError $ Error p ("Redeclaration of " ++ show name ++ "; previously defined at: " ++ show (as M.! name))

    | M.member name es =
        throwError $ Error p ("Redeclaration of " ++ show name ++ "; previously defined at: " ++ show (es M.! name))
    
    | otherwise = do
        let mnode@(Node mName _) = getMangledAutoSymbol sd node
        addIR $ mName ++ " =l alloc" ++ show wordSize ++ " " ++ show wordSize
        pure $ (mnode, scope')

    where
        Scope st sd as es = scope
        scope' = Scope st sd (M.insert name p as) es

addGlobalVariable :: (Node String) -> IR (Node String)
addGlobalVariable node@(Node name p) = do

    modify $ (\(SemState ir irb gs regNum lNum uc) -> 
                SemState ir irb (M.insert name p gs) regNum lNum uc)

    pure $ getGlobalSymbol node

evalBinOp :: ExprRes -> ExprRes -> (Word -> Word -> Word) -> String -> IR ExprRes
evalBinOp (Literal (Node l p)) (Literal (Node r _)) op _  = pure $ Literal (Node (op l r) p)

evalBinOp (Literal (Node l p)) (Temporary (Node r _)) _ opIr  = do
    reg <- newReg
    addIR $ reg ++ " =" ++ opIr ++ " " ++ show l ++ ", " ++ r

    pure $ Temporary (Node reg p)

evalBinOp (Literal (Node l p)) (Storage (Node r _)) _ opIr = do
    val <- newReg
    addIR $ val ++ " =l loadl " ++ r

    reg <- newReg
    addIR $ reg ++ " =" ++ opIr ++ " " ++ show l ++ ", " ++ val
    pure $ Temporary (Node reg p)

evalBinOp (Temporary (Node l p)) (Literal (Node r _)) _ opIr = do
    reg <- newReg
    addIR $ reg ++ " =" ++ opIr ++ " " ++ l ++ ", " ++ show r
    
    pure $ Temporary (Node reg p)

evalBinOp (Temporary (Node l p)) (Temporary (Node r _)) _ opIr = do
    reg <- newReg
    addIR $ reg ++ " =" ++ opIr ++ " " ++ l ++ ", " ++  r

    pure $ Temporary (Node reg p)

evalBinOp (Temporary (Node l p)) (Storage (Node r _)) _ opIr = do
    val <- newReg
    addIR $ val ++ " =l loadl " ++ r

    reg <- newReg
    addIR $ reg ++ " =" ++ opIr ++ " " ++ l ++ ", " ++ val

    pure $ Temporary (Node reg p)

evalBinOp (Storage (Node l pl)) lit@(Literal _) op opIr = do
    val <- newReg
    addIR $ val ++ " =l loadl " ++ l

    evalBinOp (Temporary (Node val pl)) lit op opIr

evalBinOp (Storage (Node l pl)) temp@(Temporary _) op opIr = do
    val <- newReg
    addIR $ val ++ " =l loadl " ++ l

    evalBinOp (Temporary (Node val pl)) temp op opIr

evalBinOp (Storage (Node l pl)) (Storage (Node r pr)) op opIr = do
    vall <- newReg
    addIR $ vall ++ " =l loadl " ++ l

    valr <- newReg
    addIR $ valr ++ " =l loadl " ++ r

    evalBinOp (Temporary (Node vall pl)) (Temporary (Node valr pr)) op opIr