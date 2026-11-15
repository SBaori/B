module Parser.Parser where

import Error
import Lexer.Token
import Control.Monad.State (StateT(..))
import qualified Data.Map.Strict as M
import Control.Monad.Error.Class (MonadError(throwError))

-- TODO: Create a Parser class.
type Parser = StateT [Token] (Either Error)

runParser :: Parser a -> [Token] -> Either Error (a,[Token])
runParser = runStateT

expectKind :: TokenKind -> Parser ()
expectKind tk = StateT f
    where
        tokenKindStr = tokenKindToString M.! tk

        f [] = throwError $ Error {messg = "expected \"" ++ tokenKindStr ++ "\" but ran out of input!", posError = (-1,-1)}
        f (t:ts)
            | tk == kind t = pure ((), ts)
            | otherwise = throwError $ Error {messg = "expected \"" ++ tokenKindStr ++ "\" but got \"" ++ lexeme t ++ "\"", posError = posToken t}

peek :: Int -> Parser Token
peek p = StateT f
    where
        f [] = throwError $ Error {messg = "Ran out of input while peeking(" ++ show p ++ ")!", posError = (-1,-1)}
        f tokens = pure (tokens !! p, tokens)

