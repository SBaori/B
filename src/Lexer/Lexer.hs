module Lexer.Lexer
    (lexer, runLexer, LexState(..))
where

import Data.Char (isDigit, isAlphaNum)
import qualified Data.Map.Strict as M
import Control.Monad.State (StateT (..), get, modify)
import Control.Monad.Error.Class (MonadError(throwError))
import Error
import Data.Char (ord, chr, isHexDigit)
import Control.Monad (when)
import Lexer.Token

data LexState = LexState {
    input :: String,
    currPos :: (Int,Int)
}
    deriving (Show)

type Lexer = StateT LexState (Either Error)

runLexer :: Lexer [Token] -> LexState -> Either Error ([Token], LexState)
runLexer = runStateT

consume :: Int -> Lexer ()
consume 0 = pure ()
consume num = do
    LexState chars cPos <- get

    case chars of
        [] -> throwError (Error cPos "Ran out of characters when trying to consume")

        (c:cs) -> do 
            let nPos = newPos cPos c

            modify (\_ -> LexState cs nPos)
            consume (num-1)

    where
        newPos (l', _) '\n' = (l'+1, 1)
        newPos (l',c') _ = (l', c'+1)

expectChar :: Char -> Lexer ()
expectChar c = do
    LexState chars cPos <- get

    case chars of
        [] -> throwError (Error cPos ("Ran out of characters when expecting " ++ [c]))

        (curr:_) -> do
            when (c /= curr)
                (throwError $ Error cPos ("Expected " ++ [c] ++ ", but got " ++ [curr]))

            consume 1

lexer :: Lexer [Token]
lexer = do
    LexState chars cPos <- get

    case chars of
        [] -> pure [Token (tokenKindToString M.! EOF) EOF cPos]

        (' ':_) -> consume 1 >> lexer

        ('\n':_) -> consume 1 >> lexer

        ('\t':_) -> consume 1 >> lexer

        ('\'':_) -> do
            token <- char
            (token:) <$> lexer
        
        ('\"':_) -> do
            token <- string
            (token:) <$> lexer

        ('+':'=':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer

        ('+':'+':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer

        ('+':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer
        
        ('-':'=':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer
        
        ('-':'-':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer
        
        ('-':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer
        
        ('*':'=':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer
        
        ('*':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer
        
        ('/':'/':_) -> do
            consume 2
            untilEndLine
            lexer

        ('/':'*':_) -> do
            startPos <- currPos <$> get
            consume 2
            untilEndComment startPos
            consume 2
            lexer

        ('/':'=':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer
        
        ('/':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        ('%':'=':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer
        
        ('%':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        ('<':'<':'=':_) -> do
            token <- getTokenAndConsume 3
            (token:) <$> lexer

        ('<':'<':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer

        ('<':'=':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer

        ('<':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer
        
        ('u':'<':'=':_) -> do
            token <- getTokenAndConsume 3
            (token:) <$> lexer

        ('u':'<':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer

        ('>':'>':'=':_) -> do
            token <- getTokenAndConsume 3
            (token:) <$> lexer

        ('>':'>':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer

        ('>':'=':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer

        ('>':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        ('u':'>':'=':_) -> do
            token <- getTokenAndConsume 3
            (token:) <$> lexer

        ('u':'>':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer

        ('!':'=':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer

        ('!':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        ('(':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        (')':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        ('{':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        ('}':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        ('[':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        (']':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        ('=':'=':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer

        ('=':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        ('&':'&':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer
        
        ('&':'=':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer

        ('&':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        ('|':'|':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer

        ('|':'=':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer

        ('|':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        ('^':'=':_) -> do
            token <- getTokenAndConsume 2
            (token:) <$> lexer

        ('^':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        (',':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        (';':_) -> do
            token <- getTokenAndConsume 1
            (token:) <$> lexer

        (c:_) -> do
            token <- getDecimalNumber `orElse` getIdentifier 
                    `orElse` (throwError $ Error cPos ("Unknown Character: " ++ [c]))

            (token:) <$> lexer

    where
        getTokenAndConsume :: Int -> Lexer (Token) 
        getTokenAndConsume lexLen = do
            LexState chars cPos <- get

            let lex' = take lexLen chars

            when (M.notMember lex' stringToTokenKind)
                (throwError $ Error cPos ("TokenKind not found in map for " ++ lex' ++ "!"))

            consume lexLen
            pure $ Token lex' (stringToTokenKind M.! lex') cPos

        untilEndLine = do
            LexState chars _ <- get

            case chars of
                [] -> pure ()
                ('\n':_) -> pure ()
                _ -> do
                    consume 1
                    untilEndLine

        untilEndComment startPos = do
            LexState chars _ <- get

            case chars of
                [] -> throwError $ Error startPos "Missing comment end */!"
                ('*':'/':_) -> pure ()
                _ -> do
                    consume 1
                    untilEndComment startPos

        getCharacter = do
            LexState chars cPos <- get

            case chars of
                ('\\':'n':_)      -> do
                    consume 2
                    pure $ Token (show $ ord '\n') NUM cPos

                ('\\':'t':_)      -> do 
                    consume 2
                    pure $ Token (show $ ord '\t') NUM cPos

                ('\\':'r':_)      -> do 
                    consume 2
                    pure $ Token (show $ ord '\r') NUM cPos

                ('\\':'b':_)      -> do 
                    consume 2
                    pure $ Token (show $ ord '\b') NUM cPos

                ('\\':'f':_)      -> do 
                    consume 2
                    pure $ Token (show $ ord '\f') NUM cPos

                ('\\':'v':_)      -> do 
                    consume 2
                    pure $ Token (show $ ord '\v') NUM cPos

                ('\\':'a':_)      -> do 
                    consume 2
                    pure $ Token (show $ ord '\a') NUM cPos

                ('\\':'\\':_)     -> do 
                    consume 2
                    pure $ Token (show $ ord '\\') NUM cPos

                ('\\':'\"':_)     -> do 
                    consume 2
                    pure $ Token (show $ ord '\"') NUM cPos

                ('\\':'0':_)      -> do 
                    consume 2
                    pure $ Token (show $ ord '\0') NUM cPos

                ('\\':'\'':_)     -> do 
                    consume 2
                    pure $ Token (show $ ord '\'') NUM cPos

                ('\\':_) -> throwError $ Error cPos ("Unknown escape character " ++ take 2 chars)

                (c:_) -> do
                    consume 1
                    pure $ Token (show $ ord c) NUM cPos 

                _ -> throwError $ Error cPos "Reached end of file while trying to read a character!"

        char = do
            LexState _ cPos <- get
            expectChar '\''
            LexState chars _ <- get

            case chars of
                ('\'':_) -> throwError $ Error cPos "Empty character is not allowed!"
                ('\n':_) -> throwError $ Error cPos "Missing a closing '"
                _ -> do
                    token <- getCharacter
                    expectChar '\''
                    pure token
        
        string = do
            LexState _ cPos <- get
            expectChar '\"'
            str <- helper cPos
            expectChar '\"'

            pure $ Token str STRING cPos

            where
                helper start = do
                    LexState str _ <- get

                    case str of
                        ('\n':_) -> throwError $ Error start "Missing closing \""
                        ('\"':_) -> pure ""

                        (c:_) -> do
                            Token n _ _ <- getCharacter
                            ((chr $ read n):) <$> helper start

                        [] -> throwError $ Error start "Reached end of file while trying to read a string!"


        getIdentifier = do
            LexState chars cPos <- get

            let identifier = takeWhile valid chars

            when (null identifier)
                (throwError $ Error cPos "Not an identifier!") 

            consume (length identifier)
            pure $ Token identifier (M.findWithDefault IDENTIFIER identifier stringToTokenKind) cPos

            where
                valid i = isAlphaNum i || (i == '_')

        getDecimalNumber = do
            LexState chars cPos <- get

            let num = takeWhile isDigit chars

            when (null num)
                (throwError $ Error cPos "Not a number!")

            consume (length num)
            pure $ Token (takeWhile isDigit chars) NUM cPos
