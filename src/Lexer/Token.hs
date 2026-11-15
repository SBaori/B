module Lexer.Token where

import qualified Data.Map as M

data Token = Token {
    lexeme :: String,
    kind :: TokenKind,
    posToken :: (Int, Int)
}

data TokenKind =
        NUM             -- \d+
    |   PLUS            -- +
    |   MINUS           -- -
    |   MUL             -- *
    |   DIV             -- /
    |   MOD             -- %
    |   PLUS_PLUS       -- ++
    |   MINUS_MINUS     -- --
    |   LESS            -- <
    |   ULESS           -- u<
    |   LESS_EQUAL      -- <=
    |   ULESS_EQUAL     -- u<=
    |   BIT_LEFT_SHIFT  -- <<
    |   GREATER         -- >
    |   UGREATER        -- u>
    |   GREATER_EQUAL   -- >=
    |   UGREATER_EQUAL  -- u>=
    |   BIT_RIGHT_SHIFT -- >>
    |   EQUALITY        -- ==
    |   NOT_EQUALITY    -- !=
    |   PLUS_ASSIGN     -- +=
    |   MINUS_ASSIGN    -- -=
    |   MUL_ASSIGN      -- *=
    |   DIV_ASSIGN      -- /=
    |   BITAND_ASSIGN   -- &=
    |   BITOR_ASSIGN    -- |=
    |   BITXOR_ASSIGN   -- ^=
    |   MOD_ASSIGN      -- %=
    |   BLS_ASSIGN      -- <<=
    |   BRS_ASSIGN      -- >>=
    |   NEGATION        -- !
    |   OPEN_ROUND      -- (
    |   CLOSE_ROUND     -- )
    |   OPEN_CURLY      -- {
    |   CLOSE_CURLY     -- }
    |   OPEN_SQUARE     -- [
    |   CLOSE_SQUARE    -- ]
    |   ANP             -- &
    |   AND             -- &&
    |   OR              -- ||
    |   BAR             -- |
    |   XOR             -- ^
    |   EOF             -- End of File
    |   IDENTIFIER      -- \w[\d_\w]*
    |   EQUALS          -- =
    |   VAR_KEYWORD     -- auto
    |   IF              -- if
    |   ELSE            -- else
    |   WHILE           -- while
    |   EXTERN          -- extrn
    |   RETURN          -- return
    |   CONTINUE        -- continue
    |   BREAK           -- break
    |   STRING          -- ".*"
    |   UNIT_END        -- ;       
    |   COMMA           -- ,
        deriving (Ord, Eq)

tokenKindToString :: M.Map TokenKind String
tokenKindToString = M.fromList
    [ (NUM, "number")
    , (PLUS, "+")
    , (MINUS, "-")
    , (MUL, "*")
    , (DIV, "/")
    , (MOD, "%")
    , (PLUS_PLUS, "++")
    , (MINUS_MINUS, "--")
    , (LESS, "<")
    , (ULESS, "u<")
    , (LESS_EQUAL, "<=")
    , (ULESS_EQUAL, "u<=")
    , (BIT_LEFT_SHIFT, "<<")
    , (GREATER, ">")
    , (UGREATER, "u>")
    , (GREATER_EQUAL, ">=")
    , (UGREATER_EQUAL, "u>=")
    , (BIT_RIGHT_SHIFT, ">>")
    , (EQUALITY, "==")
    , (NOT_EQUALITY, "!=")
    , (PLUS_ASSIGN, "+=")
    , (MINUS_ASSIGN, "-=")
    , (MUL_ASSIGN, "*=")
    , (DIV_ASSIGN, "/=")
    , (BITAND_ASSIGN, "&=")
    , (BITOR_ASSIGN, "|=")
    , (BITXOR_ASSIGN, "^=")
    , (MOD_ASSIGN, "%=")
    , (BLS_ASSIGN, "<<=")
    , (BRS_ASSIGN, ">>=")
    , (NEGATION, "!")
    , (OPEN_ROUND, "(")
    , (CLOSE_ROUND, ")")
    , (OPEN_SQUARE, "[")
    , (CLOSE_SQUARE, "]")
    , (OPEN_CURLY, "{")
    , (CLOSE_CURLY, "}")
    , (ANP, "&")
    , (AND, "&&")
    , (OR, "||")
    , (BAR, "|")
    , (XOR, "^")
    , (EOF, "End of File")
    , (IDENTIFIER, "Identifier")
    , (VAR_KEYWORD, "auto")
    , (IF, "if")
    , (WHILE, "while")
    , (ELSE, "else")
    , (EXTERN, "extrn")
    , (RETURN, "return")
    , (CONTINUE, "continue")
    , (BREAK, "break")
    , (STRING, "string")
    , (EQUALS, "=")
    , (UNIT_END, ";")
    , (COMMA, ",")
    ]

stringToTokenKind :: M.Map String TokenKind
stringToTokenKind = M.fromList [ (s, t) | (t, s) <- M.toList tokenKindToString ]

