module Parser.Program where

import Parser.Parser
import Grammar
import Parser.Globals

program :: Parser Program
program = Program <$> global