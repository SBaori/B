module Main where

import System.Environment (getArgs)
import Flags
import System.Exit (die)
import CompileAndLink
import System.Directory (findExecutable)
import Control.Monad (unless)
import qualified Data.Map.Strict as M

-- @todo: Add support for multiple targets!
main :: IO ()
main = do
    args <- getArgs

    either (die) 
        (\(flagsMap, files) ->
            if M.member HELP flagsMap then
                putStrLn $
                    "./B [flags] [<file>.(b|o|s|ssa)] -o <file>\n\n" ++
                    "--help - Display this output.\n" ++
                    "--keep-ir - Keep the intermediate representation file.\n" ++
                    "--keep-asm - Keep the assembly file.\n" ++
                    "--keep-all - Keep every generated file.\n" ++
                    "-c - Only compile (No linking)\n" ++
                    "-o <file_name> - Specify output file name\n"
            else do
                checkDependenciesOrDie

                compileAndLink flagsMap files
        ) $ getFlagsAndFiles args

checkDependenciesOrDie :: IO ()
checkDependenciesOrDie = do
    maybeGcc <- findExecutable "gcc"
    gccExists <- maybe (putStrLn "Could not find gcc in PATH!" >> pure False) (\_ -> pure True) maybeGcc

    maybeQbe <- findExecutable "qbe"
    qbeExists <- maybe (putStrLn "Could not find qbe in PATH!" >> pure False) (\_ -> pure True) maybeQbe

    unless (gccExists && qbeExists) (die "\nExiting!")
