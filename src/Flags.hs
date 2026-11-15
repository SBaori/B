module Flags where

import qualified Data.Map as M
import Control.Monad.Error.Class (MonadError(throwError))

data Flags = KEEP_IR | KEEP_ASM | KEEP_ALL | COMPILE_ONLY | OUTPUT_FILENAME | HELP
    deriving (Eq, Ord)

cmdFlags :: M.Map String Flags
cmdFlags = M.fromList [
        ("--keep-ir", KEEP_IR), 
        ("--keep-asm", KEEP_ASM), 
        ("--keep-all", KEEP_ALL), 
        ("--help", HELP), 
        ("-c", COMPILE_ONLY),
        ("-o", OUTPUT_FILENAME)
    ]

getFlagsAndFiles :: [String] -> Either String (M.Map Flags String, [String])
getFlagsAndFiles args = helper args (M.fromList [(OUTPUT_FILENAME, "a.out")], [])
    where
        helper [] (flagsMap, files) = pure (flagsMap, files)
        helper (a:as) (flagsMap, files)
            | take 2 a == "-o" = do
                oFile <- getOutputFileName as
                helper (drop 1 as) (M.insert (cmdFlags M.! a) oFile flagsMap, files)

            | head a /= '-' = helper as (flagsMap, a:files)
            | M.notMember a cmdFlags = throwError ("Unsupported flag: " ++ a)
            | otherwise = helper as (M.insert (cmdFlags M.! a) "" flagsMap, files)

        getOutputFileName [] = throwError "No output file specified!"
        getOutputFileName (a:_) = pure a