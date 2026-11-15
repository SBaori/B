module CompileAndLink where

import Flags 
import qualified Data.Map.Strict as M
import Error
import Control.Monad (unless, when)
import System.Exit (die)
import System.Process (callProcess)
import System.IO.Error (catchIOError)
import Control.Exception 
import IRGen.IR
import Lexer.Lexer 
import Parser.Parser
import Parser.Program
import IRGen.ProgramGen
import System.IO (hPutStrLn, stderr)
import Data.List (intercalate)
import System.Directory (removeFile)

data FileTypes = B | OBJ | ASM | IR | UNKNOWN

extensionToFileType :: M.Map String FileTypes
extensionToFileType = 
    M.fromList [
        (".b", B),
        (".o", OBJ),
        (".s", ASM),
        (".ssa", IR)
    ]

compileAndLink :: M.Map Flags String -> [String] -> IO ()
compileAndLink _ [] = die "No input files found!"

compileAndLink flagsMap files = do
    objFiles <- generateObjFiles flagsMap files

    unless (length files == length objFiles)
        (die "Compilation failed!")

    unless (M.member COMPILE_ONLY flagsMap)
        (catchIOError (callProcess "gcc" (["-o"] ++ [flagsMap M.! OUTPUT_FILENAME] ++ objFiles)) 
            (\_ -> die "\nLinking failed!")
        )

    where
        generateIR :: String -> Either Error [String]
        generateIR contents = do
            tokens <- fst <$> runLexer lexer (LexState contents (1,0))
            ast <- fst <$> runParser program tokens
            irState <- snd <$> runIRGen (programGen ast) (SemState [] [] M.empty (-1) (-1) 0)

            pure $ reverse (irBuffer irState ++ ir irState)

        generateObjFiles :: M.Map Flags String -> [String] -> IO [String]
        generateObjFiles _ [] = pure []

        generateObjFiles flags (f:fs) = do
            contents <- catchIOError (readFile f) (die . displayException)

            let fileExtn = dropWhile (/='.') f
            let fileName = (takeWhile (/='.') f)

            case M.findWithDefault UNKNOWN fileExtn extensionToFileType of

                UNKNOWN -> (die $ "Unrecognized file type: " ++ fileExtn ++ "! Recognized file types: " 
                                    ++ intercalate ", " (M.keys extensionToFileType))

                OBJ -> (f:) <$> generateObjFiles flags fs

                ASM -> do
                    let objFile = fileName ++ ".o"

                    callProcess "gcc" ["-o", objFile, "-c", f]
                    (objFile:) <$> generateObjFiles flags fs
                
                IR -> do
                    let asmFile = fileName ++ ".s"
                    let objFile = fileName ++ ".o"

                    callProcess "qbe" ["-o", asmFile, f]
                    callProcess "gcc" ["-o", objFile, "-c", asmFile]

                    unless (M.member KEEP_ASM flags || M.member KEEP_ALL flags)
                        (catchIOError (removeFile asmFile) (putStrLn . displayException))
                    
                    (objFile:) <$> generateObjFiles flags fs
                
                B -> do
                    let irFile = fileName ++ ".ssa"
                    let asmFile = fileName ++ ".s"
                    let objFile = fileName ++ ".o"

                    case generateIR contents of
                        Left e -> do
                            printCompileError contents e
                            generateObjFiles flags fs

                        Right c -> do
                            catchIOError (writeFile irFile $ unlines c) (die . displayException)

                            callProcess "qbe" ["-o", asmFile, irFile]
                            callProcess "gcc" ["-o", objFile, "-c", asmFile]

                            unless (M.member KEEP_IR flags || M.member KEEP_ALL flags)
                                (catchIOError (removeFile irFile) (putStrLn . displayException))

                            unless (M.member KEEP_ASM flags || M.member KEEP_ALL flags)
                                (catchIOError (removeFile asmFile) (putStrLn . displayException))

                            (objFile:) <$> generateObjFiles flags fs
            
            where
                printCompileError contents (Error p@(l,_) e) = do
                    let contentLines = lines contents

                    hPutStrLn stderr $ f ++ " " ++ show p ++ ": " ++ e

                    when (l > 2) (hPutStrLn stderr $ show (l-2) ++ ": " ++ contentLines !! (l-3))
                    when (l > 1) (hPutStrLn stderr $ show (l-1) ++ ": " ++ contentLines !! (l-2))

                    hPutStrLn stderr $ "-> " ++ show l ++ ": " ++ contentLines !! (l-1) ++ "\n"