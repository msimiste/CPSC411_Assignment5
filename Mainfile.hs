module Main where

import LexAssign
import ParAssign
import ErrM
import SkelAssign
import SymbolTable
import MikeIRGen
import MikeAM
import Text.Show.Pretty

import System.Environment

main = do
    args <- getArgs
    let fname = args !! 0
    fconts <- readFile fname
    let tokens = myLexer fconts
    let ptree = pProg tokens
    case ptree of
        Ok tree -> do
            let astree = transProg tree
            let symbT = beginProcess astree
            let iRep = transProgIR astree
            let am = startProg iRep
            --putStrLn $ (ppShow) symbT
            --putStrLn $ (ppShow) astree
            --putStrLn $ (ppShow) iRep
            putStrLn $  am
        Bad emgs -> putStrLn emgs
