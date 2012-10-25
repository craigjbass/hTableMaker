{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
--
-- Maintainer  :  Craig Bass <craig@clearbooks.co.uk>
-- Stability   :  UNSTABLE
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import System.IO
import Data.Maybe


tableHeader = "{| class=\"wikitable\" style=\"text-align: left; color: black;\""
tableFooter = "|}"

tableHeaderWrapper = "'''"

tableRowStart = ""
tableRowEnd   = "|-"

tableCellStart = "|"
tableCellEnd   = ""

readElement :: IO String
readElement = putStrLn "Cell Value or Return:" >> getLine

--Processes elements
processElements :: ( String  -> String )-> IO [String]
processElements wrapper = readElement >>=
  \line ->
    if line /= ""
    then ( processElements wrapper >>= \x -> return ((tableCellStart ++ wrapper line):x) )
    else return []

--Processes heading elements
processHeadingElements :: IO [String]
processHeadingElements = processElements bold

--Processes Normal elements
processNormalElements :: IO [String]
processNormalElements = processElements normal

--Processes normal rows
processNormalRows :: IO [[String]]
processNormalRows = putStrLn "==== [Insert Row] ========\n\n" >> processNormalElements >>=
  \row ->
    if row /= []
    then ( processNormalRows >>= \x -> return ((row):x) )
    else return [[]]

--Flattens the normal rows [[String]] into a nice IO [String]
processNormalRowsFlattened :: IO [String]
processNormalRowsFlattened = processNormalRows >>= return . helper
    where
       helper :: [[String]] -> [String]
       helper (x:[]) = x
       helper (x:xs) = x++[tableRowEnd]++helper xs

--Processes the headings into an IO list of strings
processHeadings :: IO [String]
processHeadings = processHeadingElements >>= return . \x -> tableHeader:helper x
    where
      helper :: [String] -> [String]
      helper (x:[]) = [x]++[tableRowEnd]
      helper (x:xs) = x:helper xs

--Wraps a field in bits to make it bold
bold :: String -> String
bold s = tableHeaderWrapper ++ s ++ tableHeaderWrapper

--Wraps a field in bits to make it normal
normal :: String -> String
normal s = s

--Produces a IO list of strings.
process :: IO [String]
process = putStrLn "hTableMaker (Super Hacky)" >>
                   putStrLn "==== [Setup Headings] ========\n\n" >>
                   processHeadings >>=
  \x -> ( processNormalRowsFlattened >>= \y -> return  (x ++ y ++ [tableFooter]) )

--Prints a List of strings
printList :: [String] -> IO ()
printList (x:[]) = putStrLn x
printList (x:xs) = putStrLn x >> printList xs

-- Bind read to write
exeMain = process >>= printList

-- Entry point for unit tests.
testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

