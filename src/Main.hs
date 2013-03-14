{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
--
-- Maintainer  :  Craig Bass <craig@clearbooks.co.uk>
-- Stability   :  Mostly-Stable
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless)
import Data.Maybe
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import System.IO(putStrLn, getLine)


tableHeader = "{| class=\"wikitable\" style=\"text-align: left; color: black;\""
tableFooter = "|}"

tableHeaderWrapper = "'''"

tableRowStart = ""
tableRowEnd   = "|-"

tableCellStart = "|"
tableCellEnd   = ""

--Does the reading of an cell from the user
readElement :: IO String
readElement = putStrLn "Cell Value or Return:" >> getLine >>= return . wikify

--Quotes a string into a wiki
wikify :: String -> String
wikify [] = []
wikify (x:[]) = wikifyChar x
wikify (x:xs) = (wikifyChar x) ++ wikify xs
 
--Wikifys a singular character
wikifyChar :: Char -> String
wikifyChar c@'|' = escape c
wikifyChar c@'-' = escape c
wikifyChar c = [c]

--Escapes a char 
escape :: Char -> String
escape c = "<nowiki>"++[c]++"</nowiki>"

--Processes elements
processElements :: (String  -> String)-> IO [String]
processElements wrapper = readElement >>= helper wrapper
    where
       helper :: (String -> String) -> String -> IO [String]
       helper wrapper l@"" = return [] --If no char (return key) do not processElements again
       helper wrapper l    = processElements wrapper >>= return . formatElement wrapper l --call processElements again to ask for next element

--Formats an element
formatElement :: (String -> String) -> String -> [String] -> [String]
formatElement wrapper elem1 elem2 = (tableCellStart ++ wrapper elem1 ++ tableCellEnd):elem2

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
processNormalRowsFlattened = processNormalRows >>= return . \x -> rowProcessor helper $ Just tableRowStart x 
    where
       helper :: [[String]] -> [String]
       helper [] = []
       helper (x:[]) = x
       helper (x:(_:[])) = x++[tableRowEnd]
       helper (x:(u@(_:_))) =  x++[tableRowEnd]++rowProcessor helper $ Just tableRowStart u
       
--Processes the headings into an IO list of strings
processHeadings :: IO [String]
processHeadings = processHeadingElements >>= return . \x -> tableHeader:rowProcessor helper $ Just tableRowStart x
    where
      helper :: [String] -> [String]
      helper [] = []
      helper (x:[]) = [x]++[tableRowEnd]
      helper (x:xs) = x:helper xs

--A one size fits all rowProcessor function
rowProcessor :: ( [a] -> [String] ) -> Maybe String -> [a] -> [String]
rowProcessor f (Just []) elems = rowProcessor f Nothing elems
rowProcessor f (Just x) elems = x:rowProcessor f Nothing elems
rowProcessor f Nothing elems = f elems

--Wraps a field in bits to make it bold
bold :: String -> String
bold s = tableHeaderWrapper ++ s ++ tableHeaderWrapper

--Wraps a field in bits to make it normal
normal :: String -> String
normal s = s

--Produces a IO list of strings.
process :: IO [String]
process = putStrLn "hTableMaker (Moderately-Super Hacky)" >>
                   putStrLn "==== [Setup Headings] ========\n\n" >>
                   processHeadings >>=
  \x -> processNormalRowsFlattened >>= \y -> return  $ x ++ y ++ [tableFooter] 

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

