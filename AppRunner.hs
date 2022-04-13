{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module AppRunner ( launchAppPrompt ) where

import XMonad ( spawn, io, X )
import XMonad.Prompt
    ( mkXPrompt,
      getNextCompletion,
      getNextOfLastWord,
      XPConfig,
      XPrompt(showXPrompt, nextCompletion, completionToCommand),
      searchPredicate )
import XMonad.Util.Run ( runInTerm )

import System.Environment ( getEnv )
import System.Directory ( listDirectory )
import qualified Data.Text as DT
import Data.List ( sort )
import Data.Maybe ( fromMaybe )
import Data.List.Split ( splitOn )
import Text.Regex.Posix ( (=~), getAllTextMatches )

data AppRunner = AppRunner

instance XPrompt AppRunner where
  showXPrompt AppRunner    = "Launch: "
  completionToCommand  _ c = c
  nextCompletion         c = getNextCompletion

-- Ispired by: https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/src/XMonad.Prompt.Shell.html#getShellCompl%27
launchAppPrompt :: XPConfig -> X()
launchAppPrompt c = do
  li       <- io listAppsFiles
  allDatas <- io $ mapM getAllData li

  let names = [ getValue "Name" x | x <- allDatas, getValue "NoDisplay" x == "" && getValue "Hidden" x == "" ]

  mkXPrompt AppRunner c (mkComplFunFromList'' (sort names) $ searchPredicate c) launchapp

mkComplFunFromList'' :: [String] -> (String -> String -> Bool) -> String -> IO [String]
mkComplFunFromList'' names p selected = return $ filter (p selected) names

launchapp :: String -> X()
launchapp str = do
  li       <- io listAppsFiles
  allDatas <- io $ mapM getAllData li

  let names = [ getValue "Name" x | x <- allDatas, getValue "NoDisplay" x == "" && getValue "Hidden" x == "" ]
  let list = getParameters (splitOn " " str) names 

  case DT.unpack (DT.toLower (DT.pack (getKeyFromValue "Terminal" (head list) allDatas))) of
    "true"    -> runInTerm "" $ checkParameters (tail list) $ getKeyFromValue "Exec" (head list) allDatas
    otherwise -> spawn $ checkParameters (tail list) $ getKeyFromValue "Exec" (head list) allDatas

getParameters :: [String] -> [String] -> [String]
getParameters (x:xs) names  
  | fromMaybe "" (getElem x names) == x = (x:xs)
  | otherwise = getParameters ((x ++ " " ++ head xs) : tail xs) names

checkParameters :: [String] -> String -> String
checkParameters param exec = unwords $ map (\x -> checkParameters' x param exec) rawParams
  where rawParams = if length (getAllTextMatches (exec =~ "%[a-zA-Z]*") :: [String]) == 0 then [""] else getAllTextMatches $ exec =~ "%[a-zA-Z]*" :: [String]

checkParameters' :: String -> [String] -> String -> String
checkParameters' "" [] exec = exec
checkParameters' rawParam [] exec = DT.unpack $ DT.replace (DT.pack rawParam) (DT.pack "") (DT.pack exec)
checkParameters' rawParam param exec = case rawParam of
  ""   -> exec
  "%f" -> DT.unpack $ DT.replace (DT.pack "%f") (DT.pack $ head param) (DT.pack exec)
  "%F" -> DT.unpack $ DT.replace (DT.pack "%F") (DT.pack $ unwords param) (DT.pack exec)
  "%u" -> DT.unpack $ DT.replace (DT.pack "%u") (DT.pack $ head param) (DT.pack exec)
  "%U" -> DT.unpack $ DT.replace (DT.pack "%U") (DT.pack $ unwords param) (DT.pack exec)
  otherwise -> DT.unpack $ DT.replace (DT.pack rawParam) (DT.pack "") (DT.pack exec)

-- Get all .desktop file
listAppsFiles :: IO [String]
listAppsFiles = do
  username  <- getEnv "USER"
  apps      <- listDirectory "/usr/share/applications"
  localapps <- listDirectory $ "/home/"++username++"/.local/share/applications"

  let apps' = map ("/usr/share/applications/" ++) apps
  let localapps' = map (("/home/"++username++"/.local/share/applications/") ++) localapps

  return [x | x <- (apps'++localapps')::[String], DT.pack ".desktop" `DT.isInfixOf` DT.pack x ]

getElem :: String -> [String] -> Maybe String 
getElem s (x:xs)
  | DT.pack s `DT.isInfixOf` DT.pack x = Just x
  | null xs                            = Nothing
  | otherwise                          = getElem s xs

-- Get Name and Exec of .desktop file
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f a = (f (fst a), f (snd a)) 

adjustStr :: DT.Text -> String
adjustStr a
  | DT.head a == '=' = DT.unpack $ DT.tail a
  | otherwise     = DT.unpack a

getAllData :: String -> IO [(String, String)]
getAllData filePath = do
  f <- readFile filePath
  return [mapTuple adjustStr $ DT.breakOn (DT.pack "=") (DT.pack x) | x <- lines f, '=' `elem` x]

getValue :: String -> [(String, String)] -> String
getValue _ [] = ""
getValue key (x:xs)
  | key == fst x = snd x
  | null xs      = ""
  | otherwise    = getValue key xs

getKey :: String -> [(String, String)] -> String
getKey _ [] = ""
getKey value (x:xs)
  | value == snd x = fst x
  | null xs        = ""
  | otherwise      = getKey value xs

getKeyFromValue :: String -> String -> [[(String, String)]] -> String
getKeyFromValue key value (x:xs)
  | getKey value x /= "" = getValue key x
  | null xs              = ""
  | otherwise            = getKeyFromValue key value xs
