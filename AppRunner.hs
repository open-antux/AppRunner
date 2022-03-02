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
import XMonad ( spawn, io, X )

import System.Environment ( getEnv )
import System.Directory ( listDirectory )
import qualified Data.Text as DT
import Data.List ( sort )
import Text.Regex.Posix ( (=~) )

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

  case DT.unpack (DT.toLower (DT.pack (getKeyFromValue "Terminal" str allDatas))) of
    "true"    -> runInTerm "" $ checkParameters $ getKeyFromValue "Exec" str allDatas
    otherwise -> spawn $ checkParameters $ getKeyFromValue "Exec" str allDatas

checkParameters :: String -> String
checkParameters a
  | not $ null match = DT.unpack $ DT.replace (DT.pack match) (DT.pack "") (DT.pack a)
  | otherwise          = a
  where match = (a =~ "%[a-zA-Z]*")::String

-- Get all .desktop file
listAppsFiles :: IO [String]
listAppsFiles = do
  username  <- getEnv "USER"
  apps      <- listDirectory "/usr/share/applications"
  localapps <- listDirectory $ "/home/"++username++"/.local/share/applications"

  let apps' = map ("/usr/share/applications/" ++) apps
  let localapps' = map (("/home/"++username++"/.local/share/applications/") ++) localapps

  return [x | x <- (apps'++localapps')::[String], DT.pack ".desktop" `DT.isInfixOf` DT.pack x ]

-- Get Name and Exec of .desktop file
getElem :: String -> [String] -> Maybe String
getElem s (x:xs)
  | DT.pack s `DT.isInfixOf` DT.pack x = Just x
  | null xs                            = Nothing
  | otherwise                          = getElem s xs

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
