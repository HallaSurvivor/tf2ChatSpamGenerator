-- HallaSurvivor, Christopher Grossack
-- 2016
--
-- A cheap joke for a friend
--
-- chatSpam config creater
-- input file should be a list of messages
-- with new messages on new lines

import qualified System.Directory as D
import qualified Data.List.Split  as S

makeAlias :: Int -> Int -> String -> String
makeAlias m n s = preface ++ "\"" ++ text ++ nextAlias ++ "\""
  where
    preface   = "alias chatSpam" ++ show (n-1) ++ " "
    text      = "say " ++ s ++ "; "
    nextAlias = "alias chatSpam chatSpam" ++ if n == m then "0" else show n

generateFile :: [String] -> String
generateFile ws = unlines $ intro ++ aliases
  where
    intro   = ["//HallaSurvivor's Chat Spam Generator"
              ,"bind [key] chatSpam // Your key here!!"
              ,"alias chatSpam chatSpam1"
              ,""
              ]
    aliases = zipWith (makeAlias (length ws)) [1..] ws

main :: IO ()
main = do
  print "What file would you like to pull from?"
  inFile <- getLine
  exists <- D.doesFileExist inFile
  if exists
     then do
      print "What would you like to name your script?"
      outFile <- getLine
      let outFile' = if length (S.splitOn "." outFile) == 1 then outFile ++ ".cfg" else outFile
      spamText <- readFile inFile
      writeFile outFile' . generateFile $ lines spamText
     else
      print "Please enter a valid input file."
