module Link where

import Data.List

globalVariablesStart = "BEGIN DEFNITION OF GLOBAL VARIABLES"
globalVariablesEnd = "END DEFNITION OF GLOBAL VARIABLES"
memoryFunctionStart = "BEGIN DEFNITION OF MEMORY FUNCTIONS"
memoryFunctionEnd = "END DEFNITION OF MEMORY FUNCTIONS"

selectGlobalVariables :: [String] -> [String]
selectGlobalVariables ss = takeWhile (not . (isInfixOf globalVariablesEnd)) $ tail $ dropWhile (not . isInfixOf globalVariablesStart) ss

selectFunctionVariables :: [String] -> [String]
selectFunctionVariables ss = takeWhile (not . (isInfixOf memoryFunctionEnd)) $ tail $ dropWhile (not . isInfixOf memoryFunctionStart) ss


parseMemoryFile :: FilePath -> IO (String, String)
parseMemoryFile p = do
    s <- lines <$> readFile p
    return (intercalate "\n" $ selectGlobalVariables s, intercalate "\n" $ selectFunctionVariables s)