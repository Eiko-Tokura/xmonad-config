{-# LANGUAGE TypeFamilies, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes #-}
module ChangeConfig where

import XMonad hiding (defaultConfig)

import System.IO
import Data.Coerce
import Data.Maybe
import Data.Bifunctor
import Data.Kind
import MonParser
import System.Directory
import System.FilePath

-- author : Eiko chan
-- This module is initially meant to used in xmonad, but it can be used in any other haskell project as well, it does not depend on xmonad
-- enables user to change the configuration file of other softwares, by pressing a keybinding without the need to open an editor
--
-- an example of configuring it to change the color of kitty terminal
-- we want to achieve : by pressing Alt+<F2>, we change things to dark mode, Alt+<F3> to light mode
-- to do so we need to know the configuration file path
-- we need to search down the file to find the lines "background" and "foreground" and change their values
-- need a parser that returns a handle to change the file
--
-- another example is to change the picom configuration file, toggle between transparent and opaque mode

--xSetConfig :: (Configurable t, MonadOf t ~ IO) => Mode t -> X ()
xSetConfig m = liftIO $ setConfig m

-- xSetConfig_ :: (Configurable t) => Mode t -> X ()
-- xSetConfig_ m = void $ liftIO $ setConfig m 

class HasConfigFile t where
  configPath :: String
  defaultConfig :: Maybe String -- Nothing for no default
  defaultConfig = Nothing -- can be overridden by given a Just value

class HasConfigFile t => Configurable t where
  data Mode t -- an associated data type used to represents the states and modes you care about

  type MonadOf t :: Type -> Type
  type MonadOf t = IO

  modeSetter :: Mode t -> String -> String 
  -- this function embodies parsing, reading, and setting the configuration. It is the only function that needs to be implemented

  extraActions :: MonadIO (MonadOf t) => Mode t -> MonadOf t () 
  extraActions _ = return () -- default to do nothing

  setConfig :: MonadIO (MonadOf t) => Mode t -> MonadOf t () -- this function is automatically derived from modeSetter
  setConfig m = do
    let path = configPath @t
    doesExist <- liftIO $ doesFileExist path
    if doesExist
    then do contents <- liftIO $ readFile' path
            let newContents = modeSetter m contents
            liftIO $ writeFile path newContents
            extraActions m
    else case defaultConfig @t of
         Just def -> do
            dirExist <- liftIO $ doesDirectoryExist (takeDirectory path)
            if not dirExist
              then liftIO $ createDirectoryIfMissing True (takeDirectory path)
              else return ()
            liftIO (writeFile path def) >> setConfig m
         Nothing -> return ()

type Keyword = String
type File = String

type End a   = a -> a
type Hom a b = a -> b

newtype Line     = Line { unLine :: String }
newtype LinePart = LinePart { unPart :: String } -- part of that line that we want to change

-- a parser that works on a single line, it is used to identify the line that starts with a keyword, and return a continuation function to change the contents
onKeyLine :: Keyword -> Parser Char (End LinePart -> Line)
onKeyLine keyword = do
  part0 <- many0 (itemIn [' ', '\t', ','])
  part1 <- string keyword
  part2 <- many0 (itemIn [' ', '\t', '='])
  modifiable <- many0 (itemNotIn [';'])
  part3 <- many0 (itemIn [';'])
  return $ \f -> Line $ part0 ++ part1 ++ part2 ++ unPart (f $ LinePart modifiable) ++ part3

fileToLines :: String -> [Line]
fileToLines = coerce . lines

linesToFile :: [Line] -> File
linesToFile = unlines . coerce

-- apply a parser that works in one line to the whole file
-- it will go through the file and apply the parser to the first matching line
searchLines
  :: Parser Char (End LinePart -> Line)
  -> Parser Line (End LinePart -> [Line])
searchLines p = go []
  where
    go acc = do 
      line <- item
      case mRunParser p (unLine line) of
        Nothing -> go (line:acc)
        Just cont -> do
          restLines <- many0 item
          return $ (reverse acc ++) . (: restLines) . cont -- \f -> reverse acc ++ (cont f : restLines)

-- change a parser that works on lines to a parser that works on the whole file
onLines :: Parser Line a -> Parser Char a
onLines pline = CreateParser $ \str ->
  let lines = fileToLines str
  in map (second linesToFile) $ runParser pline lines

-- if failed to parse, return the original string
-- if succeeded, apply the function to the string
runSetter :: Parser b (End [b]) -> End [b]
runSetter p str = maybe str ($ str) $ mRunParser p str

-- a special case of changeConfigLine, that changes the line that contains the keyword with the new string
setConfigLineKey :: Keyword -> String -> End File
setConfigLineKey k str = changeConfigLine (onKeyLine k) (const (LinePart str))

modifyConfigLineKey :: Keyword -> End LinePart -> End File
modifyConfigLineKey k = changeConfigLine (onKeyLine k)

modifyConfigLineKeyType :: (Read v, Show v) => Keyword -> End v -> End File
modifyConfigLineKeyType k = changeConfigLine (onKeyLine k) . conjugate LinePart unPart . conjugate show read
  where conjugate :: Hom a b -> Hom b a -> Hom (End a) (End b)
        conjugate = (. flip (.)) . (.) . (.)
        -- lol, it is reduced from
        -- conjugate f g h = f . h . g
        -- conjugate f g = (f .) . (. g)
        -- conjugate f = ((f .) .) . flip (.)
        -- conjugate = (. flip (.)) . (.) . (.)

modifyHSData :: (Read v, Show v) => End v -> End File
modifyHSData f = show . f . read

fromFile :: forall v m. (HasConfigFile v, Read v, MonadIO m) => (File -> v) -> m v
fromFile f = liftIO $ f <$> readFile' (configPath @v)

-- this is a pretty general function, change any line in any way specified by the parser and the changer
changeConfigLine :: Parser Char (End LinePart -> Line) -> End LinePart -> End File
changeConfigLine contLine changer = runSetter $ do
  cont <- onLines $ searchLines contLine
  return . const . linesToFile $ cont changer
