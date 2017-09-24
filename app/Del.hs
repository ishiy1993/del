import Control.Monad (unless)
import Data.Monoid
import Options.Applicative
import System.Exit (die)

import Encode
import Lib
import Parser (parseEOM)
import Syntax

main :: IO ()
main = do
  opts <- execParser optsParser
  unless (valid opts) $ die "invalid options"
  let (cs,fn,lang) = fromOpts opts
  eom <- readEOM fn
  let eom' = appEndo (foldMap (Endo . diffBy) cs) $ simplify eom
  putStr $ encode lang eom'

data Opts = Opts
  { coords :: !String
  , filename :: !String
  , format :: !String
  } deriving Show

optsParser :: ParserInfo Opts
optsParser = info (helper <*> opts) (fullDesc <> header "del")
  where
    opts =
      Opts <$> strArgument (metavar "Coordinates"
                           <> value ""
                           <> help "derivative coordinates") 
           <*> strOption (metavar "Filename"
                         <> short 'f'
                         <> value ""
                         <> help "input file name"
                         )
           <*> strOption (metavar "Format"
                         <> short 'o'
                         <> value "txt"
                         <> help "output format(default: txt)"
                         )

valid :: Opts -> Bool
valid (Opts cs _ fmt) = all (`elem` ['t','x','y','z']) cs
                       && fmt `elem` ["txt", "fmr", "cpp", "tex"]

fromOpts :: Opts -> ([Coord], Maybe String, Lang)
fromOpts (Opts cs fn fmt) = (cs', mfn, lang)
  where
    cs' = map toCoord cs
    mfn = if null fn then Nothing else Just fn
    lang = case fmt of
             "txt" -> Txt
             "fmr" -> Fmr
             "cpp" -> Cpp
             "tex" -> Tex

toCoord :: Char -> Coord
toCoord 't' = T
toCoord 'x' = X
toCoord 'y' = Y
toCoord 'z' = Z
toCoord _ = error "no match(toCoord)"

readEOM :: Maybe String -> IO EOM
readEOM mfn = do
  meom <- parseEOM <$> maybe getContents (\fn -> readFile fn) mfn
  case meom of
    Right eom -> return eom
    Left err -> die $ show err
