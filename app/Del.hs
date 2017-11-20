import Control.Monad (unless)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
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
  let (cs,fn,lang,t) = fromOpts opts
  eom <- readEOM fn
  tbl <- readTable t
  let eom' = appEndo (foldMap (Endo . diffBy) cs) $ simplify eom
  putStr $ encode lang $ changeName tbl eom'

data Opts = Opts
  { coords :: !String
  , filename :: !String
  , format :: !String
  , table :: !String
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
           <*> strOption (metavar "Table"
                         <> short 't'
                         <> value ""
                         <> help "identifier name table"
                         )

valid :: Opts -> Bool
valid (Opts cs _ fmt _) = all (`elem` ['t','x','y','z']) cs
                          && fmt `elem` ["txt", "fmr", "cpp", "tex"]

fromOpts :: Opts -> ([Coord], Maybe String, Lang, String)
fromOpts (Opts cs fn fmt tbl) = (cs', mfn, lang, tbl)
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
    
readTable :: String -> IO (HashMap String String)
readTable "" = return mempty
readTable fn = do
  tbl <- map ((\[x,y] -> (x,y)) . words) . lines <$> readFile fn
  return $ HM.fromList tbl

changeName :: HashMap String String -> EOM -> EOM
changeName tbl | null tbl = id
               | otherwise = map (\(Equation l r) -> Equation (conv l) (conv r))
  where
    conv :: Exp -> Exp
    conv (Num n) = Num n
    conv s@(Sym n as cs) = if n `HM.member` tbl then Sym (tbl HM.! n) as cs else s
    conv (Neg e) = Neg $ conv e
    conv (Add e1 e2) = Add (conv e1) (conv e2)
    conv (Mul e1 e2) = Mul (conv e1) (conv e2)
    conv (Sub e1 e2) = Sub (conv e1) (conv e2)
    conv (Div e1 e2) = Div (conv e1) (conv e2)
    conv (Pow e1 e2) = Pow (conv e1) (conv e2)
