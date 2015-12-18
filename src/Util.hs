module Util where

import           Data.Char (toLower)

data Module = Module String -- Name
                     [Import] -- Imports
                     [Dec] -- Declarations

instance Show Module where
    show (Module name is ds) =
        unlines $ ("module " ++ name ++ " where") : map show is ++ map show ds

data Import = Import String

instance Show Import where
    show (Import i) = i

data Dec = Function TypeSignature Implementation
         | Comment String

instance Show Dec where
    show (Function ts i) = unlines [ts, i]

type TypeSignature = String
type Implementation = String


main :: IO ()
main = print myModule

lit :: String -> String
lit l = "\"" ++ l ++ "\""

func :: String -> String
func = map toLower

typeDec :: String -> Int -> String
typeDec name n = unwords $ [func name, "::", "Monad m", "=>"] ++ concat (replicate n [λtex, "->"]) ++ [λtex]
  where λtex = "ΛTeXT m ()"

makeCommS :: String -> Dec
makeCommS name = Function (typeDec name 0) dec
  where dec = unwords [func name, "=", "commS", lit name]

makeComm0 :: String -> Dec
makeComm0 name = Function (typeDec name 0) dec
  where dec = unwords [func name, "=", "comm0", lit name]

makeComm1 :: String -> Dec
makeComm1 name = Function (typeDec name 1) dec
  where dec = unwords [func name, "=", "comm1", lit name]

myModule :: Module
myModule = Module "Text.LaTeX.LambdaTeX.Commands"
    [] $ concat
        [
          map makeCommS commSs
        , map makeComm0 comm0s
        , map makeComm1 comm1s
        ]

commSs :: [String]
commSs =
    [
      "hyp"
    , "clearpage"
    , "newpage"
    , "protect"
    , "hline"
    ]

comm0s :: [String]
comm0s =
    [
      "maketitle"
    , "LaTeX"
    , "par"
    , "newline"
    , "tableofcontents"
    , "appendix"
    , "cleardoublepage"
    , "today"
    , "thepage"
    , "TeX"
    , "LaTeXe"
    , "ldots"
    , "vdots"
    , "cdots"
    , "ddots"
    , "indent"
    , "noindent"
    , "hfill"
    , "vfill"
    , "textwidth"
    , "linewidth"
    , "underline"
    , "smallskip"
    , "bigskip"
    ]

comm1s :: [String]
comm1s =
    [
      "title"
    , "date"
    , "author"
    , "thanks"
    , "part"
    , "chapter"
    , "section"
    , "subsection"
    , "subsubsection"
    , "paragraph"
    , "subparagraph"
    , "cite"
    , "pagenumbering"
    , "markright"
    , "hyphenation"
    , "mbox"
    , "fbox"
    , "footnote"
    , "emph"
    , "textrm"
    , "textsf"
    , "texttt"
    , "textmd"
    , "textbf"
    , "textup"
    , "textit"
    , "textsl"
    , "textsc"
    , "textnormal"
    ]

envs :: [String]
envs =
    [
      "enumerate"
    , "itemize"
    , "description"
    , "flushleft"
    , "flushright"
    , "center"
    , "quote"
    , "verse"
    , "abstract"
    , "document"
    ]
