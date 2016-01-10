{-# LANGUAGE OverloadedStrings #-}
module Util where

import           Data.Char        (toLower)
import           Data.List        (find)
import           Data.Monoid      ((<>))

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath  (takeDirectory, (<.>), (</>))

type ModuleName = String
data Module = Module ModuleName -- ^ Name
                     [Import] -- ^ Imports
                     [Dec] -- ^ Declarations
  deriving (Show, Eq)

makeModule :: ModuleName -> [Import] -> [Dec] -> Module
makeModule name is ds = Module name is ds'
  where ds' = makeNamesUnique ds

makeNamesUnique :: [Dec] -> [Dec]
makeNamesUnique = foldl addDec []
  where
    addDec :: [Dec] -> Dec -> [Dec]
    addDec ds c@(Comment _) = ds ++ [c]
    addDec ds f@(Function name _ _)
        = case find (\(Function n _ _) -> n == name) (filter isFunction ds) of
            Nothing -> ds ++ [f]
            Just f -> addDec ds (alterFunction f)

    isFunction (Function _ _ _) = True
    isFunction _ = False

    alterFunction (Function name sig impl) = function (alterName name) sig impl
    alterName = (++ "_")

data Import = Import String
  deriving (Show, Eq)

type CommentContent = String
type TypeSignature = String
type Implementation = String
type FunctionName = String

data Dec = Function FunctionName TypeSignature Implementation
         | Comment CommentContent
  deriving (Show, Eq)

function :: FunctionName -> TypeSignature -> Implementation -> Dec
function name sig impl = Function name' sig impl
  where name' = cleanName name

cleanName :: String -> String
cleanName (f:rest) = toLower f : rest

class RenderSrc a where
    render :: a -> String

instance RenderSrc Module where
    render (Module name is ds) =
        unlines $ ("module " <> name <> " where") : "\n" : map render is <> ["\n"] <> map render ds

instance RenderSrc Dec where
    render (Function name ts i) = unlines [unwords [name, "::", ts], unwords [name, "=", i]]
    render (Comment s) = unwords ["--", s]

instance RenderSrc Import where
    render (Import i) = "import " <> i

main :: IO ()
main = putStrLn (render myModule) >> generateModule myModule

nameToFile :: ModuleName -> String
nameToFile name = map (\c -> if c == '.' then '/' else c) name <.> "hs"

moduleName :: Module -> String
moduleName (Module n _ _) = n

generateModule :: Module -> IO ()
generateModule mod = do
    let src = "src"
    let filename = nameToFile (moduleName mod)
    let file = src </> filename
    createDirectoryIfMissing True $ takeDirectory file
    let contents = render mod
    writeFile file contents


lit :: String -> String
lit l = "\"" <> l <> "\""

func :: String -> String
func = map toLower

typeDec :: Int -> String
typeDec n = unwords $ ["Monad m", "=>"] ++ concat (replicate n [λtex, "->"]) ++ [λtex]
  where λtex = "ΛTeXT m ()"


makeCommS :: String -> [Dec]
makeCommS name =
    [
      Comment $ "| The @\\\\" <> name <> "@ command"
    , Comment ""
    , Comment $ ">>> " <> name
    , Comment $ unwords $ [">>>", name]
    , Comment $ "\\" <> name
    , function name (typeDec 0) dec
    ]
  where dec = unwords ["commS", lit name]

makeComm0 :: String -> [Dec]
makeComm0 name =
    [
      Comment $ "| The @\\\\" <> name <> "@ command"
    , Comment ""
    , Comment $ unwords $ [">>>", name]
    , Comment $ "\\" <> name <> "{}"
    , function name (typeDec 0) dec
    ]
  where dec = unwords ["comm0", lit name]

makeComm1 :: String -> [Dec]
makeComm1 name = [
      Comment $ "| The @\\\\" <> name <> "@ command"
    , Comment ""
    , Comment $ unwords $ [">>>", name, "\"a\""]
    , Comment $ "\\" <> name <> "{a}"
    , function name (typeDec 1) dec
    ]
  where dec = unwords ["comm1", lit name]

myModule :: Module
myModule = makeModule "Text.LaTeX.Commands.Base"
    [
      Import "Text.LaTeX.LambdaTeX"
    , Import "Text.LaTeX.Base.Class (comm0, commS, comm1)"
    ]
    $ concat $
           map makeCommS commSs
        ++ map makeComm0 comm0s
        ++ map makeComm1 comm1s

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
    , "caption"
    -- , "cite"
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
    , "tiny"
    , "scriptsize"
    , "footnotesize"
    , "small"
    , "normalsize"
    , "large"
    , "Large"
    , "LARGE"
    , "huge"
    , "Huge"
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
