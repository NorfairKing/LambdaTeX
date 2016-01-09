{-# LANGUAGE OverloadedStrings #-}
module Util where

import           Data.Char        (toLower)
import           Data.Monoid      ((<>))

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath  (takeDirectory, (<.>), (</>))

import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T

type ModuleName = Text
data Module = Module ModuleName -- ^ Name
                     [Import] -- ^ Imports
                     [Dec] -- ^ Declarations


data Import = Import Text
type CommentContent = Text
type TypeSignature = Text
type Implementation = Text

data Dec = Function TypeSignature Implementation
         | Comment CommentContent


class RenderSrc a where
    render :: a -> Text

instance RenderSrc Module where
    render (Module name is ds) =
        T.unlines $ ("module " <> name <> " where") : "\n" : map render is <> ["\n"] <> map render ds

instance RenderSrc Dec where
    render (Function ts i) = T.unlines [ts, i]
    render (Comment s) = T.unwords ["--", s]

instance RenderSrc Import where
    render (Import i) = "import " <> i

main :: IO ()
main = generateModule myModule

nameToFile :: ModuleName -> String
nameToFile name = map (\c -> if c == '.' then '/' else c) (T.unpack name) <.> "hs"

moduleName :: Module -> Text
moduleName (Module n _ _) = n

generateModule :: Module -> IO ()
generateModule mod = do
    let src = "src"
    let filename = nameToFile (moduleName mod)
    let file = src </> filename
    createDirectoryIfMissing True $ takeDirectory file
    let contents = render mod
    T.writeFile file contents


lit :: Text -> Text
lit l = "\"" <> l <> "\""

func :: Text -> Text
func = T.map toLower

typeDec :: Text -> Int -> Text
typeDec name n = T.unwords $ [func name, "::", "Monad m", "=>"] ++ concat (replicate n [λtex, "->"]) ++ [λtex]
  where λtex = "ΛTeXT m ()"

makeCommS :: Text -> [Dec]
makeCommS name =
    [
      Comment $ "| The @\\\\" <> name <> "@ command"
    , Comment ""
    , Comment $ ">>> " <> name
    , Comment $ T.unwords $ [">>>", name]
    , Comment $ "\\" <> name
    , Function (typeDec name 0) dec
    ]
  where dec = T.unwords [func name, "=", "commS", lit name]

makeComm0 :: Text -> [Dec]
makeComm0 name =
    [
      Comment $ "| The @\\\\" <> name <> "@ command"
    , Comment ""
    , Comment $ T.unwords $ [">>>", name]
    , Comment $ "\\" <> name <> "{}"
    , Function (typeDec name 0) dec
    ]
  where dec = T.unwords [func name, "=", "comm0", lit name]

makeComm1 :: Text -> [Dec]
makeComm1 name = [
      Comment $ "| The @\\\\" <> name <> "@ command"
    , Comment ""
    , Comment $ T.unwords $ [">>>", name, "\"a\""]
    , Comment $ "\\" <> name <> "{a}"
    , Function (typeDec name 1) dec
    ]
  where dec = T.unwords [func name, "=", "comm1", lit name]

myModule :: Module
myModule = Module "Text.LaTeX.Commands.Base"
    [
      Import "Text.LaTeX.LambdaTeX"
    , Import "Text.LaTeX.Base.Class (comm0, commS, comm1)"
    ]
    $ concat $
           map makeCommS commSs
        ++ map makeComm0 comm0s
        ++ map makeComm1 comm1s

commSs :: [Text]
commSs =
    [
      "hyp"
    , "clearpage"
    , "newpage"
    , "protect"
    , "hline"
    ]

comm0s :: [Text]
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

comm1s :: [Text]
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

envs :: [Text]
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
