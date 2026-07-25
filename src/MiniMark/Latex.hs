-- minimark: LaTeX writer.  Math is emitted as the RAW TeX captured by
-- the reader (lossless), so this writer never depends on the math
-- parser's coverage.
module MiniMark.Latex(LatexOpts(..), renderLatex) where

import Data.List(intercalate)
import MiniMark.AST

data LatexOpts = LatexOpts
  { loStandalone :: Bool
  }

renderLatex :: LatexOpts -> [Block] -> String
renderLatex o bs =
  let body = intercalate "\n" (map block bs)
  in if loStandalone o
       then preamble ++ body ++ "\n\\end{document}\n"
       else body ++ "\n"

preamble :: String
preamble = concat
  [ "\\documentclass{article}\n"
  , "\\usepackage[utf8]{inputenc}\n"
  , "\\usepackage[T1]{fontenc}\n"
  , "\\usepackage{amsmath,amssymb}\n"
  , "\\usepackage{hyperref}\n"
  , "\\begin{document}\n\n"
  ]

escT :: String -> String
escT = concatMap e
  where
    e '\\' = "\\textbackslash{}"
    e '{'  = "\\{"
    e '}'  = "\\}"
    e '$'  = "\\$"
    e '&'  = "\\&"
    e '#'  = "\\#"
    e '_'  = "\\_"
    e '%'  = "\\%"
    e '~'  = "\\textasciitilde{}"
    e '^'  = "\\textasciicircum{}"
    e c    = [c]

block :: Block -> String
block b = case b of
  Heading n is ->
    let cmd = case n of
          1 -> "section"
          2 -> "subsection"
          3 -> "subsubsection"
          4 -> "paragraph"
          _ -> "subparagraph"
    in "\\" ++ cmd ++ "{" ++ inlines is ++ "}\n"
  Para is -> inlines is ++ "\n"
  CodeBlock _ lns ->
    "\\begin{verbatim}\n" ++ unlines lns ++ "\\end{verbatim}\n"
  BulletList items ->
    "\\begin{itemize}\n" ++ concatMap item items ++ "\\end{itemize}\n"
  OrderedList start items ->
    "\\begin{enumerate}\n"
    ++ (if start /= 1
          then "\\setcounter{enumi}{" ++ show (start - 1) ++ "}\n"
          else "")
    ++ concatMap item items ++ "\\end{enumerate}\n"
  Quote bs' ->
    "\\begin{quote}\n" ++ intercalate "\n" (map block bs') ++ "\\end{quote}\n"
  HRule -> "\\noindent\\hrulefill\n"
  Table aligns hdr rows ->
    let colspec = map alChar aligns
        alChar ALeft = 'l'
        alChar ACenter = 'c'
        alChar ARight = 'r'
        row cells = intercalate " & " (map inlines cells) ++ " \\\\\n"
    in "\\begin{tabular}{" ++ colspec ++ "}\n"
       ++ row hdr ++ "\\hline\n"
       ++ concatMap row rows
       ++ "\\end{tabular}\n"
  DisplayMath raw _ -> "\\[ " ++ raw ++ " \\]\n"

item :: [Block] -> String
item bs = "\\item " ++ intercalate "\n" (map block bs)

inlines :: [Inline] -> String
inlines = concatMap f
  where
    f (Str t) = escT t
    f (Emph is) = "\\emph{" ++ inlines is ++ "}"
    f (Strong is) = "\\textbf{" ++ inlines is ++ "}"
    f (CodeSpan t) = "\\texttt{" ++ escT t ++ "}"
    f (Link txt url) =
      "\\href{" ++ url ++ "}{" ++ inlines txt ++ "}"
    f (MathI raw _) = "$" ++ raw ++ "$"
