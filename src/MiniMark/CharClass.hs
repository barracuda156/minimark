-- minimark: ASCII-only character predicates.
--
-- Data.Char's isSpace/isAlpha/... fall back to the full Unicode
-- general-category table for non-ASCII chars; MicroHs builds that table
-- lazily on first use, which costs ~0.5s on x86 and would be several
-- seconds per run on a G4.  Markdown/TeX syntax chars are all ASCII, so
-- ASCII-only predicates are semantically right AND keep the table
-- unforced no matter what Unicode the document contains.
module MiniMark.CharClass(isSp, isDig, isAlphaA, isAlnumA) where

isSp :: Char -> Bool
isSp c = c == ' ' || c == '\t' || c == '\n' || c == '\r'
      || c == '\f' || c == '\v'

isDig :: Char -> Bool
isDig c = c >= '0' && c <= '9'

isAlphaA :: Char -> Bool
isAlphaA c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isAlnumA :: Char -> Bool
isAlnumA c = isAlphaA c || isDig c
