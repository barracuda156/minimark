-- minimark: TeX-subset math parser.  Never fails: anything it does not
-- understand becomes MUnknown/MChar and is rendered verbatim.  The raw
-- TeX is kept in the AST by the caller, so this parse is best-effort by
-- design.
module MiniMark.TexMath(parseMath) where

import MiniMark.CharClass(isSp, isDig, isAlphaA)
import MiniMark.AST
import MiniMark.Symbols(symTable, funcNames)

parseMath :: String -> [MExpr]
parseMath s = fst (pSeq s)

-- Parse a sequence, stopping at '}' (left unconsumed) or end of input.
pSeq :: String -> ([MExpr], String)
pSeq s = case dropWhile isSp s of
  []          -> ([], [])
  s'@('}':_)  -> ([], s')
  s'          -> let (a, r)   = pScripted s'
                     (as, r2) = pSeq r
                 in (a:as, r2)

-- An atom followed by any number of ^/_ scripts (first of each wins).
pScripted :: String -> (MExpr, String)
pScripted s =
  let (a, r) = pAtom s
  in scripts a Nothing Nothing r
  where
    scripts b sub sup t = case dropWhile isSp t of
      ('_':t') | isNothing sub ->
        let (arg, r2) = pArg t' in scripts b (Just arg) sup r2
      ('^':t') | isNothing sup ->
        let (arg, r2) = pArg t' in scripts b sub (Just arg) r2
      t' -> (wrap b sub sup, t')
    wrap b Nothing Nothing = b
    wrap b sub sup         = MScript b sub sup
    isNothing Nothing = True
    isNothing _       = False

-- Script/command argument: {..} group, \cmd, or a single character.
pArg :: String -> ([MExpr], String)
pArg s = case dropWhile isSp s of
  ('{':t) -> let (es, r) = pSeq t
             in (es, expectBrace r)
  t@('\\':_) -> let (a, r) = pAtom t in ([a], r)
  (c:t)   -> ([MChar c], t)
  []      -> ([], [])

expectBrace :: String -> String
expectBrace ('}':r) = r
expectBrace r       = r

pAtom :: String -> (MExpr, String)
pAtom ('{':t) =
  let (es, r) = pSeq t in (MGroup es, expectBrace r)
pAtom ('\\':t) = pCommand t
pAtom (c:t) = (MChar c, t)
pAtom [] = (MGroup [], [])

pCommand :: String -> (MExpr, String)
pCommand s = case span isAlphaA s of
  ([], c:r) -> (control c, r)          -- \{  \$  \,  \!  ...
  ([], [])  -> (MChar '\\', [])
  (cmd, r)  -> command cmd r

control :: Char -> MExpr
control c
  | c `elem` "{}$%&#_|" = MChar c
  | c `elem` ",;:"      = MChar '\x2009'   -- thin space
  | c == '\\'           = MChar ' '        -- row break: degrade to space
  | otherwise           = MChar c

command :: String -> String -> (MExpr, String)
command cmd r
  | cmd == "frac" =
      let (a, r1) = pArg r
          (b, r2) = pArg r1
      in (MFrac a b, r2)
  | cmd == "sqrt" =
      let (a, r1) = pArg r in (MSqrt a, r1)
  | cmd `elem` ["text", "textrm", "mathrm", "mbox", "operatorname"] =
      let (txt, r1) = rawArg r in (MText txt, r1)
  | cmd `elem` ["mathbb"]                     = styled SBb r
  | cmd `elem` ["mathcal", "cal", "mathscr"]  = styled SCal r
  | cmd `elem` ["mathfrak", "frak"]           = styled SFrak r
  | cmd `elem` ["mathbf", "bf", "boldsymbol", "bm"] = styled SBold r
  | cmd `elem` ["mathit", "it"]               = styled SItal r
  | cmd `elem` ["left", "right"]              = delimTok r
  | cmd `elem` ["limits", "nolimits", "displaystyle", "textstyle",
                "scriptstyle", "quad", "qquad", "small", "notag", "nonumber"] =
      (if cmd == "quad" || cmd == "qquad" then MChar ' ' else MGroup [], r)
  | Just comb <- lookup cmd accents =
      let (a, r1) = pArg r in (MGroup (a ++ [MChar comb]), r1)
  | cmd `elem` funcNames = (MText cmd, r)
  | Just c <- lookup cmd symTable = (MChar c, r)
  | otherwise = (MUnknown cmd, r)

styled :: MStyle -> String -> (MExpr, String)
styled st r = let (a, r1) = pArg r in (MStyle st a, r1)

-- \left( \right\rangle \left. ...
delimTok :: String -> (MExpr, String)
delimTok s = case dropWhile isSp s of
  ('\\':t) -> case span isAlphaA t of
                (cmd@(_:_), r) | Just c <- lookup cmd symTable -> (MChar c, r)
                (_, r) -> (MGroup [], r)
  ('.':t)  -> (MGroup [], t)
  (c:t)    -> (MChar c, t)
  []       -> (MGroup [], [])

-- Literal argument text (for \text{...}): no math parsing inside.
rawArg :: String -> (String, String)
rawArg s = case dropWhile isSp s of
  ('{':t) -> let (txt, r) = break (== '}') t in (txt, drop 1 r)
  (c:t)   -> ([c], t)
  []      -> ([], [])

accents :: [(String, Char)]
accents =
  [ ("hat",       '\x0302'), ("widehat",   '\x0302')
  , ("bar",       '\x0304'), ("overline",  '\x0305')
  , ("tilde",     '\x0303'), ("widetilde", '\x0303')
  , ("vec",       '\x20D7')
  , ("dot",       '\x0307'), ("ddot",      '\x0308')
  , ("check",     '\x030C'), ("breve",     '\x0306')
  , ("acute",     '\x0301'), ("grave",     '\x0300')
  , ("mathring",  '\x030A')
  ]
