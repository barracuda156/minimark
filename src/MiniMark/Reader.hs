-- minimark: Markdown reader.  Line-oriented block parser plus a
-- recursive-descent inline parser.  Deliberately dependency-free:
-- runs on MicroHs base and ports verbatim to Idris2 later.
module MiniMark.Reader(parseDocument, parseDoc, parseInlines) where

import MiniMark.CharClass(isSp, isDig, isAlphaA, isAlnumA)
import Data.List(isPrefixOf)
import MiniMark.AST
import MiniMark.TexMath(parseMath)

-- Reader-contract entry point (see Readers.hs): whole input -> Doc.
-- Front-matter extraction lands here (T1.4); meta is empty until then.
parseDocument :: String -> Doc
parseDocument s = Doc emptyMeta (parseDoc s)

parseDoc :: String -> [Block]
parseDoc s = parseBlocks (map (expandTabs . dropCR) (lines s))
  where dropCR l = [c | c <- l, c /= '\r']

expandTabs :: String -> String
expandTabs = concatMap (\c -> if c == '\t' then "    " else [c])

isBlank :: String -> Bool
isBlank = all isSp

trim :: String -> String
trim = dropWhile isSp . dropWhileEnd isSp

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x:xs) []

indentOf :: String -> Int
indentOf = length . takeWhile (== ' ')

--------------------------------------------------------------------------
-- Block structure

parseBlocks :: [String] -> [Block]
parseBlocks [] = []
parseBlocks (l:ls)
  | isBlank l                = parseBlocks ls
  | Just lang <- fenceOpen l = fencedCode lang ls
  | dmathOpen l              = displayMath (l:ls)
  | Just (n,t) <- heading l  = Heading n (parseInlines t) : parseBlocks ls
  | hruleLine l              = HRule : parseBlocks ls
  | quoteLine l              = quoteBlock (l:ls)
  | tableStart (l:ls)        = tableBlock (l:ls)
  | Just _ <- listMarker l   = listBlock (l:ls)
  | otherwise                = paraBlock (l:ls)

-- ``` or ~~~ fences, up to 3 leading spaces, optional language word
fenceOpen :: String -> Maybe String
fenceOpen l =
  let l' = dropIndent3 l in
  case span (== '`') l' of
    (bt, rest) | length bt >= 3 -> Just (trim rest)
    _ -> case span (== '~') l' of
           (td, rest) | length td >= 3 -> Just (trim rest)
           _ -> Nothing

fenceClose :: String -> Bool
fenceClose l =
  let l' = trim l
  in (length l' >= 3) && (all (== '`') l' || all (== '~') l')

dropIndent3 :: String -> String
dropIndent3 l = let (sp, r) = span (== ' ') l
                in if length sp <= 3 then r else l

fencedCode :: String -> [String] -> [Block]
fencedCode lang ls =
  let (body, rest) = break fenceClose ls
  in CodeBlock lang body : parseBlocks (drop 1 rest)

-- $$ ... $$ display math (single- or multi-line)
dmathOpen :: String -> Bool
dmathOpen l = "$$" `isPrefixOf` trim l

displayMath :: [String] -> [Block]
displayMath (l:ls) =
  let t = trim l
      inner = drop 2 t
  in if length inner >= 2 && "$$" `isPrefixOf` reverse inner
       -- one-liner:  $$ e = mc^2 $$
       then let raw = trim (take (length inner - 2) inner)
            in DisplayMath raw (parseMath raw) : parseBlocks ls
       else let (body, rest) = break (\x -> "$$" `isPrefixOf` reverse (trim x)
                                            || trim x == "$$") ls
                lastRaw = case rest of
                            (r:_) -> let t' = trim r
                                     in [take (length t' - 2) t' | t' /= "$$"]
                            []    -> []
                raw = trim (unwords (filter (not . null) (inner : body ++ lastRaw)))
            in DisplayMath raw (parseMath raw) : parseBlocks (drop 1 rest)
displayMath [] = []

heading :: String -> Maybe (Int, String)
heading l =
  let (hs, rest) = span (== '#') (dropIndent3 l)
      n = length hs
  in if n >= 1 && n <= 6 && (null rest || " " `isPrefixOf` rest)
       then Just (n, trim (dropWhileEnd (== '#') (trim rest)))
       else Nothing

-- ***, ---, ___ (spaces allowed between), >= 3 marks
hruleLine :: String -> Bool
hruleLine l =
  let cs = filter (not . isSp) l
  in length cs >= 3 && not (null cs)
     && head cs `elem` "-*_" && all (== head cs) cs

quoteLine :: String -> Bool
quoteLine l = ">" `isPrefixOf` dropIndent3 l

quoteBlock :: [String] -> [Block]
quoteBlock ls =
  let (qs, rest) = span (\x -> quoteLine x || (not (isBlank x) && lazyOK x)) ls
      lazyOK x = not (startsNewBlock [x])   -- lazy continuation lines
      strip x = case dropIndent3 x of
                  '>':' ':r -> r
                  '>':r     -> r
                  r         -> r
  in Quote (parseBlocks (map strip qs)) : parseBlocks rest

startsNewBlock :: [String] -> Bool
startsNewBlock []      = True
startsNewBlock (l:ls') =
     isBlank l || hruleLine l
  || maybe False (const True) (fenceOpen l)
  || maybe False (const True) (heading l)
  || quoteLine l
  || maybe False (const True) (listMarker l)
  || dmathOpen l
  || tableStart (l:ls')

--------------------------------------------------------------------------
-- Lists

-- Returns (isOrdered, startNum, markerWidth incl. trailing spaces)
listMarker :: String -> Maybe (Bool, Int, Int)
listMarker l =
  let ind = indentOf l
      l'  = drop ind l
  in if ind > 3 then Nothing else
     case l' of
       (c:' ':_) | c `elem` "-*+" -> Just (False, 1, ind + 2)
       _ -> case span isDig l' of
              (ds@(_:_), p:' ':_) | p == '.' || p == ')'
                  -> Just (True, readInt ds, ind + length ds + 2)
              _   -> Nothing

readInt :: String -> Int
readInt = foldl (\a c -> a * 10 + (fromEnum c - fromEnum '0')) 0

listBlock :: [String] -> [Block]
listBlock ls@(l:_) =
  case listMarker l of
    Nothing -> paraBlock ls
    Just (ord, start, _) ->
      let (items, rest) = collectItems ord ls
          mkItem (checked, lns) = ListItem checked (parseBlocks lns)
          blocks = map mkItem items
      in (if ord then OrderedList start blocks else BulletList blocks)
         : parseBlocks rest
listBlock [] = []

-- `[ ] `/`[x] `/`[X] ` at the start of an item's first line -> checkbox.
checkbox :: String -> Maybe (Bool, String)
checkbox l = case l of
  '[':' ':']':' ':r -> Just (False, r)
  '[':c:']':' ':r | c `elem` "xX" -> Just (True, r)
  _ -> Nothing

-- Split consecutive lines into items of a list of the given family,
-- each tagged with its checkbox state (marker width grows by 4 when
-- a checkbox is present, so continuation lines indent past it).
collectItems :: Bool -> [String] -> ([(Maybe Bool, [String])], [String])
collectItems ord (l:ls) =
  case listMarker l of
    Just (o, _, w0) | o == ord ->
      let raw = drop w0 l
          (checked, content, w) = case checkbox raw of
            Just (c, r) -> (Just c, r, w0 + 4)
            Nothing     -> (Nothing, raw, w0)
          (cont, rest) = itemCont w ls
          (moreItems, rest') = collectItems ord rest
      in ((checked, content : cont) : moreItems, rest')
    _ -> ([], l:ls)
collectItems _ [] = ([], [])

-- Continuation lines of one item: indented >= w, or blank lines that are
-- followed by further indented content.
itemCont :: Int -> [String] -> ([String], [String])
itemCont w (l:ls)
  | isBlank l =
      case ls of
        (n:_) | not (isBlank n) && indentOf n >= w ->
          let (cs, rest) = itemCont w ls in ("" : cs, rest)
        _ -> ([], l:ls)
  | indentOf l >= w =
      let (cs, rest) = itemCont w ls in (drop w l : cs, rest)
  | otherwise = ([], l:ls)
itemCont _ [] = ([], [])

--------------------------------------------------------------------------
-- Pipe tables

tableStart :: [String] -> Bool
tableStart (h:s:_) = '|' `elem` h && tableSep s
tableStart _       = False

tableSep :: String -> Bool
tableSep l =
  let t = trim l
  in not (null t) && '-' `elem` t && all (`elem` "|-: ") t && '|' `elem` t

tableBlock :: [String] -> [Block]
tableBlock (h:s:ls) =
  let (rows, rest) = span (\x -> '|' `elem` x && not (isBlank x)) ls
      aligns = map alignOf (splitCells s)
      hdr = map parseInlines (splitCells h)
      body = map (map parseInlines . padTo (length hdr) . splitCells) rows
  in Table (padAligns (length hdr) aligns) hdr body : parseBlocks rest
tableBlock ls = paraBlock ls

padTo :: Int -> [String] -> [String]
padTo n xs = take n (xs ++ repeat "")

padAligns :: Int -> [Align] -> [Align]
padAligns n xs = take n (xs ++ repeat ALeft)

alignOf :: String -> Align
alignOf cell =
  let t = trim cell
      l = ":" `isPrefixOf` t
      r = ":" `isPrefixOf` reverse t
  in if l && r then ACenter else if r then ARight else ALeft

-- Split on unescaped '|', dropping the outer empty cells produced by
-- leading/trailing pipes.
splitCells :: String -> [String]
splitCells l = dropOuter (go (trim l) "")
  where
    go [] acc = [reverse acc]
    go ('\\':'|':r) acc = go r ('|':'\\':acc)
    go ('|':r) acc = reverse acc : go r ""
    go (c:r) acc = go r (c:acc)
    dropOuter cs =
      let cs1 = case cs of ("":t) -> t; _ -> cs
      in case reverse cs1 of
           ("":t) -> map trim (reverse t)
           _      -> map trim cs1

--------------------------------------------------------------------------
-- Paragraphs

paraBlock :: [String] -> [Block]
paraBlock ls =
  let (ps, rest) = breakPara ls
  in Para (parseInlines (unwords (map trim ps))) : parseBlocks rest

breakPara :: [String] -> ([String], [String])
breakPara [] = ([], [])
breakPara ls@(l:rest)
  | startsNewBlock ls = ([], ls)
  | otherwise = let (ps, r) = breakPara rest in (l:ps, r)

--------------------------------------------------------------------------
-- Inlines.  `prev` is the preceding character (for word-boundary rules).

parseInlines :: String -> [Inline]
parseInlines = inl ' '

inl :: Char -> String -> [Inline]
inl _ [] = []
inl prev s = case s of
  '\\':c:r | c `elem` "\\`*_{}[]()#+-.!|$<>~" -> prepend c (inl c r)
  '`':_ -> codeSpan prev s
  '$':r -> mathSpan prev r s
  '*':'*':r -> delim prev "**" Strong r s
  '*':r     -> delim prev "*"  Emph   r s
  '_':'_':r | not (isAlnumA prev) -> delim prev "__" Strong r s
  '_':r     | not (isAlnumA prev) -> delim prev "_"  Emph   r s
  '~':'~':r -> delim prev "~~" Strike r s
  '!':'[':r -> linkSpan prev r s     -- images rendered as links
  '[':r -> linkSpan prev r s
  '<':r | httpish r -> autoAngle r s
  c:r | c == 'h' && httpish s && wordStart prev -> bareUrl s
      | otherwise -> prepend c (inl c r)
  where
    wordStart p = isSp p || p `elem` "(["

httpish :: String -> Bool
httpish t = "http://" `isPrefixOf` t || "https://" `isPrefixOf` t

prepend :: Char -> [Inline] -> [Inline]
prepend c (Str t : is) = Str (c:t) : is
prepend c is           = Str [c] : is

-- `code`, with multi-backtick fences
codeSpan :: Char -> String -> [Inline]
codeSpan _ s =
  let (bt, rest) = span (== '`') s
      n = length bt
  in case findRun n rest of
       Just (inside, after) -> CodeSpan inside : inl '`' after
       Nothing -> prepend '`' (inl '`' (drop 1 s))

-- find a run of exactly n backticks
findRun :: Int -> String -> Maybe (String, String)
findRun n = go ""
  where
    go _   [] = Nothing
    go acc t@('`':_) =
      let (bt, r) = span (== '`') t
      in if length bt == n then Just (reverse acc, r)
         else go (reverse bt ++ acc) r
    go acc (c:r) = go (c:acc) r

-- $math$ with pandoc-style guards: no space right after opening $,
-- no space right before closing $, closing $ not followed by a digit.
mathSpan :: Char -> String -> String -> [Inline]
mathSpan _ r orig =
  case r of
    (c0:_) | not (isSp c0) && c0 /= '$' ->
      case scanMath "" r of
        Just (raw, after) -> MathI raw (parseMath raw) : inl '$' after
        Nothing -> prepend '$' (inl '$' r)
    _ -> prepend '$' (inl '$' r)
  where
    scanMath acc ('\\':c:t) = scanMath (c:'\\':acc) t
    scanMath acc ('$':t)
      | not (null acc) && not (isSp (head acc))
        && (null t || not (isDig (head t))) = Just (reverse acc, t)
      | otherwise = Nothing
    scanMath acc (c:t) = scanMath (c:acc) t
    scanMath _ [] = Nothing
    _unused = orig

-- Emphasis delimiters with nesting-aware close search.
delim :: Char -> String -> ([Inline] -> Inline) -> String -> String -> [Inline]
delim _ d ctor r orig =
  case findClose d r of
    Just (inside, after) | not (null (trim inside)) ->
      ctor (parseInlines inside) : inl (last d) after
    _ -> prepend (head d) (inl (head d) (drop 1 orig))

findClose :: String -> String -> Maybe (String, String)
findClose d = go ""
  where
    go _ [] = Nothing
    go acc ('\\':c:t) = go (c:'\\':acc) t
    go acc t@('`':_) =                      -- skip code spans wholesale
      let (bt, r1) = span (== '`') t
      in case findRun (length bt) r1 of
           Just (inside, r2) -> go (reverse (bt ++ inside ++ bt) ++ acc) r2
           Nothing -> go (reverse bt ++ acc) r1
    go acc t@('*':'*':r)
      | d == "**" = Just (reverse acc, r)
      | d == "*" =                           -- skip nested strong
          case findClose "**" r of
            Just (inside, r2) -> go (reverse ("**" ++ inside ++ "**") ++ acc) r2
            Nothing -> go ('*':'*':acc) r
      | otherwise = go ('*':'*':acc) r
      where _ = t
    go acc ('~':'~':r)
      | d == "~~" = Just (reverse acc, r)
      | otherwise = go ('~':'~':acc) r
    go acc (c:t)
      | [c] == d = Just (reverse acc, t)
      | c == head d && [c] == "_" && d == "__" =
          case t of ('_':r2) -> Just (reverse acc, r2)
                    _        -> go (c:acc) t
      | otherwise = go (c:acc) t

-- [text](url) and ![alt](url)
linkSpan :: Char -> String -> String -> [Inline]
linkSpan _ r orig =
  case matchBracket 0 "" r of
    Just (txt, '(':r2) ->
      case matchParen 0 "" r2 of
        Just (url, r3) -> Link (parseInlines txt) (trim url) : inl ')' r3
        Nothing -> fallback
    _ -> fallback
  where
    fallback = prepend (head orig) (inl (head orig) (drop 1 orig))
    matchBracket :: Int -> String -> String -> Maybe (String, String)
    matchBracket _ _ [] = Nothing
    matchBracket n acc ('\\':c:t) = matchBracket n (c:'\\':acc) t
    matchBracket n acc ('[':t) = matchBracket (n+1) ('[':acc) t
    matchBracket n acc (']':t)
      | n == 0 = Just (reverse acc, t)
      | otherwise = matchBracket (n-1) (']':acc) t
    matchBracket n acc (c:t) = matchBracket n (c:acc) t
    matchParen :: Int -> String -> String -> Maybe (String, String)
    matchParen _ _ [] = Nothing
    matchParen n acc ('(':t) = matchParen (n+1) ('(':acc) t
    matchParen n acc (')':t)
      | n == 0 = Just (reverse acc, t)
      | otherwise = matchParen (n-1) (')':acc) t
    matchParen n acc (c:t) = matchParen n (c:acc) t

-- <http://...>
autoAngle :: String -> String -> [Inline]
autoAngle r orig =
  case break (== '>') r of
    (url, '>':after) -> Link [Str url] url : inl '>' after
    _ -> prepend '<' (inl '<' (drop 1 orig))

-- bare http(s)://... url, trailing punctuation trimmed
bareUrl :: String -> [Inline]
bareUrl s =
  let (u0, rest0) = break (\c -> isSp c || c `elem` "<>\"") s
      trailPunct = ".,;:!?)]}"
      u = dropWhileEnd (`elem` trailPunct) u0
      rest = drop (length u) s
  in Link [Str u] u : inl (last ('x':u)) rest
