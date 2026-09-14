-- minimark: docx reader (-f docx, T5.3).  OOXML wordprocessing: a zip
-- (T4.1) holding word/document.xml, walked as a MiniMark.Xml event
-- stream (T4.2) exactly like the odt reader before it — same shape,
-- different vocabulary (w: prefix, literal per the parser's
-- no-namespace-resolution rule).
--
-- Two indirections beyond what odt needed:
--   * hyperlinks: <w:hyperlink r:id="rId4"> stores no URL at all; the
--     target lives in a SEPARATE zip member, word/_rels/document.xml.rels,
--     as <Relationship Id="rId4" Target="http://..."/>.  Image targets
--     (a:blip r:embed) resolve through the same table.
--   * lists: a paragraph is a list item iff its pPr carries
--     <w:numPr><w:ilvl w:val="D"/><w:numId w:val="N"/></...>; whether
--     it is ordered or bullet lives in ANOTHER member, word/numbering.xml
--     (numId -> abstractNumId -> per-ilvl numFmt).  numbering.xml absent
--     or the chain broken: bullet, per the roadmap's own call.
-- Both extra members are OPTIONAL: any failure (missing, corrupt,
-- UTF-16) degrades to an empty table — links render as their text,
-- lists as bullets — never a refusal for a document whose body parses.
--
-- Consecutive list paragraphs group into one list; a deeper w:ilvl run
-- nests as a sublist inside the preceding item (ordered-vs-bullet
-- decided per level from its first item).  Headings are w:pStyle
-- "Heading1".."Heading9" by style ID (capped at 3 like every other
-- reader; localized style IDs are out of scope, documented lossiness).
-- vertAlign sub/superscript has no AST constructor — runs pass through
-- unstyled, the same call RtfReader already made for \super/\sub.
-- Empty w:p paragraphs (Word emits one per blank line) are dropped:
-- block spacing is the writers' job, not stray <p></p>s.
-- Tracked changes: w:ins recurses (inserted text shows), w:del drops
-- by construction (deleted runs hold w:delText, not w:t).
-- mc:AlternateContent takes the FIRST branch only (mc:Choice), else
-- the Choice/Fallback pair would render its content twice.
-- Doc metadata (title/author/date) fills from docProps/core.xml when
-- present — the writers' --standalone paths already consume it.
--
-- Malformed/unexpected OOXML never crashes: MiniMark.Xml is total,
-- container failures are clean refusals via MiniMark.Zip, and every
-- walk here degrades unknown elements to their text content or drops
-- them, exactly like AbwReader/OdtReader.
module MiniMark.DocxReader(parseDocx) where

import Data.ByteString(ByteString)
import MiniMark.AST
import MiniMark.Xml(XmlEvent(..), parseXml, xmlLooksUtf16, balanced, textUnder, attr)
import MiniMark.Zip(ZipEntry, zipEntries, zipFind, zipExtract)

parseDocx :: ByteString -> IO (Either String Doc)
parseDocx bs = case zipEntries bs of
  Left e -> return (Left e)
  Right es -> case zipFind es "word/document.xml" of
    Nothing -> return (Left "docx: no word/document.xml member")
    Just en -> do
      r <- zipExtract bs en
      case r of
        Left e -> return (Left ("word/document.xml: " ++ e))
        Right dbs
          | xmlLooksUtf16 dbs ->
              return (Left "word/document.xml: UTF-16 XML not supported")
          | otherwise -> do
              rels <- optXml bs es "word/_rels/document.xml.rels"
              nums <- optXml bs es "word/numbering.xml"
              core <- optXml bs es "docProps/core.xml"
              let ctx = Ctx (maybe [] parseRels rels)
                            (maybe ([], []) parseNums nums)
              return (Right (Doc (maybe emptyMeta metaOf core)
                                 (blocks ctx (findBody (parseXml dbs)))))

-- Optional members degrade to absence on ANY failure — a docx with a
-- broken rels part still renders, its links just lose their URLs.
optXml :: ByteString -> [ZipEntry] -> String -> IO (Maybe [XmlEvent])
optXml bs es nm = case zipFind es nm of
  Nothing -> return Nothing
  Just en -> do
    r <- zipExtract bs en
    return (case r of
      Right b | not (xmlLooksUtf16 b) -> Just (parseXml b)
      _ -> Nothing)

data Ctx = Ctx
  { cRels :: [(String, String)]   -- relationship Id -> Target
  , cNums :: NumTable
  }

-- (numId -> abstractNumId) and (abstractNumId, ilvl, numFmt).
type NumTable = ([(String, String)], [(String, Int, String)])

-- <w:body> holds the content; located anywhere in the stream, missing
-- (malformed input) means an empty document, matching the other
-- readers' find-the-container rule.
findBody :: [XmlEvent] -> [XmlEvent]
findBody [] = []
findBody (XStart "w:body" _ : rest) = fst (balanced rest)
findBody (_ : rest) = findBody rest

--------------------------------------------------------------------------
-- docProps/core.xml -> Meta (dc:title, dc:creator, dcterms:created).

metaOf :: [XmlEvent] -> Meta
metaOf evs = emptyMeta{ mTitle  = elemText "dc:title" evs
                      , mAuthor = elemText "dc:creator" evs
                      , mDate   = elemText "dcterms:created" evs }

elemText :: String -> [XmlEvent] -> Maybe String
elemText _ [] = Nothing
elemText nm (XStart n _ : rest)
  | n == nm = case textUnder (fst (balanced rest)) of
      "" -> Nothing
      s  -> Just s
elemText nm (_ : rest) = elemText nm rest

--------------------------------------------------------------------------
-- word/_rels/document.xml.rels: flat <Relationship Id Target/> list
-- (no prefix — the rels part uses a default namespace).

parseRels :: [XmlEvent] -> [(String, String)]
parseRels [] = []
parseRels (XStart "Relationship" as : rest) =
  case (attr "Id" as, attr "Target" as) of
    (Just i, Just t) -> (i, t) : parseRels rest
    _                -> parseRels rest
parseRels (_ : rest) = parseRels rest

--------------------------------------------------------------------------
-- word/numbering.xml: numId -> abstractNumId (w:num elements), then
-- abstractNumId + ilvl -> numFmt (w:abstractNum/w:lvl/w:numFmt).

parseNums :: [XmlEvent] -> NumTable
parseNums evs = (numMap evs, fmts evs)

numMap :: [XmlEvent] -> [(String, String)]
numMap [] = []
numMap (XStart "w:num" as : rest) =
  let (inner, after) = balanced rest
  in case (attr "w:numId" as, findVal "w:abstractNumId" inner) of
       (Just ni, Just ai) -> (ni, ai) : numMap after
       _                  -> numMap after
numMap (_ : rest) = numMap rest

fmts :: [XmlEvent] -> [(String, Int, String)]
fmts [] = []
fmts (XStart "w:abstractNum" as : rest) =
  let (inner, after) = balanced rest
  in case attr "w:abstractNumId" as of
       Just aid -> lvls aid inner ++ fmts after
       Nothing  -> fmts after
fmts (_ : rest) = fmts rest

lvls :: String -> [XmlEvent] -> [(String, Int, String)]
lvls _ [] = []
lvls aid (XStart "w:lvl" as : rest) =
  let (inner, after) = balanced rest
      il = maybe 0 (parseIntDefault 0) (attr "w:ilvl" as)
  in case findVal "w:numFmt" inner of
       Just fmt -> (aid, il, fmt) : lvls aid after
       Nothing  -> lvls aid after
lvls aid (_ : rest) = lvls aid rest

-- First <nm w:val="..."/> in a stream (linear, first hit wins).
findVal :: String -> [XmlEvent] -> Maybe String
findVal _ [] = Nothing
findVal nm (XStart n as : rest)
  | n == nm   = attr "w:val" as
  | otherwise = findVal nm rest
findVal nm (_ : rest) = findVal nm rest

-- Ordered iff the numFmt for this numId+level is neither bullet nor
-- none; any break in the chain (unknown numId, no such level — try the
-- abstract's first level before giving up) means bullet.
numOrdered :: Ctx -> String -> Int -> Bool
numOrdered ctx ni il = case lookup ni (fst (cNums ctx)) of
  Nothing  -> False
  Just aid ->
    let here = [f | (a, l, f) <- snd (cNums ctx), a == aid, l == il]
        any_ = [f | (a, _, f) <- snd (cNums ctx), a == aid]
    in case here ++ any_ of
         (f : _) -> f /= "bullet" && f /= "none"
         []      -> False

--------------------------------------------------------------------------
-- Blocks

blocks :: Ctx -> [XmlEvent] -> [Block]
blocks _ [] = []
blocks ctx (XStart "w:p" _ : rest) =
  let (inner, after) = balanced rest
  in case paraKind ctx inner of
       PHeading _ []    -> blocks ctx after      -- empty: drop (see top)
       PPara []         -> blocks ctx after
       PHeading lvl is  -> Heading lvl is : blocks ctx after
       PList il ord is  ->
         let (items, after') = listRun ctx [(il, ord, is)] after
         in listBlocks items ++ blocks ctx after'
       PPara is         -> Para is : blocks ctx after
blocks ctx (XStart "w:tbl" _ : rest) =
  let (inner, after) = balanced rest
      (hdr, body) = tableRows ctx inner
  in Table [] hdr body : blocks ctx after
blocks ctx (XStart _ _ : rest) =
  -- Unknown container (w:sectPr, w:sdt, ...): skip its content, keep
  -- walking siblings — the same call AbwReader/OdtReader made.
  blocks ctx (snd (balanced rest))
blocks ctx (_ : rest) = blocks ctx rest

data ParaKind = PHeading Int [Inline] | PList Int Bool [Inline] | PPara [Inline]

-- Classify one paragraph from its pPr (schema-first child; leading
-- whitespace text tolerated).  Scanning ONLY the pPr group keeps a
-- textbox's nested paragraphs from leaking their style outward.
-- numId "0" explicitly REMOVES numbering in OOXML — not a list.
paraKind :: Ctx -> [XmlEvent] -> ParaKind
paraKind ctx inner =
  let pr = paraProps inner
      is = inlines ctx inner
      hd = case findVal "w:pStyle" pr of
             Just sty -> headingLevel sty
             Nothing  -> Nothing
  in case hd of
       Just lvl -> PHeading lvl is
       Nothing -> case findVal "w:numId" pr of
         Just ni | ni /= "0" ->
           let il = maybe 0 (parseIntDefault 0) (findVal "w:ilvl" pr)
           in PList il (numOrdered ctx ni il) is
         _ -> PPara is

paraProps :: [XmlEvent] -> [XmlEvent]
paraProps (XText _ : rest) = paraProps rest
paraProps (XStart "w:pPr" _ : rest) = fst (balanced rest)
paraProps _ = []

headingLevel :: String -> Maybe Int
headingLevel s = case s of
  ('H':'e':'a':'d':'i':'n':'g':ds)
    | not (null ds) && all isDigitA ds -> Just (capLevel (parseIntDefault 1 ds))
  _ -> Nothing
  where isDigitA c = c >= '0' && c <= '9'

capLevel :: Int -> Int
capLevel n
  | n <= 1    = 1
  | n == 2    = 2
  | otherwise = 3

parseIntDefault :: Int -> String -> Int
parseIntDefault dflt s = case s of
  ds | not (null ds) && all isDigitA ds -> foldl step 0 ds
  _ -> dflt
  where
    step acc c = acc * 10 + (fromEnum c - fromEnum '0')
    isDigitA c = c >= '0' && c <= '9'

--------------------------------------------------------------------------
-- Lists.  listRun gathers the consecutive (depth, ordered, inlines)
-- list paragraphs after the first; listBlocks folds them into nested
-- BulletList/OrderedList blocks by depth.

listRun :: Ctx -> [(Int, Bool, [Inline])] -> [XmlEvent]
        -> ([(Int, Bool, [Inline])], [XmlEvent])
listRun ctx acc evs = case evs of
  (XText _ : rest) -> listRun ctx acc rest   -- stray text between paras: blocks drops it too
  (XStart "w:p" _ : rest) ->
    let (inner, after) = balanced rest
    in case paraKind ctx inner of
         PList il ord is -> listRun ctx ((il, ord, is) : acc) after
         _               -> (reverse acc, evs)
  _ -> (reverse acc, evs)

listBlocks :: [(Int, Bool, [Inline])] -> [Block]
listBlocks [] = []
listBlocks xs@((d, ord, _) : _) =
  let (items, rest) = listAt d xs
  in mkList ord items : listBlocks rest

-- Items at exactly depth d; a following deeper run nests inside the
-- item just built (as trailing sub-blocks), a shallower one ends this
-- level.  Entered only at the depth of the head item, so after the
-- span the next item is always <= d.
listAt :: Int -> [(Int, Bool, [Inline])]
       -> ([ListItem], [(Int, Bool, [Inline])])
listAt _ [] = ([], [])
listAt d xs@((i, _, is) : rest)
  | i /= d = ([], xs)
  | otherwise =
      let (deeper, rest') = span (\(j, _, _) -> j > d) rest
          (sibs, rest'') = listAt d rest'
      in (ListItem Nothing (Para is : listBlocks deeper) : sibs, rest'')

mkList :: Bool -> [ListItem] -> Block
mkList True  items = OrderedList 1 items
mkList False items = BulletList items

--------------------------------------------------------------------------
-- Tables: w:tbl / w:tr / w:tc, first row is the header (matches every
-- other reader/writer).  A nested table inside a cell degrades to its
-- text via the cell's linear paragraph walk.

tableRows :: Ctx -> [XmlEvent] -> ([[Inline]], [[[Inline]]])
tableRows ctx evs = case collectRows ctx evs of
  []           -> ([], [])
  (hdr : body) -> (hdr, body)

collectRows :: Ctx -> [XmlEvent] -> [[[Inline]]]
collectRows _ [] = []
collectRows ctx (XStart "w:tr" _ : rest) =
  let (inner, after) = balanced rest
  in collectCells ctx inner : collectRows ctx after
collectRows ctx (_ : rest) = collectRows ctx rest

collectCells :: Ctx -> [XmlEvent] -> [[Inline]]
collectCells _ [] = []
collectCells ctx (XStart "w:tc" _ : rest) =
  let (inner, after) = balanced rest
  in cellInlines ctx inner : collectCells ctx after
collectCells ctx (_ : rest) = collectCells ctx rest

-- Same convention as AbwReader/OdtReader: one or more paragraphs per
-- cell, inline content pulled out of each, joined with a line break.
cellInlines :: Ctx -> [XmlEvent] -> [Inline]
cellInlines ctx evs = case [is | is <- map paraOf (paras evs), not (null is)] of
  []    -> []
  [one] -> one
  many  -> intercalateInlines LineBreak many
  where
    paraOf content = inlines ctx content
    paras [] = []
    paras (XStart "w:p" _ : rs) =
      let (c, r) = balanced rs in c : paras r
    paras (_ : rs) = paras rs

intercalateInlines :: Inline -> [[Inline]] -> [Inline]
intercalateInlines sep = go
  where
    go []       = []
    go [x]      = x
    go (x : xs) = x ++ [sep] ++ go xs

--------------------------------------------------------------------------
-- Inlines.  Paragraph content is a sequence of runs (w:r), hyperlinks
-- wrapping runs, and containers we recurse through for their text.

inlines :: Ctx -> [XmlEvent] -> [Inline]
inlines _ [] = []
inlines ctx (XText t : rest) = Str t : inlines ctx rest
inlines ctx (XStart "w:pPr" _ : rest) =
  inlines ctx (snd (balanced rest))          -- paragraph props: no content
inlines ctx (XStart "w:r" _ : rest) =
  let (inner, after) = balanced rest
  in runInlines ctx inner ++ inlines ctx after
inlines ctx (XStart "w:hyperlink" as : rest) =
  let (inner, after) = balanced rest
      content = inlines ctx inner
      linked = case attr "r:id" as of
        Just rid -> case lookup rid (cRels ctx) of
          Just url -> [Link content url ""]
          Nothing  -> content       -- broken/absent rels: text survives
        Nothing -> content          -- w:anchor internal link: no URL to give
  in linked ++ inlines ctx after
inlines ctx (XStart "mc:AlternateContent" _ : rest) =
  let (inner, after) = balanced rest
  in inlines ctx (firstBranch inner) ++ inlines ctx after
inlines ctx (XStart _ _ : rest) =
  -- Unknown container (w:ins, w:sdt, w:fldSimple, ...): keep its text,
  -- drop the wrapper.  w:del's text never surfaces this way — deleted
  -- runs hold w:delText, which the run walker does not read.
  let (inner, after) = balanced rest
  in inlines ctx inner ++ inlines ctx after
inlines ctx (_ : rest) = inlines ctx rest

-- Content of the first child element only — mc:Choice, whose Fallback
-- sibling duplicates it in another vocabulary.
firstBranch :: [XmlEvent] -> [XmlEvent]
firstBranch (XStart _ _ : rest) = fst (balanced rest)
firstBranch (_ : rest) = firstBranch rest
firstBranch [] = []

--------------------------------------------------------------------------
-- Runs

data RunFmt = RunFmt
  { rBold   :: Bool
  , rItal   :: Bool
  , rStrike :: Bool
  , rMono   :: Bool
  }

runInlines :: Ctx -> [XmlEvent] -> [Inline]
runInlines ctx inner =
  let content = runContent ctx inner
  in if null content then [] else wrapFmt (runFmt inner) content

-- w:rPr is the run's schema-first child (leading whitespace text
-- tolerated); anything else first means an unformatted run.
runFmt :: [XmlEvent] -> RunFmt
runFmt (XText _ : rest) = runFmt rest
runFmt (XStart "w:rPr" _ : rest) = fromPr (fst (balanced rest))
  where
    fromPr pr = RunFmt
      { rBold   = flagSet "w:b" pr
      , rItal   = flagSet "w:i" pr
      , rStrike = flagSet "w:strike" pr || flagSet "w:dstrike" pr
      , rMono   = case findStart "w:rFonts" pr of
          Just as -> isMono (attr "w:ascii" as)
          Nothing -> False
      }
runFmt _ = RunFmt False False False False

-- Toggle properties: present with no w:val (or a true-ish one) = on;
-- w:val 0/false/none/off = explicitly off.
flagSet :: String -> [XmlEvent] -> Bool
flagSet _ [] = False
flagSet nm (XStart n as : rest)
  | n == nm = case attr "w:val" as of
      Just v  -> not (v == "0" || v == "false" || v == "none" || v == "off")
      Nothing -> True
  | otherwise = flagSet nm rest
flagSet nm (_ : rest) = flagSet nm rest

findStart :: String -> [XmlEvent] -> Maybe [(String, String)]
findStart _ [] = Nothing
findStart nm (XStart n as : rest)
  | n == nm   = Just as
  | otherwise = findStart nm rest
findStart nm (_ : rest) = findStart nm rest

isMono :: Maybe String -> Bool
isMono Nothing  = False
isMono (Just f) = f `elem` ["Courier New", "Courier", "Consolas", "Monospace", "Mono"]

-- Document text lives in w:t; w:br/w:cr are hard breaks, w:tab a tab
-- stop ('\t', matching the RTF and ODT readers), w:drawing an image.
-- w:instrText (field instructions like " TOC \o ") is dropped —
-- w:fldSimple's cached display runs still render via the generic
-- recursion in `inlines`.
runContent :: Ctx -> [XmlEvent] -> [Inline]
runContent _ [] = []
runContent ctx (XStart "w:rPr" _ : rest) = runContent ctx (snd (balanced rest))
runContent ctx (XStart "w:t" _ : rest) =
  let (inner, after) = balanced rest
  in Str (textUnder inner) : runContent ctx after
runContent ctx (XStart "w:br" _ : rest) =
  LineBreak : runContent ctx (snd (balanced rest))
runContent ctx (XStart "w:cr" _ : rest) =
  LineBreak : runContent ctx (snd (balanced rest))
runContent ctx (XStart "w:tab" _ : rest) =
  Str "\t" : runContent ctx (snd (balanced rest))
runContent ctx (XStart "w:noBreakHyphen" _ : rest) =
  Str "-" : runContent ctx (snd (balanced rest))
runContent ctx (XStart "w:drawing" _ : rest) =
  let (inner, after) = balanced rest
  in imageOf ctx inner ++ runContent ctx after
runContent ctx (XStart "w:instrText" _ : rest) =
  runContent ctx (snd (balanced rest))
runContent ctx (XStart "mc:AlternateContent" _ : rest) =
  let (inner, after) = balanced rest
  in runContent ctx (firstBranch inner) ++ runContent ctx after
runContent ctx (XStart _ _ : rest) =
  let (inner, after) = balanced rest
  in runContent ctx inner ++ runContent ctx after
runContent ctx (_ : rest) = runContent ctx rest

-- Alt text from wp:docPr (descr, else name), target resolved from the
-- rels table via a:blip r:embed — the roadmap's "alt-text placeholder".
imageOf :: Ctx -> [XmlEvent] -> [Inline]
imageOf ctx evs = [Image [Str alt] url ""]
  where
    alt = case findStart "wp:docPr" evs of
      Just as -> case attr "descr" as of
        Just d | not (null d) -> d
        _ -> case attr "name" as of
          Just nm | not (null nm) -> nm
          _ -> "image"
      Nothing -> "image"
    url = case findStart "a:blip" evs of
      Just as -> case attr "r:embed" as of
        Just rid -> maybe "" id (lookup rid (cRels ctx))
        Nothing  -> ""
      Nothing -> ""

wrapFmt :: RunFmt -> [Inline] -> [Inline]
wrapFmt f is0 =
  let is1 = if rMono f then [CodeSpan (flatText is0)] else is0
      is2 = if rStrike f then [Strike is1] else is1
      is3 = if rItal f then [Emph is2] else is2
      is4 = if rBold f then [Strong is3] else is3
  in is4

flatText :: [Inline] -> String
flatText = concatMap f
  where
    f (Str t)       = t
    f LineBreak     = " "
    f (Emph is)     = flatText is
    f (Strong is)   = flatText is
    f (Strike is)   = flatText is
    f (CodeSpan t)  = t
    f (Link t _ _)  = flatText t
    f (Image t _ _) = flatText t
    f (MathI raw _) = raw
