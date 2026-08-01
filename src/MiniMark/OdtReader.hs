-- minimark: ODF text reader (-f odt/fodt, T5.2).  fodt is flat,
-- uncompressed XML (<office:document>, everything inline); odt is the
-- same content zipped, with the body under content.xml's
-- <office:document-content> root — same body shape either way, so both
-- share one core walker (odtDoc) over MiniMark.Xml's event stream.
--
-- Unlike AbiWord's inline <c props="...">, ODF styling is INDIRECTED:
-- a <text:span text:style-name="T1"> or <text:p text:style-name="P2">
-- only names a style; the actual fo:font-weight/fo:font-style/
-- style:text-line-through-style/fo:font-family properties live in a
-- <style:style style:name="T1" ...><style:text-properties .../></...>
-- declared under <office:automatic-styles> (fodt: directly under
-- office:document; odt: same element name inside content.xml). So
-- parsing is two passes over the same flat event list: first build a
-- style-name -> StyleProps table from automatic-styles, then walk
-- office:text resolving every text:style-name reference against it.
-- A style name with no matching declaration renders unstyled — a
-- hand-edited or foreign-tool file is not grounds to crash.
--
-- Heading level: text:outline-level on <text:h>, capped at 3 like
-- every other writer/reader in this codebase (Heading Int only
-- distinguishes 1/2/3+ in practice — AbwReader made the same choice).
-- Lists: text:list/text:list-item -> BulletList (ODF's own ordering
-- comes from list-style, not from the content stream, so distinguishing
-- ordered from bullet here would require chasing yet another indirect
-- style reference for no real payoff — bullet is the safe, honest
-- default and the design note's own Sonnet-tier readers make the same
-- call for formats without an inline list marker). Tables:
-- table:table/table:table-row/table:table-cell, first row is the
-- header (matches every other reader/writer in this codebase).
--
-- Malformed/unexpected ODF never crashes: MiniMark.Xml is already
-- total, and every event walk here degrades unknown elements to their
-- text content or drops them, exactly like AbwReader.
module MiniMark.OdtReader(parseFodt, parseOdt) where

import Data.ByteString(ByteString)
import MiniMark.AST
import MiniMark.Xml(XmlEvent(..), parseXml, xmlLooksUtf16, balanced, attr)
import MiniMark.Zip(zipEntries, zipFind, zipExtract)

parseFodt :: ByteString -> Doc
parseFodt bs = odtDoc (parseXml bs)

-- odt is a zip; content.xml holds the body.  Any failure (bad zip,
-- missing member, corrupt stream) is a clean refusal, never a crash —
-- matching the container error handling MiniMark.Zip already does.
parseOdt :: ByteString -> IO (Either String Doc)
parseOdt bs = case zipEntries bs of
  Left e -> return (Left e)
  Right es -> case zipFind es "content.xml" of
    Nothing -> return (Left "odt: no content.xml member")
    Just en -> do
      r <- zipExtract bs en
      case r of
        Left e -> return (Left ("content.xml: " ++ e))
        Right cbs
          | xmlLooksUtf16 cbs -> return (Left "content.xml: UTF-16 XML not supported")
          | otherwise         -> return (Right (odtDoc (parseXml cbs)))

--------------------------------------------------------------------------
-- Shared core over a flat XmlEvent stream (fodt's whole file, or odt's
-- extracted content.xml — both have the same automatic-styles + body
-- shape once parsed).

odtDoc :: [XmlEvent] -> Doc
odtDoc evs = Doc emptyMeta (blocks styles (findText evs))
  where styles = collectStyles evs

-- <office:text> holds the body; found anywhere in the stream so it
-- doesn't matter whether the root is <office:document> (fodt) or
-- <office:document-content> (odt's content.xml) or something else
-- entirely wraps it.  Missing (malformed input): empty document.
findText :: [XmlEvent] -> [XmlEvent]
findText [] = []
findText (XStart "office:text" _ : rest) = fst (balanced rest)
findText (_ : rest) = findText rest

--------------------------------------------------------------------------
-- Styles.  A name -> StyleProps table built from every
-- <style:style style:name="N" ...><style:text-properties .../>...</>
-- found anywhere in the stream (automatic-styles is the only place
-- ODF puts these in practice, but scanning the whole event list rather
-- than locating that element specifically costs nothing and is more
-- forgiving of producers that nest things differently).

data StyleProps = StyleProps
  { spBold  :: Bool
  , spItal  :: Bool
  , spStrike :: Bool
  , spMono  :: Bool
  }

noProps :: StyleProps
noProps = StyleProps False False False False

collectStyles :: [XmlEvent] -> [(String, StyleProps)]
collectStyles [] = []
collectStyles (XStart "style:style" as : rest) =
  let (inner, after) = balanced rest
  in case attr "style:name" as of
       Just nm -> (nm, propsFromDecl inner) : collectStyles after
       Nothing -> collectStyles after
collectStyles (_ : rest) = collectStyles rest

-- Find the (first) <style:text-properties> inside a style declaration
-- and read its fo:*/style:* attributes; a style with none (e.g. a
-- paragraph style with only margin/alignment props, nothing this
-- reader models) is simply unstyled.
propsFromDecl :: [XmlEvent] -> StyleProps
propsFromDecl [] = noProps
propsFromDecl (XStart "style:text-properties" as : _) = StyleProps
  { spBold   = attr "fo:font-weight" as == Just "bold"
  , spItal   = attr "fo:font-style" as == Just "italic"
  , spStrike = case attr "style:text-line-through-style" as of
                 Just v -> v /= "none" && v /= ""
                 Nothing -> False
  , spMono   = isMono (attr "fo:font-family" as)
  }
propsFromDecl (_ : rest) = propsFromDecl rest

-- Font-family values come quoted in real ODF ("Courier New") as well
-- as bare; strip a matching pair of "..." before comparing, and accept
-- a handful of common monospace family names rather than just one.
isMono :: Maybe String -> Bool
isMono Nothing = False
isMono (Just raw) = unquoted `elem` monoFamilies
  where
    unquoted = case raw of
      '"' : r | not (null r) && last r == '"' -> init r
      _ -> raw
    monoFamilies = ["Courier New", "Courier", "Consolas", "Monospace", "Mono"]

lookupStyle :: [(String, StyleProps)] -> Maybe String -> StyleProps
lookupStyle _ Nothing = noProps
lookupStyle tbl (Just nm) = case [p | (n, p) <- tbl, n == nm] of
  (p : _) -> p
  []      -> noProps

--------------------------------------------------------------------------
-- Blocks

blocks :: [(String, StyleProps)] -> [XmlEvent] -> [Block]
blocks _ [] = []
blocks tbl (XStart "text:h" as : rest) =
  let (inner, after) = balanced rest
      lvl = case attr "text:outline-level" as of
              Just v  -> capLevel (parseIntDefault 1 v)
              Nothing -> 1
  in Heading lvl (inlines tbl inner) : blocks tbl after
blocks tbl (XStart "text:p" as : rest) =
  let (inner, after) = balanced rest
  in Para (inlines tbl inner) : blocks tbl after
blocks tbl (XStart "text:list" _ : rest) =
  let (inner, after) = balanced rest
  in BulletList (listItems tbl inner) : blocks tbl after
blocks tbl (XStart "table:table" _ : rest) =
  let (inner, after) = balanced rest
      (hdr, body) = tableRows tbl inner
  in Table [] hdr body : blocks tbl after
blocks tbl (XStart _ _ : rest) =
  -- Unknown container: skip its content, keep walking siblings.
  blocks tbl (snd (balanced rest))
blocks tbl (_ : rest) = blocks tbl rest

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

-- Every <text:list-item> becomes one ListItem; ODF's own ordered/
-- bullet distinction lives in a list-style indirection this reader
-- doesn't chase (see module comment) — always unchecked, bullet-style.
listItems :: [(String, StyleProps)] -> [XmlEvent] -> [ListItem]
listItems _ [] = []
listItems tbl (XStart "text:list-item" _ : rest) =
  let (inner, after) = balanced rest
  in ListItem Nothing (blocks tbl inner) : listItems tbl after
listItems tbl (_ : rest) = listItems tbl rest

--------------------------------------------------------------------------
-- Tables

tableRows :: [(String, StyleProps)] -> [XmlEvent] -> ([[Inline]], [[[Inline]]])
tableRows tbl evs = case rows of
  []           -> ([], [])
  (hdr : body) -> (hdr, body)
  where rows = collectRows tbl evs

collectRows :: [(String, StyleProps)] -> [XmlEvent] -> [[[Inline]]]
collectRows _ [] = []
collectRows tbl (XStart "table:table-row" _ : rest) =
  let (inner, after) = balanced rest
  in collectCells tbl inner : collectRows tbl after
collectRows tbl (_ : rest) = collectRows tbl rest

collectCells :: [(String, StyleProps)] -> [XmlEvent] -> [[Inline]]
collectCells _ [] = []
collectCells tbl (XStart "table:table-cell" _ : rest) =
  let (inner, after) = balanced rest
  in cellInlines tbl inner : collectCells tbl after
collectCells tbl (_ : rest) = collectCells tbl rest

-- A cell's content is one or more <text:p>/<text:h> paragraphs; pull
-- inline content out of each, ignoring whitespace-only XText sitting
-- directly in the cell (pretty-printing artifacts between tags).
-- Multiple paragraphs join with a line break, same convention as
-- AbwReader's cellInlines.
cellInlines :: [(String, StyleProps)] -> [XmlEvent] -> [Inline]
cellInlines tbl evs = case [is | is <- map paraOf (paras evs), not (null is)] of
  []    -> []
  [one] -> one
  many  -> intercalateInlines LineBreak many
  where
    paraOf (nm, content)
      | nm == "text:p" || nm == "text:h" = inlines tbl content
      | otherwise = []
    paras [] = []
    paras (XStart nm _ : rs)
      | nm == "text:p" || nm == "text:h" =
          let (c, r) = balanced rs in (nm, c) : paras r
    paras (_ : rs) = paras rs

intercalateInlines :: Inline -> [[Inline]] -> [Inline]
intercalateInlines sep = go
  where
    go []       = []
    go [x]      = x
    go (x : xs) = x ++ [sep] ++ go xs

--------------------------------------------------------------------------
-- Inlines

inlines :: [(String, StyleProps)] -> [XmlEvent] -> [Inline]
inlines _ [] = []
inlines tbl (XText t : rest) = Str t : inlines tbl rest
inlines tbl (XStart "text:line-break" _ : rest) = LineBreak : inlines tbl rest
-- ODF encodes whitespace structurally: <text:tab/> is a tab stop and
-- <text:s text:c="N"/> a run of N spaces (default 1; LibreOffice emits
-- one for any run of two or more).  Dropping them under the unknown-
-- element rule would glue adjacent words together, so map them back to
-- literal text ('\t' matching the RTF reader's \tab).  text:c is
-- capped so a hostile attribute cannot balloon memory.
inlines tbl (XStart "text:tab" _ : rest) =
  let (_, after) = balanced rest
  in Str "\t" : inlines tbl after
inlines tbl (XStart "text:s" as : rest) =
  let (_, after) = balanced rest
      c = case attr "text:c" as of
            Just v  -> max 0 (min 4096 (parseIntDefault 1 v))
            Nothing -> 1
  in Str (replicate c ' ') : inlines tbl after
inlines tbl (XStart "text:span" as : rest) =
  let (inner, after) = balanced rest
      p = lookupStyle tbl (attr "text:style-name" as)
  in wrapStyle p (inlines tbl inner) ++ inlines tbl after
inlines tbl (XStart "text:a" as : rest) =
  let (inner, after) = balanced rest
      url = maybe "" id (attr "xlink:href" as)
  in Link (inlines tbl inner) url "" : inlines tbl after
inlines tbl (XStart _ _ : rest) =
  -- Unknown inline element: keep its text, drop the wrapper.
  let (inner, after) = balanced rest
  in inlines tbl inner ++ inlines tbl after
inlines tbl (_ : rest) = inlines tbl rest

-- Wrap already-built inline content per a resolved style's flags.
-- CodeSpan only applies cleanly to flat text, matching AbwReader's
-- own reasoning for its font-family case.
wrapStyle :: StyleProps -> [Inline] -> [Inline]
wrapStyle p is0 =
  let is1 = if spMono p then [CodeSpan (flatText is0)] else is0
      is2 = if spStrike p then [Strike is1] else is1
      is3 = if spItal p then [Emph is2] else is2
      is4 = if spBold p then [Strong is3] else is3
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
