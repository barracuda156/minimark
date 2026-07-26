-- minimark: shared document AST for all readers/writers.
module MiniMark.AST where

-- Document wrapper: metadata + content.  Readers produce a Doc,
-- writers consume one.  Meta comes from front matter (markdown, T1.4)
-- or document properties (future binary readers); until those land the
-- readers fill in emptyMeta and the writers ignore it.
data Doc = Doc Meta [Block]

data Meta = Meta
  { mTitle  :: Maybe String
  , mAuthor :: Maybe String
  , mDate   :: Maybe String
  }

emptyMeta :: Meta
emptyMeta = Meta Nothing Nothing Nothing

data Block
  = Heading Int [Inline]
  | Para [Inline]
  | CodeBlock String [String]        -- language ("" if none), source lines
  | BulletList [ListItem]
  | OrderedList Int [ListItem]       -- start number, items
  | Quote [Block]
  | HRule
  | Table [Align] [[Inline]] [[[Inline]]]  -- column aligns, header cells, body rows
  | DisplayMath String [MExpr]       -- raw TeX (lossless), parsed form

data Align = ALeft | ACenter | ARight

data ListItem = ListItem (Maybe Bool) [Block]  -- Just True = checked

data Inline
  = Str String                       -- always newline-free (breaks are LineBreak)
  | LineBreak                        -- a hard, forced line break within a block
  | Emph [Inline]
  | Strong [Inline]
  | Strike [Inline]                  -- ~~x~~
  | CodeSpan String
  | Link [Inline] String String      -- text, url, title ("" = none)
  | Image [Inline] String String     -- alt, url, title ("" = none)
  | MathI String [MExpr]             -- raw TeX (lossless), parsed form

-- TeX-math subset. Raw source is kept alongside in MathI/DisplayMath,
-- so writers that want TeX (the LaTeX writer, --glyphs=ascii) never
-- depend on this parse being complete.
data MExpr
  = MChar Char                       -- resolved symbol or literal char
  | MText String                     -- upright text (\text{..}, sin, Hom, ...)
  | MGroup [MExpr]                   -- { ... }
  | MScript MExpr (Maybe [MExpr]) (Maybe [MExpr])  -- base, _sub, ^sup
  | MFrac [MExpr] [MExpr]
  | MSqrt [MExpr]
  | MStyle MStyle [MExpr]
  | MColor String [MExpr]            -- \textcolor{name}{..}, name verbatim
  | MUnknown String                  -- unrecognized \command, kept verbatim

data MStyle = SBb | SCal | SFrak | SBold | SItal | SRoman
  deriving (Eq)
