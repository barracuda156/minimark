-- minimark: shared document AST for all readers/writers.
module MiniMark.AST where

data Block
  = Heading Int [Inline]
  | Para [Inline]
  | CodeBlock String [String]        -- language ("" if none), source lines
  | BulletList [[Block]]
  | OrderedList Int [[Block]]        -- start number, items
  | Quote [Block]
  | HRule
  | Table [Align] [[Inline]] [[[Inline]]]  -- column aligns, header cells, body rows
  | DisplayMath String [MExpr]       -- raw TeX (lossless), parsed form

data Align = ALeft | ACenter | ARight

data Inline
  = Str String
  | Emph [Inline]
  | Strong [Inline]
  | CodeSpan String
  | Link [Inline] String             -- text, url
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
  | MUnknown String                  -- unrecognized \command, kept verbatim

data MStyle = SBb | SCal | SFrak | SBold | SItal | SRoman
