# minimark

Terminal Markdown viewer / mini-pandoc with TeX math, written in
**MicroHs** Haskell. Built for macOS PowerPC (10.5/10.6) but runs
anywhere a C compiler exists.

```
minimark FILE.md                        # render to terminal (ANSI)
minimark -t html -s FILE.md -o out.html # convert to standalone HTML
minimark -t latex -s FILE.md            # convert to LaTeX
minimark -t man FILE.md                 # convert to man(7) / roff
minimark -t plain FILE.md               # no escapes (pipes, files)
```

Input format is Markdown; `-f rtf|odt|docx|idml` reserves the seam for
the readers in progress (auto-detected from magic bytes / extension —
today these give a clean "not implemented yet" instead of mojibake).

## Features

- headings, emphasis/strong, inline code, links (incl. bare URLs), images-as-links
- fenced code blocks with framed rendering + language label
- nested ordered/unordered lists, blockquotes (incl. lazy continuation), hrules
- pipe tables with `:---:` alignment, box-drawing output, wide-char/emoji aware
- **TeX math**: `$...$` and `$$...$$` — symbols (~260 commands: category
  theory arrows, econ preference orders, Greek, operators), `^`/`_`
  scripts, `\frac`, `\sqrt`, `\mathbb/\mathcal/\mathfrak/\mathbf`,
  `\text`, accents (`\hat`, `\bar`, `\vec`, ...), `\left/\right`
- paragraph reflow to `--width` / `$COLUMNS` (default 80)
- graceful degradation: malformed input never crashes; unknown TeX
  commands render verbatim; LaTeX output always uses the *raw* TeX
  (lossless passthrough)

## Glyph levels (`--glyphs=`)

| level | meaning |
|---|---|
| `bmp` (default) | BMP codepoints only — `\mathbb{R}`→ℝ via letterlike block; safe with 10.5/10.6 system fonts |
| `full` | plane-1 math alphabets — `\mathcal{C}`→𝒞, `\pi_i`→πᵢ with full sub/superscript letters; needs modern font coverage (contour + fallback works) |
| `ascii` | math rendered as raw TeX — for terminals with no Unicode |

## man / roff output (`-t man`)

Emits man(7) macros (`.SH`/`.SS` headings, `.PP` paragraphs, `.IP`
lists, `.TS`/`.TE` tables via tbl, `\fB`/`\fI` bold/italic). Code
blocks use `.EX`/`.EE`, which need groff 1.22+; a `.nf`/`.fi` comment
sits next to each block in the output as a fallback note for older
groff. `.TH` is built from front-matter `title`/`date` when present,
else `--title` / the first filename, section 7. Non-ASCII characters
escape to `\[uXXXX]` (`groff_char(7)` form) so output is portable to
plain nroff/troff without a UTF-8 locale.

## Color (`--color=`)

`auto` (default; picks `true` if `$COLORTERM` says truecolor, else `16`),
`16` (safe in Apple Terminal on 10.5/10.6), `true` (contour, mlterm),
`none`. Emphasis uses italics only when the mode is truecolor or
`--italics=on` — Apple Terminal pre-10.7 has no italic.

## Container plumbing (`--zip=`, `--gunzip`)

The zip/gzip machinery under the future docx/odt/idml readers is
exposed directly — handy on its own, and it is how the container layer
gets exercised on real files:

```
minimark --zip=list FILE.docx                     # list members
minimark --zip=word/document.xml FILE.docx        # member to stdout (UTF-8)
minimark --zip=word/document.xml FILE.docx -o x.xml   # exact bytes
minimark --gunzip FILE.gz [-o FILE]               # same for gzip
```

Stored and deflated members are supported (the only methods office
formats use); everything is CRC-checked. zip64, multi-disk archives,
encrypted members and >=2GB fields are refused with clean errors.

## Building

Development build (needs `mhs` from MicroHs, a C compiler, zlib):

```
make        # mhs generates minimark.c, cc compiles it with the
            # vendored dist/runtime and cbits/ zlib shim
```

### PowerPC / any box with only a C compiler

`dist/` carries pregenerated portable C plus the MicroHs runtime
(Apache-2.0); `cbits/` holds the small zlib shim behind the container
readers (zlib ships with Mac OS X — nothing to install). On the target
machine:

```
cc -O2 -Idist/runtime -Idist/runtime/unix -Icbits \
   dist/runtime/main.c dist/runtime/eval.c dist/minimark.c cbits/mm_zlib.c \
   -DHEAP_CELLS=8000000 -lm -lz -o minimark
```

`-DHEAP_CELLS=8000000` bakes a ~64MB default heap (32-bit) instead of
MicroHs's 400MB+ default — right-sized for a 1GB G4. Override at run
time with `minimark +RTS -H16M -RTS ...` if ever needed.

## Testing

`make test` (or `tests/run.sh` directly, plain `sh` — works on the ppc
box) diffs each writer/flag combo against golden files in
`tests/golden/`. Add a case to `tests/cases.sh`, then
`tests/run.sh --record NAME` to seed its golden.

## Notes for MicroHs hackers

`Data.Char.isSpace`/`isAlpha`/... fall back to a lazily-built full
Unicode table for non-ASCII input — first touch costs ~0.5s on x86,
several seconds on a G4. `MiniMark.CharClass` provides ASCII-only
predicates; Markdown/TeX syntax is ASCII, so the hot paths never force
the table regardless of document content.
