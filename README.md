# minimark

Terminal Markdown viewer / mini-pandoc with TeX math, written in
**MicroHs** Haskell. Built for macOS PowerPC (10.5/10.6) but runs
anywhere a C compiler exists.

```
minimark FILE.md                        # render to terminal (ANSI)
minimark -t html -s FILE.md -o out.html # convert to standalone HTML
minimark -t latex -s FILE.md            # convert to LaTeX
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

## Color (`--color=`)

`auto` (default; picks `true` if `$COLORTERM` says truecolor, else `16`),
`16` (safe in Apple Terminal on 10.5/10.6), `true` (contour, mlterm),
`none`. Emphasis uses italics only when the mode is truecolor or
`--italics=on` — Apple Terminal pre-10.7 has no italic.

## Building

Development build (needs `mhs` from MicroHs):

```
make        # uses mhs -C; MHS=/path/to/mhs to override
```

### PowerPC / any box with only a C compiler

`dist/` carries pregenerated portable C plus the MicroHs runtime
(Apache-2.0). On the target machine:

```
cc -O2 -Idist/runtime -Idist/runtime/unix \
   dist/runtime/main.c dist/runtime/eval.c dist/minimark.c \
   -DHEAP_CELLS=8000000 -lm -o minimark
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
