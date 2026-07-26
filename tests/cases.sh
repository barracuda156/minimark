# Golden test case table for minimark.
#
# Each case is one line: name|input|args
# - name:  golden file is tests/golden/<name>.expected
# - input: path relative to tests/, usually ../../minimark_rendering/*.md
# - args:  minimark flags (input file appended by the runner)
#
# Add a case here, run `tests/run.sh --record <name>` once to seed its
# golden file, then commit the golden alongside the code change.

CASES='
math-ansi-bmp|math|-t term --glyphs=bmp --color=none --width=80
math-ansi-full|math|-t term --glyphs=full --color=none --width=80
math-ansi-ascii|math|-t term --glyphs=ascii --color=none --width=80
math-html|math|-t html -s --glyphs=bmp
math-latex|math|-t latex -s
math-plain|math|-t plain --glyphs=bmp --width=80
extensions-ansi-bmp|extensions|-t term --glyphs=bmp --color=none --width=80
extensions-ansi-full|extensions|-t term --glyphs=full --color=none --width=80
extensions-ansi-ascii|extensions|-t term --glyphs=ascii --color=none --width=80
extensions-html|extensions|-t html -s --glyphs=bmp
extensions-latex|extensions|-t latex -s
extensions-plain|extensions|-t plain --glyphs=bmp --width=80
frontmatter-ansi-bmp|frontmatter|-t term --glyphs=bmp --color=none --width=80
frontmatter-ansi-full|frontmatter|-t term --glyphs=full --color=none --width=80
frontmatter-ansi-ascii|frontmatter|-t term --glyphs=ascii --color=none --width=80
frontmatter-html|frontmatter|-t html -s --glyphs=bmp
frontmatter-latex|frontmatter|-t latex -s
frontmatter-plain|frontmatter|-t plain --glyphs=bmp --width=80
'

# Map a short input key to its file, relative to the tests/ directory.
input_path() {
  case "$1" in
    math)       echo '../../minimark_rendering/math-sample.md' ;;
    extensions) echo 'inputs/extensions.md' ;;
    frontmatter) echo 'inputs/frontmatter.md' ;;
    *)          echo "unknown input key: $1" >&2; exit 1 ;;
  esac
}
