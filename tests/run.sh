#!/bin/sh
# Golden-test harness for minimark. Plain sh, no bashisms — must run on the
# ppc box as-is. Usage:
#
#   tests/run.sh                 run all cases, report PASS/FAIL
#   tests/run.sh NAME [NAME...]  run only the named case(s)
#   tests/run.sh --record NAME   (re)generate the golden file for NAME
#   tests/run.sh --record-all    (re)generate every golden file
#
# The binary under test is $MINIMARK, default ../minimark (built by
# `make` in the project root). Override for the dist/ C build:
#   MINIMARK=../dist-build/minimark tests/run.sh

set -eu

cd "$(dirname "$0")"
. ./cases.sh

: "${MINIMARK:=../minimark}"

if [ ! -x "$MINIMARK" ]; then
  echo "tests/run.sh: no binary at $MINIMARK — build with 'make' first" >&2
  exit 1
fi

find_case() {
  # Prints "input_key<TAB>args" for case $1, or exits 1 if not found.
  name="$1"
  old_ifs=$IFS
  IFS='
'
  for case_line in $CASES; do
    IFS=$old_ifs
    [ -z "$case_line" ] && continue
    case_name=$(echo "$case_line" | cut -d'|' -f1)
    if [ "$case_name" = "$name" ]; then
      input_key=$(echo "$case_line" | cut -d'|' -f2)
      args=$(echo "$case_line" | cut -d'|' -f3-)
      printf '%s\t%s\n' "$input_key" "$args"
      return 0
    fi
    IFS='
'
  done
  IFS=$old_ifs
  return 1
}

record_one() {
  name="$1"
  line=$(find_case "$name") || { echo "tests/run.sh: no such case: $name" >&2; exit 1; }
  input_key=$(printf '%s' "$line" | cut -f1)
  args=$(printf '%s' "$line" | cut -f2)
  infile=$(input_path "$input_key")
  mkdir -p golden
  # shellcheck disable=SC2086
  "$MINIMARK" $args "$infile" > "golden/$name.expected"
  echo "recorded golden/$name.expected"
}

run_one() {
  name="$1"
  line=$(find_case "$name") || { echo "tests/run.sh: no such case: $name" >&2; exit 1; }
  input_key=$(printf '%s' "$line" | cut -f1)
  args=$(printf '%s' "$line" | cut -f2)
  infile=$(input_path "$input_key")
  expected="golden/$name.expected"
  if [ ! -f "$expected" ]; then
    echo "FAIL $name (no golden file; run: tests/run.sh --record $name)"
    return 1
  fi
  actual="/tmp/minimark-test-$name.$$"
  errfile="/tmp/minimark-test-$name.$$.err"
  # shellcheck disable=SC2086
  if ! "$MINIMARK" $args "$infile" > "$actual" 2>"$errfile"; then
    echo "FAIL $name (minimark exited nonzero)"
    cat "$errfile" >&2
    rm -f "$actual" "$errfile"
    return 1
  fi
  rm -f "$errfile"
  if cmp -s "$expected" "$actual"; then
    echo "PASS $name"
    rm -f "$actual"
    return 0
  else
    echo "FAIL $name (output differs from $expected)"
    diff -u "$expected" "$actual" 2>/dev/null | head -20 || true
    rm -f "$actual"
    return 1
  fi
}

all_names() {
  old_ifs=$IFS
  IFS='
'
  for case_line in $CASES; do
    IFS=$old_ifs
    [ -z "$case_line" ] && continue
    echo "$case_line" | cut -d'|' -f1
    IFS='
'
  done
  IFS=$old_ifs
}

case "${1:-}" in
  --record)
    shift
    for n in "$@"; do record_one "$n"; done
    ;;
  --record-all)
    for n in $(all_names); do record_one "$n"; done
    ;;
  '')
    fail=0
    for n in $(all_names); do
      run_one "$n" || fail=1
    done
    exit "$fail"
    ;;
  *)
    fail=0
    for n in "$@"; do
      run_one "$n" || fail=1
    done
    exit "$fail"
    ;;
esac
