#!/bin/sh
# End to end through GridCraft itself: loads the add-in in gridcraft-cli and checks formulas,
# the trust flow (per formula's workbook: plugin protocol 2.2), scripts, blanks and plots.
#
#     GRIDCRAFT_CLI=/path/to/gridcraft-cli sh tests/e2e.sh
#
# Build the CLI from a GridCraft checkout with `cargo build --release -p gridcraft-cli`.
set -u
CLI="${GRIDCRAFT_CLI:-gridcraft-cli}"
DIR=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
export GRIDCRAFT_R_DATA="$WORK/data"
unset GRIDCRAFT_R_TRUST
fails=0
n=0
expect() { # name, got, want
  n=$((n + 1))
  if [ "$2" != "$3" ]; then
    echo "FAIL: $1: got [$2], want [$3]"
    fails=$((fails + 1))
  fi
}
last() { tail -n 1 | sed 's/[[:space:]]*$//'; }
run() { "$CLI" --plugin "$DIR" "$@" 2>&1; }
R='plugins.run={"plugin":"io.github.sirmmo.r","command":'

expect "statistics need no trust" "$(run eval '=R.QUANTILE({1,2,3,4,5,6,7,8,9,10},0.9)' | last)" "9.1"
expect "R.VERSION" "$(run eval '=R.VERSION()' | last | cut -c1-14)" "GridCraft R 0."
expect "R.EVAL in a new, unsaved workbook (eval's Book1)" "$(run eval '=R.EVAL("1+1")' | last)" "2"
expect "GRIDCRAFT_R_TRUST=all" "$(env GRIDCRAFT_R_TRUST=all "$CLI" --plugin "$DIR" eval "=R.EVAL(\"paste(x, collapse='-')\",{1,2;3,4})" 2>&1 | last)" "1-3-2-4"

# A new workbook (Book1) is trusted: its formulas are yours.
out=$(run run --cmd 'cell.set={"cell":"A1","input":"=R.EVAL(\"1+1\")"}' \
  --cmd 'cell.set={"cell":"A2","input":"=R.CALL(\"sqrt\",16)"}' --print A1:A2)
expect "Book1: R on" "$(printf '%s\n' "$out" | tail -n 2 | tr -d ' ' | tr '\n' ',')" "2,4,"

# Saving a trusted workbook keeps it trusted in its file.
run run --cmd 'cell.set={"cell":"A1","input":"=R.EVAL(\"6*7\")"}' --cmd "file.save={\"path\":\"$WORK/mine.xlsx\"}" >/dev/null
expect "saved Book1 stays trusted" "$(run run --in "$WORK/mine.xlsx" --cmd 'formulas.calculateNow={}' --print A1 | last)" "42"

# Blanks: NA is an empty cell, not 0.
out=$(run run --cmd 'cell.set={"cell":"A1","input":"=R.EVAL(\"NA\")"}' --cmd 'cell.set={"cell":"B1","input":"=ISBLANK(A1)"}' \
  --cmd 'cell.set={"cell":"C1","input":"=A1&\"|\""}' --print C1)
expect "NA shows blank" "$(printf '%s\n' "$out" | last)" "|"

# A saved workbook: off until trusted, on afterwards, also when opened again.
# (Written without the add-in: a workbook from elsewhere. One saved from a trusted Book1 would
# stay trusted.)
"$CLI" run --cmd 'range.setValues={"range":"A1","values":[[1,10],[2,21],[3,29],[4,41]]}' \
  --cmd 'cell.set={"cell":"C1","input":"=R.EVAL(\"sum(x)\",A1:B4)"}' --out "$WORK/book.xlsx" >/dev/null 2>&1
expect "saved workbook: off" "$(run run --in "$WORK/book.xlsx" --cmd 'formulas.calculateNow={}' --print C1 | last)" "#VALUE!"
ask=$(run run --in "$WORK/book.xlsx" --cmd "${R}\"trust\"}" | last)
case "$ask" in *'"command":"trustConfirm"'*) ok=yes ;; *) ok=no ;; esac
expect "trust asks first" "$ok" "yes"
# Trusting recalculates at once (host/functions/invalidate): no Calculate Now.
expect "trusted" "$(run run --in "$WORK/book.xlsx" --cmd "${R}\"trustConfirm\",\"params\":{\"path\":\"$WORK/book.xlsx\",\"name\":\"book.xlsx\"}}" --print C1 | last)" "111"
# Opened again: the add-in hears of it before the first calculation.
expect "trusted, opened again" "$(run run --cmd "file.open={\"path\":\"$WORK/book.xlsx\"}" --print C1 | last)" "111"

# An untrusted workbook opened while a trusted one is open stays off (0.1.0's main risk).
cp "$WORK/book.xlsx" "$WORK/other.xlsx"
expect "untrusted beside a trusted one: off" "$(run run --in "$WORK/book.xlsx" --cmd "file.open={\"path\":\"$WORK/other.xlsx\"}" --cmd 'formulas.calculateNow={}' --print C1 | last)" "#VALUE!"
expect "the trusted one still on" "$(run run --in "$WORK/other.xlsx" --cmd "file.open={\"path\":\"$WORK/book.xlsx\"}" --print C1 | last)" "111"

# Scripts, the console, plots.
out=$(run run --in "$WORK/book.xlsx" --cmd 'selection.set={"range":"A1:B4"}' \
  --cmd "${R}\"runScript\",\"params\":{\"code\":\"data.frame(n=nrow(x), total=sum(x), txt='=1+1')\"}}" --print A6:C7)
expect "runScript writes below" "$(printf '%s\n' "$out" | tail -n 1 | tr -s ' ')" "4 111 =1+1"
out=$(run run --in "$WORK/book.xlsx" --cmd 'selection.set={"range":"A1:B4"}' \
  --cmd "${R}\"runScript\",\"params\":{\"code\":\"c('007', '1/2')\"}}" --print A6:A7)
expect "runScript writes text literally" "$(printf '%s\n' "$out" | tail -n 2 | tr -d ' ' | tr '\n' ',')" "007,1/2,"
out=$(run run --in "$WORK/book.xlsx" --cmd "${R}\"console\",\"params\":{\"code\":\"f <- function(a) a*3\"}}" \
  --cmd 'cell.set={"cell":"E1","input":"=R.CALL(\"f\",5)"}' --print E1)
expect "console definitions reach R.CALL" "$(printf '%s\n' "$out" | last)" "15"
if Rscript --vanilla -e 'quit(status = !(isTRUE(unname(capabilities("png"))) || isTRUE(unname(capabilities("cairo")))))' >/dev/null 2>&1; then
  out=$(run run --in "$WORK/book.xlsx" --cmd 'selection.set={"range":"A1:B4"}' \
    --cmd "${R}\"plot\",\"params\":{\"code\":\"plot(x[,1], x[,2], type='b')\",\"width\":400,\"height\":300}}" --out "$WORK/plot.xlsx")
  images=$("$CLI" info "$WORK/plot.xlsx" --json | grep -o '"images": *[0-9]*' | head -n 1 | tr -dc '0-9')
  expect "plot inserts a picture" "$images" "1"
else
  echo "(no PNG device: plot check skipped)"
fi

# Stop Trusting recalculates too.
expect "untrusted again: off at once" "$(run run --in "$WORK/book.xlsx" --cmd "${R}\"untrust\"}" --print C1 | last)" "#VALUE!"

echo "$n end-to-end checks, $fails failed"
[ "$fails" -eq 0 ]
