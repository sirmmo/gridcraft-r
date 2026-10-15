#!/bin/sh
# End to end through GridCraft itself: loads the add-in in gridcraft-cli and checks formulas,
# the trust flow, scripts and plots.
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
expect "R.EVAL is off in an unknown workbook" "$(run eval '=R.EVAL("1+1")' | last)" "#VALUE!"
expect "GRIDCRAFT_R_TRUST=all" "$(env GRIDCRAFT_R_TRUST=all "$CLI" --plugin "$DIR" eval "=R.EVAL(\"paste(x, collapse='-')\",{1,2;3,4})" 2>&1 | last)" "1-3-2-4"

# A new workbook (Book1) is trusted once a command has told the add-in about it.
out=$(run run --cmd 'cell.set={"cell":"A1","input":"=R.EVAL(\"1+1\")"}' \
  --cmd "${R}\"trust\"}" --cmd 'cell.set={"cell":"A2","input":"=R.CALL(\"sqrt\",16)"}' \
  --cmd 'formulas.calculateNow={}' --print A1:A2)
expect "Book1: R on after a command" "$(printf '%s\n' "$out" | tail -n 2 | tr -d ' ' | tr '\n' ',')" "2,4,"

# A saved workbook: off until trusted, on afterwards, also when opened again.
run run --cmd 'range.setValues={"range":"A1","values":[[1,10],[2,21],[3,29],[4,41]]}' \
  --cmd 'cell.set={"cell":"C1","input":"=R.EVAL(\"sum(x)\",A1:B4)"}' --out "$WORK/book.xlsx" >/dev/null
expect "saved workbook: off" "$(run run --in "$WORK/book.xlsx" --cmd 'formulas.calculateNow={}' --print C1 | last)" "#VALUE!"
ask=$(run run --in "$WORK/book.xlsx" --cmd "${R}\"trust\"}" | last)
case "$ask" in *'"command":"trustConfirm"'*) ok=yes ;; *) ok=no ;; esac
expect "trust asks first" "$ok" "yes"
expect "trusted" "$(run run --in "$WORK/book.xlsx" --cmd "${R}\"trustConfirm\",\"params\":{\"path\":\"$WORK/book.xlsx\",\"name\":\"book.xlsx\"}}" --print C1 | last)" "111"
expect "trusted, opened again" "$(run run --cmd "file.open={\"path\":\"$WORK/book.xlsx\"}" --cmd 'formulas.calculateNow={}' --print C1 | last)" "111"

# Scripts, the console, plots.
out=$(run run --in "$WORK/book.xlsx" --cmd 'selection.set={"range":"A1:B4"}' \
  --cmd "${R}\"runScript\",\"params\":{\"code\":\"data.frame(n=nrow(x), total=sum(x), txt='=1+1')\"}}" --print A6:C7)
expect "runScript writes below" "$(printf '%s\n' "$out" | tail -n 1 | tr -s ' ')" "4 111 =1+1"
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

echo "$n end-to-end checks, $fails failed"
[ "$fails" -eq 0 ]
