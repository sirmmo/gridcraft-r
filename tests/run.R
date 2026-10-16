# GridCraft R's tests: the JSON reader and writer, the conversions, then the add-in itself driven
# over standard input and output the way GridCraft drives it (one JSON-RPC message a line).
#
#     Rscript tests/run.R            (from the repository's folder; base R only)
#
# Exits with status 1 when a check fails.

args <- commandArgs(trailingOnly = FALSE)
me <- sub("^--file=", "", args[grep("^--file=", args)][1L])
root <- normalizePath(file.path(if (is.na(me)) "tests" else dirname(me), ".."))
Sys.setenv(GRIDCRAFT_R_NO_MAIN = "1")
source(file.path(root, "gridcraft_r.R"))
Sys.unsetenv("GRIDCRAFT_R_NO_MAIN")
g <- .gridcraft_r

failures <- 0L
checks <- 0L
check <- function(name, ok) {
  checks <<- checks + 1L
  if (!isTRUE(ok)) {
    failures <<- failures + 1L
    cat("FAIL:", name, "\n")
  }
}
same <- function(name, got, want) {
  ok <- isTRUE(all.equal(got, want, check.attributes = FALSE))
  if (!ok) {
    cat("  got: "); utils::str(got)
    cat("  want: "); utils::str(want)
  }
  check(name, ok)
}
fails <- function(name, expr) check(name, inherits(tryCatch({ expr; NULL }, error = function(e) e), "error"))

# Ranges come back from the reader compactly: as plain lists of rows for the checks.
plain <- function(x) {
  if (inherits(x, "json_rows")) {
    return(lapply(seq_len(x$nrow), function(r) {
      w <- which(x$row == r)
      lapply(w, function(i) switch(x$type[i] - 6L, x$str[i], x$num[i], TRUE, FALSE, NA))
    }))
  }
  if (is.list(x)) {
    nm <- names(x)
    x <- lapply(x, plain)
    names(x) <- nm
  }
  x
}

# ---- JSON reading -----------------------------------------------------------------------------
P <- g$json_parse
same("number", P("42"), 42)
same("negative exponent", P("-1.5E-2"), -0.015)
same("exponent", P("1e3"), 1000)
same("true", P("true"), TRUE)
same("null is NA", P("null"), NA)
same("string", P("\"abc\""), "abc")
same("escapes", P("\"a\\\"b\\\\c\\/d\\n\\t\""), "a\"b\\c/d\n\t")
same("unicode escape", utf8ToInt(P("\"\\u00e9\"")), 0xE9)
same("surrogate pair", utf8ToInt(P("\"\\ud83d\\ude00\"")), 0x1F600)
same("lone surrogate", utf8ToInt(P("\"\\ud83dx\"")), c(0xFFFD, 0x78))
same("escaped backslash before u", P("\"\\\\u0041\""), "\\u0041")
same("raw UTF-8", utf8ToInt(P("\"\u00e9\U0001F600\"")), c(0xE9, 0x1F600))
same("NUL dropped", P("\"a\\u0000b\""), "ab")
o <- P("{\"a\": 1, \"b\": [true, null, \"x\"], \"a\": 2, \"c\": {}}")
same("object keys", names(o), c("b", "a", "c"))
same("duplicate key: last wins", o$a, 2)
same("array in object", o$b, list(TRUE, NA, "x"))
check("empty object", is.list(o$c) && length(o$c) == 0L && !is.null(names(o$c)))
same("empty array", P("[]"), list())
same("nested", P("[[1,[2,[3]]],{\"k\":[]}]")[[1L]][[2L]][[2L]], list(3))
r <- P("[[1,\"a\"],[null,false],[2.5,true]]")
check("rows read compactly", inherits(r, "json_rows") && r$nrow == 3L)
same("rows as grid", g$grid_value(g$as_grid(r)), matrix(c("1", NA, "2.5", "a", "FALSE", "TRUE"), 3))
r <- P("[[1,{\"error\":\"#N/A\"}],[3,4]]")
check("rows with an error: plain lists", !inherits(r, "json_rows") && is.list(r))
same("error in a range", g$grid_error(g$as_grid(r)), "#N/A")
same("ragged rows padded with #N/A", g$grid_error(g$as_grid(P("[[1,2],[3]]"))), "#N/A")
fails("empty text", P(""))
fails("unclosed array", P("[1,"))
fails("missing comma", P("[1 2]"))
fails("missing colon", P("{\"a\" 1}"))
fails("bad literal", P("tru"))
fails("unclosed string", P("\"abc"))
fails("extra bracket", P("[]]"))
fails("leading zero", P("01"))
fails("trailing data", P("1 2"))
fails("wrong closing bracket", P("[1}"))
fails("too deep", P(paste0(strrep("[", 200), strrep("]", 200))))
same("64 deep is fine", length(P(paste0(strrep("[", 64), strrep("]", 64)))), 1L)

# ---- JSON writing -----------------------------------------------------------------------------
J <- g$to_json
same("write number", J(0.1), "0.1")
same("write third", P(J(1/3)), 1/3)
same("write big", P(J(1e300)), 1e300)
same("write integer", J(3L), "3")
same("write NA", J(NA), "null")
same("write NaN as null", J(NaN), "null")
same("write vector", J(c(1, 2)), "[1,2]")
same("write I()", J(I("a")), "[\"a\"]")
same("write object", J(list(a = 1, b = list())), "{\"a\":1,\"b\":[]}")
same("write empty object", J(structure(list(), names = character(0))), "{}")
same("write empty I()", J(I(character(0))), "[]")
tricky <- paste0("q\"b\\s/\n\t", rawToChar(as.raw(1L)), "\u00e9\U0001F600")
same("string round trip", P(J(tricky)), tricky)
g$ascii_out <- TRUE
w <- J(tricky)
check("ASCII output", all(utf8ToInt(w) < 128L))
same("ASCII round trip", P(w), tricky)
check("ASCII surrogates", grepl("\\ud83d\\ude00", w, fixed = TRUE))
g$ascii_out <- FALSE
same("base64 3", g$base64(charToRaw("Man")), "TWFu")
same("base64 2", g$base64(charToRaw("Ma")), "TWE=")
same("base64 1", g$base64(charToRaw("M")), "TQ==")
same("base64 0", g$base64(raw(0)), "")
same("base64 binary", g$base64(as.raw(c(0, 255, 128, 1))), "AP+AAQ==")

# ---- R values to cells ------------------------------------------------------------------------
V <- function(v) plain(P(g$cells_value(g$value_cells(v))))
same("scalar", V(2), 2)
same("vector is a column", V(1:3), list(list(1), list(2), list(3)))
same("matrix", V(matrix(1:4, 2)), list(list(1, 3), list(2, 4)))
same("NA is empty text", V(c(1, NA)), list(list(1), list("")))
same("Inf and NaN are #NUM!", V(c(Inf, NaN))[[2L]][[1L]], list(error = "#NUM!"))
same("logical", V(c(TRUE, FALSE)), list(list(TRUE), list(FALSE)))
same("factor is text", V(factor("b")), "b")
same("Date is ISO text", V(as.Date("2024-01-05")), "2024-01-05")
same("named vector: two columns", V(c(a = 1, b = 2)), list(list("a", 1), list("b", 2)))
df <- V(data.frame(n = 1:2, s = c("x", NA), stringsAsFactors = TRUE))
same("data frame header", df[[1L]], list("n", "s"))
same("data frame body", df[[3L]], list(2, ""))
same("NULL is empty", V(NULL), "")
check("a model prints", is.list(V(stats::lm(dist ~ speed, cars))))
same("too big is #NUM!", tryCatch(g$value_cells(numeric(2e6)), error = function(e) conditionMessage(e)), "#NUM!")
same("long text is cut", nchar(V(strrep("a", 40000))), 32767L)
same("commands keep text as text", g$value_cells("=1+1", quote_text = TRUE)[1L, 1L], "\"'=1+1\"")
same("commands write dates as dates", g$value_cells(as.Date("2024-01-05"), quote_text = TRUE)[1L, 1L], "\"2024-01-05\"")

# ---- The add-in over standard input and output ------------------------------------------------
data_dir <- tempfile("gridcraft-r-test-")
invisible(dir.create(data_dir))
Sys.setenv(GRIDCRAFT_R_DATA = data_dir, GRIDCRAFT_R_TIME_LIMIT = "2")
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

# Sends `lines` to a fresh `Rscript main.R`; returns the answers by id (and the raw lines).
session <- function(lines) {
  input <- tempfile(fileext = ".jsonl")
  con <- file(input, "wb")
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\n"), "\n"))), con)
  close(con)
  err <- tempfile()
  out <- system2(rscript, c("--vanilla", shQuote(file.path(root, "main.R"))), stdin = input, stdout = TRUE, stderr = err)
  Encoding(out) <- "UTF-8"
  status <- attr(out, "status")
  answers <- list()
  for (l in out) {
    m <- tryCatch(plain(P(l)), error = function(e) NULL)
    if (is.list(m) && !is.null(m$id)) answers[[as.character(m$id)]] <- m
  }
  list(answers = answers, lines = out, status = status, log = readLines(err, warn = FALSE))
}

req <- function(id, method, params) {
  paste0("{\"jsonrpc\":\"2.0\",\"id\":", id, ",\"method\":\"", method, "\",\"params\":", params, "}")
}
INIT <- req(0, "initialize", "{\"protocolVersion\":1,\"host\":{\"id\":\"gridcraft\",\"version\":\"0.7.0\",\"protocol\":\"2.0\",\"features\":[\"dialogs\",\"open\",\"events\",\"document\"]}}")
fcall <- function(id, name, args) req(id, "functions/call", paste0("{\"name\":\"", name, "\",\"args\":", args, "}"))
cmd <- function(id, command, params = "{}", doc = "{\"path\":null,\"name\":\"Book1\",\"dirty\":false}", extra = "") {
  req(id, "commands/run", paste0("{\"command\":\"", command, "\",\"params\":", params,
                                 ",\"context\":{\"workbook\":\"Book1\",\"sheet\":\"Sheet1\",\"active\":\"B2\",\"selection\":\"B2:C4\"",
                                 ",\"values\":[[\"a\",\"b\"],[1,2],[3,4]],\"sheets\":[{\"index\":0,\"name\":\"Sheet1\"}],\"document\":", doc, extra, "}}"))
}
value <- function(s, id) plain(s$answers[[as.character(id)]]$result$value)
err_msg <- function(s, id) s$answers[[as.character(id)]]$error$message

book <- file.path(data_dir, "Budget.xlsx")
invisible(file.create(book))
book_json <- g$json_str(normalizePath(book, winslash = "/"))
docjson <- paste0("{\"path\":", book_json, ",\"name\":\"Budget.xlsx\",\"dirty\":false}")

t0 <- proc.time()[["elapsed"]]
s <- session(c(
  INIT,
  fcall(1, "R.VERSION", "[]"),
  fcall(2, "R.EVAL", "[\"1+1\"]"),                          # nothing known yet: off
  fcall(3, "R.CALL", "[\"system\",\"echo hi\"]"),            # off too
  fcall(4, "R.MEDIAN", "[1]"),                               # not ours
  fcall(5, "R.QUANTILE", "[[[1],[2],[3],[4],[5],[6],[7],[8],[9],[10]],0.9]"),
  fcall(6, "R.QUANTILE", "[[[1],[2],[3],[4]]]"),
  fcall(7, "R.LM", "[[[1],[2],[3.1],[3.9],[5.2]],[[1],[2],[3],[4],[5]]]"),
  fcall(8, "R.CORR", "[[[\"u\",\"v\"],[1,2],[2,4],[3,7]],\"spearman\",true]"),
  fcall(9, "R.TTEST", "[[[1],[2],[3],[4]],[[2],[3],[4],[6]]]"),
  fcall(10, "R.SUMMARY", "[[[1,\"a\"],[2,null],[3,\"b\"]]]"),
  fcall(11, "R.SUMMARY", "[[[1],[{\"error\":\"#N/A\"}]]]"),
  cmd(12, "about"),                                          # Book1, unsaved: on
  fcall(13, "R.EVAL", "[\"1+1\"]"),
  fcall(14, "R.EVAL", "[\"x*2+y\",[[1,2],[3,4]],10]"),
  fcall(15, "R.EVAL", "[\"c(..1, ..4, length(args))\",1,2,3,4]"),
  fcall(16, "R.EVAL", "[\"data.frame(a=1:2, b=c('\\u00e9', NA))\"]"),
  fcall(17, "R.EVAL", "[\"stop('boom')\"]"),
  fcall(18, "R.EVAL", "[\"nchar(x)\",\"\\u00e9\\ud83d\\ude00\"]"),
  fcall(19, "R.EVAL", "[\"x\",\"\\u00e9\\ud83d\\ude00\"]"),
  fcall(20, "R.EVAL", "[\"c(1/0, NaN, NA, 2)\"]"),
  fcall(21, "R.EVAL", "[\"x\",[[1,{\"error\":\"#DIV/0!\"}]]]"),
  fcall(22, "R.EVAL", "[\"cat('noise'); print(1:3); message('note'); warning('careful'); 7\"]"),
  fcall(23, "R.EVAL", "[\"sink(tempfile()); 8\"]"),
  fcall(24, "R.EVAL", "[\"quit('no')\"]"),
  fcall(25, "R.EVAL", "[\"x <- 0; repeat { x <- x + 1 }\"]"),
  fcall(26, "R.EVAL", "[\"1:2e6\"]"),
  fcall(27, "R.EVAL", "[\"x[2,1]\",[[\"a\",null],[true,5]]]"),
  fcall(28, "R.EVAL", "[\"(\"]"),
  fcall(29, "R.CALL", "[\"median\",[[1],[5],[3]]]"),
  fcall(30, "R.CALL", "[\"stats::mad\",[[1],[2],[3],[4],[100]]]"),
  fcall(31, "R.CALL", "[\"system('rm -rf ~')\"]"),
  fcall(32, "R.CALL", "[\"no_such_function\"]"),
  "garbage",
  "{",
  "{\"jsonrpc\":\"2.0\",\"id\":33}",
  req(34, "no/such", "{}"),
  paste0("{\"jsonrpc\":\"2.0\",\"id\":35,\"method\":\"functions/call\",\"params\":{\"name\":\"R.EVAL\",\"args\":[\"1\",", strrep("[", 300), strrep("]", 300), "]}}"),
  "{\"jsonrpc\":\"2.0\",\"method\":\"some/notification\",\"params\":{}}",
  fcall(36, "R.EVAL", "[\"'still here'\"]"),
  fcall(37, "R.EVAL", "[\"x\",[[1]]]"),
  fcall(38, "R.EVAL", "[\"x\",[[]]]")
))
cat(sprintf("session 1: %d answers in %.1f s\n", length(s$answers), proc.time()[["elapsed"]] - t0))
same("initialize", s$answers[["0"]]$result$protocolVersion, 1)
check("R.VERSION", grepl("^GridCraft R 0.2.0 on R ", value(s, 1)))
same("R.EVAL off before any workbook is known", err_msg(s, 2), "R is turned off for this workbook: R > Trust This Workbook")
same("R.CALL off too", err_msg(s, 3), "R is turned off for this workbook: R > Trust This Workbook")
same("unknown function", s$answers[["4"]]$error$code, -32601)
same("quantile 0.9", value(s, 5), 9.1)
same("quantile table", value(s, 6)[[2L]], list(0, 1))
same("lm slope", value(s, 7)[[3L]][[2L]], 1.03)
same("lm R squared label", value(s, 7)[[5L]][[1L]], "R squared")
same("corr labels", value(s, 8)[[1L]], list("", "u", "v"))
same("corr spearman", value(s, 8)[[2L]][[3L]], 1)
same("ttest t label", value(s, 9)[[1L]][[1L]], "t")
check("ttest p", abs(value(s, 9)[[3L]][[2L]] - stats::t.test(1:4, c(2, 3, 4, 6))$p.value) < 1e-12)
same("summary n", value(s, 10)[[2L]], list("n", 3, 2))
same("summary missing", value(s, 10)[[3L]], list("missing", 0, 1))
same("summary of text: empty", value(s, 10)[[4L]][[3L]], "")
same("summary: an error propagates", value(s, 11), list(error = "#N/A"))
check("about", grepl("R in this workbook: on for Book1", s$answers[["12"]]$result$dialog$prompt, fixed = TRUE))
same("eval", value(s, 13), 2)
same("eval matrix + scalar", value(s, 14), list(list(12, 14), list(16, 18)))
same("eval ..1 ..4 args", value(s, 15), list(list(1), list(4), list(4)))
same("eval data frame", value(s, 16), list(list("a", "b"), list(1, "\u00e9"), list(2, "")))
same("eval error", err_msg(s, 17), "R error: boom")
same("unicode argument", value(s, 18), 2)
same("unicode round trip", value(s, 19), "\u00e9\U0001F600")
same("Inf NaN NA", value(s, 20), list(list(list(error = "#NUM!")), list(list(error = "#NUM!")), list(""), list(2)))
same("error argument propagates", value(s, 21), list(error = "#DIV/0!"))
same("printed output stays out of the protocol", value(s, 22), 7)
check("output went to the log", any(grepl("noise", s$log)) && any(grepl("careful", s$log)))
same("a sink left open", value(s, 23), 8)
check("quit is refused", grepl("quit() is turned off", err_msg(s, 24), fixed = TRUE))
msg25 <- err_msg(s, 25); check("time limit", grepl("too long", msg25)); if (!grepl("too long", msg25)) cat("  time limit message was:", msg25, "\n")
same("result too big", value(s, 26), list(error = "#NUM!"))
same("mixed range is character", value(s, 27), "TRUE")
check("syntax error", grepl("can't read the code", err_msg(s, 28)))
same("R.CALL median", value(s, 29), 3)
same("R.CALL pkg::fun", value(s, 30), stats::mad(c(1, 2, 3, 4, 100)))
check("R.CALL refuses code", grepl("isn't the name of an R function", err_msg(s, 31)))
check("R.CALL unknown", grepl("no R function called", err_msg(s, 32)))
same("no method", s$answers[["33"]]$error$code, -32600)
same("unknown method", s$answers[["34"]]$error$code, -32601)
check("too deep: an error, not a crash", is.null(s$answers[["35"]]))
same("still answering", value(s, 36), "still here")
same("one cell", value(s, 37), 1)
check("an empty row", !is.null(s$answers[["38"]]))
check("every line on stdout is JSON", all(vapply(s$lines, function(l) !is.null(tryCatch(P(l), error = function(e) NULL)), NA) | s$lines == "" | s$lines == "hi"))
check("exit status 0", is.null(s$status) || s$status == 0)

# Trust: a saved workbook, the events, the commands.
s <- session(c(
  INIT,
  req(1, "events/documentOpened", paste0("{\"path\":", book_json, ",\"name\":\"Budget.xlsx\"}")),
  fcall(2, "R.EVAL", "[\"1\"]"),
  cmd(3, "runScript", "{\"code\":\"1\"}", docjson),
  cmd(4, "trust", "{}", docjson),
  cmd(5, "trustConfirm", "{\"path\":\"/somewhere/else.xlsx\",\"name\":\"else.xlsx\"}", docjson),
  cmd(6, "trustConfirm", paste0("{\"path\":", book_json, ",\"name\":\"Budget.xlsx\"}"), docjson),
  fcall(7, "R.EVAL", "[\"2\"]"),
  req(8, "events/documentClosed", paste0("{\"path\":", book_json, ",\"name\":\"Budget.xlsx\"}")),
  fcall(9, "R.EVAL", "[\"3\"]"),
  req(10, "events/documentOpened", paste0("{\"path\":", book_json, ",\"name\":\"Budget.xlsx\"}")),
  fcall(11, "R.EVAL", "[\"4\"]"),
  cmd(12, "runScript", "{\"code\":\"x\",\"header\":true}", docjson),
  cmd(13, "runScript", "{\"fields\":{\"code\":\"summary(c(1,2))\",\"output\":\"sheet\"}}", docjson),
  cmd(14, "runScript", "{}", docjson),
  cmd(15, "console", "{\"code\":\"double <- function(v) v * 2; double(21)\"}", docjson),
  fcall(16, "R.CALL", "[\"double\",4]"),
  cmd(17, "console", "{\"code\":\"stop('nope')\"}", docjson),
  cmd(18, "trusted", "{}", docjson),
  cmd(19, "untrust", "{}", docjson),
  fcall(20, "R.EVAL", "[\"5\"]"),
  cmd(21, "trust", "{}", "{\"path\":null,\"name\":\"Book3\",\"dirty\":true}"),
  cmd(22, "trust", "{}", "{\"path\":null,\"name\":\"Sales sample\",\"dirty\":false}"),
  fcall(23, "R.EVAL", "[\"6\"]"),
  cmd(24, "trustConfirm", "{\"path\":null,\"name\":\"Sales sample\"}", "{\"path\":null,\"name\":\"Sales sample\",\"dirty\":false}"),
  fcall(25, "R.EVAL", "[\"7\"]"),
  req(26, "events/documentSaved", "{\"path\":\"/tmp/new-book.xlsx\",\"name\":\"new-book.xlsx\",\"format\":\"xlsx\"}"),
  fcall(27, "R.EVAL", "[\"8\"]"),
  cmd(28, "plot", "{\"code\":\"plot(x[,1], x[,2])\",\"width\":300,\"height\":200}", "{\"path\":null,\"name\":\"Sales sample\",\"dirty\":false}"),
  cmd(29, "plot", "{\"code\":\"invisible(1)\"}", "{\"path\":null,\"name\":\"Sales sample\",\"dirty\":false}"),
  cmd(30, "about", "{}", "{\"path\":null,\"name\":\"Sales sample\",\"dirty\":false}"),
  cmd(31, "runScript", paste0("{\"code\":\"", strrep("1", 10), "\",\"output\":\"below\"}"), "{\"path\":null,\"name\":\"Sales sample\",\"dirty\":false}",
      ",\"selection\":\"A1048575:A1048576\""),
  "{\"jsonrpc\":\"2.0\",\"method\":\"shutdown\"}",
  fcall(99, "R.EVAL", "[\"'after shutdown'\"]")
))
check("opened untrusted: off", grepl("turned off", err_msg(s, 2)))
same("runScript refused, offering trust", s$answers[["3"]]$result$dialog$command, "trust")
same("trust asks first", s$answers[["4"]]$result$dialog$command, "trustConfirm")
check("trust spells out the risk", grepl("anything you can", s$answers[["4"]]$result$dialog$prompt))
check("confirm for another workbook fails", grepl("changed", err_msg(s, 5)))
same("trusted", s$answers[["6"]]$result$message, "R is on for Budget.xlsx")
same("trusted: recalculates", s$answers[["6"]]$result$commands[[1L]]$command, "formulas.calculateNow")
check("trusted.json written", file.exists(file.path(data_dir, "trusted.json")))
same("on after trusting", value(s, 7), 2)
check("closed: off", grepl("turned off", err_msg(s, 9)))
same("opened again: on", value(s, 11), 4)
w <- s$answers[["12"]]$result$commands
same("runScript writes below the selection", w[[1L]]$params$range, "B6")
same("runScript data frame values", w[[1L]]$params$values, list(list("'a", "'b"), list(1, 2), list(3, 4)))
same("runScript bolds the header", w[[2L]]$params$range, "B6:C6")
w <- s$answers[["13"]]$result$commands
same("new sheet", w[[1L]]$params$name, "R Output")
same("new sheet values", w[[2L]]$params$values[[3L]], list("'Median", 1.5))
same("no code: the form", s$answers[["14"]]$result$dialog$kind, "form")
check("console", grepl("[1] 42", s$answers[["15"]]$result$dialog$prompt, fixed = TRUE))
same("console definitions reach R.CALL", value(s, 16), 8)
check("console shows errors", grepl("Error: nope", s$answers[["17"]]$result$dialog$prompt, fixed = TRUE))
same("trusted list", s$answers[["18"]]$result$dialog$items[[1L]]$label, "Budget.xlsx")
check("untrust", grepl("R is off for Budget.xlsx", s$answers[["19"]]$result$message, fixed = TRUE))
check("off after untrust", grepl("turned off", err_msg(s, 20)))
check("unsaved BookN is trusted", grepl("new and unsaved", s$answers[["21"]]$result$message))
same("another unsaved workbook asks", s$answers[["22"]]$result$dialog$command, "trustConfirm")
check("not yet trusted", grepl("turned off", err_msg(s, 23)))
same("trusted for the session", value(s, 25), 7)
check("saved under a new path: off", grepl("turned off", err_msg(s, 27)))
if (isTRUE(unname(capabilities("png"))) || isTRUE(unname(capabilities("cairo")))) {
  p <- s$answers[["28"]]$result$commands[[1L]]
  same("plot inserts a picture", p$command, "insert.picture")
  check("plot is a PNG", startsWith(p$params$base64, "iVBORw0KGgo"))
  same("plot goes right of the selection", p$params$at, "D2")
  check("drew nothing", grepl("drew nothing", err_msg(s, 29)))
} else {
  cat("(no PNG device here: plot checks skipped)\n")
}
same("about says on", s$answers[["30"]]$result$result$trusted, TRUE)
check("result past the last row", grepl("doesn't fit", err_msg(s, 31)))
check("shutdown stops", is.null(s$answers[["99"]]))

# GridCraft 2.2: trust follows each formula's own workbook; trusting and untrusting recalculate
# through host/functions/invalidate; blanks; literal text; code fields.
INIT22 <- req(0, "initialize", paste0("{\"protocolVersion\":1,\"host\":{\"id\":\"gridcraft\",\"version\":\"0.7.0\",\"protocol\":\"2.2\",",
  "\"features\":[\"dialogs\",\"open\",\"events\",\"document\",\"push\",\"collab\",\"functionDocument\",\"openedBeforeCalc\",",
  "\"multiline\",\"emptyResult\",\"literalValues\",\"invalidate\"]}}"))
fdoc <- function(id, name, args, doc) req(id, "functions/call", paste0("{\"name\":\"", name, "\",\"args\":", args, ",\"document\":", doc, "}"))
answer <- function(rid) paste0("{\"jsonrpc\":\"2.0\",\"id\":\"", rid, "\",\"result\":{\"functions\":[\"R.EVAL\",\"R.CALL\"]}}")
book2 <- file.path(data_dir, "Model.xlsx")
invisible(file.create(book2))
b2 <- g$json_str(normalizePath(book2, winslash = "/"))
d_model <- paste0("{\"path\":", b2, ",\"name\":\"Model.xlsx\",\"dirty\":false,\"id\":\"doc-7\"}")
d_book1 <- "{\"path\":null,\"name\":\"Book1\",\"dirty\":false,\"id\":\"doc-1\"}"
d_sales <- "{\"path\":null,\"name\":\"Sales sample\",\"dirty\":false,\"id\":\"doc-9\"}"
d_sales2 <- "{\"path\":null,\"name\":\"Sales sample\",\"dirty\":false,\"id\":\"doc-10\"}"
saved <- file.path(data_dir, "sales.xlsx")
d_saved <- paste0("{\"path\":", g$json_str(normalizePath(saved, winslash = "/", mustWork = FALSE)), ",\"name\":\"sales.xlsx\",\"dirty\":false,\"id\":\"doc-9\"}")
s <- session(c(
  INIT22,
  cmd(1, "about", "{}", d_book1),                            # the add-in heard of Book1 (trusted)...
  fdoc(2, "R.EVAL", "[\"1+1\"]", d_model),                    # ...but this formula is in Model.xlsx: off
  fdoc(3, "R.EVAL", "[\"1+2\"]", d_book1),                    # Book1's own formula: on
  fcall(4, "R.EVAL", "[\"1+3\"]"),                           # no document: off
  cmd(5, "trust", "{}", d_model),
  cmd(6, "trustConfirm", paste0("{\"path\":", b2, ",\"name\":\"Model.xlsx\",\"id\":\"doc-7\"}"), d_model),
  answer("r1"),
  fdoc(7, "R.EVAL", "[\"2*3\"]", d_model),
  cmd(8, "trust", "{}", d_sales),
  cmd(9, "trustConfirm", "{\"path\":null,\"name\":\"Sales sample\",\"id\":\"doc-9\"}", d_sales),
  answer("r2"),
  fdoc(10, "R.EVAL", "[\"10\"]", d_sales),
  fdoc(11, "R.EVAL", "[\"11\"]", d_sales2),                   # same name, another workbook: off
  req(12, "events/documentSaved", d_saved),
  fdoc(13, "R.EVAL", "[\"13\"]", d_saved),                    # saved: the same workbook, still on
  fdoc(14, "R.EVAL", "[\"NA\"]", d_model),
  fdoc(15, "R.EVAL", "[\"c(1, NA)\"]", d_model),
  fdoc(16, "R.EVAL", "[\"NULL\"]", d_model),
  cmd(17, "runScript", "{\"code\":\"data.frame(d = as.Date(c('2024-01-05', NA)), s = c('=1+1', '007'), n = 1:2)\"}", d_model),
  cmd(18, "runScript", "{}", d_model),
  cmd(19, "untrust", "{}", d_model),
  answer("r3"),
  fdoc(20, "R.EVAL", "[\"20\"]", d_model),
  cmd(21, "trusted", "{}", d_book1),
  cmd(22, "untrustPaths", "{\"ids\":[\"id:doc-9\"]}", d_book1),
  answer("r4"),
  fdoc(23, "R.EVAL", "[\"23\"]", d_sales),
  cmd(24, "untrust", "{}", d_book1),
  fdoc(25, "R.EVAL", "[\"25\"]", d_book1),
  fdoc(26, "R.QUANTILE", "[[[1],[2],[3]],0.5]", d_sales2)     # statistics need no trust
))
check("2.2: the formula's workbook decides, not the last one heard of", grepl("turned off", err_msg(s, 2)))
same("2.2: Book1's formula", value(s, 3), 3)
check("2.2: no document: off", grepl("turned off", err_msg(s, 4)))
same("2.2: trust asks first, naming the workbook's id", s$answers[["5"]]$result$dialog$params$id, "doc-7")
same("2.2: trusting invalidates R.EVAL and R.CALL", s$answers[["r1"]]$method, "host/functions/invalidate")
same("2.2: ...by name", s$answers[["r1"]]$params$names, list("R.EVAL", "R.CALL"))
same("2.2: trusted", s$answers[["6"]]$result$message, "R is on for Model.xlsx")
check("2.2: no Calculate Now needed", is.null(s$answers[["6"]]$result$commands))
same("2.2: on after trusting", value(s, 7), 6)
same("2.2: an unsaved workbook trusted by id", value(s, 10), 10)
check("2.2: another workbook with the same name: off", grepl("turned off", err_msg(s, 11)))
check("2.2: saved, it stays trusted", grepl("stays trusted", s$answers[["12"]]$result$message))
same("2.2: on in the saved file", value(s, 13), 13)
check("2.2: the saved file is on the list", normalizePath(saved, winslash = "/", mustWork = FALSE) %in% g$json_parse(paste(readLines(file.path(data_dir, "trusted.json")), collapse = ""))$workbooks)
same("2.2: NA is a blank", value(s, 14), list(empty = TRUE))
same("2.2: NA in a column is a blank", value(s, 15), list(list(1), list(list(empty = TRUE))))
same("2.2: NULL is a blank", value(s, 16), list(empty = TRUE))
w <- s$answers[["17"]]$result$commands
same("2.2: text is written literally", w[[1L]]$params$literal, TRUE)
same("2.2: no apostrophes", w[[1L]]$params$values[[1L]], list("d", "s", "n"))
same("2.2: formulas and numbers as text stay text", w[[1L]]$params$values[[2L]][2:3], list("=1+1", 1))
same("2.2: '007' too", w[[1L]]$params$values[[3L]][[2L]], "007")
same("2.2: dates are written as dates", w[[2L]]$params[c("range", "values")], list(range = "B7", values = list(list("2024-01-05"))))
check("2.2: ...not literally", is.null(w[[2L]]$params$literal))
same("2.2: header bold", w[[3L]]$params$range, "B6:D6")
f <- s$answers[["18"]]$result$dialog
same("2.2: a code field", f$fields[[1L]][c("multiline", "code")], list(multiline = TRUE, code = TRUE))
check("2.2: no ';' advice", !grepl("Separate statements", f$prompt))
same("2.2: untrusting invalidates too", s$answers[["r3"]]$method, "host/functions/invalidate")
check("2.2: untrusted", grepl("^R is off for Model.xlsx$", s$answers[["19"]]$result$message))
check("2.2: off after untrust", grepl("turned off", err_msg(s, 20)))
labels <- vapply(s$answers[["21"]]$result$dialog$items, function(i) i$label, "")
check("2.2: the list shows the unsaved workbook", "Sales sample" %in% labels)
same("2.2: removing invalidates", s$answers[["r4"]]$method, "host/functions/invalidate")
check("2.2: removed by id: off", grepl("turned off", err_msg(s, 23)))
check("2.2: a new Book1 stays on", grepl("stays on", s$answers[["24"]]$result$message))
same("2.2: Book1 still on", value(s, 25), 25)
same("2.2: statistics in any workbook", value(s, 26), 2)
check("2.2: every line on stdout is JSON", all(vapply(s$lines, function(l) !is.null(tryCatch(P(l), error = function(e) NULL)), NA) | s$lines == ""))

# Old hosts (no dialogs, no document): functions work, R code stays off, commands say why.
s <- session(c(
  req(0, "initialize", "{\"protocolVersion\":1,\"host\":{\"id\":\"gridcraft\",\"version\":\"0.6.1\"}}"),
  fcall(1, "R.QUANTILE", "[[[1],[2],[3]],0.5]"),
  cmd(2, "runScript", "{}", "null"),
  req(3, "commands/run", "{\"command\":\"runScript\",\"params\":{\"code\":\"1\"},\"context\":{\"selection\":\"A1\"}}"),
  fcall(4, "R.EVAL", "[\"1\"]"),
  req(5, "commands/run", "{\"command\":\"about\",\"params\":{},\"context\":{}}")
))
same("old host: stats", value(s, 1), 2)
check("old host: no form without dialogs", grepl("Give the R code", err_msg(s, 2)))
check("old host: R code needs the document", grepl("doesn't say which workbook", err_msg(s, 3)))
check("old host: R.EVAL off", grepl("turned off", err_msg(s, 4)))
check("old host: about", grepl("GridCraft R", s$answers[["5"]]$result$message))

# A million cells in, and the limit.
rows <- paste0("[", paste0("[", seq_len(499999), ",", seq_len(499999) / 2, "]", collapse = ","), "]")
over <- paste0("[", paste0("[", seq_len(500001), ",1]", collapse = ","), "]")
Sys.setenv(GRIDCRAFT_R_TRUST = "all")
t0 <- proc.time()[["elapsed"]]
s <- session(c(INIT, fcall(1, "R.EVAL", paste0("[\"sum(x)\",", rows, "]")), fcall(2, "R.EVAL", paste0("[\"sum(x)\",", over, "]"))))
Sys.unsetenv("GRIDCRAFT_R_TRUST")
cat(sprintf("a million cells: %.1f s\n", proc.time()[["elapsed"]] - t0))
same("a million cells", value(s, 1), sum(as.numeric(seq_len(499999))) * 1.5)
same("over a million cells", value(s, 2), list(error = "#VALUE!"))

unlink(data_dir, recursive = TRUE)
cat(sprintf("%d checks, %d failed\n", checks, failures))
if (failures > 0L) quit(status = 1L)
