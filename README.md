# GridCraft R

R for [GridCraft](https://github.com/storytold/gridcraft): R code in formulas, ready-made
statistics that spill tables (linear models, t-tests, quantiles, correlations, summaries), R
scripts that write their result into the sheet, R plots inserted as pictures, and an R console.
It's written in base R only (its JSON reader and writer included), so it needs nothing but R.

## Install

In GridCraft, open **Automate › Add-ins** (or **File › Options › Add-ins**), paste

```
sirmmo/gridcraft-r
```

in **Install from GitHub** and confirm. GridCraft takes the latest release's
`gridcraft-r-<version>.zip` (the same zip on every platform) and loads it. To use a copy of this
folder instead, **Load** it from Automate › Add-ins, or start GridCraft with `--plugin <folder>`.

From the command line:

```sh
gridcraft-cli run --cmd 'plugins.install={"url":"sirmmo/gridcraft-r","dir":"'"$HOME"'/plugins"}'
gridcraft-cli --plugin ~/plugins/io.github.sirmmo.r eval '=R.QUANTILE({1,2,3,4,5,6,7,8,9,10},0.9)'
```

It needs GridCraft 0.7 or later for the commands, the dialogs and the trust model (older versions
run the statistics functions only; see [Trust](#trust)). With a GridCraft that speaks plugin
protocol 2.2, trust follows each formula's own workbook, the code boxes have several lines, empty
results are blank and scripts write text as text; on earlier 0.7 versions it works as 0.1.0 did,
with the limits listed under [Older GridCraft versions](#older-gridcraft-versions).

### R

R 3.4 or later, with `Rscript` on the `PATH` GridCraft sees. No packages are needed; the ones you
have installed are available to your R code.

- **Linux, BSD:** your distribution's R (`apt install r-base-core`, `dnf install R`,
  `pkg install R`).
- **macOS:** the installer from [CRAN](https://cran.r-project.org/bin/macosx/) puts `Rscript` in
  `/usr/local/bin`; Homebrew (`brew install r`) in `/opt/homebrew/bin`. An app started from the
  Dock doesn't see your shell's `PATH`: if GridCraft can't start R, link `Rscript` into
  `/usr/local/bin`, or change `run.command` in `plugin.json` to its full path.
- **Windows:** the CRAN installer doesn't add R to `PATH`. Add `C:\Program Files\R\R-4.x.y\bin`
  to `PATH` (System › Environment Variables), or set `run.command` in `plugin.json` to the full
  path, for example `"C:\\Program Files\\R\\R-4.4.2\\bin\\Rscript.exe"`.

The add-in runs `Rscript --vanilla main.R`: no `.Rprofile`, site profile or saved workspace is
read, so every computer behaves the same. Packages in your personal library are still found.
**R › About** shows the R version, `R_HOME`, the libraries and the packages installed.

## Functions

They're listed in Insert Function under **Add-ins** and suggested while you type. Ranges arrive as
R matrices (see [Data conversion](#data-conversion)); tables spill.

| Function | Returns |
|---|---|
| `R.EVAL(code, [x], [y], [z], …)` | The value of R `code`, with the arguments bound as `x`, `y`, `z`, all of them as the list `args` and as `..1`, `..2`, … **Trusted workbooks only** |
| `R.CALL(function, [arg1], …)` | Calls the R function named `function` (`"median"`, `"stats::mad"`, or one you defined in the console) with the arguments. **Trusted workbooks only** |
| `R.LM(known_y, known_x, [const])` | A linear model `y ~ x` (`known_x` may have several columns): a table of term, estimate, std. error, t value and p value, then R squared, adjusted R squared, residual std. error, F statistic and its p value, observations and residual df. `const` FALSE fits without an intercept. Rows with an empty or text cell are left out |
| `R.QUANTILE(values, [probabilities], [type])` | Quantiles with any of R's nine definitions (`type` 1–9, default 7, as `QUANTILE.INC`). One probability gives a number; several (default 0, 0.25, 0.5, 0.75, 1) a table of probability and quantile |
| `R.CORR(values, [method], [headers])` | The correlation matrix of the columns, labelled: `method` `"pearson"` (default), `"spearman"` or `"kendall"`, pairwise complete. `headers` TRUE takes the labels from the first row (otherwise `x1`, `x2`, …) |
| `R.TTEST(x, [y], [paired], [mu], [alternative])` | Student's t-test (one sample against `mu`, default 0; Welch's for two samples; paired with `paired` TRUE): t, df, p value, the 95% confidence interval, the means, the method. `alternative` `"two.sided"` (default), `"less"` or `"greater"` |
| `R.SUMMARY(values, [headers])` | Per column, like `summary()`: n, missing, min, 1st quartile, median, mean, 3rd quartile, max, std. dev. Text columns show n and missing |
| `R.VERSION()` | The add-in and R versions |

The statistics functions use the numbers in their ranges, leaving out text, logicals and empty
cells as Excel's do; an error value in a range is the answer (`#N/A` in, `#N/A` out).

Examples:

```
=R.EVAL("x * 2", A1:B10)                       a 10 × 2 range, doubled
=R.EVAL("sd(x) / mean(x)", A1:A100)            coefficient of variation
=R.EVAL("rev(sort(table(x)))", C2:C500)        a frequency table: labels and counts
=R.EVAL("cor(x, y, method = 'kendall')", A2:A50, B2:B50)
=R.EVAL("format(Sys.Date(), '%A')")            the day of the week (computed once: see below)
=R.EVAL("head(mtcars, 5)")                     a data frame: a header row, then the rows
=R.CALL("median", A1:A10)
=R.CALL("stats::mad", A1:A10)
=R.LM(B2:B30, C2:D30)
=R.QUANTILE(A1:A100, {0.05, 0.95})
=R.QUANTILE(A1:A100, 0.9, 1)
=R.CORR(A1:D50, "spearman", TRUE)
=R.TTEST(A2:A20, B2:B20, TRUE)
=R.SUMMARY(A1:F200, TRUE)
```

Text in a formula is in double quotes, so write R strings in single quotes (`'kendall'`), or
double the quotes (`""kendall""`).

**Results are kept.** The functions aren't volatile: GridCraft keeps a result for the same
arguments while the add-in runs, and recalculates a cell only when its inputs change. A
formula with `runif()` or `Sys.time()` doesn't change on F9. Errors aren't kept, so after fixing
their cause (defining a function in the console, installing a package) **Formulas › Calculate
Now** runs them again.

**Empty results.** `NA` and `NULL` show as empty cells (with GridCraft before protocol 2.2: as
empty text, since it would show 0).

**Errors.** An R error shows `#VALUE!` in the cell, and R's message goes to GridCraft's log
(`R error: object 'z' not found`). So does a syntax error, and code that runs more than 25
seconds (it's stopped). Output that the code prints (`print`, `cat`, `message`, warnings) goes
to the log too, never into the sheet.

## Commands

On the **R** tab, and two of them in the cell's right-click menu:

| Command | Does |
|---|---|
| **Run Script…** (`runScript`) | A form for R code, or an `.R` file to run, with the selection's values as `x` (a data frame when **The first row holds column names** is ticked). The result is written below the selection (one empty row between) or on a new sheet called R Output; a data frame's header row is bold. Printed output shows in a dialog. Trusted workbooks only |
| **Plot…** (`plot`) | A form for base R plotting code with the selection as `x`, and the picture's size. The plot is drawn into a PNG and inserted to the right of the selection (`insert.picture`). Trusted workbooks only |
| **Console…** (`console`) | Runs R code in R's workspace (the global environment) and shows what it prints; **Again…** asks for the next line. Functions and values you define stay there for `R.CALL`, `R.EVAL` and later runs, until GridCraft stops the add-in. `x` is the selection while the code runs. Trusted workbooks only |
| **Trust This Workbook** (`trust`) | Turns R code on for the active workbook, after a dialog that spells out the risk |
| **Stop Trusting** (`untrust`) | Turns it off again |
| **Trusted Workbooks…** (`trusted`) | The list of trusted workbooks; select some and **Remove** them |
| **About R** (`about`) | The add-in and R versions, `R_HOME`, the libraries, whether PNG plots work, whether R is on for this workbook, the installed packages |

Each command is one undoable step. The code boxes take several lines (monospace; Tab indents). On
a GridCraft before protocol 2.2 they're single-line: separate statements with `;`
(`m <- lm(x[,2] ~ x[,1]); coef(m)`), or put a longer script in a file and give its path.

Agents and scripts run them with `plugins.run`; with `code` in the params no dialog opens:

```sh
gridcraft-cli --plugin ~/plugins/io.github.sirmmo.r run --in data.xlsx \
  --cmd 'selection.set={"range":"A1:C20"}' \
  --cmd 'plugins.run={"plugin":"io.github.sirmmo.r","command":"runScript","params":{"code":"summary(x)","header":true,"output":"sheet"}}' \
  --out data.xlsx
```

| Command | Params |
|---|---|
| `runScript` | `{code? or file?, header?: bool, output?: "below" \| "sheet"}` |
| `plot` | `{code, header?: bool, width?: 50–4000 (640), height?: 50–4000 (480)}` |
| `console` | `{code}` → `result.output` |
| `trust`, `untrust`, `trusted`, `about` | `{}` |
| `trustConfirm` | `{path, name, id}` of the active workbook (as `context.document` gives them): trusts it without asking (the OK of Trust This Workbook) |
| `untrustPaths` | `{ids: [path, "id:<session id>", …]}` |

## Trust

R code can do anything you can: read, change or delete your files, send them over the internet,
start programs. Formulas run on every recalculation, and a workbook someone sends you can carry
`=R.EVAL("…")` formulas. So `R.EVAL`, `R.CALL` (a function name is code too:
`R.CALL("system", "…")`), Run Script, Plot and the Console work only in **trusted workbooks**.
The statistics functions (`R.LM`, `R.QUANTILE`, `R.CORR`, `R.TTEST`, `R.SUMMARY`, `R.VERSION`)
run no code from the workbook and always work.

**Each formula is checked against its own workbook.** GridCraft tells the add-in which workbook
every formula belongs to (plugin protocol 2.2: `functions/call`'s `document`), and the add-in runs
an `R.EVAL` or `R.CALL` only when that workbook is trusted. Which workbook is active, which one you
opened last or ran a command in doesn't matter: an untrusted workbook's R formulas show `#VALUE!`
even while a trusted one is open beside it, and when it opens, GridCraft tells the add-in before it
calculates anything.

- **Trust This Workbook** asks first, then adds the workbook's file (its absolute path) to
  `trusted.json` in the add-in's data folder: `~/.local/share/gridcraft-r` on Linux and BSD
  (`$XDG_DATA_HOME/gridcraft-r` when set), `~/Library/Application Support/gridcraft-r` on macOS,
  `%APPDATA%\gridcraft-r` on Windows. The list survives updates of the add-in and is shared by
  every GridCraft window. GridCraft recalculates the workbook's R formulas at once.
- **Saving keeps trust.** A trusted workbook saved under a new name (Save As, or a new workbook's
  first save) is the same workbook: its new file goes on the list too, and the status bar says so.
  A file moved or renamed outside GridCraft must be trusted again.
- **A new workbook** that was never saved (Book1, Book2…) is trusted: you're the one writing its
  formulas. Other workbooks without a file (an imported `.ods` or `.xls`, a sample) can be trusted
  until GridCraft closes; each is told apart by GridCraft's id for it, not by its name.
- **Stop Trusting** and removing workbooks in **Trusted Workbooks…** recalculate their R formulas
  too, so they show `#VALUE!` at once.
- In an untrusted workbook, `R.EVAL` and `R.CALL` cells show `#VALUE!` and the log says
  `R is turned off for this workbook: R > Trust This Workbook`; the commands offer to trust it.
- `GRIDCRAFT_R_TRUST=all` in the environment trusts every workbook: for scripts and CI that
  evaluate their own workbooks (`gridcraft-cli eval`), never for opening other people's files.

### Limits

- The rare formulas GridCraft evaluates outside any workbook (none in normal use) get no workbook,
  so their R code is off.
- If the add-in takes more than 10 seconds to answer that a workbook opened (R starting on a very
  slow disk), GridCraft calculates it anyway; its R formulas show `#VALUE!` until **Calculate Now**
  (F9). They never run untrusted.
- A workbook opened by another add-in's command is announced to this one as usual; the per-formula
  check still applies, so nothing runs in it unless it's trusted.

### Older GridCraft versions

A GridCraft 0.7 without plugin protocol 2.2 doesn't say which workbook a formula is in. There, the
add-in follows the workbook GridCraft last told it about (the `document` of the last R command you
ran, and the documentOpened, documentSaved and documentClosed events), as 0.1.0 did:

- **Opening a workbook calculates it before GridCraft says it opened**, so its R formulas are
  computed while the add-in still believes the previous workbook is active. **Don't open workbooks
  you don't trust while a trusted one is open**, or update GridCraft. A trusted workbook opened
  while the add-in knew nothing shows `#VALUE!` in its R cells until **Calculate Now** (F9).
- **Switching windows says nothing**: running any R command (About R, say) in a workbook tells the
  add-in which one you're in. Just after GridCraft started, R code is off until a command ran.
- Results already calculated stay in the cells after **Stop Trusting** until their inputs change.
- A saved new workbook is a new file to trust; unsaved workbooks are told apart by name.
- GridCraft versions before 0.7 don't say which workbook is open at all: R code stays off, and
  only the statistics functions work.

Trust is about workbooks, not about the add-in: like any GridCraft add-in, this one runs with your
permissions. Install add-ins only from people you trust.

## Data conversion

**Into R** (arguments, and `x` in commands):

| Cells | R |
|---|---|
| one cell | a scalar: number (double), character, logical, or `NA` when empty |
| a range of numbers (and empty cells) | a numeric matrix, rows × columns, `NA` for empty cells |
| a range of logicals | a logical matrix |
| anything else | a character matrix: numbers as text (`"2.5"`), `TRUE`/`FALSE`, `NA` for empty cells |
| Run Script / Plot with **column names** ticked | a data frame: the first row names the columns, each column numeric, logical or character on its own |
| an error value | the function's answer is that error (it isn't passed to R) |

A one-column range is still a matrix (`n × 1`); use `c(x)` or `x[, 1]` for a plain vector. Dates
are numbers in a spreadsheet (days since 1899-12-30): `as.Date(x, origin = "1899-12-30")` turns
them into R dates.

**Out of R**:

| R value | Cells |
|---|---|
| a scalar | one cell |
| an unnamed vector | a column (spills) |
| a named vector (`summary()`, `quantile()`, a 1-D `table`) | two columns: names, values |
| a matrix, a 2-D table | rows × columns (no dimnames; use `as.data.frame()` to keep column names) |
| a data frame | a header row of column names, then the rows |
| a list of single values | like a vector |
| factor | its labels (text) |
| logical | `TRUE` / `FALSE` |
| `NA`, `NULL` | an empty cell (blank; before protocol 2.2, empty text in a formula's result, since GridCraft would show 0) |
| `NaN`, `Inf`, `-Inf` | `#NUM!` |
| `Date` | ISO text, `2024-01-05` (functions can't return dates; Run Script writes real dates) |
| `POSIXct`, `POSIXlt` | text, `2024-01-05 14:30:00` (local time) |
| anything else (a model, a function, nested lists) | what `print()` shows, one line a cell |

Text longer than 32,767 characters is cut. A result over 1,000,000 cells is `#NUM!` (functions)
or an error (commands); so is an argument list over 1,000,000 cells (`#VALUE!`). Run Script
writes text as text: a leading `=` stays text, not a formula, and `007` or `1/2` stay as written
(protocol 2.2's literal values; before it, text is written with a leading apostrophe).

## Troubleshooting

- **Every R formula shows `#VALUE!`.** Look at GridCraft's log: `R is turned off for this
  workbook (…)` names the workbook that isn't trusted (on older GridCraft versions the add-in may
  not know which workbook it's in yet: run **R › Trust This Workbook**); `R error: …` is R's own
  message.
- **The add-in doesn't start / is "failed"**: GridCraft can't find `Rscript`. Check that
  `Rscript --version` works in a terminal, then see [R](#r) for `PATH` on macOS and Windows.
  After fixing it, **Activate** the add-in again (or reload it) in Automate › Add-ins.
- **"This R can't draw PNG pictures"**: the R build has neither cairo nor libpng
  (`capabilities()` in R). Install R from CRAN or your distribution's full `r-base` package.
- **The first plot is slow** on Linux: fontconfig builds its font cache once.
- **A formula doesn't update**: results are kept for the same arguments (see Functions). Change an
  argument, or turn the add-in off and on to start R afresh.
- **Long computations**: R code in formulas is stopped after 25 seconds, in commands after 50.
  Run heavy work from Run Script, or precompute it in the console.
- **R prints to the terminal from `system()`**: programs started by R write to the add-in's
  output. GridCraft ignores lines that aren't messages, but prefer `system(…, intern = TRUE)`.
- **Text with accents looks wrong on old Windows R (before 4.2)**: those use a code page; the
  add-in then writes everything as ASCII escapes, so the cells are right, but R code in the
  workbook may see the native encoding.

## How it works

GridCraft starts `Rscript --vanilla main.R` the first time one of the add-in's functions or
commands is used, or a document event arrives, and keeps that R running for the session. It sends
JSON-RPC messages, one per line, on standard input; the add-in answers on standard output
([docs/plugins.md](https://github.com/storytold/gridcraft/blob/main/docs/plugins.md), protocol
1.2: dialogs, `context.document`, document events; protocol 2.2, used when `initialize` lists the
features: `functionDocument`, `openedBeforeCalc`, `multiline`, `emptyResult`, `literalValues`,
`invalidate`). `main.R` loads `gridcraft_r.R`, which holds
everything: a vectorised JSON reader (a million cells in seconds) and writer, the conversions,
the functions, the commands and the trust list. Answers are flushed after each message, and an R
error in your code, in a message or in the add-in never stops the process.

Tests: `Rscript tests/run.R` (base R: the JSON code, the conversions, and the add-in driven over
standard input and output as GridCraft does, with the trust flow, malformed messages and a
million cells, as a GridCraft before and after protocol 2.2), and
`GRIDCRAFT_CLI=/path/to/gridcraft-cli sh tests/e2e.sh` (through GridCraft itself).

## Licence

MIT
