# GridCraft R: GridCraft starts this script (`Rscript --vanilla main.R`) and talks to it over
# standard input and output. The add-in itself is gridcraft_r.R: Rscript reads a script given
# as a file one line at a time, which is slow for a long one, while source() reads it at once.
local({
  args <- commandArgs(trailingOnly = FALSE)
  me <- sub("^--file=", "", args[grep("^--file=", args)][1L])
  dirs <- c(if (!is.na(me)) dirname(me), Sys.getenv("GRIDCRAFT_PLUGIN_DIR"), Sys.getenv("CRAFT_PLUGIN_DIR"), ".")
  files <- file.path(dirs[nzchar(dirs)], "gridcraft_r.R")
  source(files[file.exists(files)][1L], keep.source = FALSE, encoding = "UTF-8")
})
if (!nzchar(Sys.getenv("GRIDCRAFT_R_NO_MAIN"))) .gridcraft_r$main()
