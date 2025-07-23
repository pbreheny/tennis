if (!interactive()) setwd('../..')
path <- getwd()

out <- utils::capture.output(tools::checkDocFiles(dir = path))
expect_equal(out, character(0), info=paste(out, collapse='
'))
out <- utils::capture.output(tools::checkDocStyle(dir = path))
expect_equal(out, character(0), info=paste(out, collapse='
'))
out <- utils::capture.output(tools::checkReplaceFuns(dir = path))
expect_equal(out, character(0), info=paste(out, collapse='
'))
out <- utils::capture.output(tools::checkS3methods(dir = path))
expect_equal(out, character(0), info=paste(out, collapse='
'))
out <- utils::capture.output(tools::checkRdContents(dir = path))
expect_equal(out, character(0), info=paste(out, collapse='
'))

prb <- character()
for (f in list.files('R', full.names=TRUE)) {
  if (stringr::str_detect(f, '^R/zzz\.[rR]')) next
  if (scan(f, character(), nmax=1, quiet=TRUE) != "#'") prb <- c(prb, f)
}
expect_true(length(prb) == 0, info=paste0('The following files have no documentation: ', paste(prb, collapse=', ')))

prb <- character()
for (f in list.files('R', full.names=TRUE)) {
  x <- readLines(f)
  if (sum(stringr::str_detect(x, '<- ?function\\(')) != 1) prb <- c(prb, f)
}
expect_true(length(prb) == 0, info=paste0('The following files define multiple functions: ', paste(prb, collapse=', ')))