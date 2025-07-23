library(tinytest)
suppressPackageStartupMessages(devtools::load_all())

# Create example test files
for (f in list.files('tinytest', pattern='examples-.*', full.names=TRUE)) unlink(f)
for (f in list.files('../man', full.names=TRUE)) {
  id <- basename(tools::file_path_sans_ext(f))
  cat(sprintf("setwd('../../')
expect_silent(pkgload::run_example('%s', quiet=TRUE))
",
              paste0('man/', id, '.Rd')),
      file=paste0('tinytest/examples-', id, '.r'))
}

# Test
run_test_dir('tinytest', pattern='*.[rR]')
