# Package names
packages <- c(
  "deSolve",
  "foreach",
  "xts",
  "ggplot2",
  "pracma",
  "contfrac",
  "stats",
  "R.matlab",
  "writexl",
  "dplyr",
  "rbenchmark",
  "data.table",
  "tictoc",
  "NCmisc",
  "sysid",
  "minpack.lm",
  "rugarch",
  "kableExtra",
  "patchwork",
  "httr",
  "purrr",
  "ghql",
  "jsonlite",
  "xts",
  "tidyverse"
)

#install.packages("/data/code/BSC/Haptic/haptic_poc/NORMT3", repos=NULL,  type="source")
#devtools::install_github("jessevent/crypto")
#devtools::install_github("deanfantazzini/bitcoinFinance")
# devtools::install_github("https://github.com/cran/somebm.git", force=TRUE)

# for (i in 1:length(c)) {
#  install.packages(packages[i])
# }
# suppressPackageStartupMessages(library(plyr))

# webshot::install_phantomjs()

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE, quietly = TRUE))

`?` <- function(x, y)
    eval(
      sapply(
        strsplit(
          deparse(substitute(y)), 
          ":"
      ), 
      function(e) parse(text = e)
    )[[2 - as.logical(x)]])