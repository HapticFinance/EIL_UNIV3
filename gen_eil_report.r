#!/usr/bin/env Rscript
path = getwd()
source(paste(path, "/install.r", sep=""))

rmarkdown::render("gen_eil.rmd",
                  output_file = glue::glue("pdf/eil.pdf"))
 

