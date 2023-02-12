# Package names
packages <- c(
  "rmarkdown",
  "pracma",
  "kableExtra",
  "ggplot2",
  "tictoc"
)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE, quietly = TRUE))
