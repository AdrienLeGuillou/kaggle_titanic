make_biblio <- function() {
  base <- c("stats", "graphics", "grDevices", "utils", "datasets", "methods")

  required_packages <- 
    (.packages())[!(.packages()) %in% base] %>%
    sort()

  write.bib(required_packages, file = "assets/packages")

  packages_cite <-
    str_c("@", required_packages) %>%
    str_c(collapse = ", ")

  packages_cite <- str_c(
    "---\nnocite: |\n  ",
    packages_cite,
    "\n...\n")

  bib <- str_c(read_file("assets/references.bib"),
               read_file("assets/packages.bib"), sep = "")

  write(bib, file = "references.bib", sep = "")

  write(packages_cite, "assets/citations.Rmd")
}

