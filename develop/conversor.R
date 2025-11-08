"""
#' @title R Markdown Conversion Utilities
#' @description Helper functions to convert between notebook formats used in this
#'     repository. These utilities support converting `.ipynb` to R Markdown and
#'     knitting `.Rmd` to GitHub‑friendly `.md` with figures placed under a
#'     deterministic folder. They are intended for developer workflow automation.
#' @references Xie, Y. (2018). R Markdown: The Definitive Guide. CRC Press.
#' @keywords internal tooling
"""

#' Convert a Jupyter notebook to R Markdown
#' @param input Path to a `.ipynb` file.
#' @return Invisibly returns the output file path or an error message string on
#'     failure.
#' @examples
#' # convert_ipynb_to_rmarkdown("Rmd/examples/demo.ipynb")
convert_ipynb_to_rmarkdown <- function(input) {
  if (!require("rmarkdown")) return("Missing necessary package: 'rmarkdown'")
  if (tolower(xfun::file_ext(input)) != "ipynb") {
    return("Error: invalid file format; expected .ipynb")
  }
  rmarkdown::convert_ipynb(input)
}

#' Delete a Jupyter notebook file
#' @param input Path to a `.ipynb` file to remove.
#' @return Logical indicating success.
delete_ipynb <- function(input) {
  file.remove(input)
}

#' Knit an Rmd file to a GitHub‑style Markdown
#' @param input Path to a `.Rmd` file.
#' @return Invisibly returns the output `.md` file path or an error message
#'     string on failure.
#' @examples
#' # convert_rmd_md("Rmd/examples/nab_samples.Rmd")
convert_rmd_md <- function(input) {
  if (!require("rmarkdown")) return("Missing necessary package: 'rmarkdown'")
  if (!require("markdown"))  return("Missing necessary package: 'markdown'")
  if (!require("knitr"))     return("Missing necessary package: 'knitr'")

  if (tolower(xfun::file_ext(input)) != "rmd") {
    return("Error: invalid file format; expected .Rmd")
  }

  md_file <- xfun::with_ext(input, "md")
  md_file <- gsub("Rmd/", "", md_file)
  fig_dir <- sprintf("%s/fig/%s", dirname(md_file), basename(xfun::with_ext(input, "")))

  unlink("figure", recursive = TRUE)
  unlink(fig_dir, recursive = TRUE)

  # Knit to markdown (generates figure/)
  knitr::knit(input, md_file)

  # Move figures to deterministic location used by repo
  if (dir.exists("figure")) file.rename("figure", fig_dir)

  # Fix figure paths in the md
  con <- file(md_file, encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  data <- readLines(con)
  data <- gsub("figure/", sprintf("fig/%s/", basename(fig_dir)), data)
  writeLines(data, con = file(md_file, encoding = "UTF-8"))

  invisible(md_file)
}



dir <- "Rmd"

texs <- list.files(path = dir, pattern = ".ipynb$", full.names = TRUE, recursive = TRUE)
if (FALSE) {
  for (tex in texs) {
    print(tex)
    convert_ipynb_to_rmarkdown(tex)
  }
}
if (FALSE) {
  for (tex in texs) {
    print(tex)
    delete_ipynb(tex)
  }
}

dir <- "Rmd"
texs <- list.files(path = dir, pattern = ".Rmd$", full.names = TRUE, recursive = TRUE)
if (TRUE) {
  for (tex in texs) {
    print(tex)
    convert_rmd_md(tex)
  }
}
#Procurar por ## Error
