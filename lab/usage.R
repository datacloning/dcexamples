## requires: knitr and rmarkdown

renderDcExample <- function(file, ...)
    rmarkdown::render(file, ...)

renderDcExample("~/repos/dcexamples/lab/seeds.R",
    output_format="pdf_document", clean=FALSE)
