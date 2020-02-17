options(knitr.duplicate.label = "allow")  # RESOLVE THIS !!!

build_nocode <- function(params) {
  root_dir <- knitr::opts_knit$get("output.dir")
  site_dir <- file.path(root_dir,"_site")
  exer_dir <- file.path(site_dir,"exercises")
  fname <- paste0(params$basename,".tasks.Rmd")
  i <- file.path(root_dir,fname)
  o <- sub(".Rmd$",".nocode.html",basename(file.path(site_dir,fname)))
  rmarkdown::render(input = i ,
                    output_file = o,
                    output_dir = root_dir,
                    output_format = "html_document")
}
