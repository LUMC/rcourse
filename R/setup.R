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
                    output_format = "html_document", params=list(ref="aap"))
}


.read_schedule <- function() {
  root_dir <- knitr::opts_knit$get("output.dir")
  site_dir <- file.path(root_dir,"_site")
  exer_dir <- file.path(site_dir,"exercises")
  cfg <- file.path(root_dir,"_schedule.yml")
  if (file.exists(cfg) ) {
    .schedule_ <- yaml.load_file( cfg  )
  } else {
    error("missing _schedule.yml !")
  }  
  .schedule_
}

.next_slot<- function(base_name) {
  .read_schedule()[["course"]][["slots"]][[base_name]][["next"]]
}

.prev_slot <- function(base_name) {
  .read_schedule()[["course"]][["slots"]][[base_name]][["prev"]]
}

.related <- function(base_name) {
    rel <- .read_schedule()[["course"]][["slots"]][[base_name]][["related"]]
    if (!is.null(rel))
      strsplit(rel," ")[[1]]
    else 
      NULL
}
  
.read_pulse <- function() {
  read_csv("_data/pulse.csv", col_types = cols(
    id = col_character(),
    name = col_character(),
    height = col_double(),
    weight = col_double(),
    age = col_double(),
    gender = col_character(),
    smokes = col_character(),
    alcohol = col_character(),
    exercise = col_character(),
    ran = col_character(),
    pulse1 = col_double(),
    pulse2 = col_double(),
    year = col_double()
  ) )  
}
.read_csv_data <- function(file) {
  root_dir <- knitr::opts_knit$get("output.dir")
  read.csv(file.path(root_dir,"_data",file))
}
