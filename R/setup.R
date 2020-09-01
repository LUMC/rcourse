.hasEmo <- suppressWarnings( require( "emo", quietly = TRUE ) )

build_nocode <- function(params) {
  root_dir <- knitr::opts_knit$get("output.dir")
  site_dir <- file.path(root_dir,"_site")
  exer_dir <- file.path(site_dir,"exercises")
  fname <- paste0(params$basename,".tasks.Rmd")
  i <- file.path(root_dir,fname)
  o <- sub(".Rmd$",".nocode.html",basename(file.path(site_dir,fname)))
  root_dir <- knitr::opts_knit$set(solutions="no")
  rmarkdown::render(input = i ,
                    output_file = o,
                    output_dir = root_dir,
                    output_format = "html_document")
  root_dir <- knitr::opts_knit$set(solutions="yes")
  o <- sub(".Rmd$",".html",basename(file.path(site_dir,fname)))
  rmarkdown::render(input = i ,
                    output_file = o,
                    output_dir = root_dir,
                    output_format = "html_document")
}


read_schedule <- function() {
  root_dir <- knitr::opts_knit$get("output.dir")
  site_dir <- file.path(root_dir,"_site")
  exer_dir <- file.path(site_dir,"exercises")
  cfg <- file.path(root_dir,"_schedule.yml")
  if (file.exists(cfg) ) {
    schedule_ <- yaml.load_file( cfg  )
  } else {
    error("missing _schedule.yml !")
  }
  schedule_
}


qa <- function(msg) {
  id <- paste("tag",digest(msg, algo="md5"),sep="")
  txt <- paste("<a class=\"btn btn-primary\" role=\"button\" data-toggle=\"collapse\" href=\"#",id,"\"",sep="")
  txt <- paste(txt, "aria-expanded=\"false\" aria-controls=\"collapseExample\">Answer</a>")
  txt <- paste(txt, "<div class=\"collapse\" id=\"",id,"\"><div class=\"well\">",msg,"</div> </div><br>", sep="")
  cat(knitr::knit_child(text=txt, quiet=TRUE), sep = "\n")
}

info_block0 <- function(msg) {
  txt <- paste("<ion-icon name=\"information-circle\" size=\"large\"></ion-icon> ", "___", msg, "___","<br><br>", sep="")
  cat(knitr::knit_child(text=txt, quiet=TRUE), sep = "\n")
}

info_block <- function(msg) {
  paste("<ion-icon name=\"information-circle\" size=\"large\"></ion-icon> ", "___", msg, "___","<br><br>", sep="")
}

alert_block <- function(params,alert="alert-info", align="right") {
  .next <- next_slot(params$basename)
  .prev <- prev_slot(params$basename)
  .next_html <- paste(.next,".html", sep="")
  .prev_html <- paste(.prev,".html", sep="")
  .next_label <- ""
  .prev_label <- ""

  txt <- paste("<div class=\"alert ",alert,"\" role=\"alert\" style=\"text-align: ",align,";\" >", sep="")

  if (!is.null(.prev)) {
    .prev_label <- strsplit(sub("[0-9]+","",.prev),"_")[[1]]
    .prev_label <- .prev_label[length(.prev_label)]
    txt <- paste(txt,"<a class=\"btn btn-primary\" href=",.prev_html,"role=\"button\">&laquo;",.prev_label,"</a>")
  } else {
    txt <- paste(txt,"<a class=\"btn btn-primary\" onclick=\"goBack()\" role=\"button\">&laquo;",.prev_label,"back</a>" )
  }

  if (!is.null(.next)) {
    .next_label <- strsplit(sub("[0-9]+","",.next),"_")[[1]]
    .next_label <- .next_label[length(.next_label)]
    txt <- paste(txt,"<a class=\"btn btn-primary \" href=",.next_html,"role=\"button\">",.next_label,"&raquo;</a>" )
  }
  txt <- paste(txt, "</div>")
  cat(knitr::knit_child(text=txt, quiet=TRUE), sep = "\n")
}

next_slot<- function(base_name) {
  read_schedule()[["course"]][["slots"]][[base_name]][["next"]]
}

prev_slot <- function(base_name) {
  read_schedule()[["course"]][["slots"]][[base_name]][["prev"]]
}

related_slots <- function(base_name) {
    rel <- read_schedule()[["course"]][["slots"]][[base_name]][["related"]]
    if (!is.null(rel))
      strsplit(rel," ")[[1]]
    else
      NULL
}

read_pulse <- function() {
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

read_survey <- function() {
  read_csv("_data/survey.csv", col_types = cols(
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



# .read_csv_data <- function(file) {
#   root_dir <- knitr::opts_knit$get("output.dir")
#   read.csv(file.path(root_dir,"_data",file))
# }

show_color <- function( text, color ) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color,  text )
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, text)
  } else text
}

show_emoji <- function( id, text = id, color = "red" ) {
  if( .hasEmo ) {
    emo::ji( id )
  } else {
    show_color( text = paste0( "[", text, "] " ), color = color )
  }
}

show_link <- function( url, title, newTab = TRUE ) {
  if( newTab ) {
    paste0( "[", title, "](", url, '){target="_blank"}' )
  } else {
    paste0( "[", title, "](", url, ")" )
  }
}

show_menu <- function( menu ) {
  paste0(
    sapply( menu, function( v ) paste0( "`", v, "`" ) ),
    collapse = show_emoji( "down_right_arrow", text = "\\", color = "gray" )
  )
}

todo <- function( text, color = "yellow" ) {
  paste0(
    show_emoji( "construction", text = "TODO", color = color ),
    text
  )
}

suggest <- function( text ) {
  paste0(
    show_emoji( "warning", text = "?"),
    "<i><u>", text,  "</u></i>"
  )
}

direct_exercise <- function( text, color = "black" ) {
  paste0(
    show_emoji( "running_man", text = "EXERCISE", color = color ),
    "<i>", text, "</i>"
  )
}

show_warning <- function( text, color = "orange" ) {
  paste0(
    show_emoji( "warning", text = "WARNING", color = color ),
    text
  )
}

show_info <- function( text, color = "blue" ) {
  paste0(
    show_emoji( "information", text = "INFO", color = color ),
    text
  )
}

go_extern <- function( url, title, goal, color = "blue" ) {
  paste0(
    show_emoji( "right_arrow", text = "GO TO", color = color ),
    "Go to ", show_link( url = url, title = title ),
    " ", goal, "."
  )
}

watch_extern_video <- function( url, title, src = NULL, color = "blue" ){
  paste0(
    show_emoji( "movie_camera", text = "VIDEO", color = color ),
    " Watch ", show_link( url = url, title = title ),
    ( if( is.null( src ) ) "" else paste0( " from ", src ) ),
    "."
  )
}

