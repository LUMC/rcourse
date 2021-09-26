library( tidyverse )

#' @export
add <- function( x, ... ) UseMethod( "add", x )

validOr <- function( a, b ) if( !is.null( a ) && !is.na( a ) ) a else b

# Old style functions ----------------------------------------------------

#' @export
read_pulse <- function() {
  read_csv("data/pulse.csv", col_types = cols(
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

#' @export
read_survey <- function() {
  read_csv("data/survey.csv", col_types = cols(
    name = col_character(),
    gender = col_character(),
    span1 = col_double(),
    span2 = col_double(),
    hand = col_character(),
    fold = col_character(),
    pulse = col_double(),
    clap = col_character(),
    exercise = col_character(),
    smokes = col_character(),
    height = col_double(),
    m.i = col_character(),
    age = col_double()    
  ) )
}

#' @export
build_nocode <- function( ... ) {
  #  warning( "building_nocode is oboleted" )
}

#' @export
navigate_slots <- function( ... ) {
  #  warning( "navigate_slots is oboleted" )
}

#' @export
related_slots <- function( ... ) {
  #  warning( "related_slots is oboleted" )
}

#' @export
qa <- function(msg) {
  id <- paste("tag",digest::digest(msg, algo="md5"),sep="")
  txt <- paste("<a class=\"btn btn-primary\" role=\"button\" data-toggle=\"collapse\" href=\"#",id,"\"",sep="")
  txt <- paste(txt, "aria-expanded=\"false\" aria-controls=\"collapseExample\">Answer</a>")
  txt <- paste(txt, "<div class=\"collapse\" id=\"",id,"\"><div class=\"well\">",msg,"</div> </div><br>", sep="")
  cat(knitr::knit_child(text=txt, quiet=TRUE), sep = "\n")
}

#' @export
show_emoji <- function( id, text = id, color = "red" ) {
  #  if( .hasEmo ) {
  emo::ji( id )
  #  } else {
  #    show_color( text = paste0( "[", text, "] " ), color = color )
  #  }
}

#' @export
show_link <- function( url, title, newTab = TRUE ) {
  if( newTab ) {
    paste0( "[", title, "](", url, '){target="_blank"}' )
  } else {
    paste0( "[", title, "](", url, ")" )
  }
}

#' @export
show_menu <- function( menu ) {
  paste0(
    sapply( menu, function( v ) paste0( "`", v, "`" ) ),
    collapse = "&#8611;"
    #    collapse = show_emoji( "down_right_arrow", text = "\\", color = "gray" )
  )
}

#' @export
todo <- function( text, color = "yellow" ) {
  paste0(
    show_emoji( "construction", text = "TODO", color = color ),
    text
  )
}

#' @export
suggest <- function( text ) {
  paste0(
    show_emoji( "warning", text = "?"),
    "<i><u>", text,  "</u></i>"
  )
}

#' @export
direct_exercise <- function( text, color = "black" ) {
  paste0(
    show_emoji( "running_man", text = "EXERCISE", color = color ),
    "<i>", text, "</i>"
  )
}

#' @export
show_warning <- function( text, color = "orange" ) {
  paste0(
    show_emoji( "warning", text = "WARNING", color = color ),
    text
  )
}

#' @export
show_info <- function( text, color = "blue" ) {
  paste0(
    show_emoji( "information", text = "INFO", color = color ),
    text
  )
}

#' @export
go_extern <- function( url, title, goal, color = "blue" ) {
  paste0(
    show_emoji( "right_arrow", text = "GO TO", color = color ),
    "Go to ", show_link( url = url, title = title ),
    " ", goal, "."
  )
}

#' @export
watch_extern_video <- function( url, title, src = NULL, color = "blue" ){
  paste0(
    show_emoji( "movie_camera", text = "VIDEO", color = color ),
    " Watch ", show_link( url = url, title = title ),
    ( if( is.null( src ) ) "" else paste0( " from ", src ) ),
    "."
  )
}

#' @export
info_block0 <- function(msg) {
  txt <- paste("<ion-icon name=\"information-circle\" size=\"large\"></ion-icon> ", "___", msg, "___","<br><br>", sep="")
  cat(knitr::knit_child(text=txt, quiet=TRUE), sep = "\n")
}

#' @export
info_block <- function(msg) {
  # paste("<ion-icon name=\"information-circle\" size=\"large\"></ion-icon> ", "___", msg, "___","<br><br>", sep="")
  paste(icon_style( icons::ionicons("information-circle"),
                    position = "relative",  right= "40px", top="30px", scale = 2, fill="skyblue") , "___", msg, "___","<br><br>", sep="")
}

#' @export
data_file <- function( files = list.files( path = "data", pattern = "*" ) ) {
  fs <- sapply( files, function( f ) {
    paste0(
      "[`", f, "`](data/", f, ") ",
      '<a href="data/', f, '" class="fas fa-download" download target="_blank"></a>'
    )
  } )
  paste0( fs, collapse = ", " )
}

#' @export
lecture <- function( ... ) { Lecture$new( ... ) }

#' @export
session <- function( ... ) { Session$new( ... ) }

#' @export
add.Session <- function( x, ... ) x$add( ... )

#' @export
material <- function( ... ) { Material$new( ... ) }

#' @export
add.Material <- function( x, ... ) x$add( ... )

#' @export
theCourse <- function( ... ) TheCourse$new( ... )

#' @export
add.TheCourse <- function( x, ... ) x$add( ... )


genTestCourse <- function( testOnly = FALSE ) {
  startDate <- as.Date( "2021-06-08" )
  morning <- "09:00-12:30"
  afternoon <- "13:30-17:00"
  course <- theCourse( id = "Boerhaave_2021_Jun", dir = "BrightspaceTest", label = "LUMC/Boerhaave, June 2021: R for data analysis" )
  course <- course$add(
    session( id = "slot1", label = "R and RStudio basics", date = startDate, timeRange = morning ) %>%
      add( lecture( id = "index", label = "Course Introduction", hasTasks = FALSE, min = 15 ) ) %>%
      add( lecture( id = "introduction0", label = "R Introduction", hasTasks = FALSE, min = 10 ) ) %>%
      add( lecture( id = "basic_calculator0", label = "Calculator", min = 25 ) ) %>%
      add( lecture( id = "basic_variables0", label = "Variables", min = 25, pauseMin = 30 ) ) %>%
      add( lecture( id = "course_data0", label = "Example data (pulse, survey)", hasTasks = FALSE, min = 10 ) ) %>%
      add( lecture( id = "basic_projects0", label = "Projects", hasTasks = FALSE, min = 20 ) ) %>%
      add( lecture( id = "basic_scripts0", label = "Scripts", hasTasks = FALSE, min = 60 ) )
  )
  if( !testOnly ) {
    course <- course$add(
      session( id = "slot2", label = "Data structures 1/2", date = startDate, timeRange = afternoon ) %>%
        add( lecture( id = "basic_vectors0", label = "Vectors", min = 120, pauseMin = 30 ) ) %>%
        add( lecture( id = "basic_factors0", label = "Factors", min = 60 ) ) %>%
        add( lecture( id = "packages0", label = "Install/use packages", hasTasks = FALSE, min = 30 ) )
    )
    course <- course$add(
      session( id = "slot3", label = "Data manipulation 1/3", date = startDate + 1, timeRange = morning ) %>%
        add( lecture( id = "tidyverse0", label = "Tidyverse library", hasTasks = FALSE, min = 45 ) ) %>%
        add( lecture( id = "dplyr_tibble0", label = "Tibble", min = 30 ) ) %>%
        add( lecture( id = "dplyr_select0", label = "Select variables/cols", min = 30 ) ) %>%
        add( lecture( id = "dplyr_filter0", label = "Filter observations/rows", min = 30 ) ) %>%
        add( lecture( id = "dplyr_mutate0", label = "Add/modify variables", min = 30 ) )
    )
    course <- course$add(
      session( id = "slot4", label = "Data manipulation 2/3", date = startDate + 1, timeRange = afternoon ) %>%
        add( lecture( id = "dplyr_pipe0", label = "Pipe operator", min = 45 ) ) %>%
        add( lecture( id = "dplyr_summarise0", label = "Summarise", min = 30 ) ) %>%
        add( lecture( id = "dplyr_group0", label = "Groups", min = 30 ) )
    )
    course <- course$add(
      session( id = "slot5", label = "Data structures 2/2", date = startDate + 2, timeRange = morning ) %>%
        add( lecture( id = "basic_lists0", label = "Lists", min = 45 ) ) %>%
        add( lecture( id = "basic_formulas0", label = "Formulas", min = 30 ) ) %>%
        add( lecture( id = "basic_matrices0", label = "Matrices", min = 30 ) )
    )
    course <- course$add(
      session( id = "slot6", label = "Graphics", date = startDate + 2, timeRange = afternoon ) %>%
        add( lecture( id = "ggplot_basics0", label = "Plots/ggplot2", min = 45 ) ) %>%
        add( lecture( id = "ggplot_scales0", label = "Plot axes/scales", min = 30 ) ) %>%
        add( lecture( id = "ggplot_facets_themes0", label = "Plot panels/facets", min = 30 ) ) %>%
        add( lecture( id = "ggplot_geoms0", label = "Plot types", hasTasks = FALSE, min = 30 ) )
    )
    course <- course$add(
      session( id = "slot7", label = "Data manipulation 3/3 and functions", date = startDate + 3, timeRange = morning ) %>%
        add( lecture( id = "dplyr_join0", label = "Merging/joining tables", min = 45 ) ) %>%
        add( lecture( id = "tidyr_reshape0", label = "Reshaping tables", min = 30 ) ) %>%
        add( lecture( id = "advanced_user_functions0", label = "User-defined functions", min = 30 ) ) %>%
        add( lecture( id = "useful_functions0", label = "Useful R functions", hasTasks = FALSE, min = 30 ) )
    )
    course <- course$add(
      session( id = "slot8", label = "Self-study assignment", date = startDate + 3, timeRange = afternoon )
    )
  }
  course <- course$add(
    material( id = "pulse", label = "Pulse dataset, CSV format", path = "data/pulse.csv", outPath = "data/pulse.csv"  ),
    material( id = "survey", label = "Survey dataset, CSV format", path = "data/survey.csv", outPath = "data/survey.csv" ),
    material( id = "exProjDir", label = "RStudio project directory example", path = "materials/RStudio_Project_Dir_Example.zip", outPath = "materials/RStudio_Project_Dir_Example.zip" )
  )
  
  renderer <- Renderer$new( outDir = "tmp" )
  #renderer <- BrightspaceRenderer$new( outDir = "tmp" )
  renderer$makeAll( course = course )
}

