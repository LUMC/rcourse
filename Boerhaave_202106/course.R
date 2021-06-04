library( rooc )

genCourse <- function( dir, testOnly = FALSE ) {
  startDate <- as.Date( "2021-06-08" )
  morning <- "09:00-12:30"
  afternoon <- "13:30-17:00"
  breaks3x60 <- "l60:b15:l60:b15:l60"
  breaks2x90 <- "l90:b30:l90"
  course <- theCourse( id = "Boerhaave_202106", dir = dir, label = "LUMC/Boerhaave, Jun2021: Using R for data analysis" )
  course <- course$add(
    session( id = "slot1", label = "R and RStudio basics", date = startDate, timeRange = morning, breaksPattern = breaks3x60 ) %>%
      add( lecture( id = "index", label = "Course Introduction", hasTasks = FALSE, min = 15 ) ) %>%
      add( lecture( id = "introduction0", label = "R Introduction", hasTasks = FALSE, min = 15 ) ) %>%
      add( lecture( id = "basic_calculator0", label = "Calculator", min = 30 ) ) %>%
      add( lecture( id = "basic_variables0", label = "Variables", min = 30 ) ) %>%
      add( lecture( id = "course_data0", label = "Example data (pulse, survey)", hasTasks = FALSE, min = 10 ) ) %>%
      add( lecture( id = "basic_projects0", label = "Projects", hasTasks = FALSE, min = 20 ) ) %>%
      add( lecture( id = "basic_scripts0", label = "Scripts", hasTasks = FALSE, min = 60 ) )
  )
  if( !testOnly ) {
    course <- course$add(
      session( id = "slot2", label = "Data structures 1/2", date = startDate, timeRange = afternoon, breaksPattern = breaks3x60 ) %>%
        add( lecture( id = "basic_vectors0", label = "Vectors", min = 100 ) ) %>%
        add( lecture( id = "basic_factors0", label = "Factors", min = 60 ) ) %>%
        add( lecture( id = "packages0", label = "Install/use packages", hasTasks = FALSE, min = 20 ) )
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
      session( id = "slot5", label = "Data structures 2/2", date = startDate + 2, timeRange = morning, breaksPattern = breaks3x60 ) %>%
        add( lecture( id = "basic_lists0", label = "Lists", min = 60 ) ) %>%
        add( lecture( id = "basic_formulas0", label = "Formulas", min = 60 ) ) %>%
        add( lecture( id = "basic_matrices0", label = "Matrices", min = 60 ) )
    )
    course <- course$add(
      session( id = "slot6", label = "Graphics", date = startDate + 2, timeRange = afternoon, breaksPattern = breaks2x90 ) %>%
        add( lecture( id = "ggplot_basics0", label = "Plots/ggplot2", min = 45 ) ) %>%
        add( lecture( id = "ggplot_scales0", label = "Plot axes/scales", min = 45 ) ) %>%
        add( lecture( id = "ggplot_facets_themes0", label = "Plot panels/facets/size", min = 45 ) ) %>%
        add( lecture( id = "ggplot_geoms0", label = "Plot types", hasTasks = FALSE, min = 45 ) )
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

  course
}

course <- genCourse( dir = "Boerhaave_202106", FALSE )
#course$lecturesTibble( TRUE ) # check times

renderer <- Renderer$new( outDir = "Boerhaave_202106.site" )
#renderer <- BrightspaceRenderer$new( outDir = "tmp" )
renderer$makeAll( course = course )
