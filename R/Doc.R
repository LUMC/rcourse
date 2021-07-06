library( R6 )
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

# Doc --------------------------------------------------------------------

#' @export
Doc <- R6Class(
  "Doc",
  private = list(
    id_ = NULL,
    label_ = NULL
  ),
  public = list(
    initialize = function(
      id, label = NULL
    ) {
      private$id_ <- id
      private$label_ <- label
    },
    id = function() { private$id_ },
    label = function() { private$label_ },
    url = function() { stop( "Not overridden" ) }
  )
)

testthat::test_that( "Doc properties", {
  doc <- Doc$new( id = "lecture1" )
  testthat::expect_identical( doc$id(), "lecture1" )
} )

# CopiedDoc --------------------------------------------------------------

CopiedDoc <- R6Class(
  "CopiedDoc",
  inherit = Doc,
  private = list(
    path_ = NULL,
    outPath_ = NULL
  ),
  public = list(
    initialize = function(
      path, outPath = NULL, ...
    ) {
      super$initialize( ... )
      private$path_ <- path
      private$outPath_ <- outPath
    },
    path = function() { private$path_ },
    outPath = function() { private$outPath_ },
    url = function() { private$outPath_ }
  )
)

testthat::test_that( "CopiedDoc properties", {
  doc <- CopiedDoc$new( id = "lecture1", label = "Lecture 1", path = "abc.Rmd", outPath = "src/abc.Rmd" )
  testthat::expect_identical( doc$id(), "lecture1" )
  testthat::expect_identical( doc$path(), "abc.Rmd" )
  testthat::expect_identical( doc$outPath(), "src/abc.Rmd" )
  #testthat::expect_identical( doc$outDir(), NULL )
} )

# RenderedDoc ------------------------------------------------------------

RenderedDoc <- R6Class(
  "RenderedDoc",
  inherit = Doc,
  private = list(
    rmdFile_ = NULL,
    naviIds_ = NULL,
    type_ = NULL,
    sessionIdx_ = NULL,
    lectureIdx_ = NULL
  ),
  public = list(
    initialize = function(
      rmdFile = NULL, naviIds = NULL,
      type = "?", sessionIdx = 0L, lectureIdx = 0L, ...
    ) {
      super$initialize( ... )
      private$rmdFile_ <- rmdFile
      private$naviIds_ <- naviIds
      private$type_ <- type
      private$sessionIdx_ <- sessionIdx
      private$lectureIdx_ <- lectureIdx
    },
    rmdFile = function() { private$rmdFile_ },
    naviIds = function() { private$naviIds_ },
    type = function( long = FALSE ) {
      if( !long ) {
        private$type_
      } else {
        ( c(
          "?" = "unknown", "l" = "lecture",
          "s" = "solutions", "p" = "practice",
          "c" = "toc", "M" = "materials", "m" = "material"
        ) )[[ private$type_ ]]
      }
    },
    sessionIdx = function() { private$sessionIdx_ },
    lectureIdx = function() { private$lectureIdx_ },
    setupFun = function() {
      ( c(
        "l" = function() {
          knitr::opts_chunk$set( echo = TRUE, eval = TRUE, comment = NA, paged.print=FALSE )
        },
        "s" = function() {
          knitr::opts_chunk$set( echo = TRUE, eval = TRUE, comment = NA, paged.print=FALSE )
        },
        "p" = function() {
          knitr::opts_chunk$set( echo = FALSE, eval = FALSE, comment = NA, paged.print=FALSE )
        }
      ) )[[ private$type_ ]]
    },
    hideFoldedCode = function() { # NA: no folding, TRUE: hide, FALSE: show
      ( c(
        "?" = NA, "l" = NA,
        "s" = TRUE, "p" = NA,
        "c" = NA, "M" = NA, "m" = NA
      ) )[[ private$type_ ]]
    }
  )
)

testthat::test_that( "RenderedDoc properties", {
  doc <- RenderedDoc$new( id = "lecture1", label = "Lecture 1", naviIds = list( "next" = "lecture2" ), rmdFile = "abc.Rmd" )
  testthat::expect_identical( doc$id(), "lecture1" )
  testthat::expect_identical( doc$rmdFile(), "abc.Rmd" )
} )

# Lecture ----------------------------------------------------------------

#' @export
Lecture <- R6Class(
  classname = "Lecture",
  private = list(
    id_ = NULL,
    label_ = NULL,
    rmdFile_ = NULL,
    hasTasks_ = NULL,
    min_ = NULL
  ),
  public = list(
    initialize = function( id, label = NULL, rmdFile = NULL, hasTasks = TRUE, min = NA, ... ) {
      stopifnot( !is.null( id ) )
      private$id_ <- id
      private$label_ <- label
      private$rmdFile_ <- rmdFile
      private$hasTasks_ <- hasTasks
      private$min_ <- min
    },
    id = function() { private$id_ },
    label = function() { validOr( private$label_, private$id_ ) },
    rmdFile = function() {
      rmdFile <- private$rmdFile_
      if( is.null( rmdFile ) ) rmdFile <- paste0( private$id_, ".Rmd" )
      rmdFile
    },
    hasTasks = function() { private$hasTasks_ },
    min = function() { private$min_ },
    asTibble = function() {
      d <- tibble(
        id = self$id(), label = self$label(), rmdFile = self$rmdFile(),
        hasTasks = self$hasTasks(), min = self$min()
      )
      colnames( d ) <- paste0( "lecture.", colnames( d ) )
      d
    }
  )
)

#' @export
lecture <- function( ... ) { Lecture$new( ... ) }

testthat::test_that( "Lecture properties", {
  l1 <- lecture( id = "lecture1", label = "Lecture1", min = 30 )
  testthat::expect_equal( l1$id(), "lecture1" )
  testthat::expect_equal( l1$label(), "Lecture1" )
  testthat::expect_equal( l1$rmdFile(), "lecture1.Rmd" )
  testthat::expect_equal( l1$hasTasks(), TRUE )

  d <- l1$asTibble()
  testthat::expect_equal( nrow( d ), 1L )
  testthat::expect_true( all( c( "lecture.id", "lecture.label", "lecture.rmdFile" ) %in% colnames( d ) ) )
})

# Session ----------------------------------------------------------------

#' @export
Session <- R6Class(
  classname = "Session",
  private = list(
    id_ = NULL,
    attrs_ = NULL,
    lectures_ = NULL
  ),
  public = list(
    initialize = function( id, ... ) {
      stopifnot( !is.null( id ) )
      private$id_ <- id
      private$attrs_ <- list( ... )
      private$lectures_ <- list()
    },
    id = function() { private$id_ },
    lectureIds = function() { sapply( private$lectures_, function( l ) l$id() ) },
    add = function( x ) {
      stopifnot( inherits( x, "Lecture" ) )
      stopifnot( !( x$id() %in% self$lectureIds() ) )
      private$lectures_ <- c( private$lectures_, x )
      invisible( self )
    },
    lecturesTibble = function() {
      d <- tibble( id = private$id_ )
      a <- map( private$attrs_, ~ ( if( is.null( .x ) ) NA else .x ) )
      if( length( private$attrs_ ) > 0 ) d <- d %>% bind_cols( as_tibble( a ) )
      colnames( d ) <- paste0( "session.", colnames( d ) )
      bind_rows( lapply( seq_len( length( private$lectures_ ) ), function( idx ) {
        d %>% bind_cols(
          private$lectures_[[ idx ]]$asTibble() %>% mutate( lecture.idx = idx )
        )
      } ) )
    }
  )
)

#' @export
session <- function( ... ) { Session$new( ... ) }

#' @export
add.Session <- function( x, ... ) x$add( ... )

testthat::test_that( "Session properties", {
  l1 <- lecture( id = "lecture1", min = 30 )
  l2 <- lecture( id = "lecture2", min = 45 )
  s1 <- session( id = "session1" ) %>% add( l1 ) %>% add( l2 )
  testthat::expect_identical( s1$lectureIds(), c( "lecture1", "lecture2" ) )
})

# Material ---------------------------------------------------------------

#' @export
Material <- R6Class(
  classname = "Material",
  private = list(
    id_ = NULL,
    label_ = NULL,
    path_ = NULL,
    outPath_ = NULL
  ),
  public = list(
    initialize = function( id, label, path, outPath ) {
      stopifnot( !is.null( id ) )
      stopifnot( !is.null( path ) )
      private$id_ <- id
      private$label_ <- label
      private$path_ <- path
      private$outPath_ <- outPath
    },
    id = function() { private$id_ },
    label = function() { private$label_ },
    path = function() { private$path_ },
    outPath = function() { private$outPath_ },
    asTibble = function() {
      d <- tibble(
        id = self$id(), label = self$label(),
        path = self$path(), outPath = self$outPath()
      )
      colnames( d ) <- paste0( "material.", colnames( d ) )
      d
    }
  )
)

#' @export
material <- function( ... ) { Material$new( ... ) }

#' @export
add.Material <- function( x, ... ) x$add( ... )

testthat::test_that( "Material properties", {
  m1 <- material( id = "material1", label = "First dataset", path = "material1.csv", outPath = "data/material1.csv" )
  testthat::expect_equal( m1$id(), "material1" )
})


# TheCourse --------------------------------------------------------------

#' @export
TheCourse <- R6Class(
  "TheCourse",
  private = list(
    id_ = NULL,
    dir_ = NULL,
    label_ = NULL,
    attrs_ = NULL,
    sessions_ = NULL,
    materials_ = NULL,

    d_ = NULL,
    invalidate = function() { d_ = NULL },
    validate = function() {
      if( is.null( private$d_ ) ) {
        d <- tibble( id = private$id_ )
        if( length( private$attrs_ ) > 0 ) d <- d %>% bind_cols( as_tibble( private$attrs_ ) )
        colnames( d ) <- paste0( "course.", colnames( d ) )
        private$d_ <- bind_rows( lapply( seq_len( length( private$sessions_ ) ), function( idx ) {
          d %>% bind_cols(
            private$sessions_[[ idx ]]$lecturesTibble() %>% mutate( session.idx = idx )
          )
        } ) ) %>%
          arrange( session.idx, lecture.idx ) %>%
          mutate( lecture.prevId = lag( lecture.id ) ) %>%
          mutate( lecture.nextId = lead( lecture.id ) )
      }
    },
    addLectureBreaks = function( d ) {
      d %>% split( .$session.id ) %>% lapply( function( ls ) {
        if( nrow( ls ) > 0 ) {
          sess <- ls %>% select( matches( "^(course|session)[.]" ) ) %>% distinct() %>%
            mutate( lecture.label = "Break" )
          stopifnot( nrow( sess ) == 1 )
          if( !is.na( sess$session.breaksPattern ) ) {
            bs <- strsplit( sess$session.breaksPattern, "[:]" ) %>% unlist() %>% lapply( function( b ) {
              m <- regmatches( b, regexec( "^([lb])([0-9]+)$", b ) )[[1]]
              tibble( isBreak = m[2] == "b", min = as.numeric( m[3] ) )
            } ) %>% bind_rows()

            ret <- tibble()
            while( nrow( bs ) > 0L || nrow( ls ) > 0L ) {
              if( nrow( bs ) == 0L && nrow( ls ) > 0L ) {
                stop( "Session '", sess$session.id, "' too long." )
              } else if( nrow( bs ) > 0L && nrow( ls ) == 0L ) {
                warning( "Session '", sess$session.id, "' too short." )
                break
              } else if( bs$isBreak[[1]] ) {
                ret <- bind_rows( ret, sess %>% mutate( slot.min = bs$min[[1]] ) ) # emit break
                bs <- bs[-1,]
              } else {
                if( ls$lecture.min[[1]] <= bs$min[[1]] ) {
                  bs$min[[1]] <- bs$min[[1]] - ls$lecture.min[[1]]
                  ret <- bind_rows( ret, ls[1,] %>% mutate( slot.min = lecture.min[[1]] ) ) # emit complete lecture
                  ls <- ls[-1,]
                  if( bs$min[[1]] <= 0 ) bs <- bs[-1,]
                } else {
                  ls$lecture.min[[1]] <- ls$lecture.min[[1]] - bs$min[[1]]
                  ret <- bind_rows( ret, ls[1,] %>% mutate( slot.min = bs$min[[1]] ) ) # emit part of lecture
                  bs <- bs[-1,]
                }
              }
            }
            ls <- ret
          }
        }
        ls %>% mutate( slot.idx = seq_len( n() ) )
      } ) %>% bind_rows()
    }
  ),
  public = list(
    initialize = function( id, dir, label = id, ... ) {
      stopifnot( !is.null( id ) )
      private$id_ <- id
      private$dir_ <- dir
      private$label_ <- label
      private$attrs_ <- list( ... )
      private$sessions_ <- list()
      private$materials_ <- list()

      private$d_ <- NULL
    },
    id = function() { private$id_ },
    dir = function() { private$dir_ },
    label = function() { private$label_ },
    sessionIds = function() { sapply( private$sessions_, function( l ) l$id() ) },
    lectureIds = function() {
      do.call( c, lapply( private$sessions_, function( l ) l$lectureIds() ) )
    },
    lectureWithTasksIds = function() {
      self$lecturesTibble() %>% filter( lecture.hasTasks ) %>% pull( lecture.id )
    },
    materialIds = function() { sapply( private$materials_, function( l ) l$id() ) },
    id2label = function( docId ) {
      self$lectureDoc( lectureId = docId )$label()
    },
    add = function( ... ) {
      for( x in list( ... ) ) {
        if( inherits( x, "Session" ) ) {
          stopifnot( !( x$id() %in% self$sessionIds() ) )
          stopifnot( !any( x$lectureIds() %in% self$lectureIds() ) )
          private$invalidate()
          private$sessions_ <- c( private$sessions_, x )
        } else if( inherits( x, "Material" ) ) {
          stopifnot( !( x$id() %in% self$materialIds() ) )
          private$invalidate()
          private$materials_ <- c( private$materials_, x )
        } else {
          stop( "Trying to add object of incorrect type." )
        }
      }
      invisible( self )
    },
    materialsTibble = function() {
      bind_rows( lapply( private$materials_, function( l ) l$asTibble() ) )
    },
    lecturesTibble = function( withBreaks = FALSE ) {
      private$validate()
      d <- private$d_
      if( withBreaks ) {
        d <- private$addLectureBreaks( d )
      }
      d
    },
    lectureDoc = function( lectureId ) {
      d <- self$lecturesTibble() %>% filter( lecture.id == lectureId )
      stopifnot( nrow( d ) == 1 )
      li <- as.list( d )

      RenderedDoc$new(
        id = lectureId,
        label = li$lecture.label,
        rmdFile = li$lecture.rmdFile,
        naviIds = list(
          "prev" = li$lecture.prevId,
          "next" = li$lecture.nextId,
          "tasks" = li$lecture.hasTasks
        ),
        type = "l",
        sessionIdx = li$session.idx,
        lectureIdx = li$lecture.idx
      )
    },
    taskDoc = function( lectureId, enableCode ) {
      d <- self$lecturesTibble() %>% filter( lecture.id == lectureId )
      stopifnot( nrow( d ) == 1 )
      li <- as.list( d )

      RenderedDoc$new(
        id = paste0( lectureId, ".tasks", ( if( enableCode ) '.code' else '.nocode' ) ),
        label = paste0( li$lecture.label, ( if( enableCode ) ' (solutions)' else ' (practice)' ) ),
        rmdFile = gsub( "[.]Rmd", ".tasks.Rmd", li$lecture.rmdFile ),
        naviIds = list(
          "lecture" = lectureId
        ),
        type = if( enableCode ) 's' else 'p',
        sessionIdx = li$session.idx,
        lectureIdx = li$lecture.idx
      )
    },
    tocDoc = function() {
      RenderedDoc$new(
        id = "toc",
        label = "Table of Contents",
        rmdFile = NULL,
        naviIds = list(),
        type = 'c',
        sessionIdx = 0,
        lectureIdx = 1
      )
    },
    materialsDoc = function() {
      RenderedDoc$new(
        id = "materials",
        label = "Materials",
        rmdFile = NULL,
        naviIds = list(),
        type = 'M',
        sessionIdx = 0,
        lectureIdx = 2
      )
    },
    materialDoc = function( materialId ) {
      d <- self$materialsTibble() %>% filter( material.id == materialId )
      stopifnot( nrow( d ) == 1 )
      li <- as.list( d )

      CopiedDoc$new(
        id = materialId,
        label = li$material.label,
        path = li$material.path,
        outPath = li$material.outPath
      )
    }
  )
)

#' @export
theCourse <- function( ... ) TheCourse$new( ... )

#' @export
add.TheCourse <- function( x, ... ) x$add( ... )

testthat::test_that( "TheCourse properties", {
  l11 <- lecture( id = "lecture11", min = 30 )
  l12 <- lecture( id = "lecture12", label = "Lecture12", min = 45 )
  l21 <- lecture( id = "lecture21", min = 30 )
  l22 <- lecture( id = "lecture22", min = 45 )
  s1 <- session( id = "session1" ) %>% add( l11 ) %>% add( l12 )
  s2 <- session( id = "session2" ) %>% add( l21 ) %>% add( l22 )
  m1 <- material( id = "material1", label = "First dataset", path = "material1.csv", outPath = "data/material1.csv" )
  course <- theCourse( id = "course", dir = "." ) %>% add( s1 ) %>% add( s2 ) %>% add( m1 )
  testthat::expect_identical( course$sessionIds(), c( "session1", "session2" ) )
  testthat::expect_identical( course$lectureIds(), c( "lecture11", "lecture12", "lecture21", "lecture22" ) )
  testthat::expect_identical( course$id2label( "lecture12" ), "Lecture12" )
  testthat::expect_identical( course$materialIds(), c( "material1" ) )

  d <- course$lecturesTibble()
  testthat::expect_identical( d$lecture.id, c( "lecture11", "lecture12", "lecture21", "lecture22" ) )
  testthat::expect_identical( d$lecture.prevId, c( NA, "lecture11", "lecture12", "lecture21" ) )
  testthat::expect_identical( d$lecture.nextId, c( "lecture12", "lecture21", "lecture22", NA ) )

  doc <- course$lectureDoc( "lecture12" )
  testthat::expect_identical( doc$id(), "lecture12" )
} )

# BaseRenderer -----------------------------------------------------------

#' @export
BaseRenderer <- R6Class(
  "BaseRenderer",
  private = list(
    outDir_ = NULL,

    normalizeRmdFile = function( from, to, overwrite, course, doc ) {
      if( file.exists( to ) && !overwrite ) {
        stop( "Can't overwrite '", to, "'." )
      }

      outCon <- file( to, "wt" )
      private$writeRmdHeader( outCon, course, doc )
      private$writeRmdHeadNavi( outCon, course, doc )

      if( !is.null( from ) ) {
        lines <- readLines( from )
        blks <- private$splitRmdBlocks( lines )
        private$assertNoH1HeadersInRmdBlocks( blks )
        private$writeRmdBlocks( outCon, blks )
      } else if( doc$type( TRUE ) == "toc" ) {
        lines <- private$writeRmdBodyToc( outCon, course = course )
      } else if( doc$type( TRUE ) == "materials" ) {
        lines <- private$writeRmdBodyMaterials( outCon, course = course )
      } else {
        stop( "Don't know how to generate content." )
      }

      private$writeRmdFootNavi( outCon, course, doc )
      close( outCon )
    },

    splitRmdBlocks = function( lines ) {
      curDoc <- list()
      curMode <- "text"; curLines <- c()
      .append <- function( curDoc, curMode, curLines ) {
        if( length( curLines ) > 0 ) {
          curDoc[[ length(curDoc)+1 ]] <- list( mode = curMode, lines = curLines )
        }
        curDoc
      }
      for( lineIdx in seq_len( length( lines ) ) ) {
        line <- lines[[ lineIdx ]]
        if( curMode == "text" ) {
          if( lineIdx == 1 && grepl( "^---$", line ) ) {
            curDoc <- .append( curDoc, curMode, curLines )
            curMode <- "header"; curLines <- c( line )
          } else if( grepl( "^```", line ) ) {
            curDoc <- .append( curDoc, curMode, curLines )
            curMode <- "chunk"; curLines <- c( line )
          } else {
            curLines <- c( curLines, line )
          }
        } else if( curMode == "header" ) {
          curLines <- c( curLines, line )
          if( grepl( "^---$", line ) ) {
            curDoc <- .append( curDoc, curMode, curLines )
            curMode <- "text"; curLines <- c()
          }
        } else if( curMode == "chunk" ) {
          curLines <- c( curLines, line )
          if( grepl( "^```", line ) ) {
            curDoc <- .append( curDoc, curMode, curLines )
            curMode <- "text"; curLines <- c()
          }
        } else {
          stop( "Unknown doc mode" )
        }
      }
      curDoc <- .append( curDoc, curMode, curLines )
      curDoc
    },
    assertNoH1HeadersInRmdBlocks = function( blks ) {
      for( blkIdx in seq_len( length( blks ) ) ) {
        cd <- blks[[blkIdx]]
        if( cd$mode == "text" ) {
          topLevelHeaders <- grep( "^#[^#].*", cd$lines )
          if( sum( topLevelHeaders ) > 0 ) {
            stop(
              "There are level-1 (single hash) headers in the file:\n",
              paste0( lines[ topLevelHeaders ] )
            )
          }
        }
      }
    },
    writeRmdBlocks = function( con, blks ) {
      for( blkIdx in seq_len( length( blks ) ) ) {
        cd <- blks[[blkIdx]]
        if( blkIdx > 1 || cd$mode != "header" ) {
          writeLines( con = con, text = cd$lines )
        }
      }
    },

    writeRmdHeader = function( outCon, course, doc ) {
      h <- list(
        output = list(
          html_document = list(
#            toc = TRUE,
#            toc_depth = 3,
#            toc_float = list(
#              collapsed = TRUE
#            ),
            number_sections = FALSE
          )
        )
      )
      hfc <- doc$hideFoldedCode()
      if( !is.na( hfc ) ) {
        #{r class.source = 'fold-show' or 'fold-hide'}
        h$output$html_document$code_folding <- if( hfc ) "hide" else "show"
      }

      .gen <- function( h, prefix = "" ) {
        unlist( lapply( names( h ), function( k ) {
          if( is.list( h[[k]] ) ) {
            if( length( h[[k]] ) == 0 ) {
              paste0( prefix, k )
            } else {
              c( paste0( prefix, k, ":" ), .gen( h[[k]], paste0( prefix, "  " ) ) )
            }
          } else {
            paste0( prefix, k, ": ", h[[k]] )
          }
        } ) )
      }

      docHeader <- c(
        "---",
        sprintf( 'title: "%s"', doc$label() ),
        .gen( h ),
        "---",
        ""
      )
      writeLines( con = outCon, text = docHeader )
    },
    writeRmdHeadNavi = function( outCon, course, doc ) {},
    writeRmdFootNavi = function( outCon, course, doc ) {},
    writeRmdBodyToc = function( outCon, course ) {},
    writeRmdBodyMaterials = function( outCon, course ) {}
  ),
  public = list(
    initialize = function( outDir ) {
      stopifnot( !is.null( outDir ) )
      private$outDir_ <- outDir
    },
    outDir = function() private$outDir_,
    colorHtml = function( ..., color ) {
      paste0(
        '<span style="color: ', color, ';">', ..., '</span>',
        collapse = "", sep = ""
      )
    },
    emojiHtml = function( id, text = id, color = "red" ) {
      emo::ji( id )
    },
    specialBlockHtml = function( ... ) {
      paste0(
        '\n\n<div class="alert alert-info" role="alert" style="text-align: right;">',
        ...,
        '</div>\n\n',
        collapse = "", sep = ""
      )
    },
    extRefHtml = function( ..., url ) stop( "Not inherited." ),
    intRefHtml = function( ..., url ) stop( "Not inherited." ),
    #navigationBarHtml = function( course, doc ) stop( "Not inherited." ),
    id2url = function( id ) stop( "Not inherited." ),

    makeAll = function( course, ... ) {
      self$clearDir()
      allFiles <- c(
        self$makeLectures( course, ... ),
        self$makeTasks( course, enableCode = FALSE, ... ),
        self$makeTasks( course, enableCode = TRUE, ... ),
        self$makeToc( course, ... ),
        self$makeMaterials( course, ... )
      ) %>% unlist() %>% as.vector()

      outZipFile <- paste0( self$outDir(), ".zip" )
      attr( outZipFile, "contentType" ) <- "application/zip"
      file.remove( outZipFile )
      zip( zipfile = outZipFile, files = allFiles, flags = "-9Xp" )
      invisible( outZipFile )
    },
    makeToc = function( course, ... ) {
      doc <- course$tocDoc()
      self$makeDoc( course = course, doc = doc, ... )
    },
    makeMaterials = function( course, materialIds = NULL, ... ) {
      doc <- course$materialsDoc()
      ret <- self$makeDoc( course = course, doc = doc, ... )

      if( is.null( materialIds ) ) materialIds <- course$materialIds()
      lapply( setNames( nm = materialIds ), function( materialId ) {
        doc <- course$materialDoc( materialId = materialId )
        self$makeDoc( course = course, doc = doc, ... )
      } ) %>% c( ret )
    },
    makeLectures = function( course, lectureIds = NULL, ... ) {
      if( is.null( lectureIds ) ) lectureIds <- course$lectureIds()
      lapply( setNames( nm = lectureIds ), function( lectureId ) {
        doc <- course$lectureDoc( lectureId = lectureId )
        self$makeDoc( course = course, doc = doc, ... )
      } )
    },
    makeTasks = function( course, enableCode, lectureIds = NULL, ... ) {
      if( is.null( lectureIds ) ) lectureIds <- course$lectureWithTasksIds()
      lapply( setNames( nm = lectureIds ), function( lectureId ) {
        doc <- course$taskDoc( lectureId = lectureId, enableCode = enableCode )
        self$makeDoc( course = course, doc = doc, ... )
      } )
    },

    # returns local url to the document (based at the course root reference)
    docUrl = function( doc ) {
      if( inherits( doc, "RenderedDoc" ) ) {
        sprintf(
          "S%02dL%02d%s_%s.html", doc$sessionIdx(), doc$lectureIdx(),
          doc$type(), doc$id()
        )
      } else {
        doc$url()
      }
    },

    mapTmpRmdFile = function( doc ) {
      rmdFile <- doc$rmdFile()
      if( !is.null( rmdFile ) ) {
        tmpRmdFile <- paste0( "tmp.", gsub( "[.]Rmd$", paste0( ".", doc$type( TRUE ), ".Rmd" ), doc$rmdFile() ) )
      } else {
        tmpRmdFile <- paste0( "tmp.", doc$type( TRUE ), ".Rmd" )
      }
      tmpRmdFile
    },
    mapOutHtmlFile = function( doc ) {
      sprintf(
        "S%02dL%02d%s_%s.html", doc$sessionIdx(), doc$lectureIdx(),
        doc$type(), doc$id()
      )
    },
    mapOutMaterialFile = function( doc ) {
      doc$outPath()
    },
    clearDir = function() {
      outDir <- normalizePath( file.path( self$outDir() ), mustWork = FALSE )
      if( !dir.exists( outDir ) ) {
        dir.create( path = outDir )
        message( "Created output directory '", outDir, "'" )
      } else {
        unlink( x = file.path( outDir, "*.html" ), recursive = FALSE, force = TRUE )
        message( "Cleaned output directory '", outDir, "'" )
      }
    },

    makeDoc = function( course, doc ) {
      message()
      message( "----- Processing document '", doc$id(), "' -----" )

      if( inherits( doc, "RenderedDoc" ) ) {
        self$renderDoc( course, doc )
      } else if( inherits( doc, "CopiedDoc" ) ) {
        self$copyDoc( course, doc )
      } else {
        stop( "Don't know how to make '", class( doc ), "'" )
      }
    },
    copyDoc = function( course, doc ) {
      stopifnot( inherits( doc, "CopiedDoc" ) )

      srcPath <- normalizePath( file.path( course$dir(), doc$path() ), mustWork = TRUE )
      outDir <- normalizePath( file.path( self$outDir() ), mustWork = TRUE )
      outPath <- normalizePath( file.path( outDir, doc$outPath() ), mustWork = FALSE )
      message( "Copying '", srcPath, "' to '", outPath, "'..." )
      if( !identical( dirname( outPath ), "." ) ) {
        dir.create( dirname( outPath ), recursive = TRUE )
      }
      stopifnot( file.copy( from = srcPath, to = outPath, overwrite = TRUE ) )
      stopifnot( file.exists( outPath ) )
      file.path( self$outDir(), doc$outPath() )
    },
    renderDoc = function( course, doc, quiet = TRUE ) {
      stopifnot( inherits( doc, "RenderedDoc" ) )

      # ----- rewrite the Rmd source with header/footer added (in srcDir) -----
      tmpRmdPath <- normalizePath( file.path( course$dir(), self$mapTmpRmdFile( doc ) ), mustWork = FALSE )
      on.exit( file.remove( tmpRmdPath ), add = TRUE )
      srcRmdPath <- doc$rmdFile()
      if( !is.null( srcRmdPath ) ) {
        srcRmdPath <- normalizePath( file.path( course$dir(), srcRmdPath ), mustWork = TRUE )
        stopifnot( !identical( srcRmdPath, tmpRmdPath ) ) # avoid accidental src overwrite
      }
      private$normalizeRmdFile( from = srcRmdPath, to = tmpRmdPath, overwrite = TRUE, course = course, doc = doc )

      # ----- render Rmd (from srcDir) to html (to outDir) -----
      outDir <- normalizePath( file.path( self$outDir() ), mustWork = TRUE )
      outHtmlPath <- normalizePath( file.path( outDir, self$mapOutHtmlFile( doc ) ), mustWork = FALSE )
      message( "Rendering '", tmpRmdPath, "' to '", outHtmlPath, "'..." )

      e <- new.env()
      #assign( x = ".renderer", value = self, envir = e )
      #assign( x = ".course", value = course, envir = e )
      #assign( x = ".doc", value = doc, envir = e )
      setupFun <- doc$setupFun()
      if( !is.null( setupFun ) ) setupFun()
      rmarkdown::render(
        input = tmpRmdPath, output_dir = outDir,
        output_format = "html_document", output_file = outHtmlPath,
        intermediates_dir = outDir,
        #knit_root_dir = outDir,
        runtime = "static",
        envir = e, clean = TRUE, quiet = quiet
      )
      file.path( self$outDir(), self$mapOutHtmlFile( doc ) )
    }
  )
)

testthat::test_that( "BaseRenderer properties", {
  l1 <- lecture( id = "lecture1", min = 30 )
  s1 <- session( id = "session1" ) %>% add( l1 )
  course <- theCourse( id = "course", dir = "." ) %>% add( s1 )

  renderer <- BaseRenderer$new( outDir = "." )
  testthat::expect_identical( renderer$outDir(), "." )
} )

# Renderer ---------------------------------------------------------------

#' @export
Renderer <- R6Class(
  "Renderer", inherit = BaseRenderer,
  private = list(
    refHtml = function( ..., url ) {
      paste0(
        '<a class="btn btn-primary" href="', url, '" role="button">', ..., '</a>',
        collapse = "", sep = ""
      )
    },
    intRefHtml = function( ..., url ) {
      private$refHtml( ..., url = url )
    },
    intRefFile = function( ..., url, file ) {
      paste0(
        '<a href="', url, '" download="', file, '">', ..., '</a>',
        collapse = "", sep = ""
      )
    },
    lectureNavigationBarHtml = function( course, doc ) {
      elems <- c(
        private$intRefHtml( "&#x2302;&nbsp;Contents", url = self$docUrl( course$tocDoc() ) ),
        private$intRefHtml( "&#x1F4C1;&nbsp;Materials", url = self$docUrl( course$materialsDoc() ) )
      )

      naviIds <- doc$naviIds()
      if( !is.null( naviIds$tasks ) && naviIds$tasks ) {
        url <- self$docUrl( course$taskDoc( lectureId = doc$id(), enableCode = FALSE ) )
        elems <- c( elems, private$intRefHtml( '&darr;&nbsp;Practice', url = url ) )
      }
      if( !is.null( naviIds$prev ) && !is.na( naviIds$prev ) ) {
        aDoc <- course$lectureDoc( lectureId = naviIds$prev )
        url <- self$docUrl( aDoc )
        elems <- c( elems, private$intRefHtml( '&larr;&nbsp;', aDoc$label(), url = url ) )
      }
      if( !is.null( naviIds$`next` ) && !is.na( naviIds$`next` ) ) {
        aDoc <- course$lectureDoc( lectureId = naviIds$`next` )
        url <- self$docUrl( aDoc )
        elems <- c( elems, private$intRefHtml( aDoc$label(), '&nbsp;&rarr;', url = url ) )
      }

      self$specialBlockHtml( paste0( elems, collapse = "&nbsp;" ) )
    },
    taskNavigationBarHtml = function( course, doc, enableCode, label ) {
      lectureId <- doc$naviIds()$lecture

      elems <- c(
        private$intRefHtml( "&#x2302;&nbsp;Contents", url = self$docUrl( course$tocDoc() ) ),
        private$intRefHtml( "&#x1F4C1;&nbsp;Materials", url = self$docUrl( course$materialsDoc() ) ),
        private$intRefHtml( '&uarr;&nbsp;Lecture', url = self$docUrl( course$lectureDoc( lectureId = lectureId ) ) ),
        private$intRefHtml( '&#x21c4;&nbsp;', label, url = self$docUrl( course$taskDoc( lectureId = lectureId, enableCode = enableCode ) ) )
      )
      self$specialBlockHtml( paste0( elems, collapse = "&nbsp;" ) )
    },
    tocNavigationBarHtml = function( course ) {
      elems <- c(
        private$intRefHtml( "&#x1F4C1;&nbsp;Materials", url = self$docUrl( course$materialsDoc() ) )
      )
      self$specialBlockHtml( paste0( elems, collapse = "&nbsp;" ) )
    },
    materialsNavigationBarHtml = function( course ) {
      elems <- c(
        private$intRefHtml( "&#x2302;&nbsp;Contents", url = self$docUrl( course$tocDoc() ) )
      )
      self$specialBlockHtml( paste0( elems, collapse = "&nbsp;" ) )
    },
    navigationBarHtml = function( course, doc ) {
      if( doc$type( long = TRUE ) == "lecture" ) {
        text <- private$lectureNavigationBarHtml( course, doc )
      } else if( doc$type( long = TRUE ) == "practice" ) {
        text <- private$taskNavigationBarHtml( course, doc, TRUE, "Solutions" )
      } else if( doc$type( long = TRUE ) == "solutions" ) {
        text <- private$taskNavigationBarHtml( course, doc, FALSE, "Practice" )
      } else if( doc$type( long = TRUE ) == "toc" ) {
        text <- private$tocNavigationBarHtml( course )
      } else if( doc$type( long = TRUE ) == "materials" ) {
        text <- private$materialsNavigationBarHtml( course )
      } else {
        text <- ""
      }
      text
    },
    writeRmdHeadNavi = function( outCon, course, doc ) {
      text <- private$navigationBarHtml( course = course, doc = doc )
      writeLines( text = text, con = outCon )
    },
    writeRmdFootNavi = function( outCon, course, doc ) {
      text <- private$navigationBarHtml( course = course, doc = doc )
      writeLines( text = text, con = outCon )
    },
    writeRmdBodyToc = function( outCon, course ) {
      d <- course$lecturesTibble( TRUE )
      dd <- d %>%
        mutate( Session = sprintf( "%s (%s, %s)", session.label, lubridate::stamp( "Sunday, May 1", quiet = TRUE )( session.date ), session.timeRange ) )%>%
        extract( col = session.timeRange, into = c( "session.startTime" ), regex = "^([0-9:]+)" ) %>%
        mutate( session.startTime = lubridate::parse_date_time( session.startTime, c("%H:%M"), exact = TRUE ) ) %>%
        mutate( Title = lecture.label ) %>%
        mutate( Title = if_else( Title == "Break", "&#x2015; *break* &#x2015;", Title ) ) %>%
        mutate( Lecture = "", Practice = "", Solutions = "" ) %>%
        group_by( Session ) %>%
          mutate( Time = if_else( !is.na( slot.min ), paste0(
              lubridate::stamp( "23:45", quiet = TRUE )( session.startTime + ( cumsum( slot.min ) - slot.min )*60L ),
              "-",
              lubridate::stamp( "23:45", quiet = TRUE )( session.startTime + cumsum( slot.min )*60L )
            ), "" ) ) %>%
        ungroup()
      for( idx in seq_len( nrow( dd ) ) ) {
        lId <- d$lecture.id[[idx]]
        if( !is.na( lId ) ) {
          url <- self$docUrl( aDoc <- course$lectureDoc( lectureId = lId ) )
          dd$Lecture[[idx]] <- private$intRefHtml( "Lecture", url = url )
          if( d$lecture.hasTasks[[idx]] ) {
            url <- self$docUrl( course$taskDoc( lectureId = lId, enableCode = FALSE ) )
            dd$Practice[[idx]] <- private$intRefHtml( "Practice", url = url )

            url <- self$docUrl( course$taskDoc( lectureId = lId, enableCode = TRUE ) )
            dd$Solutions[[idx]] <- private$intRefHtml( "Solutions", url = url )
          }
        }
      }
      r <- rle( dd$Session )
      ke <- dd %>%
        select( Title, Time, Lecture, Practice, Solutions ) %>%
        kableExtra::kbl( format = "html", escape = FALSE ) %>%
        #kableExtra::kable_paper("striped", full_width = F) %>%
        #kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>%
        kableExtra::kable_styling( bootstrap_options = c( "hover" ), full_width = TRUE ) %>%
        #kable_material(c("striped", "hover")) %>%
        kableExtra::pack_rows( "Session", index = setNames( r$lengths, r$values ) )

      writeLines( c(
        "---",
        paste0( "title: '", course$label(), "'" ),
        "---",
        "",
        "## Contents"
      ), con = outCon )

      cat( ke, file = outCon, append = TRUE )
    },
    writeRmdBodyMaterials = function( outCon, course ) {
      dd <- bind_rows( lapply( course$materialIds(), function( materialId ) {
        aDoc <- course$materialDoc( materialId = materialId )
        url <- self$mapOutMaterialFile( aDoc )
        tibble(
          Title = aDoc$label(),
          Material = private$intRefFile( aDoc$path(), url = url, file = basename( aDoc$path() ) )
        )
      } ) )

      ke <- kableExtra::kbl( dd, format = "html", escape = FALSE ) %>%
        kableExtra::kable_styling(bootstrap_options = c("hover"),full_width = TRUE)

      writeLines( c(
        "---",
        paste0( "title: '", course$label(), "'" ),
        "---",
        "",
        "## Materials"
      ), con = outCon )

      cat( ke, file = outCon, append = TRUE )
    }
  ),
  public = list()
)

# BrightspaceRenderer ----------------------------------------------------

#' @export
BrightspaceRenderer <- R6Class(
  "BrightspaceRenderer", inherit = BaseRenderer,
  private = list(),
  public = list(
    #navigationBarHtml = function( course, doc ) c()
  )
)

testthat::test_that( "BrightspaceRenderer properties", {
  renderer <- BrightspaceRenderer$new( outDir = "tmp" )
  testthat::expect_identical( renderer$outDir(), "tmp" )
} )

#https://brightspace.universiteitleiden.nl/d2l/common/dialogs/quickLink/quickLink.d2l?ou=9965&type=coursefile&fileId=basic_calculator0.html
#
#cat( "<div id='nav' class='d2l-hide'>" )
#cat( "<a href='basic_calculator0.html' class='d2l-nav-next'>Calculator</a>" )
#cat( "</div>" )

# ------------------------------------------------------------------------

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
if( 0 ) {
  l <- genTestCourse( TRUE )
}
