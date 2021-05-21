library( R6 )
library( tidyverse )

add <- function( x, ... ) UseMethod( "add", x )
validOr <- function( a, b ) if( !is.null( a ) && !is.na( a ) ) a else b

# Old style functions ----------------------------------------------------

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

build_nocode <- function( ... ) {
#  warning( "building_nocode is oboleted" )
}

navigate_slots <- function( ... ) {
#  warning( "navigate_slots is oboleted" )
}

related_slots <- function( ... ) {
#  warning( "related_slots is oboleted" )
}

qa <- function(msg) {
  id <- paste("tag",digest::digest(msg, algo="md5"),sep="")
  txt <- paste("<a class=\"btn btn-primary\" role=\"button\" data-toggle=\"collapse\" href=\"#",id,"\"",sep="")
  txt <- paste(txt, "aria-expanded=\"false\" aria-controls=\"collapseExample\">Answer</a>")
  txt <- paste(txt, "<div class=\"collapse\" id=\"",id,"\"><div class=\"well\">",msg,"</div> </div><br>", sep="")
  cat(knitr::knit_child(text=txt, quiet=TRUE), sep = "\n")
}

show_emoji <- function( id, text = id, color = "red" ) {
#  if( .hasEmo ) {
    emo::ji( id )
#  } else {
#    show_color( text = paste0( "[", text, "] " ), color = color )
#  }
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
    collapse = "&#8611;"
    #    collapse = show_emoji( "down_right_arrow", text = "\\", color = "gray" )
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

info_block0 <- function(msg) {
  txt <- paste("<ion-icon name=\"information-circle\" size=\"large\"></ion-icon> ", "___", msg, "___","<br><br>", sep="")
  cat(knitr::knit_child(text=txt, quiet=TRUE), sep = "\n")
}

info_block <- function(msg) {
  paste("<ion-icon name=\"information-circle\" size=\"large\"></ion-icon> ", "___", msg, "___","<br><br>", sep="")
}

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

Doc <- R6Class(
  "Doc",
  private = list(
    id_ = NULL,
    label_ = NULL,
    naviIds_ = NULL,
    rmdFile_ = NULL,
    type_ = NULL,
    sessionIdx_ = NULL,
    lectureIdx_ = NULL,
    fileVersion_ = NULL,
    setupFun_ = NULL
  ),
  public = list(
    initialize = function(
      id, label = NULL, naviIds = NULL, rmdFile = NULL,
      type = "?", sessionIdx = 0L, lectureIdx = 0L, fileVersion = NULL,
      setupFun = function() {}
    ) {
      private$id_ <- id
      private$label_ <- label
      private$naviIds_ <- naviIds
      private$rmdFile_ <- rmdFile
      private$type_ <- type
      private$sessionIdx_ <- sessionIdx
      private$lectureIdx_ <- lectureIdx
      private$fileVersion_ <- fileVersion
      private$setupFun_ <- setupFun
    },
    id = function() { private$id_ },
    label = function() { private$label_ },
    naviIds = function() { private$naviIds_ },
    rmdFile = function() { private$rmdFile_ },
    type = function( long = FALSE ) {
      if( !long ) {
        private$type_
      } else {
        ( c(
          "?" = "unknown", "l" = "lecture",
          "s" = "solutions", "p" = "practice", "c" = "toc"
        ) )[[ private$type_ ]]
      }
    },
    sessionIdx = function() { private$sessionIdx_ },
    lectureIdx = function() { private$lectureIdx_ },
    fileVersion = function() { private$fileVersion_ },
    setupFun = function() { private$setupFun_ }
  )
)

testthat::test_that( "Doc properties", {
  doc <- Doc$new( id = "lecture1", naviIds = list( "next" = "lecture2" ) )
  testthat::expect_identical( doc$id(), "lecture1" )
} )

# Lecture ----------------------------------------------------------------

Lecture <- R6Class(
  classname = "Lecture",
  private = list(
    id_ = NULL,
    label_ = NULL,
    rmdFile_ = NULL,
    hasTasks_ = NULL
  ),
  public = list(
    initialize = function( id, label = NULL, rmdFile = NULL, hasTasks = TRUE, ... ) {
      stopifnot( !is.null( id ) )
      private$id_ <- id
      private$label_ <- label
      private$rmdFile_ <- rmdFile
      private$hasTasks_ <- hasTasks
    },
    id = function() { private$id_ },
    label = function() { validOr( private$label_, private$id_ ) },
    rmdFile = function() {
      rmdFile <- private$rmdFile_
      if( is.null( rmdFile ) ) rmdFile <- paste0( private$id_, ".Rmd" )
      rmdFile
    },
    hasTasks = function() { private$hasTasks_ },
    asTibble = function() {
      d <- tibble(
        id = self$id(), label = self$label(), rmdFile = self$rmdFile(),
        hasTasks = self$hasTasks()
      )
      colnames( d ) <- paste0( "lecture.", colnames( d ) )
      d
    }
  )
)

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
    asTibble = function() {
      d <- tibble( id = private$id_ )
      if( length( private$attrs_ ) > 0 ) d <- d %>% bind_cols( as_tibble( private$attrs_ ) )
      colnames( d ) <- paste0( "session.", colnames( d ) )
      bind_rows( lapply( seq_len( length( private$lectures_ ) ), function( idx ) {
        d %>% bind_cols(
          private$lectures_[[ idx ]]$asTibble() %>% mutate( lecture.idx = idx )
        )
      } ) )
    }
  )
)

session <- function( ... ) { Session$new( ... ) }
add.Session <- function( x, ... ) x$add( ... )

testthat::test_that( "Session properties", {
  l1 <- lecture( id = "lecture1", min = 30 )
  l2 <- lecture( id = "lecture2", min = 45 )
  s1 <- session( id = "session1" ) %>% add( l1 ) %>% add( l2 )
  testthat::expect_identical( s1$lectureIds(), c( "lecture1", "lecture2" ) )
})

# TheCourse --------------------------------------------------------------

TheCourse <- R6Class(
  "TheCourse",
  private = list(
    id_ = NULL,
    dir_ = NULL,
    label_ = NULL,
    attrs_ = NULL,
    sessions_ = NULL,

    d_ = NULL,
    invalidate = function() { d_ = NULL },
    validate = function() {
      if( is.null( private$d_ ) ) {
        d <- tibble( id = private$id_ )
        if( length( private$attrs_ ) > 0 ) d <- d %>% bind_cols( as_tibble( private$attrs_ ) )
        colnames( d ) <- paste0( "course.", colnames( d ) )
        private$d_ <- bind_rows( lapply( seq_len( length( private$sessions_ ) ), function( idx ) {
          d %>% bind_cols(
            private$sessions_[[ idx ]]$asTibble() %>% mutate( session.idx = idx )
          )
        } ) ) %>%
          arrange( session.idx, lecture.idx ) %>%
          mutate( lecture.prevId = lag( lecture.id ) ) %>%
          mutate( lecture.nextId = lead( lecture.id ) )
      }
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
      self$asTibble() %>% filter( lecture.hasTasks ) %>% pull( lecture.id )
    },
    id2label = function( docId ) {
      self$lectureDoc( lectureId = docId )$label()
    },
    add = function( x ) {
      stopifnot( inherits( x, "Session" ) )
      stopifnot( !( x$id() %in% self$sessionIds() ) )
      stopifnot( !any( x$lectureIds() %in% self$lectureIds() ) )
      private$invalidate()
      private$sessions_ <- c( private$sessions_, x )
      invisible( self )
    },
    asTibble = function() {
      private$validate()
      private$d_
    },
    lectureDoc = function( lectureId ) {
      d <- self$asTibble() %>% filter( lecture.id == lectureId )
      stopifnot( nrow( d ) == 1 )
      li <- as.list( d )

      Doc$new(
        id = lectureId,
        label = li$lecture.label,
        rmdFile = li$lecture.rmdFile,
        naviIds = list(
          "prev" = li$lecture.prevId,
          "next" = li$lecture.nextId,
          "tasks" = li$lecture.hasTasks
        ),
        type = "l",
        lectureIdx = li$lecture.idx,
        sessionIdx = li$session.idx,
        setupFun = ( function() {
          knitr::opts_chunk$set( echo = TRUE, eval = TRUE, comment = NA, paged.print=FALSE )
        } )
      )
    },
    taskDoc = function( lectureId, enableCode ) {
      d <- self$asTibble() %>% filter( lecture.id == lectureId )
      stopifnot( nrow( d ) == 1 )
      li <- as.list( d )

      if( enableCode ) {
        setupFun <- function() {
          knitr::opts_chunk$set( echo = TRUE, eval = TRUE, comment = NA, paged.print=FALSE )
        }
      } else {
        setupFun <- function() {
          knitr::opts_chunk$set( echo = FALSE, eval = FALSE, comment = NA, paged.print=FALSE )
        }
      }

      Doc$new(
        id = paste0( lectureId, ".tasks", ( if( enableCode ) '.code' else '.nocode' ) ),
        label = paste0( li$lecture.label, ( if( enableCode ) ' (solutions)' else ' (practice)' ) ),
        rmdFile = gsub( "[.]Rmd", ".tasks.Rmd", li$lecture.rmdFile ),
        naviIds = list(
          "lecture" = lectureId
        ),
        type = if( enableCode ) 's' else 'p',
        lectureIdx = li$lecture.idx,
        sessionIdx = li$session.idx,
        setupFun = setupFun
      )
    },
    tocDoc = function() {
      Doc$new(
        id = "toc",
        label = "Table of Contents",
        rmdFile = NULL,
        naviIds = list(),
        type = 'c',
        lectureIdx = 1,
        sessionIdx = 0,
        setupFun = function() {}
      )
    }
  )
)

theCourse <- function( ... ) TheCourse$new( ... )
add.TheCourse <- function( x, ... ) x$add( ... )

testthat::test_that( "TheCourse properties", {
  l11 <- lecture( id = "lecture11", min = 30 )
  l12 <- lecture( id = "lecture12", label = "Lecture12", min = 45 )
  l21 <- lecture( id = "lecture21", min = 30 )
  l22 <- lecture( id = "lecture22", min = 45 )
  s1 <- session( id = "session1" ) %>% add( l11 ) %>% add( l12 )
  s2 <- session( id = "session2" ) %>% add( l21 ) %>% add( l22 )
  course <- theCourse( id = "course", dir = "." ) %>% add( s1 ) %>% add( s2 )
  testthat::expect_identical( course$sessionIds(), c( "session1", "session2" ) )
  testthat::expect_identical( course$lectureIds(), c( "lecture11", "lecture12", "lecture21", "lecture22" ) )
  testthat::expect_identical( course$id2label( "lecture12" ), "Lecture12" )

  d <- course$asTibble()
  testthat::expect_identical( d$lecture.id, c( "lecture11", "lecture12", "lecture21", "lecture22" ) )
  testthat::expect_identical( d$lecture.prevId, c( NA, "lecture11", "lecture12", "lecture21" ) )
  testthat::expect_identical( d$lecture.nextId, c( "lecture12", "lecture21", "lecture22", NA ) )

  doc <- course$lectureDoc( "lecture12" )
  testthat::expect_identical( doc$id(), "lecture12" )
} )

# BaseRenderer -----------------------------------------------------------

BaseRenderer <- R6Class(
  "BaseRenderer",
  private = list(
    outDir_ = NULL,

    normalizeRmdFile = function( from, to, overwrite, course, doc ) {
      if( file.exists( to ) && !overwrite ) {
        stop( "Can't overwrite '", to, "'." )
      }

      outCon <- file( to, "wt" )
      private$writeDocHeader( outCon, course, doc )
      private$writeHeadNavi( outCon, course, doc )

      if( !is.null( from ) ) {
        lines <- readLines( from )
        blks <- private$splitRmdBlocks( lines )
        private$assertNoH1HeadersInBlocks( blks )
        private$writeRmdBlocks( outCon, blks )
      } else if( doc$type( TRUE ) == "toc" ) {
        lines <- private$writeToc( outCon, course = course )
      } else {
        stop( "Don't know how to generate content." )
      }

      private$writeFootNavi( outCon, course, doc )
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
    assertNoH1HeadersInBlocks = function( blks ) {
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

    writeDocHeader = function( outCon, course, doc ) {
      type2header <- list(
        "lecture" = c(),
        "practice" = c(),
        "solutions" = c(
          "output:",
          "  html_document:",
          "    code_folding: show"   #{r class.source = 'fold-show' or 'fold-hide'}
        )
      )
      docHeader <- c(
        "---",
        sprintf( 'title: "%s"', doc$label() ),
        type2header[[ doc$type( TRUE ) ]],
        "---",
        ""
      )
      writeLines( con = outCon, text = docHeader )
    },
    writeHeadNavi = function( outCon, course, doc ) {},
    writeFootNavi = function( outCon, course, doc ) {},
    writeToc = function( outCon, course ) {}
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

    renderAll = function( course, ... ) {
      self$clearDir()
      self$renderToc( course, ... )
      self$renderLectures( course, ... )
      self$renderTasks( course, enableCode = FALSE, ... )
      self$renderTasks( course, enableCode = TRUE, ... )
    },
    renderToc = function( course, ... ) {
      doc <- course$tocDoc()
      self$renderDoc( course = course, doc = doc, ... )
    },
    renderLectures = function( course, lectureIds = NULL, ... ) {
      if( is.null( lectureIds ) ) lectureIds <- course$lectureIds()
      lapply( setNames( nm = lectureIds ), function( lectureId ) {
        doc <- course$lectureDoc( lectureId = lectureId )
        self$renderDoc( course = course, doc = doc, ... )
      } )
    },
    renderTasks = function( course, enableCode, lectureIds = NULL, ... ) {
      if( is.null( lectureIds ) ) lectureIds <- course$lectureWithTasksIds()
      lapply( setNames( nm = lectureIds ), function( lectureId ) {
        doc <- course$taskDoc( lectureId = lectureId, enableCode = enableCode )
        self$renderDoc( course = course, doc = doc, ... )
      } )
    },

    mapOutRmdFile = function( doc ) {
      rmdFile <- doc$rmdFile()
      if( !is.null( rmdFile ) ) {
        outRmdFile <- gsub( "[.]Rmd$", paste0( ".", doc$type( TRUE ), ".Rmd" ), doc$rmdFile() )
      } else {
        outRmdFile <- paste0( doc$type( TRUE ), ".Rmd" )
      }
      outRmdFile
    },
    mapOutHtmlFile = function( doc ) {
      fileVersion <- ""
      if( !is.null( doc$fileVersion() ) ) {
        fileVersion <- paste0( ".", doc$fileVersion() )
      }
      sprintf(
        "S%02dL%02d%s_%s%s.html", doc$sessionIdx(), doc$lectureIdx(),
        doc$type(), doc$id(), fileVersion
      )
    },
    clearDir = function() {
      outDir <- normalizePath( file.path( self$outDir() ), mustWork = FALSE )
      unlink( x = file.path( outDir, "*.html" ), recursive = FALSE, force = TRUE )
    },
    renderDoc = function( course, doc, quiet = TRUE ) {
      message()
      message( "----- Processing document '", doc$id(), "' -----" )

      outDir <- normalizePath( file.path( self$outDir() ), mustWork = FALSE )
      if( !dir.exists( outDir ) ) {
        dir.create( path = outDir )
        message( "Created output directory '", outDir, "'" )
      }

      outRmdFile <- normalizePath( file.path( course$dir(), self$mapOutRmdFile( doc ) ), mustWork = FALSE )

      rmdFile <- doc$rmdFile()
      if( !is.null( rmdFile ) ) {
        srcRmdFile <- normalizePath( file.path( course$dir(), doc$rmdFile() ), mustWork = TRUE )
        #includedFiles <- self$detectIncludes( srcRmdFile )
        message( "Normalizing '", srcRmdFile, "' to '", outRmdFile, "'..." )
        private$normalizeRmdFile( from = srcRmdFile, to = outRmdFile, overwrite = TRUE, course = course, doc = doc )
      } else {
        message( "Generating '", outRmdFile, "'..." )
        private$normalizeRmdFile( from = NULL, to = outRmdFile, overwrite = TRUE, course = course, doc = doc )
      }

      outHtmlFile <- normalizePath( file.path( self$outDir(), self$mapOutHtmlFile( doc ) ), mustWork = FALSE )
      e <- new.env()
      #assign( x = ".renderer", value = self, envir = e )
      #assign( x = ".course", value = course, envir = e )
      #assign( x = ".doc", value = doc, envir = e )

      message( "Rendering '", outRmdFile, "' to '", outHtmlFile, "'..." )
      setupFun <- doc$setupFun()
      if( !is.null( setupFun ) ) setupFun()
      rmarkdown::render(
        input = outRmdFile, output_dir = outDir,
        output_format = "html_document", output_file = outHtmlFile,
        intermediates_dir = outDir,
        #knit_root_dir = outDir,
        runtime = "static",
        envir = e, clean = TRUE, quiet = quiet
      )

      message( "Removing copied source '", outRmdFile, "'..." )
      file.remove( outRmdFile )
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
    lectureNavigationBarHtml = function( course, doc ) {
      elems <- c()

      aDoc <- course$tocDoc()
      url <- self$mapOutHtmlFile( aDoc )
      elems <- c( elems, private$intRefHtml( "&#x2302;&nbsp;Index", url = url ) )

      naviIds <- doc$naviIds()
      if( !is.null( naviIds$tasks ) && naviIds$tasks ) {
        aDoc <- course$taskDoc( lectureId = doc$id(), enableCode = FALSE )
        url <- self$mapOutHtmlFile( aDoc )
        elems <- c( elems, private$intRefHtml( '&darr;&nbsp;Practice', url = url ) )
      }
      if( !is.null( naviIds$prev ) && !is.na( naviIds$prev ) ) {
        aDoc <- course$lectureDoc( lectureId = naviIds$prev )
        url <- self$mapOutHtmlFile( aDoc )
        elems <- c( elems, private$intRefHtml( '&larr;&nbsp;', aDoc$label(), url = url ) )
      }
      if( !is.null( naviIds$`next` ) && !is.na( naviIds$`next` ) ) {
        aDoc <- course$lectureDoc( lectureId = naviIds$`next` )
        url <- self$mapOutHtmlFile( aDoc )
        elems <- c( elems, private$intRefHtml( aDoc$label(), '&nbsp;&rarr;', url = url ) )
      }

      self$specialBlockHtml( paste0( elems, collapse = "&nbsp;" ) )
    },
    taskNavigationBarHtml = function( course, doc, enableCode, label ) {
      lectureId <- doc$naviIds()$lecture

      elems <- c()

      aDoc <- course$tocDoc()
      url <- self$mapOutHtmlFile( aDoc )
      elems <- c( elems, private$intRefHtml( "&#x2302;&nbsp;Index", url = url ) )

      aDoc <- course$lectureDoc( lectureId = lectureId )
      url <- self$mapOutHtmlFile( aDoc )
      elems <- c( elems, private$intRefHtml( '&uarr;&nbsp;Lecture', url = url ) )

      aDoc <- course$taskDoc( lectureId = lectureId, enableCode = enableCode )
      url <- self$mapOutHtmlFile( aDoc )
      elems <- c( elems, private$intRefHtml( '&#x21c4;&nbsp;', label, url = url ) )

      self$specialBlockHtml( paste0( elems, collapse = "&nbsp;" ) )
    },
    navigationBarHtml = function( course, doc ) {
      if( doc$type( long = TRUE ) == "lecture" ) {
        text <- private$lectureNavigationBarHtml( course, doc )
      } else if( doc$type( long = TRUE ) == "practice" ) {
        text <- private$taskNavigationBarHtml( course, doc, TRUE, "Solutions" )
      } else if( doc$type( long = TRUE ) == "solutions" ) {
        text <- private$taskNavigationBarHtml( course, doc, FALSE, "Practice" )
      } else {
        text <- ""
      }
      text
    },
    writeHeadNavi = function( outCon, course, doc ) {
      text <- private$navigationBarHtml( course = course, doc = doc )
      writeLines( text = text, con = outCon )
    },
    writeFootNavi = function( outCon, course, doc ) {
      text <- private$navigationBarHtml( course = course, doc = doc )
      writeLines( text = text, con = outCon )
    },
    writeToc = function( outCon, course ) {
      d <- course$asTibble()
      dd <- d %>%
        mutate( Session = session.label, Title = lecture.label ) %>%
        mutate( Lecture = "", Practice = "", Solutions = "" )
      for( idx in seq_len( nrow( dd ) ) ) {
        aDoc <- course$lectureDoc( lectureId = d$lecture.id[[idx]] )
        url <- self$mapOutHtmlFile( aDoc )
        dd$Lecture[[idx]] <- private$intRefHtml( "Lecture", url = url )
        if( d$lecture.hasTasks[[idx]] ) {
          aDoc <- course$taskDoc( lectureId = d$lecture.id[[idx]], enableCode = FALSE )
          url <- self$mapOutHtmlFile( aDoc )
          dd$Practice[[idx]] <- private$intRefHtml( "Practice", url = url )

          aDoc <- course$taskDoc( lectureId = d$lecture.id[[idx]], enableCode = TRUE )
          url <- self$mapOutHtmlFile( aDoc )
          dd$Solutions[[idx]] <- private$intRefHtml( "Solutions", url = url )
        }
      }
      r <- rle( dd$Session )
      ke <- kableExtra::kbl( dd %>% select( Title, Lecture, Practice, Solutions ), format = "html", escape = FALSE ) %>%
        #kableExtra::kable_paper("striped", full_width = F) %>%
        #kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>%
        kableExtra::kable_styling(bootstrap_options = c("hover"),full_width = TRUE) %>%
        #kable_material(c("striped", "hover")) %>%
        kableExtra::pack_rows( "Session", index = setNames( r$lengths, r$values ) )

      writeLines( c(
        "---",
        paste0( "title: '", course$label(), "'" ),
        "---",
        "",
        "## Table of Contents"
      ), con = outCon )

      cat( ke, file = outCon, append = TRUE )
    }
  ),
  public = list()
)

# BrightspaceRenderer ----------------------------------------------------

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

genTestCourse <- function() {
  course <- theCourse( id = "Boerhaave_2021_Jun", dir = "BrightspaceTest", label = "LUMC/Boerhaave, June 2021: R for data analysis" )
  course <- course$add(
    session( id = "slot1", label = "R and RStudio basics" ) %>%
      add( lecture( id = "index", label = "Course Introduction", hasTasks = FALSE, min = 5 ) ) %>%
      add( lecture( id = "introduction0", label = "R Introduction", hasTasks = FALSE, min = 30 ) ) %>%
      add( lecture( id = "basic_calculator0", label = "Calculator", min = 45 ) ) %>%
      add( lecture( id = "basic_variables0", label = "Variables", min = 45 ) ) %>%
      add( lecture( id = "basic_projects0", label = "Projects", hasTasks = FALSE, min = 45 ) ) %>%
      add( lecture( id = "basic_scripts0", label = "Scripts", hasTasks = FALSE, min = 45 ) )
  )
  course <- course$add(
    session( id = "slot2", label = "Data structures 1/2" ) %>%
      add( lecture( id = "basic_vectors0", label = "Vectors", min = 45 ) ) %>%
      add( lecture( id = "basic_factors0", label = "Factors", min = 30 ) ) %>%
      add( lecture( id = "packages0", label = "Install/use packages", hasTasks = FALSE, min = 30 ) )
  )
  course <- course$add(
    session( id = "slot3", label = "Data manipulation 1/3" ) %>%
      add( lecture( id = "tidyverse0", label = "Tidyverse library", hasTasks = FALSE, min = 45 ) ) %>%
      add( lecture( id = "dplyr_tibble0", label = "Tibble", min = 30 ) ) %>%
      add( lecture( id = "dplyr_select0", label = "Select variables/cols", min = 30 ) ) %>%
      add( lecture( id = "dplyr_filter0", label = "Filter observations/rows", min = 30 ) ) %>%
      add( lecture( id = "dplyr_mutate0", label = "Add/modify variables", min = 30 ) )
  )
  course <- course$add(
    session( id = "slot4", label = "Data manipulation 2/3" ) %>%
      add( lecture( id = "dplyr_pipe0", label = "Pipe operator", min = 45 ) ) %>%
      add( lecture( id = "dplyr_summarise0", label = "Summarise", min = 30 ) ) %>%
      add( lecture( id = "dplyr_group0", label = "Groups", min = 30 ) )
  )
  course <- course$add(
    session( id = "slot5", label = "Data structures 2/2" ) %>%
      add( lecture( id = "basic_lists0", label = "Lists", min = 45 ) ) %>%
      add( lecture( id = "basic_formulas0", label = "Formulas", min = 30 ) ) %>%
      add( lecture( id = "basic_matrices0", label = "Matrices", min = 30 ) )
  )
  course <- course$add(
    session( id = "slot6", label = "Graphics" ) %>%
      add( lecture( id = "ggplot_basics0", label = "Plots/ggplot2", min = 45 ) ) %>%
      add( lecture( id = "ggplot_scales0", label = "Plot axes/scales", min = 30 ) ) %>%
      add( lecture( id = "ggplot_facets_themes0", label = "Plot panels/facets", min = 30 ) ) %>%
      add( lecture( id = "ggplot_geoms0", label = "Plot types", hasTasks = FALSE, min = 30 ) )
  )
  course <- course$add(
    session( id = "slot7", label = "Data manipulation 3/3 and functions" ) %>%
      add( lecture( id = "dplyr_join0", label = "Merging/joining tables", min = 45 ) ) %>%
      add( lecture( id = "tidyr_reshape0", label = "Reshaping tables", min = 30 ) ) %>%
      add( lecture( id = "advanced_user_functions0", label = "User-defined functions", min = 30 ) ) %>%
      add( lecture( id = "useful_functions0", label = "Useful R functions", hasTasks = FALSE, min = 30 ) )
  )

  renderer <- Renderer$new( outDir = "tmp" )
  #renderer <- BrightspaceRenderer$new( outDir = "tmp" )
  renderer$renderAll( course = course )
}
if( 0 ) {
  genTestCourse()
}
