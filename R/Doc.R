library( R6 )
library( tidyverse )

# Doc -----

#' Doc
#'
#' @description ...
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

# CopiedDoc -----

#' CopiedDoc
#'
#' @description ...
#' @export
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

# RenderedDoc -----

#' RenderedDoc
#'
#' @description ...
#' @export
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
          "c" = "toc", "n" = "materials", "m" = "material"
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
        "c" = NA, "n" = NA, "m" = NA
      ) )[[ private$type_ ]]
    }
  )
)

# Lecture -----

#' Lecture
#'
#' @description ...
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

# Session -----

#' Session
#'
#' @description ...
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

# Material -----

#' Material
#'
#' @description ...
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

# TheCourse -----

#' TheCourse
#'
#' @description ...
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

# BaseRenderer -----

#' BaseRenderer
#'
#' @description ...
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

    makeAll = function( course, makeZip = TRUE, ... ) {
      self$clearDir()
      allFiles <- c(
        self$makeLectures( course, ... ),
        self$makeTasks( course, enableCode = FALSE, ... ),
        self$makeTasks( course, enableCode = TRUE, ... ),
        self$makeToc( course, ... ),
        self$makeMaterials( course, ... )
      ) %>% unlist() %>% as.vector()

      if( makeZip ) {
        outZipFile <- paste0( self$outDir(), ".zip" )
        attr( outZipFile, "contentType" ) <- "application/zip"
        file.remove( outZipFile )
        zip( zipfile = outZipFile, files = allFiles, flags = "-9Xp" )
      }

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

# Renderer -----

#' Renderer
#'
#' @description ...
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
        #private$intRefHtml( "&#x2302;&nbsp;Contents", url = self$docUrl( course$tocDoc() ) ),
        #private$intRefHtml( "&#x1F4C1;&nbsp;Materials", url = self$docUrl( course$materialsDoc() ) )
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
        #private$intRefHtml( "&#x2302;&nbsp;Contents", url = self$docUrl( course$tocDoc() ) ),
        #private$intRefHtml( "&#x1F4C1;&nbsp;Materials", url = self$docUrl( course$materialsDoc() ) ),
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
        #private$intRefHtml( "&#x2302;&nbsp;Contents", url = self$docUrl( course$tocDoc() ) )
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
#      } else if( doc$type( long = TRUE ) == "toc" ) {
#        text <- private$tocNavigationBarHtml( course )
#      } else if( doc$type( long = TRUE ) == "materials" ) {
#        text <- private$materialsNavigationBarHtml( course )
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

# BrightspaceRenderer -----

#' BrightspaceRenderer
#'
#' @description ...
#' @export
BrightspaceRenderer <- R6Class(
  "BrightspaceRenderer", inherit = BaseRenderer,
  private = list(),
  public = list(
    #navigationBarHtml = function( course, doc ) c()
  )
)
