#' Class to build a web-site with rmarkdown rendering functionalities.
#' 
#' @description The class generates a web-site based on the Rmd files in the directory 'site'. The directory has a flat structure 
#' and contains several categories of files:
#'
#' \describe{
#'   \item{\strong{_site.yml}}{This file in YAML format describing the structure and the look of the site with menues and sub-menus.}
#'   \item{\strong{_schedule.yml}}{This file contains information about the course (see details).}
#'   \item{\strong{<module-name>.Rmd}}{These files contain the course material on a specific topic.}
#'   \item{\strong{_<name>.Rmd}}{These Rmd files can be re-used and are called from inside other Rmd files. They do not have a html 
#'   counterpart in the _site directory.}   
#'   \item{\strong{data and images}}{ These directories are copied into _site directory}
#'   \item{\strong{footer.html}}{Footer content for all pages.}
#'   \item{\strong{setup.R}}{This is needed if the individual Rmd files need to be generated inside RStudio using the 'knit' button.}
#'   \item{\strong{Styles.css}}{Stylesheet file for the appearance}
#' }
#' 
#' The files prefixed with \strong{_} will not be rendered and are for internal use.  
#' 
#' @details 
#' 
#' The \strong{schdule} file has the following structure:
#' 
#' \preformatted{
#'    course:
#'        title: <course-title>
#'        start: <start-date>
#'        end: <end-date>
#'        exam: 
#'            date: <exam-date>
#'            time: <time-time>
#'            venue: <venue>
#'        slots:
#'            <slot-id>:
#'                title:  <session-title>
#'                subtitle: <session-subtitle>
#'                goal: <goals/description>
#'                date: <session-date>
#'                time: <session-time>
#'                venue: <venue>
#'                tasks : "yes | no"     
#'            <slot-id>:
#'                ...
#' }
#' 
#' 
#' 
#' @docType class
#' @importFrom R6 R6Class
# @export
#' @format An \code{\link{R6Class}} generator object
#' @section Methods:
#'
#'@examples
#' rcourse <- Course$new()
#' rcourse$view()
#'
#'@export
Course <- R6Class("Course",
    private = list(
      sources_ = NULL,
      url_ = NULL,
      site_ = NULL,
      schedule_ = NULL,
      modified = function(f) {
        # Returns TRUE if the html file of the corresponding 'Rmd' file is absent or the 
        # Rmd' file's modification date exceeds the html's.         
        html_file <- file.path(self$src(),"_site",paste0(f,".html"))
        ifelse(!file.exists(html_file), TRUE, 
               ((file.info(html_file)$mtime - file.info(file.path(self$src(), paste0(f,".Rmd") ) )$mtime )  <= 0) )
      },
      zip_ = function(zip_file){
        prefix <- sub(".zip","",zip_file)
        if (file.exists(prefix))
          unlink(prefix)
        if (file.exists(zip_file))
          unlink(zip_file)
        file.symlink(from = private$sources_, to = prefix)
        zip(zipfile = paste0(prefix,".zip"), 
            files = paste0(prefix,"/",self$listing()), flags = "-r")
        unlink(prefix)
      },
      read_schedule = function() {
        cfg <- file.path(self$src(),"_schedule.yml")
        if (file.exists(cfg) ) {
          private$schedule_ <- yaml.load_file( cfg  )
        } else {
          error("missing _schedule.yml !")
        }  
      }
    ),
    public = list(
      #' @param site name of the course, create it if it does not exist, otherwise instantiate it.
      #' @param ... arguments to rmarkdown::render_site
      #' @description Instantiates a 'Course' object. It will load _schedule.yml 
      #' and renders the site for the default course (see current tag in _schedule.yml)..
      initialize = function(site="site", ...) {
        options(knitr.duplicate.label = "allow")  # RESOLVE THIS !!!
        options(width=120)
        private$sources_ <- site
        
        if (!file.exists(self$src())) {
          cat('Starting new course ',site,'.\n', sep = "")
          Sys.sleep(2)
          rmarkdown::draft(file = site, template = "inst/rmarkdown/templates/course", edit = FALSE)
          unlink(file.path(site,paste0(site,'.Rmd')))
          rmarkdown::draft(file = file.path(site,"index.Rmd"), template = "inst/rmarkdown/templates/index", edit = FALSE)
        }
        # schedule file
        private$read_schedule() 
        # render and set url and site path. 
        rmarkdown::render_site(self$src(),...)
        self$clear_nocode_html()
        private$site_ <- file.path(self$src(),"_site")
        private$url_ <- file.path(self$site(),"index.html")
      },
      #' @description Path to site's directory containing all Rmd files. 
      src = function() {
        file.path(rprojroot::find_rstudio_root_file(),private$sources_)
      },
      #' @description clear generated nocode html file
      clear_nocode_html = function() {
        html_files <- dir(self$src(),pattern = ".nocode.html$", full.names = TRUE)
        unlink(html_files)
      },
      #' @param clean If true the clean the site first.
      #' @param ... arguments to rmarkdown::render_site
      #' @description Render the site only for modified Rmd's. 
      render = function(clean=FALSE,...){
        if (clean)
          self$clean()
        lapply(self$lstmod(),function(b) {
          rmarkdown::render_site(file.path(self$src(),paste0(b,".Rmd")),...)  
        })
        self$clear_nocode_html()
      },
      #' @description Return the path to site's directory.    
      site = function() {
        private$site_
      },
      #' @description Return the path to site's index.html    
      url = function() {
        private$url_
      },
      #' @description View the site in the browser.    
      view = function() {
        browseURL(self$url())
      },
      #' @description Print the 'current' course summary.    
      summary = function() {
       private$schedule_         
      },
      #' @description Edit '_schedule.yml'. Render the pages by render() to enforce the changes.
      schedule = function() {
        file.edit(file.path(self$src(),"_schedule.yml"))
      },
      #' @description Returns the list of course slots. The data is taken from '_schedule.yml'.   
      slots = function() {
        schedule <- private$schedule_
        slots <- schedule[["course"]][["slots"]]
        names(slots)
      },
      #' @description Returns the list of modified files.
      lstmod = function() {
        rmds <- dir(self$src(), pattern = ".Rmd")
        rmds <- rmds[!grepl('^_',rmds)]
        file_basenames <- sapply(rmds[!grepl('^_',rmds)], function(x) sub(".Rmd","",x)) %>% as.vector()
        file_basenames[sapply(file_basenames, private$modified)]
      },
      #' @description Returns the list of files for zip archive.
      listing = function(){
        schedule <- self$summary()
        course <- schedule[["course"]]
        rmds <- dir(self$src(),pattern=".Rmd")
        other <- c("images","data","_schedule.yml","styles.css","_site.yml","footer.html")
        c(rmds,other)
      },
      #' @param filename name of zip archive.
      #' @description Create a zip archive.
      zip = function(filename="RCourse.zip"){
        if (!grepl(".zip$",filename))
          stop("invalud suffix, use extension .zip !")
        cat('exporting to ',filename, '...\n')
        Sys.sleep(2)
        msg <- try ( private$zip_(filename), silent = TRUE )
        if (class(msg)=="try-error") {
          message(msg)
        } else {
          file.copy(from = filename, to = file.path(private$sources_,"_site"), overwrite = TRUE)
          file.remove(filename)
        }
      },
      clean = function() {
        rmarkdown::clean_site(self$src())
      }
    )
)