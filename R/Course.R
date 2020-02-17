#' Class to build a web-site with rmarkdown rendering functionalities.
#' 
#' @description The class generates a web-site based on the Rmd files in the directory 'site'. The directory has a flat structure 
#' and contains several categories of files:
#'
#' \describe{
#'   \item{_site.yml}{This file in YAML format describing the structure and the look of the site with menues and sub-menus.}
#'   \item{_meta/schedule.yml}{This file contains information about the course (see details).}
#'   \item{<module-name>.Rmd}{These files contain the course material on a specific topic.}
#'   \item{_<name>.Rmd}{These Rmd files can be re-used and are called from inside other Rmd files. They do not have a html 
#'   counterpart in the _site directory.}   
#'   \item{data and images}{ These directories copied into _site directory}
#'   \item{footer.html}{Footer content for all pages.}
#'   \item{setup.R}{This is needed only if the individual Rmd files need to be genrated inside RStudio using the 'knit' button.}
#'   \item{Styles.css}{Stylesheet file for the appearance}
#' }
#' 
#' 
#' 
#' @docType class
#' @importFrom R6 R6Class
# @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#' @section Methods:
#' \describe{
#'   \item{...}{...}
#' }
#'
#' @seealso
#' \code{\link{Course}}
#'
#'@examples
#'
#'@export
Course <- R6Class("Course",
    private = list(
      url_ = NULL,
      site_ = NULL,
      schedule = NULL,
      modified = function(f) {
        # Returns TRUE if the html file of the corresponding 'Rmd' file is absent or the 
        # Rmd' file's modification date exceeds the html's.         
        html_file <- file.path(self$src(),"_site",paste0(f,".html"))
        ifelse(!file.exists(html_file), TRUE, 
               ((file.info(html_file)$mtime - file.info(file.path(self$src(), paste0(f,".Rmd") ) )$mtime )  <= 0) )
      }
    ),
    public = list(
      #' @description Instantiates a 'Course' object. It will load the schedule.yml 
      #' and renders the site for the default course (see current tag in shedule.yml)..
      initialize = function() {
        options(knitr.duplicate.label = "allow")  # RESOLVE THIS !!!
        options(width=120)
        #
        # schedule file
        #
        cfg <- file.path(rprojroot::find_rstudio_root_file(),"site/_meta/schedule.yml")
        if (file.exists(cfg) ) {
          private$schedule <- yaml.load_file( cfg  )
        } else {
          error("missing schedule.yml !")
        }  
        #
        # render and set url and site path. 
        #
        rmarkdown::render_site(self$src())
        private$site_ <- file.path(self$src(),"_site")
        private$url_ <- file.path(self$site(),"index.html")
      },
      #' @description Path to site's directory containing all Rmd files. 
      src = function() {
        file.path(rprojroot::find_rstudio_root_file(),"site")
      },
      #' @param clean If true the clean the site first.
      #' @description Render the site only for modified Rmd's. 
      render = function(clean=FALSE,...){
        if (clean)
          rmarkdown::clean_site(self$src())
        lapply(self$lstmod(),function(b) {
          rmarkdown::render_site(file.path(self$src(),paste0(b,".Rmd")),...)  
        })
        html_files <- dir(self$src(),pattern = ".nocode.html$", full.names = TRUE)
        unlink(html_files)
      },
      #' @description Return the path to site's directory.    
      site = function() {
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
       private$schedule         
      },
      #' @description Returns the list of course slots. The data is taken from 'shedule.yml'.   
      slots = function() {
        schedule <- private$schedule
        course <- schedule[["current"]]
        slots <- schedule[[course]] [["slots"]]
        names(slots)
      },
      #' @description Returns the list of modified files.
      lstmod = function() {
        rmds <- dir(self$src(), pattern = ".Rmd")
        rmds <- rmds[!grepl('^_',rmds)]
        file_basenames <- sapply(rmds[!grepl('^_',rmds)], function(x) sub(".Rmd","",x)) %>% as.vector()
        file_basenames[sapply(file_basenames, private$modified)]
      },
      listing = function(){
        shedule <- self$summary()
        course <- shedule[[ shedule[["current"]] ]]
        rmds <- names(course[["slots"]])
        other <- c("images","data","_meta","styles.css","_site.yml","footer.html")
      }
    )
)