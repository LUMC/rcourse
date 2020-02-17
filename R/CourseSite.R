#' CourseSite
#'
#' Collection of methods to build a web-site with rmarkdown::render_site().
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
#' \code{\link{CourseSite}}
#'
#'@examples
#'
#'
#'@export
CourseSite <- R6Class("CourseSite",
    private = list(
      url_ = NULL,
      site_ = NULL,
      config = NULL
    ),
    public = list(
      
      initialize = function() {
        #
        # config file
        #
        cfg <- file.path(rprojroot::find_rstudio_root_file(),"site/_meta/schedule.yml")
        if (file.exists(cfg) ) {
          private$config <- yaml.load_file( cfg  )
        } else {
          error("missing config.yaml !")
        }  
        rmarkdown::render_site(self$src())
        private$site_ <- file.path(self$src(),"_site")
        private$url_ <- file.path(self$src(),"_site/index.html")
        html_files <- dir(self$src(),pattern = ".nocode.html$", full.names = TRUE)
        # self$build()
      },
      src = function() {
        file.path(rprojroot::find_rstudio_root_file(),"site")
      },
      build = function(){
        
        lapply(self$lstmod(),function(b) {
          rmarkdown::render_site(file.path(self$src(),paste0(b,".Rmd")))  
        })
        html_files <- dir(self$src(),pattern = ".nocode.html$", full.names = TRUE)
        unlink(html_files)
      },
      url = function() {
        private$url_
      },
      path = function() {
        private$site_
      },
      view = function() {
        browseURL(self$url())
      },
      courseSummary = function() {
      },
      slots = function() {
        config <- private$config
        course <- config[["current"]]
        slots <- config[[course]] [["slots"]]
        names(slots)
      },
      modified = function(f) {
        (file.info(file.path(self$src(),"_site",paste0(f,".html") ))$mtime - 
         file.info(file.path(self$src(), paste0(f,".Rmd") ) )$mtime )  <= 0 
      },
      lstmod = function() {
        rmds <- dir(self$src(), pattern = ".Rmd")
        rmds <- rmds[!grepl('^_',rmds)]
        file_basenames <- sapply(rmds[!grepl('^_',rmds)], function(x) sub(".Rmd","",x)) %>% as.vector()
        file_basenames[sapply(file_basenames, self$modified)]
      } 
    )
)