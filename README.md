R Open Online Course (ROOC) is an R package to develop a course material in modular fashion. See class `Course`, see  `?Course` for more details. An instance of the this object can be used to generate the web-site for materials. 


**Quick start** Create an RStudio project from this repository and proceed with build/install. The directory `rcourse` contains all .Rmd files necessary for building the course material. 

**Build course material:**

```r
library(rooc)                    # load ROOC library

rc <- Course$new(path="rcourse") # instantiate course 'rcourse'

rc$render()                      # render R Markdown files into 'docs' folder (conform 
                                 # github pages) and docs.zip    
                                   
rc$view()                        # view the course in a browser
```


**Visit live pages:**

> [https://lumc.github.io/rcourse/](https://lumc.github.io/rcourse/)


