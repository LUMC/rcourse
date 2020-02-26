# ROOC

Right Open Online Course (ROOC) is an R package to develop a course in a modular fashion. it has a single class `Course`, see  `?Course` for more details. An instance of the this object can be used to generate the web-site for materials. 


**Quick start** Create a RStudio project from git repository `https://git.lumc.nl/r.monajemi/rooc.git` and build it. The directory `site` contains all .Rmd files necessary for building the the web-pages for the course. The following code should work: 

```{r}
> library(rooc)
> rcourse <- Course$new() # instantiate and render the site
> rcourse$view()          # view in browser
```


The file `shedule.yml` ([YAML format](https://en.wikipedia.org/wiki/YAML)) holds all the information needed about the course to build the site. The file may contain multiple courses and can be edited.

Below is an example of a course: 

```code
R_course:
  title: "BAST Week"
  description: 
  start: "01-01-2020"   
  end: "31-12-2020"
  exam:
  slots:
    dplyr0:
      title: "tidyverse: data manipulation" 
      subtitle: "dplyr basics"
      gaol: 
      date: "dd-mm-yyyy"
      time: "00:00-00:00"
      venue: "venue"
      tasks : "yes"      
    dplyr_pipe0:
      title: "tidyverse: data manipulation"
      subtitle: "dplyr '%>%' operator (pipe)"
      gaol: 
      date: "dd-mm-yyyy"
      time: "00:00-00:00"
      venue: "venue"
      tasks : "yes"  
```


A limitations of this solution is its `flat directory structure` requirement. It is not straightforward to organise Rmd files into a directory structure, this may clutter the `site` folder. A workaround would be to archive each course and devise a new set of files for new courses. 








