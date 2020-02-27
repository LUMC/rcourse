---
title: "ROOC R package"
author:
- name: R. Monajemi
  affiliation: Biomedical Data Sciences | LUMC
date: "27 februari 2020"
output: 
  html_document:
    keep_md : true
    theme: cerulean
---



Right Open Online Course (ROOC) is an R package to develop a course in a modular fashion. it has a single class `Course`, see  `?Course` for more details. An instance of the this object can be used to generate the web-site for materials. 


**Quick start** Create a RStudio project from git repository `https://git.lumc.nl/r.monajemi/rooc.git` and build it. The directory `site` contains all .Rmd files necessary for building the the web-pages for the course. The following code should work: 


```r
> library(rooc)
> rcourse <- Course$new() # instantiate and render the 'site' (base example)
> rcourse$view()          # view in browser
```

## Create a course

First instantiate the course object with the course name which will be also the directory name for the source files. We will take the CRiP course (1st year) in sept/oct 2020 as example: 


```r
crip <- Course$new(site = "CRiP-sept-2020", quiet=TRUE) # See `?Course` for further usage.
```

This will create a course template with the following structure: 

```code 
CRiP-sept-2020
├── _build_exercises.Rmd   : helper Rmd, builds the task counterpart of the slot  
├── _contact.Rmd           : helper Rmd, contacts
├── data.Rmd               : data description
├── _exercises_links.Rmd   : helper Rmd, creates links to the tasks
├── footer.html            : footer content  
├── index.Rmd              : 
├── _schedule.Rmd          : helper Rmd, generates the schedule page based on _schedule.yml
├── schedule.Rmd           : schedule page
├── _schedule.yml          : the course schedule  
├── setup.R                : global functions
├── _site                  : generated at the initial instantiation with a call to render()
│   ├── data.html
│   ├── footer.html
│   ├── index.html
│   ├── schedule.html
│   ├── setup.R
│   ├── site_libs
│   └── styles.css
├── _site.yml              : site structure and appearance
└── styles.css             : stylesheet (not used!)   
```

The sub-directory `_site` contains the generated html files and can be deployed on the web-server.   

**Top level files** The files differ in type and naming. The file starting with an underscore, e.g. _build_exercises.Rmd, _schedule.yml etc, 
will not be included in the `_site` directory. 

The file `_shedule.yml` ([YAML format](https://en.wikipedia.org/wiki/YAML)) holds all the information needed about the course to build the 
site. Below is an example of a course with a single slot schedule: 

```code
course:
  title: "Course title"
  start: "01-01-2020"
  end: "10-01-2020"
  exam:
    date: "date"
    time: "time-time"
    venue: "venue"
  slots:
    stat0:
      title: "title"
      subtitle: ""
      gaol: "" 
      date: "date"
      time: "time-time"
      venue: "venue"
      tasks : "yes" # {yes | no}   
```

Creation of this file has to be done once, edit the file manually:


```r
crip$schedule()
```

It can also be generated from a table: 


```r
schedule <- read.csv("crip-schedule-sep-2020.csv", stringsAsFactors = FALSE) %>%  as_tibble()
schedule
```

```
# A tibble: 11 x 6
   slots  title                            date      time     venue        tasks
   <chr>  <chr>                            <chr>     <chr>    <chr>        <chr>
 1 slot1  General Introduction             21-09-20… 09.15-1… CZ-6         yes  
 2 slot2  R1 - R language - general intro… 22-09-20… 09.15-1… CZ-6         yes  
 3 slot3  R2 - Base R and Introduction to… 23-09-20… 09.15-1… CZ-6         yes  
 4 slot4  R3 - Data manipulation and visu… 24-09-20… 11.15-1… CZ-6         yes  
 5 slot5  Practicum 1 (STAT, R, EPI)       24-09-20… 14.15-1… V2-26/28, V… yes  
 6 slot6  Practicum 2 (STAT, R, EPI)       25-09-20… 10.15-1… V2-10/14, V… yes  
 7 slot7  R4 - Data manipulation and visu… 25-09-20… 14.15-1… CZ-6         yes  
 8 slot8  R5 - Reshaping data              28-09-20… 10.15-1… CZ-5_route-… yes  
 9 slot9  R6 - Data integration            29-09-20… 11.15-1… CZ-1_route-… yes  
10 slot10 R7 - Report example              01-10-20… 10.15-1… CZ-6         yes  
11 slot11 Examination R                    09-10-20… 10.15-1… USC-sportce… yes  
```



```r
#
# Construct _schedule.yml
#
slots <- split(schedule[,-1], schedule$slots)  # split entries into list of entries without slots variable
slots <- slots[schedule$slots][-length(slots)] # order slots and remove exam entry
yml <- as.yaml(                                # yaml::as.yaml convers a list structure into YAML format      
  list(course=list(
    title="CRiP course sept/oct 2020",
    start="21-09-2020",
    end="01-10-2020",
    exam=(schedule %>% filter(slots=="slot11") %>%  select(date,time,venue)),  
    slots=slots))
)
write(x = yml, file = "CRiP-sept-2020/_schedule.yml")
```

Here is an extract of the file: 

```code 
course:
  title: CRiP course sept/oct 2020
  start: 21-09-2020
  end: 01-10-2020
  exam:
    date: 09-10-2020
    time: 10.15-12.00
    venue: USC-sportcentrum
  slots:
    slot1:
      title: General Introduction
      date: 21-09-2020
      time: 09.15-12.00
      venue: CZ-6
      tasks: 'yes'
...
```


Render the pages to and view the schedule page:


```r
crip$render()
```


## Notes

A limitations of this solution is its `flat directory structure` requirement. It is not straightforward to organise Rmd files into a directory structure, this may clutter the top level folder. A workaround would be to archive each course and devise a new set of files for new courses. 








