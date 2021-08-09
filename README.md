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


R Open Online Course (ROOC) is an R package to develop a course in a modular fashion. it has a single class `Course`, see  `?Course` for more details. An instance of the this object can be used to generate the web-site for materials. 


**Quick start** Create a RStudio project from git repository https://git.lumc.nl/r.monajemi/rooc.git and build it. The directory site contains all .Rmd files necessary for building the the web-pages for the course. The following code should work:

```r
library(rooc) # load ROOC library
crip <- Course$new(path="CRiP_202109") # instantiate course CRiP_202109 (see below for details of `path`)
crip$render()   # render R Markdown files in CRiP_202109  
rcourse$view()  # view the course in a browser
```