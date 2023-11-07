R Open Online Course (ROOC) is an R package to develop a course material in modular fashion. See class `Course`, see  `?Course` for more details. An instance of the this object can be used to generate the web-site for materials. 

### Environment preparation

Set up the environment and libraries first based on the YAML file. Then, additionally manually install some libraries not provided through conda.

```bash
conda env create -f envs/rooc.yaml
conda activate rooc
R --vanilla -e 'devtools::install_github("hadley/emo")'
R --vanilla -e 'devtools::install_github("mitchelloharawild/icons")'
```

### Quick start

Create an RStudio project from this repository and proceed with build/install. The directory `rcourse` contains all .Rmd files necessary for building the course material. 

### Build course material

```r
library(rooc)                    # load ROOC library
rc <- Course$new(path="rcourse") # instantiate course 'rcourse'
rc$render()                      # render R Markdown files into 'docs' folder (conform 
                                 # github pages) and docs.zip    
                                   
rc$view()                        # view the course in a browser

rc$render(out_dir = "docs/<course_yyyymm>")
```

To include the course in github pages update `docs/index.Rmd` with link to `docs/<course_yyyymm` and knit.

### Tagging course iterations

The material  is tagged with each iteration of the course, with tags such as `B1R_202311`. Create the tags locally and push them to master as follows:    

```r
git tag -a <course-tag> -m "<short-description>"     # create a tag locally
git push origin <course-tag>                         # push to remote respo.
```

You'll need to fetch remote tags, unknown to you, using:

```r
git fetch --tags
```


### Visit live pages

> [https://lumc.github.io/rcourse/](https://lumc.github.io/rcourse/)

