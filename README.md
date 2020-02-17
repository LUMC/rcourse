# DAR

Data Analysis with R (DAR) is an R package to develop the R course in a modular fashion. 


The file `shedule.yml` holds all the information needed to buil the site for the corresponding course. 
Multiple courses can ben created using this file. Below is an example of a course: 

```
BAST_march_2020:
  title: "BAST week"
  start: "23-03-2020"   
  end: "26-03-2020"
  exam:
    date:
    doc:
  slots:
    dplyr0:
      title: "tidyverse" 
      subtitle: "dplyr basics"
      date: "23-03-2020"
      time:
      doc: dplyr0.html
      exercises: dplyr0.tasks.nocode.html 
      solutions: dplyr0.tasks.html 
    dplyr_pipe:
      title: "tidyverse"
      subtitle: "dplyr '%>%' operator (pipe)"
      date: 
      time:
      doc: dplyr_pipe.html
      exercises: dplyr_pipe.tasks.nocode.html 
      solutions: dplyr_pipe.tasks.html 
```

A course has a:

  - `title`, 
  - `start` and `end` date,
  - exam with a `date` and the document name `doc`
  - collection of `slots` with each slot having
    - title, subtitle, data, time, document, exercises and it solutions

For each course such block can be constructed and kept and archived. 




