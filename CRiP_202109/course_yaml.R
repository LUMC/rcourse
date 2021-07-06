library( rooc )

genCourse <- function(dir, file = "config.yml", testOnly = FALSE) {
  # Course config file (YAML)
  config <- yaml.load_file(file.path(dir,file))

  # global course info
  startDate <- as.Date( config[["startDate"]] )

  # TheCourse object
  course <- TheCourse$new( id = config[["course_id"]], dir = dir, label =  config[["course_label"]]  )

  slots <- config[["slots"]]
  for( i in seq_len( length( slots ) ) ) {
    slot_id <- names(slots)[[i]]
    slot <- slots[[i]]
    session_ <- Session$new(
      id = slot_id, label = slot[["slot_label"]],
      date = startDate + slot[["slot_date"]],
      timeRange = slot[["slot_time"]],
      breaksPattern = slot[["slot_plan"]]
    )

    for( lecture in slot[["lectures"]] ) {
      lecture_ <- strsplit(lecture,":")[[1]] # [id,label,hasTasks,min]
      session_$add(Lecture$new(id=lecture_[1], label=lecture_[2],hasTasks=as.logical(lecture_[3]),min=as.numeric(lecture_[4])))
    }

    course$add(session_)
  }

  # Materials
  materials <- lapply(config[["materials"]], function(m) {
    m_ <- strsplit(m,":")[[1]] # id:label:path:out_path
    material_  <- Material$new(id=m_[1],label=m_[2],path=m_[3],outPath=m_[4])
    course$add(material_)
  })

  course
}

courseDir <- "CRiP_202109"
course_ <- genCourse( dir = courseDir )
renderer <- Renderer$new( outDir = paste0( courseDir, ".site" ) )
#renderer <- BrightspaceRenderer$new( outDir = "tmp" )
renderer$makeAll( course = course_ )

