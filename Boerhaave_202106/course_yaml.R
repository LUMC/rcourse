library( rooc )
library(yaml)

genCourse <- function(dir, testOnly = FALSE) {
  
  # Read YAML file
  config <- yaml.load_file(file.path(dir,"config.yml"))
  # global course info
  startDate <- as.Date( config[["startDate"]] )
  morning <- config[["morning"]]
  afternoon <- config[["afternoon"]]
  
  # TheCourse object
  course <- TheCourse$new( id = config[["course_id"]], dir = config[["course_path"]], label =  config[["course_label"]]  )
  
  slots <- config[["slots"]]
  invisible( lapply(1:length(slots), function(i) {
    slot_id <- names(slots[i])
    slot <- slots[[i]]
    session_ <- Session$new( id = slot_id, label = slot[["slot_label"]], date = startDate + ((i-1)%/%2), timeRange = slot[["slot_time"]] ) 
    # add lectures
    lectures <- lapply(slot[["lectures"]], function(lecture) {
      lecture_ <- strsplit(lecture,":")[[1]] # [id,label,hasTasks,min]
      session_$add(Lecture$new(id=lecture_[1], label=lecture_[2],hasTasks=as.logical(lecture_[3]),min=lecture_[4]))
    })
    course$add(session_)
  })) 
  
  # Materials
  materials <- lapply(config[["materials"]], function(m) {    
    m_ <- strsplit(m,":")[[1]] # id:label:path:out_path
    material_  <- Material$new(id=m_[1],label=m_[2],path=m_[3],outPath=m_[4])
    course$add(material_)
  })
  
  course
}

course_ <- genCourse( dir = "Boerhaave_202106", FALSE )
renderer <- Renderer$new( outDir = "Boerhaave_202106.site" )
#renderer <- BrightspaceRenderer$new( outDir = "tmp" )
renderer$makeAll( course = course_ )
