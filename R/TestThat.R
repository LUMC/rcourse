#'
#'
testthat::test_that( "Doc properties", {
  doc <- Doc$new( id = "lecture1" )
  testthat::expect_identical( doc$id(), "lecture1" )
} )


#'
#'
testthat::test_that( "CopiedDoc properties", {
  doc <- CopiedDoc$new( id = "lecture1", label = "Lecture 1", path = "abc.Rmd", outPath = "src/abc.Rmd" )
  testthat::expect_identical( doc$id(), "lecture1" )
  testthat::expect_identical( doc$path(), "abc.Rmd" )
  testthat::expect_identical( doc$outPath(), "src/abc.Rmd" )
  #testthat::expect_identical( doc$outDir(), NULL )
} )

#'
#'
testthat::test_that( "RenderedDoc properties", {
  doc <- RenderedDoc$new( id = "lecture1", label = "Lecture 1", naviIds = list( "next" = "lecture2" ), rmdFile = "abc.Rmd" )
  testthat::expect_identical( doc$id(), "lecture1" )
  testthat::expect_identical( doc$rmdFile(), "abc.Rmd" )
} )

#'
#'
testthat::test_that( "Lecture properties", {
  l1 <- lecture( id = "lecture1", label = "Lecture1", min = 30 )
  testthat::expect_equal( l1$id(), "lecture1" )
  testthat::expect_equal( l1$label(), "Lecture1" )
  testthat::expect_equal( l1$rmdFile(), "lecture1.Rmd" )
  testthat::expect_equal( l1$hasTasks(), TRUE )
  
  d <- l1$asTibble()
  testthat::expect_equal( nrow( d ), 1L )
  testthat::expect_true( all( c( "lecture.id", "lecture.label", "lecture.rmdFile" ) %in% colnames( d ) ) )
})


#'
#'
testthat::test_that( "Session properties", {
  l1 <- lecture( id = "lecture1", min = 30 )
  l2 <- lecture( id = "lecture2", min = 45 )
  s1 <- session( id = "session1" ) %>% add( l1 ) %>% add( l2 )
  testthat::expect_identical( s1$lectureIds(), c( "lecture1", "lecture2" ) )
})


#'
#'
testthat::test_that( "Material properties", {
  m1 <- material( id = "material1", label = "First dataset", path = "material1.csv", outPath = "data/material1.csv" )
  testthat::expect_equal( m1$id(), "material1" )
})

#'
#'
testthat::test_that( "TheCourse properties", {
  l11 <- lecture( id = "lecture11", min = 30 )
  l12 <- lecture( id = "lecture12", label = "Lecture12", min = 45 )
  l21 <- lecture( id = "lecture21", min = 30 )
  l22 <- lecture( id = "lecture22", min = 45 )
  s1 <- session( id = "session1" ) %>% add( l11 ) %>% add( l12 )
  s2 <- session( id = "session2" ) %>% add( l21 ) %>% add( l22 )
  m1 <- material( id = "material1", label = "First dataset", path = "material1.csv", outPath = "data/material1.csv" )
  course <- theCourse( id = "course", dir = "." ) %>% add( s1 ) %>% add( s2 ) %>% add( m1 )
  testthat::expect_identical( course$sessionIds(), c( "session1", "session2" ) )
  testthat::expect_identical( course$lectureIds(), c( "lecture11", "lecture12", "lecture21", "lecture22" ) )
  testthat::expect_identical( course$id2label( "lecture12" ), "Lecture12" )
  testthat::expect_identical( course$materialIds(), c( "material1" ) )
  
  d <- course$lecturesTibble()
  testthat::expect_identical( d$lecture.id, c( "lecture11", "lecture12", "lecture21", "lecture22" ) )
  testthat::expect_identical( d$lecture.prevId, c( NA, "lecture11", "lecture12", "lecture21" ) )
  testthat::expect_identical( d$lecture.nextId, c( "lecture12", "lecture21", "lecture22", NA ) )
  
  doc <- course$lectureDoc( "lecture12" )
  testthat::expect_identical( doc$id(), "lecture12" )
} )

#'
#'
testthat::test_that( "BaseRenderer properties", {
  l1 <- lecture( id = "lecture1", min = 30 )
  s1 <- session( id = "session1" ) %>% add( l1 )
  course <- theCourse( id = "course", dir = "." ) %>% add( s1 )
  
  renderer <- BaseRenderer$new( outDir = "." )
  testthat::expect_identical( renderer$outDir(), "." )
} )

#'
#'
testthat::test_that( "BrightspaceRenderer properties", {
  renderer <- BrightspaceRenderer$new( outDir = "tmp" )
  testthat::expect_identical( renderer$outDir(), "tmp" )
} )

