#
# o1 : graphNEL
#
library(graph) ; library(Rgraphviz)
o1 <- randomGraph(letters[1:10], 1:4, 0.5)

#
# o2 : lm
#
o2 <- lm(extra ~ group, data=sleep)

#
# o3 : density
#
o3 <- density(mtcars$hp)

#
# o4 : table
#
o4 <- table(sample(letters, 20, replace=T))

#
# o5 : 
#
o5 <- 1:10

#
# o6 : named vector
#
names(o5) <- letters[1:10]


