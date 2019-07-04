
set_similarity(full_set[[1]], full_set[[4]], method = "Jaccard")

A <- set("aba", "besta", "cabra")
B <- set("cobra", "dobra", "besta")
set_similarity(A, B)
set_dissimilarity(A, B)


sapply(A, B, function(A, B) {
   set_similarity(B, A, method="Jaccard")
})

set_similarity(full_set[[1]], full_set[[2]], method="Jaccard")

# does the order matter? for the jaccard - NO. For dl, yes, a lot. Too much.
stringsim(a="New true", b="true weN", method="jaccard", useBytes=FALSE) # 1
stringsim(a="New true", b="true weN", method="dl", useBytes=FALSE) # 0

A <- sapply(full_set, function(X) {as.set(unlist(X))})

as.set(unlist(full_set[3]))

real_set <- NULL
for(i in 1:length(full_set)) {
   real_set <- c(real_set,unique(full_set[[i]]))
   #real_set <- c(real_set,as.set(full_set[[i]]))
}
View(real_set)
View(full_set)

as.set(full_set[[5]])

unique(full_set[[5]])




intersect(full_set[[1]],full_set[[5]])


#' Calculates the Jaccard similarity index between the unique elements of the two given vectors.
#' Formula is Jaccard = intersection(v1,v2) / union(v1,v2)
#'
#' @param v1 first vector - usually the one you are looking for
#' @param v2 second vector - usually your base list
#'
#' @return double value in the range [0-1] being: 
#'       [0]: indicates that there is no common elements, thus there is no similarity
#'       [1]: indicates that all elements of one vector are present
#' @export
#'
#' @examples
jaccardSimil <- function(v1, v2) {
   if ( length(v1) > 0 & length(v2 > 0 ) ) {
      i <- length(intersect(v1,v2))
      if ( i > 0 ) {
         return ( i / length(union(v1, v2)) ) 
      } 
   }
   return(0)
}


#' Wrapper for the jaccardSimil() function, calling it for each one of the elements
#' of the second vector. The first vector will be the also in this position in the inner call.
#' The vList is assumed to be a list of lists, thus the elements are iterated as vList[[i]]
#'
#' @param v 
#' @param vList 
#'
#' @return
#' @export
#'
#' @examples
jaccardSimList <- function(v, vList) {
   jList <- NULL
   for(i in 1:length(vList)) {
      jList <- c(jList,jaccardSimil(v,vList[[i]]))
   }
   return(jList)
}

jaccardSimil(full_set[[1]],full_set[[2]])
set_similarity(full_set[[1]], full_set[[2]], method="Jaccard")


jaccardSimList(full_set[[1]], full_set)
set_similarity(full_set[[1]], full_set[[2]], method="Jaccard")
