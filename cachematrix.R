## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function( m = matrix() ) { 
 
 
 	##the inverse and then I set the matrix 
     ca <- NULL 
     setMatrix <- function( matrix ) { 
             m <<- matrix 
             ca <<- NULL 
} 
     ##  getting the stored matrix and return it
     get <- function() { 
     	m 
     } 
 
 
     ## set the inverse of the matrix and then I get the inverse of the matrix
     setInverse <- function(solve) { 
         ca <<- solve 
     } 
 
     getInverse <- function() { 
         ca 
     } 
 
     ## Return a list of the methods 
     list(setMatrix = setMatrix, get = get, 
          setInverse = setInverse, 
          getInverse = getInverse) 
 } 
 
 ## Calculate  the inverse of  matrix from "makeCacheMatrix" 
 ## then  "cachesolve" should retrieve the inverse. 
 cacheSolve <- function(x, ...) { 
 
 
     ## Return a matrix that is the inverse of 'x'  and then return the inverse
     m <- x$getInverse() 
     if( !is.null(m) ) { 
             message("getting data from cache") 
             return(m) 
     } 
 
 
     ## then getting  the matrix from the object, and then compute the inverse with %*% for matrix X 
     data <- x$get() 
     m <- solve(data) %*% data 
 
 
     ## finnaly Setting  the inverse and return the matrix 
     x$setInverse(m) 
     m 
 } 
