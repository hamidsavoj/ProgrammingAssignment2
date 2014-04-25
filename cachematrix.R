
## The functions makeCacheMatrix and cacheSolve are described below.
## These two functions work together to reduce runtime in computing the 
## inverse of the same matrix many times by caching the inverse matrix.

##The function, makeacheMatrix creates a list for storing a matrix
##containing the following functions:

##   set() set the matrix
##   get() get the stored matrix
##   setInverse()  sets the inverse matrix
##   getInverse() gets the inverse matrix

##   Any change in given matrix sets the inverse to NULL


makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(invMat) inverse <<- invMat
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## This function computes the inverse of the stored matrix.
## However, it first checks to see if the inverse is already calculated.
## If inverse already exists, the inverse is taken from Cache.
## Otherwise, inverse is computed  and stored via setInverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}


## I used the following code for testing these functions
## m<-matrix(1:4,nrow=2,ncol=2)
## a<-makeCacheMatrix(m)
## a$get()
## cacheSolve(a)
## cacheSolve(a) %*% a$get()
## a$getInverse() %*% a$get()
## m<-matrix(2:5,nrow=2,ncol=2)
## a$set(m)
## a$getInverse()

