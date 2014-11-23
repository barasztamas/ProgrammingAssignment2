## Functions to create and use special extended "matrix variables" which are able to cache their inverse
## Work with square matrices only


## makeCacheMatrix function
## Creates a special extended "matrix variable" able to cache its inverse
## The created variable really is a list of three functions:
## 1. set, to set the value of the matrix
## 2. get, to get the value of the matrix
## 3. getinverse, to get (and if not already done cache) the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x    
## getinverse function checks first whether inverse is already cached
## - if not yet cached, calculates inverse, sets and returns the cache
## - if already cached returns inverse from cache
    getinverse <- function() {
        if(is.null(i)) {
            message("calculating and caching inverse")
            i <<- solve(x)
            return(i)
        }
        else{
            message("getting cached inverse")
            return(i)
        }
    }
    list(set = set, 
         get = get,
         getinverse = getinverse)
}


## cacheSolve function
## returns inverse of a special extended "matrix variable" created by the makeCacheMatrix function
## if the inverse is already cached, returns from cache, otherwise calculates inverse, sets cache and returns
## no code needed here for cache checking and setting as this is implemented in the getinverse function

cacheSolve <- function(x) {
    return(x$getinverse())
}
