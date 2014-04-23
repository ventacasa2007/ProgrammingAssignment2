## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## First initialize the variable miverse to null
## After define de function set, get, setInverse and getInevrse
## Return a list wirh all functions

makeCacheMatrix <- function(x = matrix()) {
        minverse <- NULL
        set <- function(y) {
                x <<- y
                minverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) minverse <<- inv
        getInverse <- function() minverse
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## First use the function getInverse to the variable minverse
## After check if the value is null
## If it's not null means that it has been calculated alredy so return the value.
## If it's null call the function solve to get the inverse and use the function SetInverse to put the value

cacheSolve <- function(x, ...) {
        
        minverse <- x$getInverse()
        
        if(!is.null(minverse)) {
                message("getting cached data")
                return(minverse)
        }
        
        data <- x$get()
                minverse <- solve(data, ...)
        x$setInverse(minverse)
        minverse
}
