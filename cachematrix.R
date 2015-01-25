## Assignment 2 requires us to create a way to cache the inverse of a matrix
## For this purpose, two functions are necessary. The first function (makeCacheMatrix)
## allows the user to create a special "matrix" and also stores the inverse in cache.
## The second function (cacheSolve) checkes if the inverse has been previously
## calculatated and stored in cache. If it has been stored, cacheSolve will retreive
## the inverse matrix from cache. If it has not been stored, cacheSolve will newly
## calculate the inverse of the special "matrix" that was inputted and send it to 
## makeCacheMatrix to be cached.

## makeCacheMatrix is actually a function consisting of a list of 4 functions which 
## are designed to: 
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the inverse of a matrix and to cache the inverse
## 4. get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        setmatrix <- function (y){
                x <<- y
                i <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function () i
        list (setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
        
}

## cacheSolve is a function that calculates the inverse of the "special" matrix that was
## set using makeCacheMatrix. Before caclulating the inverse, it will check if the
## inverse had been previously calculated and stored in cache. If it has been stored
## then cacheSolve will retrieve the cached value. If nothing is stored in cache, then
## cacheSolve will recalculate the inverse and send it to makeCacheMatrix for caching.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$getmatrix()
        i <- solve(data,...)
        x$setinverse(i)
        i
}   