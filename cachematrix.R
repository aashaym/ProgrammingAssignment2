## This is part of the Assignment 2 
## Below is a function to calculate the inverse of a given matrix 
## the program solves the problem by employing a feature called "LEXICAL SCOPING"
##  the function "makeCacheMatrix"  creates an object and stores the matrix and result of it 
##  therby making it easier to compute the inverse of the Matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The cacheSolve  function computes the inverse of the special
## "matrix" returned by makeCacheM<atrix above.If the  inverse has already been calculated, then
## the cachesolve should retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}


## The below results demonstrates the IDEA 
v<-makeCacheMatrix(matrix(1:4,2,2))
v$get()
   [,1] [,2]
[1,]    1    3
[2,]    2    4

v$getInverse()
NULL
cacheSolve(v)
 [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

v$getInverse()
 [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5


v$set(matrix(c(1,2,6,7),2,2))
v$get()
 [,1] [,2]
[1,]    1    6
[2,]    2    7
cacheSolve(v)
     [,1] [,2]
[1,] -1.4  1.2
[2,]  0.4 -0.2
v$getInverse()
     [,1] [,2]
[1,] -1.4  1.2
[2,]  0.4 -0.2


