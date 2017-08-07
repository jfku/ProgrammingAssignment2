## purpose is to assign a value to an object in an environment that is different from the current environment. 
## Below are two functions that are used to create a special object that stores a numeric vector and cache's its mean.

## makeCacheMatrix takes a square invertible matrix as input
## assigns variables outside the current environment
## set the matrix, get the matrix, set the inverse, get the inverse
#" returns a list of functions as output

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## if cached then skip the computation otherwise perform the calculation of the inverse matrix
## get the inverse of the matrix by solve function
    
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## test
## x <- matrix(c(1,2,3,4),2)
## cacheSolve(makeCacheMatrix(x))



