## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
#this function is very similar to the one presented for the vector, but as a matrix as argument, and returns the inverse instead of the mean

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

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        # first, test if inverse has been calculated
        inv <- x$getinverse()
        #if yes, then acquire the inverse
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)?
        }
        #otherwise, then calculate the inverse
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        #return
        inv
}

