## Put comments here that give an overall description of what your
## functions do
## Description: The following code creates a pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function
## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ## initialize inverse variable to NULL
        inverse <- NULL
        
        ## define function to set matrix value and clear inverse variable
        setmatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## define function to return matrix stored in variable x
        getmatrix <- function() x
        
        ## define function to assign passed argument to inverse variable
        setinverse <- function(z) inverse <<- z
        
        ## define function to return inverse variable
        getinverse <- function() inverse
        
        ## return list of each function defined above
        list(setmatrix = setmatrix, getmatrix = getmatrix, 
             setinverse = setinverse, getinverse = getinverse)
        
}


## Write a short comment describing this function
## computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        ## first check if inverse is already populated, if it is then return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse) 
        }

        ## if inverse is NULL, then re-assign inverse and return it
        data <- x$getmatrix()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
