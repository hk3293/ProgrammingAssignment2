## write a pair of functions that cache the inverse of a matrix

## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

## makeCacheMatrix: This function creates a special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        #initializing inverse as NULL
        inv <- NULL
        
        #Setting the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #Getting the matrix x
        get <- function()x
        
        #Setting the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        
        #Getting the inverse of the matrix
        getinverse <- function() inv
        
        #Returning a list of the methods
        list(set = set, get = get, 
             setinverse= setinverse, 
             getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        
        ##returning a matrix that is the inverse of the matrix x
        inv <- x$getinverse()
        
        #returning the inverse if the value is already there
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #getting the matrix from the object
        data <- x$get()
        
        #calculating the inverse
        inv <- solve(data)
        
        #setting the inverse to the object
        x$setinverse(inv)
        
        #returning the matrix
        inv
}
