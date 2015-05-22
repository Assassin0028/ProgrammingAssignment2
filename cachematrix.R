## This function creates a special kind of matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    # Initializing the inverse
    
        inver <<- matrix(NA,nrow=nrow(x),ncol=ncol(x))
    
    # Method to set the matrix
        
        set <- function(y) {
            x <<- y
            inver <<- matrix(NA,nrow=nrow(x),ncol=ncol(x))
        }
    
    # Method to get the matrix    
        get <- function() x

    # Method to set the inverse of the matrix  
    
    setinverse <- function(inverse) inver <<- inverse
        
    # Method to get the inverse of the matrix  
    
    getinverse <- function() inver
    
    # List of all methods
    
    list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    
}

## This function computes the inverse of the special matrix returned by the "makeCacheMatrix" function. 
## - If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinverse()

    #  Checking if the inverse already exists in the Cache
        if(!is.na(inverse[1,1])) {
            message("getting cached data")
            return(inverse)
        }
        
    #  If the Inverse hasn't been calculated till now
    
    ## getting the matrix from our object
    
        data <- x$get()

    #  Computing a Identity matrix of the same size to pass to solve function
        y<-matrix(1,nrow=nrow(data),ncol=ncol(data))
        inverse <- solve(data,y, ...)

    #  The value of inverse is passed to the Cache 
        x$setinverse(inverse)
    
    #  The function returns the inverse   
        inverse
}
