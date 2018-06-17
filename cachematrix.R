## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
         inverse <- NULL                                  
                                                         
        
        set <- function(y) {                              
                x <<- y                                   
                inverse <<- NULL                          
        }
        get <- function() x                               
        setinverse <- function(solve) inverse <<- solve  
        getinverse <- function() inverse                  
        list(set = set, get = get,                        
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

# The  function calculates the inverse of the matrix created with the above function (makeCacheMatrix). It first checks to see 
#if the inverse of the matrix has already been calculated. If so, it gets the inverse of the matrix from the cache and skips the 
#computation.  Otherwise, it calculates the inverse of the matrix of the data and sets the value 
# of the inverse of the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          
        inverse <- x$getinverse()                    
        if(!is.null(inverse)) {                       
                message("getting cached data")        
                return(inverse)                       
        }
        
        data <- x$get()                                    
        inverse <- solve(data, ...)                  
        x$setinverse(inverse)                         
        inverse  
}
