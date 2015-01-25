## Put comments here that give an overall description of what your
## functions do

##For a matrix, the first function makes a cache which can store the value of the matrix and its inverse.
##The second matrix calculates the inverse of the matrix and stores the result in the cache.


## Write a short comment describing this function

## This function creates a list containing four functions to (i) set the value of the matrix,
## (ii) to get the value of the matrix, (iii) to calculate the inverse of the matrix,
## and to (iv) get the value of the matrix. When entering a new matrix in the function 
## (for which the inverse has not been calculated yet), the matrix is stored, 
## there is no value (yet) for the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
      m <- NULL
  
      set <- function(y) {
    
          x <<- y
          m <<- NULL
      
      }

      get <- function() x

      setinverse <- function(solve) m <<- solve
    
      getinverse <- function() m
    
      list(set = set, get = get,    ## a list of functions
           setinverse = setinverse, 
           getinverse = getinverse)

}


## Write a short comment describing this function

##This function does calculation of the inverse of the matrix. 
## It first checks if the inverse has already been calculated. If this is the case, it gets the result
## (inverted matrix) from the cache and skips calculating the inverse.

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
       
      ## checking if the inverse matrix can be found in the cache
  
      m <- x$getinverse()
      
      if(!is.null(m)) {
        
            message("getting cached data")
            return(m)
      }

      matrix <- x$get()  ### gets the matrix from object x
      
      m <- solve(matrix, ...) ## calculates the inverse matrix
      
      x$setinverse(m) ## assigns the calculated inverse matrix to x
      
      m
}
