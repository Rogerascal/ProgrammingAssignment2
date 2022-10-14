## Put comments here that give an overall description of what your
## functions do
## Coursera Data Science: R Programming Week 3 Assignment;GitHub user: Rogerascal
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## creates a "matrix" object that caches its inverse

 makeCacheMatrix <- function(x = matrix()){
 
     inv <- NULL                             
     ## inv initialized as NULL
     
     set <- function(y) {  
          x <<- y  
     ## define the set function to assign new value of matrix in parent environment
                                 
         inv <<- NULL                        
         ## inv is reset to NULL
     }
    
    get <- function() x                    
     ## get function returns value of the matrix argument

     setinverse <- function(inverse) inv <<- inverse 
     ## assigns value of inv in parent environment
     
     getinverse <- function() inv                     
     
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
 }
}


## Write a short comment describing this function 
## This function computes the inverse of the "matrix" returned by the makeCacheMatrix 
## If the inverse has already been calculated, cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
     if(!is.null(inv)) {
         message("getting cached data")
         return(inv)
     }
     matrix_to_invert <- x$get()
     inv <- solve(matrix_to_invert, ...)
     x$setinverse(inv)
     inv
 }
}
