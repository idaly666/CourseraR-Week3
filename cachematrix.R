## Coursera R / Week 3 Programming Assignment 2
## Caching the Inverse of a Matrix
# These functions cache the inverse of a matrix to reduce cost of computation

## 1. makeCacheMatrix: This function creates a special matrix 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        #inv_function is the value of inverse in this function
        inv_function <- NULL
        
        # 1. set value of the matrix
        set <- function(y) {
                x <<- y
                inv_function <<- NULL
        }
        
        # 2. get the value of the matrix
        get <- function() x
        
        # 3. set the value of the inverse of matrix
        set_inverse <- function(inverse) inv_function <<- inverse
        
        # 4. get the value of the inverse of matrix
        get_inverse <- function() inv_function
        list (set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
        

}


## 2. cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        ## set inv_function
        inv_function <- x$get_inverse()
        
        ## If the inverse has already been calculated (and the matrix has not changed), 
        ##then cacheSolve should retrieve the inverse from the cache.
        
        if(!is.null(inv_function)){
            message("getting cached data")
            return(inv_function)
        }
        
        mat <- x$get()
        ## returns inverse
        inv_function <- solve(mat, ...)
        x$set_inverse(inv_function)
        inv_function
}
