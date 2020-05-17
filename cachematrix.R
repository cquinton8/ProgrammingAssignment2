## Put comments here that give an overall description of what your
## functions do

## Input a matrix x, with the aim to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        # get & set for matrix inverse
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        
        #returns list
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', assuming it is invertible
        
        inv <- x$getInverse()
        
        #return if already computed
        if(!is.null(inv)){
                message("Cache data:")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv          
}
