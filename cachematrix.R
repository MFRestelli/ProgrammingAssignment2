## The following functions creates an special object 
## that stores a numeric matrix and cache`s its inverse.

## This function creates and stores the special object 
## that caches the inverse of a given matrix 
## (but does not calculates it)
## This special object is a list containing the functions:
##set, asigns matrix values
##get, returns the matrix values 
##setinv, caches the matrix inverse
##getinv, returns the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
## Initialize the variable "inv" where the inverse will be store
        
      set <- function(y) {
              x <<- y
              inv <<- NULL
## When the matrix values are changed,the cache will delete itself
      
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function returnes the inverse of a matrix 
## returned by the function "makeCacheMatrix" 


cacheSolve <- function(x, ...) {
    inv <- x$getinv() 
## If the inverse it has been stored in the caché,
## then the cacheSolve will retrieve it 
   
    if(!is.null(inv)) { 
        message("getting cached data")
    return(inv)
    }
## If not, cacheSolve will calculate it and store it

    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
## Returns the inverse

}
