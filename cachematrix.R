makeCacheMatrix <- function(x = matrix()) {
## The first function, makeCacheMatrix, is a list containing a function to set a matrix, get a matrix, set the inverse of the matrix, and get the inverse of the matrix. This list will be used as the input for the next function, which is cacheSolve. 
makeCacheMatrix <- function(x = matrix()) {
inv = NULL
set = function(y) {
x <<- y
inv <<- NULL
}
get = function() x
setinv = function(inverse) inv <<- inverse 
getinv = function() inv
list(set=set, get=get, setinv=setinv, getinv=getinv)
}
## The second function computes the inverse of the matrix returned by the makeCacheMatrix function. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
inv <- x$getinverse()
if(!is.null(inv)) {
message("getting cached data")
return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$setinverse(inv)
inv
}

