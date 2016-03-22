makeCacheMatrix <- function(x = matrix()) {
inv = NULL
set = function(y) {
x <<- y
inv <<- NULL
}
get = function() x
setinv = function(inverse) inv <<- inverse 
getinv = function() inv
}

cachesolve <- function(x, ...) {
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

