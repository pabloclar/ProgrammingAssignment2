## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# as in the vector example first function makeCache... creates a list with the arguments 
# that will be used by the second Cache...
# argument for first function must be the matrix to be inverted


makeCacheMatrix <- function(x = matrix()){
        mat_inv <- NULL
        set <- function(y) {
                x <<- y
                mat_inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) mat_inv <<- solve
        getinv <- function() mat_inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}
# cacheInv will receive as argument the result of makeCache
# function will first test whether there is already a value calculated and if yes, will get 
# that value without recalculating
cacheSolve <- function(x, ...){
        mat_inv <- x$getinv()
        if (!is.null(mat_inv)){
                message("getting matrix cached data")
                return(mat_inv)
        }
        matriz <- x$get()
        mat_inv <- solve(matriz, ...)
        x$setinv(mat_inv)
        return(mat_inv)
}