## Functions for caching inverse matrix. First of all, create cacheMatrix with function 
## matr <- makeCacheMatrix(m)
## Then, use function cacheSolve(matr) for getting inverse matrix.

## For example, make matrix 2 by 2 with values 4,3,3,2 and cache it:
## matr<-makeCacheMatrix(matrix(c(4, 3, 3, 2), nrow=2, ncol=2))
## Then, use cacheSolve for getting inverse matrix
## cacheSolve(matr)

## This function is for caching purposes. The cached object is stored in m.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y) {
        x <<- y
        m <<- NULL
    }
    get<-function() x
    setMatrix <- function(matr) m <<- matr
    getMatrix <- function() m
    list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)

}


## This function is proxy to standard solve() function, but with check in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getMatrix()
        if(!is.null(m)) {
            message("getting cached matrix")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}
