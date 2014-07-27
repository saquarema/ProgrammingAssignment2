## This function returns a list with two functions that are used to create
## a special object that stores a matrix and cache's its inverse

makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL
        set <- function(y) {
                x <<- y
                inversematrix <<- NULL
        }
        get <- function() x
        setinversematrix <- function(solvematrix) inversematrix <<- solvematrix
        getinversematrix <- function() inversematrix
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)        
}


## Function to get an inverse matrix. If the original matrix is the same
## and the inverse already exists, the function can use the matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Checks if the inverse matrix exists
        im <- x$getinversematrix()
        if(!is.null(im)) {
                message("getting the inverse matrix from the cached data")
                return(im)
        }
        ## Calculates the inverse matrix and sends it to the cache 
        data <- x$get()
        im <- solve(data)
        x$setinversematrix(im)
        im
}
