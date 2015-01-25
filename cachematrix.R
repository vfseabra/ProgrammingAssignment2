## makeCacheMatrix description
## creates "matrix"
## set the value of the matrix
## get the value of the matrix
## set the value of the imatrix
## get the value of the imatrix
## example: 
## B<-matrix(c(4, 2, 7, 6), 2, 2)
## BC<-makeCacheMatrix(B)

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
        set <- function(y) {  			 	
                x <<- y
                m <<- NULL
}
        get <- function() x					
        setimatrix <- function(solve) m <<- solve	
        getimatrix <- function() m				
        list(set = set, get = get,
             setimatrix = setimatrix,
             getimatrix = getimatrix)
}



## cacheSolveMatrix description
## Return a matrix that is the inverse of 'x'
## it first checks to see if the mean has already been calculated. 
## If so, it gets the invert matrix from the cache and skips the computation. 
## If not, it calculates the invert matrix from the matrix data the cache via the solve function.
## example: 
## cacheSolveMatrix(BC)

cacheSolveMatrix <- function(x, ...) {
        m <- x$getimatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setimatrix(m)
        m
}
