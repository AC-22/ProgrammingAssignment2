## Write a short comment describing this function
## makeCacheMatrix creates a list which contains 
## following functions
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	 matInv <- NULL
	set <- function(y) {
                x <<- y
                matInv <<- NULL
	  }
	get <- function() x
	setInvMat <- function(inverse) matInv <<- inverse
      getInvMat <- function() matInv
        list(set = set, get = get,
             setInvMat = setInvMat,
             getInvMat = getInvMat)
}


## The following function returns the inverse of matrix
## However, it first checks to see if the inverse has 
## already been calculated.If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the matrix and sets the  
## value of the inverse of matrix in the cache via the 
## setInvMat function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	matInv <- x$getInvMat()
	if(!is.null(matInv)) {
                message("getting cached data")
                return(matInv)
        }
        data <- x$get()
	  ## solve is the function for finding matrix inverse
        matInv <- solve(data)
        x$setInvMat(matInv)
        matInv

}
