makeCacheMatrix <- function(x = matrix()) {
        # x: a square invertible matrix
        inv = NULL
        set = function(y) {
                # set the matrix
                x <<- y
                inv <<- NULL
        }
        #  get the matrix
        get = function() x
        # set the inverse
        setinv = function(inverse) inv <<- inverse 
        # get the inverse
        getinv = function() inv
        
        # return: a list containing functions to this list is used as the input to cacheSolve()
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## x: output of makeCacheMatrix()
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
               
        #return: inverse of the original matrix input to makeCacheMatrix()
        return(inv)
}
