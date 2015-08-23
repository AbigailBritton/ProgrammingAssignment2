## These functions check is the inverse of a matrix has alreday been claculated and cached.
## If so, it returns the cached inverse. If not it calculates it. 


makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
        set <- function(y) {
                ## causes a search in the parent envirnoment for an
		    ## existing definition of the variable being assigned
		    x <<- y
                i <<- NULL
        }
get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



cacheSolve <- function(x, ...) {

        i <- x$getinverse()
        ## If i is not null it has already been calculated, return the cached inverse
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ##Otherwise, calculate the inverse
        data <- x$get()
        ## 'solve' is the matrix inversion function 
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
 
## I used the website https://github.com/sefakilic/coursera-rprog-assignment2/blob/master/cachematrix.R for help
