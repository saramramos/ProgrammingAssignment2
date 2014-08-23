##function that creates a special "matrix" object that can caches its inverse

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL
        set <- function(y) { ##set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x ##get the value of the matrix
        setinverse <- function(inverse) m <<- inverse ##set the value of the inverse
        getinverse <- function() m ##get the value of the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##function that computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) { ##checks to see if the inverse matrix has already been calculated, if so, it gets the inverse matrix from the cache and skips computation
                message("getting cached data")
                return(m)
        }
        data <- x$get() ##get the matrix from the cache
        m <- solve(data) ##solve function gets the inverse matrix
        x$setinverse(m) ##sets the inverse matrix in the cache via the setinverse function
        m ##return the inverse matrix
}
