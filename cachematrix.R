## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Create a special matrix that can cache the inverse
makeCacheMatrix <- function(x = matrix()) { ##Set the null value and initialize matrix
  a <- NULL ## initialize  a 
        set <- function(y) { ## set the value of the matrix
                x <<- y
                a <<- NULL 
        }
        get <- function() x ## get the value of the vector
        setInverse <- function(inverse) a <<- inverse ## set the value of the inverse operation
        getInverse <- function() a ## get the value of the inverse operation
        list(set = set, get  = get,
             setInverse = setInverse,
             getInverse = getInverse) ##return as a list the inverse values
}


## Write a short comment describing this function
## cacheSolve will return the inverse of the matrix found above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a <- x$getInverse() ##get the inverse in the above
        if(!is.null(a)) { ## if something has been calculated above, state that it is getting cached data
                message("getting cached data")
                return(a) 
        }
        data <- x$get() 
        a <- solve(data, ...) ## solve function returns the inverse
        x$setInverse(a)
        a ## displays the inverse of the above matrix
}
