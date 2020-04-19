## Our goal is to compute the inverse of an invertible matrix
## in such a way that if this calculation has already been done
## then it retrives the information without computing it once
## again. 

## This function creates a list of functions:
## set: sets the matrix that was passed as an argument
## get: gets the matrix that was captured
## setinv: saves the inverted matrix
## getinv: get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## This function takes as argument the list of functions that
## were created with "makeCacheMatrix". If the inverse function
## has been computed then it retuns that matrix. If not, it
## calculates its inverse. Then it sets that
## as the "new matrix" for the inverse and prints it out. That way when
## we call it again, it will not compute the inverse
## again. It will print the message "getting cached data" 
## and retrieve the value stored in the variable inv.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}