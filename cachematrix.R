# makeCacheMatrix creates a special "matrix" object that can cache its inverse

# The object does not calculate the inverse, it just saves it inside

# Saves the matrix to variable x and its inverse to variable s in scope

# Returns object, which is a list, and contains the following functions:

# set: sets matrix and resets cached inverse
# get: returns matrix
# setSolve: saves solve value
# getSolve: returns cached inverse value

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() {
                x
        }
        setSolve <- function(solve) {
                s <<- solve
        }
        getSolve <- function() {
                s
        }
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

# cashSolve function is to get the inversed matrix from a special object created by makeCacheMatrix.

# It takes the object of that type as an argument 'x'

# It checks if the inverse value is already cached

# If it is chached, it returns the cached value

# If it's not, it calculates the inverse for the matrix saved in the 'x'

# Then it saves it into 'x' cache using function 'setSolve'

# And returns the result

cacheSolve <- function(x, ...) {
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}

