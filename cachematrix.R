## Programming Assignment 2: Lexical Scoping

## The following two functions, when combined, provide an example of caching 
## a previously calculated object (in this case, the inverse of a matrix) 
## to speed the runtime of an R program. When run for the first time, the program
## actually calculates the inverse matrix; subsequent runs simply fetch the 
## desired result from memory cache. To test this, uncomment the six last lines 
## of this script or type them sequentially in the console.


## This function creates a special "matrix" object that can cache its inverse.
## Default choice is a 5x5 square matrix. Variable "dimension" sets 
## the number of rows & columns of the square matrix to be inverted; must
## be set before calling makeCacheMatrix (see example at the bottom of this
## script.)

makeCacheMatrix <- function(x = matrix(nrow = dimension, ncol = dimension)) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse_matrix <- function(cacheSolve) m <<- cacheSolve
        get_inverse_matrix <- function() m
        list(set = set, get = get,
             set_inverse_matrix = set_inverse_matrix,
             get_inverse_matrix = get_inverse_matrix)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache. Appropiate messages are sent to
## the console in either case.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse_matrix()
        if(!is.null(m)) {
                message("*** already calculated, getting cached data ***")
                return(m)
        }
        data <- x$get()
        data
        m <- solve(data)
        x$set_inverse_matrix(m)
        message("*** first calculation, data not cached ***")
        m
}


## To test these functions, uncomment the lines below. Variable "dimension" sets 
## the number of rows & columns of the square matrix to be inverted.

## dimension <- 9
## z <- makeCacheMatrix()
## inverse_matrix <-cacheSolve(z)
## inverse_matrix
## inverse_matrix <-cacheSolve(z)
## inverse_matrix
