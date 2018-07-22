## Following two functions are written as partial fullfilment for Coursera Data Science: R Programming course.
## Github User : Chimpa
## Name: Chinthaka Senaratne

## Following functions will solve the inverse of an input matrix. To speed up the 
## process already calculated values are being cached in the environment.

## makeCacheMatrix() function creates a special matrix object to store the cached inverse
## value of an invertible input matrix and returns a list of four functions to get and set 
## the invertible matrix and inverse of the same matrix
makeCacheMatrix <- function(x = matrix()) {
        inverseMat <- NULL ## set the cached inverse matrix value to be null for input x
        
        set <- function(y) {
                        x <<- y
                        inverseMat <<- NULL
        }
        
        get <- function() x  ## get the invertible input matrix
        setInverse <- function(inverse) inverseMat <<- inverse ## set inverse matrix value in parent environment
        getInverse <- function() inverseMat  ## get the inverse matrix value in the parent environement
        
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve() function computes the inverse of the input invertible matrix used for makeCacheMatrix()
## function. If the inverse is already been calculated and cached (and the matrix has not changed),
## then the cached value should be returned without calculating the inverse again.
cacheSolve <- function(x, ...) {
        inverseMat <- x$getInverse()
        
        ## if the inverse is already calculated and cached in variable inverseMat
        if(!is.null(inverseMat)) {
                message("getting cached data")
                return(inverseMat)
        }
        
        data <- x$get()                 ## get the matrix value stored using function makeCacheMatrix
        inverseMat <- solve(data, ...)  ## calculate the inverse of the matrix
        x$setInverse(inverseMat)        ## set the inverse matrix to be used as cache
        inverseMat
}

