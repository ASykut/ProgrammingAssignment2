## The following pair of functions cache the inverse of a square matrix.
## Assignment completed by Alexis Sykut for R Programming Coursera course

## Sample Code for Testing:
## create a random uniform 2 column, 2 row matrix
## > mymatrix <- matrix(runif(2),2,2)

## Calculate the inverse of the matrix to identify expected output
## > inversemymatrix <- solve(mymatrix)
## > inversemymatrix
##           [,1]      [,2]
## [1,]  3.622541 -3.097338
## [2,] -2.170542  3.155007

## results should match
## > mycachematrix <- makeCacheMatrix(mymatrix)

## > cacheinversemymatrix <- cacheSolve(mycachematrix)
## > cacheinversemymatrix
## [,1]      [,2]
## [1,]  3.622541 -3.097338
## [2,] -2.170542  3.155007

## makeCacheMatrix --This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) m <<- solve
                getinverse <- function() m
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }


## cacheSolve --This function checks to see if the data exists in the Cache. If it does
## it returns the cached value, otherwise it calculates the solution from the main dataset

cacheSolve <- function(x) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

