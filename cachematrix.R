## The following pair of functions cache the inverse of a square matrix.
## Assignment completed by Alexis Sykut for R Programming Coursera course

## Sample Code for Testing:
## create a random uniform 2 column, 2 row matrix
## > mymatrix <- matrix(runif(4),2,2)
## > mymatrix
## [,1]      [,2]
## [1,] 0.1759625 0.1914676
## [2,] 0.2354451 0.3335228

## Calculate the inverse of the matrix to identify expected output
## > inversemymatrix <- solve(mymatrix)
## > inversemymatrix
## [,1]      [,2]
## [1,]  24.51044 -14.07087
## [2,] -17.30276  12.93140

## results should match
## > mycachematrix <- makeCacheMatrix(mymatrix)

## > cacheinversemymatrix <- cacheSolve(mycachematrix)
## > cacheinversemymatrix
## [,1]      [,2]
## [1,]  24.51044 -14.07087
## [2,] -17.30276  12.93140

## makeCacheMatrix --This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## set the value of the matrix
        
        set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                
        ##get the value of the matrix
        get <- function() x
                
        
        ##set the value of the inverse (solve())        
        setinverse <- function(solve) m <<- solve
        
        ##get the value of the inverse (solve())        
        getinverse <- function() m
        
        list(set = set, get = get,
        setinverse = setinverse,
                     getinverse = getinverse)
        }


## cacheSolve --This function checks to see if the data exists in the Cache. If it does
## it returns the cached value, otherwise it calculates the solution from the main dataset

cacheSolve <- function(x) {
        
        ##call the inverse of the data from the cache
        m <- x$getinverse()
        
        ##if m returns data then return the inverse data from the cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ##if not cached, get the original matrix data and inverse solve for m
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}


