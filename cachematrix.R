## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default "matrix"
    inv <- NULL                             ## initialize inv to NULL which holds matrix inverse 
    set <- function(y) {                    ## set function to assign new matrix values 
        x <<- y                             ## value of matrix in parent environment
        inv <<- NULL                        ## reset inv to NULL for new matrix
    }
    get <- function() x                     ## get value of the matrix members
    
    setinverse <- function(inverse) inv <<- inverse  ## assign inverse value in parent environment
    getinverse <- function() inv                     ## get the value of inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## to the functions calls with the $ operator
}


## Write a short comment describing this function
## This function computes the inverse of the matrix created by makeCacheMatrix
## If the inverse value is not null (and the matrix has not changed),
## then cacheSolve retrieves inverse from cache memory
      
cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) { # If inv value is not null
        message("getting cached data")
        return(inv)
    }
    data <- x$get()# load data with get function above
    inv <- solve(data, ...)#Compute the inverse of the matrix
    x$setinverse(inv)# set inverse value to inv variable from above
    inv # return inv value
}

Running the functions:
>mat$set(matrix(c(4,2,7,6), 2, 2)
>mat$get()
     [,1] [,2]
[1,]    4    7
[2,]    2    6
> cacheSolve(l)
     [,1] [,2]
[1,]  0.6 -0.7
[2,] -0.2  0.4
> mat$getinverse()
     [,1] [,2]
[1,]  0.6 -0.7
[2,] -0.2  0.4
> 
> cacheSolve(l)
getting cached data
     [,1] [,2]
[1,]  0.6 -0.7
[2,] -0.2  0.4


