## Put comments here that give an overall description of what your
## functions do
## Create R code to cache an invertible matrix and then compute
## its inverse and retrive it

## Write a short comment describing this function
## Cache a square matrix 

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
## Make sure the matrix is invertible i.e. its determinant is non-zero 

det()

## Write a short comment describing this function
## Create a function called cacheSolve to compute inverse of the "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

____________________________________________checking the function____________________

> m <- matrix(rnorm(9),3,3)
> m
           [,1]       [,2]        [,3]
[1,] -0.2309299 -1.4745237 -0.47116995
[2,]  1.0356485  0.3909963 -0.07923172
[3,]  0.6315022 -0.1398515  1.38197251
> det(m)
[1] 2.24653
> m1 <- makeCacheMatrix(m)
> m2 <- cacheSolve(m1)
> m2
           [,1]         [,2]       [,3]
[1,]  0.2355924  0.936397325  0.1340088
[2,] -0.6593602 -0.009612117 -0.2253535
[3,] -0.1743809 -0.428866182  0.6395620
> 
