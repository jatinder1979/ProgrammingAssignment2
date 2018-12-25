# The objective is to write a pair of functions that cache the inverse of a matrix
# There are two functions:
# makeCacheMatrix: This function creates a special matrix that can cache its inverse
#cacheSolve: This function computes the inverse of the special matrix returned by above function.
#If the inverse has already been calculated (and the matrix is not changed) then this function should retrieve the inverse from cache.

makeCacheMatrix <- function(x=matrix())
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse)  inv<<- inverse
  getinv <- function()inv
  list (set=set, get=get, 
        setinv= setinv,
        getinv = getinv)
}

cacheSolve <- function(x,...){
  inv <- x$getinv()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  
  inv <- solve(data,...)
  x$setinv(inv)
  inv
  
}

#Testing of functions
#> m <- matrix(c(1,2,3,4),2,2)
#> m1 <- makeCacheMatrix(m)
#> cacheSolve(m1)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(m1)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(m1)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
