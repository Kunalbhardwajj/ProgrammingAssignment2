## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function which creates a special "matrix" object that can cache its inverse for the input
## (which is an invertible square matrix).

## cacheSolve is a function which computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),then the cachesolve will retrieve 
## the inverse from the cache.

## Write a short comment describing this function
## makeCacheMatrix is a function that performs the following operations:
## set the values of elements of a matrix
## get the values of elements of a matrix
## set the values of elements of the inverse of a matrix 
## get the values of elements of the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
  
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function above. 
## If the inverse has already been calculated (and the matrix has not changed),then the cachesolve will retrieve 
## the inverse from the cache.



cacheSolve <- function(x, ...) 
{
  inv <- x$getInverse()
  if(!is.null(inv)) 
  {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}




