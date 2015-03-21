## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(mtrx)
    {
      if (!identical(mtrx, x))
      {
        print("set new matrix values and reset cache")
        x <<- mtrx
        inv <<- NULL
      }
      else
        print("set identical matrix -- cache won't be reseted")
    }
    get <- function()
    {
      x
    }
    setInverse <- function(i)
    {
      inv <<- i
    }
    getInverse <- function()
    {
      inv
    }
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    if (is.null(x$getInverse()))
    {
        print("recalc inverse")
        x$setInverse( solve(x$get()))
    }
    x$getInverse()
}
