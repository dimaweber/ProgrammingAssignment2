## Cached matrix 'class'. Allow to cache inverse calculation
## also DO NOT recalculate inverse if 'object' assigned with new data identical to already present in 'object'


## 'Object' constructor -- return 'object' with 4 methods initialiazed with given values.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # this field will contain matrix inverse. lazy computated
    set <- function(mtrx) # set new value to matrix. Reset inverse cache if new data not identical to existing.
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
    get <- function() # return matrix data 
    {
      x
    }
    setInverse <- function(i) # set inverse cache value
    {
      inv <<- i
    }
    getInverse <- function() # return inverse for matrix
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
        # need to recalculate inverse and save it to cache
        print("recalc inverse")
        x$setInverse( solve(x$get()))
    }
    x$getInverse() # return inverse (eith recalculated or cached)
}
