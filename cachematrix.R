## For overall of the function can be summarized as the checking cache inverse matrix before the computation 
## makeCacheMatrix function provides the checking cache method of the inverse matrix before computation

makeCacheMatrix <- function(x = matrix()) 
{
  im <- NULL
  set <- function(y) 
  {
    x <<- y
    im <<- NULL
  }
  get <- function() 
  {
    x
  }
  setInverse <- function(inverse) 
  {
    im <<- inverse
  }
  getInverse <- function() 
  {
    im
  }
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## cacheSolve function provides the inverse matrix by checking the cache of the input first and then compute the inverse matrix return to the user

cacheSolve <- function(x, ...)
{
  im <- x$getInverse()
  if(!is.null(im)) 
  {
    message("getting cached of inverse matrix")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInverse(im)
  im
  ## Return a matrix that is the inverse of 'x'
}
