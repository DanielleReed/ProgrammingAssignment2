#two functions check if a precomputed matrix inverse is available to avoid expensive computation
#code generated using (a) coursework, (b)tips for struggling students and (c) hints from my daugther
#input 'x' is a matrix to be inverted, e.g., numbers 1, 2, 3, 4 in a two-by-two square
#the first function creates a list of four functions 
#the set, get, set inverse and get inverse are steps needed to check if the inverse is already available
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#the second function checks to see if there is a cashed solution
#if there is a solution (i.e., the get$inverse is not null), it gets it
#if there is no cache solution, the function 'solve' computes it
#the last 'i' at the end shows the matrix inverse 
cacheSolve <- function(x) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}