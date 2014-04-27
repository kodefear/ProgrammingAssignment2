## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                 #m represents the inverse value
  set <- function(y) {      #set function  sets input matrix to y and m to null
    x <<- y
    m <<- NULL
  }
  get <- function() x       #get functions gives us current x
  setInverse <- function(inverse) m <<- inverse   #function used to inverse
  getInverse <- function() m                      #function used to get
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse) #returns a list
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()               #query the x vector's cache  
  if(!is.null(m)) {                 #if there is a cache
    message("getting cached data")
    return(m)                       #just return the cache, no computation needed
  }
  data <- x$get()                    #if there's no cache
  m <- solve(data, ...)              #we actually compute them here
  x$setInverse(m)                    #save the result back to x's cache
  m                                   #return the result
 
  
  
}
