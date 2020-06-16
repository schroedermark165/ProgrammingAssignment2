# The first function caches the matrix the first time it is received.
# If the matrix needs to be called again, in our case for the inverse,
# it can be retrieved from the cache which is much faster.

# Creates a matrix that is cached
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      
      set <- function(y){
            x <<- y
            i <<- NULL
      }
      
      get <- function(){
            x
      }
      
      set_inverse <- function(inverse){
            i <<- inverse
      }
      
      get_inverse <- function(){
            i
      }
      
      list(set = set,
           get = get,
           setinverse = set_inverse,
           getinverse = get_inverse)
}


# This function computes the inverse of the matrix by calling "makeCacheMatrix" above.
# If the matrix is already inversed and cached, it does not need to inverse it again.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      
      if(!is.null(i)){
            message("Data is cached: ")
            return(i)
      }
      
      matr <- x$get()
      i <- solve(matr, ...)
      x$setinverse(i)
      i
}