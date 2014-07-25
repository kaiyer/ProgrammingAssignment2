## Put comments here that give an overall description of what your
## functions do

## This function implements a "special" matrix  that has helper functions 
## to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of a matrix. It first checks
## the enclosing environments till the top environment and returns the 
## cached inverse matrix, If it does not find it, it computes the inverse
## matrix and saves it 
## in the top enclosing environment if it cannot find an already existng
## matrix by that name

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

matequal <- function(x,y)
    is.matrix(x) && is.matrix(y) && dim(x)==dim(y) && all(x==y)

test <- function() {
  myMatrix <- matrix(c(3,8,5,6,9,3,1,2,8), nrow=3)
  myx <- makeCacheMatrix(myMatrix)
  myInv1 <- cacheSolve(myx)
  # now call the inverse again and you will see the cache message
  myInv2 <- cacheSolve(myx)
  # check if the two matrices are equal
  if (matequal(myInv1, myInv2)==TRUE) 
    message("cached and uncached inverse martices equal!")
}
