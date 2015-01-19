##Two function to calculate inverse of matrix and store in cache.

## 1. makeCacheMatrix -> creates a special matrix that will cache the inverse of original matrix. Specifically it will:
#1. set the values of the matrix (set)
#2. get the values of the matrix (get)
#3. set the value of the inverse of the matrix (setinv)
#4. get the value of the inverse of the matrix (getinv)

makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL #set inverse as null, not present
  set <- function(y) {
    x <<- y #set matrix values into x
    inv <<- NULL
  }
  get <- function() x #get matrix values
  setinv <- function(inverse) inv <<- inverse #determine inverse
  getinv <- function() inv #recover inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)   
      #above linereturns 'special matrix' with new functions.
}

## 2.cacheSolve -> Returns (and calculates, if necessary) the inverse of the matrix. If the inverse had already been calculated, then the inverse is retrieved from cache. If not, the inverse is calculated and placed into the cache.

cacheSolve <- function(x, ...) {
 inv  <- x$getinv()
  if(!is.null(inv)){ 
    message("getting cached data") #recover from cache, if present
    return(inv) #return cached inverse matrix
  }
  
  data  <- x$get() #if no inverse, will retrieve original matrix
  inv <- solve(data,...) #calculate inverse matrix
  x$setinv(inv) #set inverse into cache in main env
  inv #return inverse
}

#testmatrix
#matrix = makeCacheMatrix(matrix(c(3,12,31,40), nrow=2, ncol=2))
#matrix$get()
#cacheSolve(matrix)
#matrix$getinv()
#cacheSolve(matrix)
#matrix$set(matrix(rnorm(9), nrow=3, ncol=3)) # change to new matrix
#matrix$get()
#cacheSolve(matrix)
#matrix$getinv()
#cacheSolve(matrix)
