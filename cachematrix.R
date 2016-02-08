## Create a matrix representation in a list with 4 functions:
## get(): return the matrix
## set(): set a new matrix
## setinv(matrix()): set the matrix inverse (it's for internal use)
## getinv(): return matrix inverse if it was already calculated, else return NULL 
makeCacheMatrix <- function(x = matrix()) {
  ## set inv as null ( we're creating a new matrix)
  inv <- NULL
  
  ##reset inv value if we change matrix values
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## return the matrix 
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  ##return the inverse
  getinv <- function() inv
  
  ##return a list with all operations
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Calculate inverse of a matrix X
## if inverse's been calculated, function takes O(1) to solve  
cacheSolve <- function(x, ...) {
  ## get the actual value for x inverse
  inv<- x$getinv()
  ## it was calculated, return it
   if(!is.null(inv)){
     return(inv)
   }
  
  ##calculate inverse 
   data <- x$get()
   inv <- solve(data,...)
   
  ##set inverse for future use 
   x$setinv(inv)
   inv
}
