## makeCacheMatrix() returns a "matrix" object with the below function list.
##      These functions return empty values.
## makeCacheMatrix(x) returns a special "matrix" object which contains a list
##      of data manipulation functions (see below). These functions are used by
##      cacheSolve to access and modify the matrix data inverse.
## Returns list of the following functions: 
##      $set(y): sets the internal data variable (note the internal data can also
##              be set by calling makeCacheMatrix(x))
##      $get(): returns the internal matrix data
##      $setSolve(solution): sets the cache for the solution (note: no computation 
##              occurs inside of makeCacheMatrix)
##      $getSolve(): returns the cached solution value (returns NULL if 
##              setSolve has not been called for this object instance yet)

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL               ##set inverse of a matrix to null
  
  ## set matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## get matrix
  get <- function() x           
  ## setting inverse matrix
  setimatrix_1 <- function(imatrix_1) 
    i <<- imatrix_1
  ## getting inverse matrix
  getimatrix_1 <- function() i                 
  
  ## list names
  list(set = set, get = get,
       setimatrix_1 = setimatrix_1,
       getimatrix_1 = getimatrix_1)

}


## cacheSolve(x), where x is the returned function list from makeCacheMatrix.
## Returns: inverse of "matrix" x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
i <- x$getimatrix_1() ## gets the invmat in previous function
  
  ## if inverse matrix is already stored, get cached data and return the inv matrix
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if not, calculate inverse of the matrix 
  ## get the matrix
  data <- x$get()       
  
  ## calculate the inverse
  i <- solve(data, ...)  
  
  ## set the inverse of the matrix
  x$setimatrix_1(i)     
  
  ## print the inverse of the matrix
  i          
        
}
