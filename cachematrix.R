## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
