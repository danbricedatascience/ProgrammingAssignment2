## Factory function managing cache matrix in its scope
## and building setters and getters functions
##
##  Parameters:
##    - mtx : initial matrix to invert
##
##  Variables inside:
##    - new_mtx : new matrix to invert 
##    - cached_invert_matrix : cache storing the inverted matrix
##
##  Return:
##    - list of functions, so enable calling them
##      and access the cached variable (cached_invert_mtx)

makeCacheMatrix <- function(mtx = matrix()) {
  cached_invert_mtx <- NULL
  set <- function(new_mtx) {
    mtx <<- new_mtx
    cached_invert_mtx <<- NULL
  }
  get <- function() mtx
  setinvert <- function(invert_mtx) cached_invert_mtx <<- invert_mtx
  getinvert <- function() cached_invert_mtx
  list(set = set, 
       get=get, 
       setinvert=setinvert, 
       getinvert=getinvert)
}


## Function calculating the inverse of a matrix
## If the origin matrix has not been changed, the cached
## inverted matrix is returned. Otherwise a 
## new inverted matrix is calculated and set in the cache
## for later use
##
##  Parameters:
##    - mtx_cache : matrix cache
##
##  Variables inside:
##    - invert : inverted matrix (set to the cache)
##    - data : matrix to invert (get from the cache) 
##
##  Return:
##    - inverted matrix


cacheSolve <- function(mtx_cache, ...) {
  ## Return a matrix that is the inverse of 'mtx'
  invert <- mtx_cache$getinvert()
  if (!is.null(invert)){
    message("getting cached data")
    return(invert)
  }
  data <- mtx_cache$get()
  invert <- solve(data, ...)
  mtx_cache$setinvert(invert)
  invert
}
