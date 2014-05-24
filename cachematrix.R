## These 2 functions are used to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        cachedMatrix <- NULL
        cachedInverse <- NULL
        
        set <- function(y){
                x <<- y
                cachedInverse <<- NULL
        }
        
        get <- function() x
        
        caluInverse <- function(){
                cachedMatrix <<- x
                cachedInverse <<- solve(x)
        }
        
        getInverse <- function() cachedInverse
        
        changed <- function(){
                if(x == cachedMatrix)  FALSE 
                else TRUE
        }
        
        list(set = set, get = get, 
             caluInverse = caluInverse,
             getInverse = getInverse, 
             changed = changed)
}


## This function computes the inverse of the special "matrix" returned by 
##  above function. If the inverse has already been calculated (and 
## the matrix has not changed), then this function should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse) && !x$changed()){
                message("getting cached Inverse")
                return(inverse)
        }
        else{
                x$caluInverse()
                inverse <- x$getInverse()
        }
        
        inverse
}
