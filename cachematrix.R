## The overall idea of this project is to find the inverse of a matrix
## which will be cached so if we need it again then no computation
## will be done

## This function will create a matrix that will cached it's inverse in
## cacheSolve function
library(MASS)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function(){x}
    setInverse <- function(inverse){
        inv <<- inverse
    }
    getInverse <- function(){
            inver <- ginv(x)
            inver%*%x
    }
    list(set = set,get = get,setInverse = setInverse, getInverse = getInverse)
}


## This function will calculate the inverse of the matrix and make it cached
## if it is already cached then it will return the matrix

cacheSolve <- function(x, ...) {
       inv <- x$getInverse()
       if(!is.null(inv)){
            message("Cached Data")
            return(inv)
       }
       mat <-x$get()
       inv <- solve(mat,...)
       x$setInverse(inv)
       inv
}
