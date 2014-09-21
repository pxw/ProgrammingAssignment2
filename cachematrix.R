## This solution mirrors the elegant vector example supplied....Hence....

## `makeCacheMatrix`: creates a special "matrix" object that can cache its inverse.
## `cacheSolve`: computes the inverse of the special "matrix" returned by `makeCacheMatrix`.
## ..If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
##
## Note that makeCacheMatrix returns a *list* that allows re-accessing the cache environment.
## That means if we call:  special_matrix <- makeCacheMatrix(My_matrix), then 
## we will need to call:  cacheSolve(special_matrix) to access the cached version of the inverse
## and potentially could use special_matrix$set(New_Matrix) to cache a new matrix

makeCacheMatrix <- function(x = matrix()) {   
    ## Create a special "matrix", and return
    ## a list containing a function to
    ##   1.  set the value of the matrix
    ##   2.  get the value of the matrix
    ##   3.  set the value of the inverse
    ##   4.  get the value of the inverse

    inv <- NULL   # inv will become the persistant inverse when it is cached
    
    set <- function(y){          ## 1. set
        x <<- y
        inv <<- NULL
    }       
    get <- function(){           ## 2. get
        x
    }  
    setinv <- function(inverse){ ## 3. setinv
        inv <<- inverse    #storage only; actual calucation of inverse occurs in "cacheSolve"
    }
    getinv <- function(){        ## 4. getinv
        inv
    } 
    list(set = set, get = get, setinv = setinv, getinv = getinv)    ## the returned list  
}

##---------------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
    ## Calculate the inverse of the special "matrix"
    ## created with the above function, first checking to see if the
    ## inverse has already been calculated. If so, it `get`s the inverse from the
    ## cache and skips the computation. Otherwise, it calculates the inverse of
    ## the data and sets the value of the inverse in the cache via the `setinv`
    ## function.  This function does not check to see if the matrix is invertable. (as per instructions)
    
    ## Attempt to get the inverse from the cache
    ## (It will be null if doesn't exist, or the cached matrix has been changed via set function.)
    inv <- x$getinv()
    if(!is.null(inv)) {
        message('gettingcached data')
        return(inv)
    }
    ## if returned value is null, then generate inverse and store it in the cache
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv   
}
