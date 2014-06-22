# The following sets of functions provide the capability to cache the inverse 
# computation of a matrix. Basically, if the inverse is already computed and 
# the original matrix has not changed, the inverse is not recalculated and the
# alreay existing inverse is returned.The package consists of two functions:
#  - makeCacheMatrix:
#      provides the methods to set and get the matrix as well as its inverse. 
#  - cacheSolve:
#      calculate the inverse. If the inverse alreay exists, it does not 
#      recalculate the result.


# provide the methods to set and get the matrix as well as its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    # set the inverse to nukk
    inv <- NULL
    
    # define the set function which assigns the matrix and
    # nullifies the inv in the enviroment they are origianlly 
    # defined.
    set <- function(y){
        x   <<- y
        inv <<- NULL
    }
    
    # simply get the matrix
    get <- function() x
    
    # set the inverse
    setinv <- function(inv_in) inv <<- inv_in
    
    # get the inverse
    getinv <- function() inv
    
    # return a list of the functions
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}



# calculate the inverse. If the inverse alreay exists, it does not 
# recalculate the result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # get the inverser
    inv <- x$getinv()
    # if the value it assined, just return it
    if ( !is.null(inv) ) {
        message('getting cached inverse')
        return(inv)
    }
    # otherwise, recalcuate the inverse
    data <- x$get()
    # calculate the inverser
    inv <- solve(data,...)
    # set the inverse for later calculation
    x$setinv(inv)
    # and return the inverse
    inv
    
}
    
