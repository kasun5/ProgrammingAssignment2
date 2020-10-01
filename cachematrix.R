## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix function where we create setters and getters fuctions
##cacheSolve function get inverse matirx from cache if already calculaed of not solve and returned the value

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL                                #initiate inverte matirx as null
    set <- function(y) {                       #function to set the matrix 
        x <<- y
        inv <<- NULL
    }
    get <- function() x                        #function to get the matirx
    setinv <- function(inverse) inv <<- inverse  #function to set the inverse value 
    getinv <- function() inv                      #function to get the inverse value
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinv()                            #get the inverse matirx from chache    
    if(!is.null(inv)) {                          #check whether it is null or not
        message("getting cached data")           #if not null return invers matrix with the message 
        return(inv)
    }
    data <- x$get()                              #else get input matix from chache
    inv <- solve(data, ...)                       #solve the inverse matrix
    x$setinv(inv)                                #set the answer in the the chache
    inv                                          #return the invers matrix as output
        ## Return a matrix that is the inverse of 'x'
}
