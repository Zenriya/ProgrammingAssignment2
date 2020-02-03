## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        setMat <- function(y) { #set matrix value 
                x <<- y
                invMat <<- NULL
        }
        
        getMat <- function() x                           #get matrix value                   
        setInv <- function(inverse) invMat <<- inverse   #set invertible matrix value
        getInv <- function() invMat                      #get invertible matrix value
        list(setMat = setMat, getMat = getMat,
             setInv = setInv, getInv = getInv)
        
        #creates a special matrix that can cache its inverse
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        invMat <- x$getInv()
        if(!is.null(invMat)) {
                message("Cached Inv Mat")   
                return(invMat)                             
        }
        MatData <- x$getMat()                     
        invMat <- solve(MatData, ...)             
        x$setInv(invMat)                          
        return(invMat)     
        #Return a matrix that is the inverse of 'x'
        #computes the inverse matrix returned by makeCacheMatrix above
}
