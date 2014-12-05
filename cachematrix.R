## FUNCTION: makeCacheMatrix( x )
##
## DESCRIPTION:
##
## Create a special matrix object that caches the inverse of the matrix,
## by using the cacheSolve() function dedicated for that.
## So multiple calls to cacheSolve() are CPU efficient because the inverse
## matrix is calculated only the very first time, and subsequent calls return
## only the cached data
##
## ARGUMENT: 'x' MUST BE a matrix object that is always invertible
##
## EXAMPLE: Create a matrix of a 2x2 matrix with 1,2,11,12 numbers
##
## mat <- makeCacheMatrix( matrix( c(1, 3, 2, 4), nrow = 2, ncol = 2 ) )
##
## Remark:to see the original matrix, you could use $get() function on this object
##
## mat$get()
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4
##
## To calculate the inverse matrix, you MUST USED the cacheSolve(mat) function
##
## cacheSolve(mat)
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
##
makeCacheMatrix <- function(x = matrix()) {
    ## Create a 's' object that the cacheSolve() function will used later to
    ## store the result of the inverse matrix of 'x
    s <- NULL

    ## Create a set() function so the user could change later the 'x' matrix
    ## If we change the initial
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Create a get() function that is used by cacheSolve() function to get the
    ## initial matrix 'x' create by makeCacheMatrix(x)
    get <- function() {
        x
    }

    ## Create a setsolve() function used by cacheSolve() to store the result of
    ## the inverse matrix of 'x'
    setsolve <- function(solve) {
        s <<- solve
    }

    ## Create a getsolve() function used by cacheSolve() to retrieve the cached
    ## result of the inverse matrix of 'x' if already calculated.
    getsolve <- function() {
        s
    }

    ## Create and return a list that contain the 4 functions we just created.
    ## This list of 4 function can now be used as the argument of function
    ## cacheSolve to calculate the inverse of the matrix and caching the result.
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

##
## FUNCTION: cacheSolve( x )
##
## DESCRIPTION:
##
## Return a matrix that is the inverse of matrix 'x' and using caching
## capability, so multiple calls to cacheSolve() for the same matrix are
## CPU efficient
##
## ARGUMENT: 'x' MUST BE an object create with the makeCacheMatrix() function
##
## EXAMPLE: Calculate the inverse matrix of a 2x2 matrix with 1,2,11,12 numbers
##
## mat <- makeCacheMatrix( matrix( c(1, 2, 11, 12), nrow = 2, ncol = 2 ) )
##
## cacheSolve( mat )    ## --> the very first call to cacheSolve
##      [,1] [,2]
## [1,] -1.2  1.1
## [2,]  0.2 -0.1
##
## cacheSolve( mat )    ## --> subsequent call to cacheSolve use cached data
## getting cached data  ## --> when cached, a message is display to the user
##      [,1] [,2]
## [1,] -1.2  1.1
## [2,]  0.2 -0.1
## 
cacheSolve <- function(x, ...) {

    ## Store in local variable 's' the last calculated inverse of the matrix
    ## of argument 'x' if any. If it is the first time the cacheSolve function
    ## is called for this "x" object then x$getsolve() return NULL
    s <- x$getsolve()

    ## If 's' is not equal to NULL, then we can return the variable 's' wich
    ## contains the already calculated inverse of the matrix 'x' and quit the
    ## function. NOTE: we also send an information "message" to the user just
    ## to precise that the value returned was 'in cache' and not need to be
    ## calculated.
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }

    ## Copy in the local variable 'data' the original matrix of object 'x' via
    ## the get() calling function. Then caculate the inverse matrix of 'data'
    ## and store it in 's' variable
    data <- x$get()
    s <- solve(data, ...)
    
    ## Special note: in fact it is possible to shorter the previous two lines
    ## in only one without using the 'data' variable like this:
    ##
    ## s <- solve( x$get(), ...)
    ##
    ## But for code clarity and code sharing, it could be better to break the
    ## code in multiple lines ! ;-)
    
    ## Store in object 'x' the inverse of the matrix 's' by calling setsolve()
    x$setsolve(s)

    ## return the inverse matrix of 'x' that is store in 's' and quit the function
    s
}

