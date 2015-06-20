### Overall, the functions below are meant to take an inverse of a matrix.
### The idea is the cache the results of the inverting process rather than running
### the program to recompute the results multiple times. There are two functions created
### that will be used for this process. Please see below
### there is the output of a test case 5x5 below the coding for the two functions

### THe first function 'makeCacheMatrix' will create a list
### A. set the value of the matrix
### B. get the value of the matrix
### C. set the value of inverse of the matrix
### D. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,getInverse = getInverse)
        
}
        

### Second function returns the inverse of the matrix 
### A. Checks if the inverse has already been computed
### B-True. If yes, retreives the cached result and no need to compute
### B-False. If not, it computes the inverse of the matric and sets the value
### in cache through setinverse function.

### This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()                          
        inv <- solve(data)
        x$setInverse(inv) 
        inv
        
}

### Test output below


###> x <- matrix(rnorm(25), nrow = 5)
###> TEST <- makeCacheMatrix(x)   
###> TEST$get()
###[,1]       [,2]       [,3]       [,4]        [,5]
###[1,] -1.7677142879  0.6552348  0.5177319 -0.6969290 -0.09855905
###[2,]  0.4075819628 -1.4812468 -0.6882713 -1.2200855  1.12363822
###[3,] -0.0009090906  0.3798101  1.4168140 -0.5912191  0.60483117
###[4,] -0.8593838963  1.3807943 -1.0366382 -0.5497335 -0.22861434
###[5,]  1.7728231587 -0.2446309 -1.2763382 -0.8880156 -0.68898899
###> cacheSolve(TEST) 
###[,1]        [,2]       [,3]        [,4]        [,5]
###[1,] -0.4184932 -0.08297206  0.3254285  0.05413025  0.19226731
###[2,] -0.4006750 -0.18617361  0.4198740  0.58633991 -0.07227178
###[3,]  0.2296637 -0.18305590  0.3318071 -0.38131607  0.08641296
###[4,] -0.4734433 -0.15614294 -0.2379649  0.04957864 -0.41226932
###[5,] -0.7497952  0.39296365  0.3803129  0.57357685 -0.55974005
###> cacheSolve(TEST) 
###getting cached data
###[,1]        [,2]       [,3]        [,4]        [,5]
###[1,] -0.4184932 -0.08297206  0.3254285  0.05413025  0.19226731
###[2,] -0.4006750 -0.18617361  0.4198740  0.58633991 -0.07227178
###[3,]  0.2296637 -0.18305590  0.3318071 -0.38131607  0.08641296
###[4,] -0.4734433 -0.15614294 -0.2379649  0.04957864 -0.41226932
###[5,] -0.7497952  0.39296365  0.3803129  0.57357685 -0.55974005