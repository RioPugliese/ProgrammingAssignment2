# -----
# This function receives an input and, on the top of that, builds all the functions to create and get a matrix 
# and to calculate and get its inverse. Then, it stores everything into a list and returns it.
makeCacheMatrix <- function(mt=matrix()){
    
    # Reset the initial value of the inverse matrix
    invMtx <- NULL
    
    # Set the matrix
    setMtx <- function(given_mt){
        mt <<- given_mt
        invMtx <<- NULL
    }
    
    # Get the matrix
    getMtx <- function(){mt}
    
    # Set the inverse value of the matrix
    setInverseMtx <- function(inverse){invMtx <<- inverse}
    
    # Get the inverse value of the matrix
    getInverseMtx <- function(){invMtx}
    
    # Return all results in a list
    list(setMtx=setMtx,
         getMtx=getMtx,
         setInverseMtx=setInverseMtx, 
         getInverseMtx=getInverseMtx)
}

# -----
# This function checks weather an inverse of the provided matrix already exists (i.e., invMtx != NULL). 
# If that's the case, then it returns that cached value. 
# Otherwise, it calculates it and then returns the new value.
cacheSolve <- function(mt, ...){
    
    # Get the existing value of the inverse matrix
    invMtx <- mt$getInverseMtx()
    
    # If the existing value is not null, then use this one
    if(!is.null(invMtx)){
        message("Reusing cached value")
        return(invMtx)
    }
    
    # Else: 1. get the matrix...
    mtx <- mt$getMtx()
    
    # ... 2. Calculate its inverse value...
    invMtx <- solve(mtx, ...)
    
    # ... 3. And store it in the object
    mt$setInverseMtx(invMtx)
    
    # Return the value
    invMtx
}

# ======
# Checks
my_mtx <- makeCacheMatrix(matrix(c(4, 7, 3, 6), ncol=2))
my_mtx$getMtx()
my_mtx$getInverseMtx()
cacheSolve(my_mtx)

