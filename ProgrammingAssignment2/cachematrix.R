## Put comments here that give an overall description of what your
## functions do
## There are 2 functions defined in this file:
## (1) makeCacheMatrix
## This function creates a cachematrix object that will be used
## as an input object to the other function defined in this file,
## i.e. cacheSolve
## (2) cacheSolve
## This function takes the cachematrix object described above 
## as an argument, and computes the inverse of the matrix that
## was "set" in the cachematrix object. If the same input matrix
## is used upon subsequent invocations of cacheSolve(), then
## cacheSolve() will not re-compute the inverse of the matrix but
## will instead use the cached results of the previous 
## inversion operation for the matrix.

## Write a short comment describing this function
## Creates a "cachematrix" object upon being supplied a matrix
## as an argument
## To test this function, in the R/RStudio console:
## (1) Create a simple 2 x 2 test matrix, e.g.:
## > test_matrix <- matrix(c(4,2,7,6),2,2)
## (2) Create the "cachematrix" object using the test matrix
## created in (1):
## > cache_matrix <- makeCacheMatrix(test_matrix)
## (3) Print out the contents of the cache_matrix object. You 
## should see a list of the functions defined in the cache_matrix
## object, such as "$set", "$get", etc.
makeCacheMatrix <- function(x = matrix()) {
    x_inverse <- NULL
    # Define functions
    set <- function(y) {
        message("called set()")
        # Save y as x in parent environment
        x <<- y
        # Save m in parent environment, with value NULL
        x_inverse <<- NULL
    }
    
    get <- function() x
    set_inverse <- function(matrix_inverse) x_inverse <<- matrix_inverse
    get_inverse <- function() x_inverse
    # Create a list of the defined functions and return them
    list(set = set, get = get, 
         get_inverse = get_inverse, 
         set_inverse = set_inverse )
    
}


## Write a short comment describing this function
## Computes the inverse of the matrix that was the argument to 
## the function "makeCacheMatrix" when the cachematrix object
## was created
## To test this function, in the R/RStudio console:
## (1) First, execute the "makeCacheMatrix" function as described
## above
## (2) Use the cachematrix object created in (1) to compute the
## inverse of the matrix supplied to the "makeCacheMatrix" function
## when creating the cachematrix object, by passing the cachematrix
## object as an argument to the "cacheSolve" function, e.g.:
## > cacheSolve(cache_matrix)
## The expected output is the inverse of the matrix, preceded by
## the message "getting cached matrix" if cacheSolve() is 
## executed more than 1 time to compute the inverse of the same
## matrix -- because in that case cacheSolve() will retrieve the
## cached result of the previous inversion operation, instead of
## computing the inversion again. The first time that cacheSolve()
## is executed, the message "getting cached matrix" will not
## precede the inverse of the matrix that is printed to the
## console.
## (3) To compute the inverse of another matrix, perhaps a more
## complex one, execute the following steps:
## (i) Use the "set" function defined in the cachematrix object
## to specify the new matrix, e.g., to create an 8 x 8 matrix of
## integers in the range [1..25] with replacement, use the 
## cachematrix object's set() function, as follows:
## > cache_matrix$set(matrix(sample.int(25, size = 8*8, replace = TRUE), nrow = 8, ncol = 8))
## (ii) Now that the new matrix is "loaded" into the cachematrix
## object, execute the cacheSolve() function as before, passing
## the modified cachematrix object as an argument exactly as was
## done previously:
## cacheSolve(cache_matrix)
## Again, the expected output is the inverse of the matrix provided
## during the "set" operation, preceded by the message "getting 
## cached matrix" if cacheSolve() is executed more than 1 time
cacheSolve <- function(x, ...) {
    x_i <- x$get_inverse()
    if(!is.null(x_i)) {
        message("getting cached matrix")
        return(x_i)
    }
    # Get the matrix
    m <- x$get()
    # Perform the matrix inversion operation using R's solve()
    # function
    x_i <- solve(m)
    # Save (cache) the results of the matrix inversion operation
    x$set_inverse(x_i)
    # Return the inverted matrix
    x_i
}
