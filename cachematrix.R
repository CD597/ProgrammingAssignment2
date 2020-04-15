## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## for my inverse matrix
    set <- function(y) {
        x <<- y    ##Set input matrix to x in parent environment
        i <<- NULL  ## set i to NULL in parent environment
    }
    get <- function () x  ## get matrix x
    setimatrix <- function (inv) i <<-inv ## set inverse matrix
    getimatrix <- function () i  ## get inverse matrix
    list (set =set, get = get, 
          setimatrix = setimatrix, getimatrix = getimatrix)
}

## makeCacheMatrix's input is a matrix.  Returns a list of functions to the parent environment, which also 
## which is why the access to the input argument varible is retained upon function completion.  The set function
## is used to set a new matrix after initial or first function call.  It also sets the values
## of x and i within the parent environment.  get, retrives x from the parent environment or the returned function environment.  
## setimatrix will set the inverse matrix to i in the parent environment.  getimatrix will retrieve
## the inverted matrix from the partent environment.  Finally all functions are stored in a list by
## their name, so each can be called by '$' and their name.


cacheSolve <- function(x, ...) {
    i <- x$getimatrix() ## Get matrix
    if (!is.null(i)) {   ## check if the inverse is already in the cache - use identical () to see if same
        print ("Returning cached data")
        return (i)
    }
    matrix <-x$get()  ## if there is no cache, get the matrix 
    i <-solve(matrix)       ## invert matrix
    x$setimatrix(i)         ## set matrix
    i
}

## This function either retrieves the inverse matrix from memory or it computes the inverse matrix
## if the matrix is new.  It works in conjunction with the makeCachematrix.  The function will first look to
## retrieve the inverse matrix from the parent environment.  If it finds it, it just returns it.  If there is not
## an inverse matrix it calulates one and then stores it through the makeCachematrix into memory.  The key to 
## two functions and how they work together is the inverse matrix, once computed, is stored outside the environment
## of the functions.  Then in the $getimatrix function i is not defined so due to the scoping rules R looks 
## to the parent environment to find i.
