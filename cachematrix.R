## makeCacheMatrix and cacheSolve are two functions that help saving time and resources 
## on possible repeated invertions of a given matrix by caching the original object and 
## its inverted version in the global environment using the '<<-' operator

## makeCacheMatrix function accepts an invertible matrix as an argument 'x'
## it creates an inverted version of 'x'
## then creates a list 'z' in the global environment. 
## 'z' contains both the original matrix 'x' and its inverted version for future use

makeCacheMatrix <- function(x = matrix()) {
        y <- solve(x)
        z <<- list(incoming = x, inversion = y)        
}


## cacheSolve accepts an invertible matrix as an argument 'x'
## first, it checks if 'x' is identical to the original matrix cached in 'z' list in the global environment
## if not, it gives a warning message that the new object differs from the original
## then it inverts the new matrix and assigns the outcome to the 'result' variable
## second, if 'x' matches the cached matrix, the function checks if the inversion was already done before
## by addressing to the 'z' list in the global environment
## if the inverted version is there, it is assigned to the 'result' variable
## if not, the function inverts the matrix and assigns the outcome to the 'result' variable
## finally, the function returns 'result' variable

cacheSolve <- function(x, ...) {
        if(identical(z$incoming, x)) {
                if(!is.null(z$inversion)) {
                        print("getting cache data")
                        result <- z$inversion
                }
                else {
                        result <- solve(x)
                }
        }
        else {
                print("incoming matrix differs from original")
                print("inverted version of new matrix is")
                result <- solve(x)
        }
        return(result)
        
        ## Return a matrix that is the inverse of 'x'
}
