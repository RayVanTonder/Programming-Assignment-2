## Create a special 'Matrix' containing a list
## Test if input is a Matrix
## set data in matrix
## get data in matrix
## set data in the inverse of the matrix
## get the data of the inverse of the matrix

makeCacheMatrix <- function(x){
        if(!is.matrix(x))
        {
                stop("Input must be a Matrix")
        }
        m <- NULL
        set <- function(y)
        {
                # deep assignment arrow <<-, modifies an existing,
                # variable by the parent environments.
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
## Test if the inverse of 'x' was calculated
## if calculated, get the inverse in cache, getinverse function
## if not, calculate the inverse of 'x', use the solve function
## sets the inverse in cache, setinverse function.

cacheSolve <- function(x, ...){
        if((!is.list(x))
           |(names(x[1])!="set")
           |(names(x[2])!="get")
           |(names(x[3])!="setinverse")
           |(names(x[4])!="getinverse"))
                {
        stop("Input must be a list generated in makeCacheMatrix")
                }
        m<-x$getinverse()
        if(!is.null(m))
        {
                message("getting cached Matrix data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        # return matrix inverse, see help solve function 
}
