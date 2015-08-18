## The following functions allow you to cache the inverse of a matrix.  Matrix inversion can be a 
## costly computation and there may be some benefit to caching he inverse of a matrix rather than
## computing it repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){        # Creates a matrix from the given set of values
        m <- NULL                                 #sets m to NUll
        set <- function(y){                       #The set function caches (<<-) the values passed to x
                x <<- y
                m <- NULL
        }
        get <- function() x                        #The get function contains the cached values of x
        setinverse <- function(solve) m <<- solve  #Creates a factor to get the inverse of a matrix and stores in m
        getinverse <- function () m                #The getinverse function contains the cached inverse stored in m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #Identifies the parts of the list
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...){    #Takes the special matrix created by makeCacheMatrix
        m <- x$getinverse()        #Gets the stored inverse value
        if(!is.nul(m)){            #Checks the value of m and returns the cached value
                message("getting cached data")
                return(m)
        }
        data <- x$get()            #If the value of m is NULL get the stored matrix pass to data
        m <- solve(data, ...)      #Calulate the inverse of the stored data with the solve function
        x$setinverse(m)            #Pass the inverse to setinverse (makCacheMatrix function)
        m                          #Return the value of the inverse
}