## Together these functions check that the calculation of the invesre of the input matrix has been done before; 
## if not it will perform the calculation; 
## if so it will retrieve the values from cache instead.

#SUMMARY:
#makeCacheMatrix() is passed a matrix which turns it into an object of type 'matrix'. 
#This object stores two things: the original matrix's value and what will be the cached value, m,
#which is initially set to 'NULL'. 
#You use makeCacheMatrix() to create an object, then access that object (and not makeCacheMatrix itself).
#For example: bigMatrix <- makeCacheMatrix(1:1000) - this creates an object 'bigMatrix' of type 'matrix'.
#Note this object is a generic storage object (you can store a number representing anything, not just the mean 
#or inverse).  
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL    # 'm' will be the inverse and it's reset every time 'makeCacheMatrix()' is called
                  
                 # The following three functions are not run when 'makeCacheMatrix()' is called.
                 # Instead, they will be used by cacheSolve to get calues for 'c' or for 'm', and
                 # for setting the inverse.
    
    #The following is a function (specifically a 'method') which reuses the object by setting the stored
    #inverse to NULL and setting a new value for the vector 'z'.
    set <- function(newValue) {    # Takes the input vector 'newValue'
        z <<- newValue             # Variable 'z' in the containing environment (global) is set to 'newValue'
        m <<- NULL                 # 'm' is set to NULL in the global environment
    }

    #The following line is actually a function, so I have added '{}' for clarity; it reads and returns the value of x,
    #the original matrix that was passed as an argument to makeCacheMatrix() when makeCacheMatrix() is called.
    get <- function() { x }

    #The following line is actually a function, so I have added '{}' for clarity; it assigns the inverse to object 'm'.
    #It is called by 'cacheSolve()' during its first access and it will store the value using superassignment.
    #This means that that the object 'm' will exist (and its value will be accessible) in all other R environments.
    setinverse <- function(inverse) { m <<- inverse }

    #The following line is actually a function, so I have added '{}' for clarity; 
    #it returns the value of m and loads it into 'getinverse', i.e. caches it.
    #This will return the cached value to 'cacheSolve()' on subsequent accesses.
    getinverse <- function() { m }
    
    #Unlike the functions above, the following is accessed every time 'makeCacheMatrix()' is called.
    #It is a list of the "internal functions", i.e those functions defined within the 'makeCacheMatrix()' function.
    #These types of functions are called "methods" - 'cacheSolve()' can access these methods by using the '$' symbol.
    #Doing so returns a list of named values where the values are the functions (aka closures). 
    #The fact that they're named allows you to reference them by using e.g. "x$get()" within 'cacheSolve()'.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#Example: bigMatrix <- makeCacheMatrix(1:1000)   # Now there exists an object 'bigMatrix' of type 'matrix'
#         biggerMatrix <- makeCacheMatrix(1:100000)   # 'biggerMatrix' of type 'matrix' with 100,000 numbers
#The above calls 'makeCacheMatrix()' twice, creating two objects called 'bigMatrix' and 'biggerMatrix'.  
#They can be accessed by 'cacheSolve()'.  Note we do not call 'makeCacheMatrix()', we reference the objects 
#created by earlier calls to 'makeCacheMatrix()'.



#SUMMARY:
#'cacheSolve()' accesses the object (not the 'makeCacheMatrix()' function, but the object created when 
#makeCacheMatrix() was called) by fetching the value of the matrix used to create the object, 
#this vector having been stored when the object was created. 
#The input is an object created by makeCacheMatrix.
cacheSolve <- function(z, ...) {    #the 'z' is the name of the object created by 'makeCacheMatrix()'
    
    
    m <- z$getinverse()   #Accesses the object 'z' and gets the value of its inverse into m
    
    #The following if statement checks to see if 'm' now contains non-null values.
    if(!is.null(m)) {     # Check if the inverse was already stored (cached) in 'm' (i.e. is not null).
        message("getting cached data")    # If so, send this message to the console, ...
        return(m)                         # ... return the value of 'm' (the inverse), and exit 'cacheSolve()'
    }
    
    #If 'm' is NULL however, put the output of the 'get()' function into 'data' and then use the 'solve' 
    #builtin function to get the inverse of 'data' and put that result into 'm'.
    data <- z$get()       # Call the 'get()' function defined in 'makeCacheMatrix()' on 'x' and store the result in 'data'
    m <- solve(data, ...) # Call the 'solve()' funcion on 'data' and store the result in 'm'
    
    
    z$setinverse(m)       # Now store the calculated inverse value in x (see 'setinverse()' in 'makeCacheMatrix()')
    m                     # Return the inverse to the code that called this function
}
