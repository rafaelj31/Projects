#set the input x as a matrix:
my_matrix <- function(x = matrix()) {
        
        # set the solved value "inverse" as a null
        inversa <- NULL
        
        #Set code for the matrix value
        set <- function(y){
                #The <<- operator allows "set" to create an instance of x 
                #outside the function environment
                x <<- y
                inversa <<- NULL
        }
        
        #get values from array
        get <- function() x #return x
        
        #Get the value of the inverse and store it in "inverse"
        setinversa <- function(inv) inversa <<- inv
        
        #Recover the value of the inverse
        getinversa <- function() inversa 
        
        #List functions so you can call them later
        list(set = set, get = get,
             setinversa = setinversa,
             getinversa = getinversa)
}

#The following function will get the cached inverse
cacheInversa <- function(x, ...){
        
        #Get the return value of the inverse
        inversa <- x$getinversa()
        
        #Error condition
        #If cache value is NOT null, get cache ...
        if(!is.null(inversa)){
                message("Getting Data in Cache")
                return(inversa)
        }
        # ... If IS null, then get the inverse (with solve()) and cache it
        valor <- x$get()
        inversa <- solve(valor,...)
        x$setinversa(inversa)
        inversa #return inverse
}
#apply the function to see how it works on a matrix x with 2 rows and 5 columns:

x <-matrix(c(2,2,1,4), nrow=2, ncol=2)
x

#Applying the cacheInversa function on the matrix x the following result is 
#obtained:
cachetest <- my_matrix(x)
cachetest$get()

y <- cacheInversa(cachetest)
y