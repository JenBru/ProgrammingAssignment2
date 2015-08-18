# Programming assignment 3
# caching the inverse of a matrix inversion via a pair of functions:
#  1. makeCacheMatrix (creates special matrix object that can cache its inverse)
#  2. cacheSolve (computes inverse of special matrix returned by 1st function, 
#     if already computed, just retrieve result from cache)
# this code was adapted from the example for means of a vector on coursera web 
# https://class.coursera.org/rprog-031/human_grading/view/courses/975105/assessments/3/submissions

# makeCacheMatrix function will create a list of functions:set, get, setmatrix 
# & getmatrix
makeCacheMatrix <- function(x = matrix()) {
        
        # initialize value of inverted matrix to 0
        m<-NULL
        
        # set the values of the original matrix x
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        
        # get is a function that just returns the matrix x that is passed in
        get<-function()x
        
        # setmatrix assigns values to the inverted matrix m
        setmatrix<-function(mat) m<<-mat
        
        #getmatrix returns values of the inverted matrix m
        getmatrix<-function() m
        
        # this function returns a list of functions
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## cacheSolve checks whether the matrix has been inverted with results in cache
# if inversion exists, returns the stored result; if not, computes it and returns 
# note: argument x to cacheSolve is a list of functions
# it is not the same matrix x that was passed in as args to makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #if the inverted matrix result exists (not null), it will get it & 
        # return it along with the message "getting cached matrix"
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached matrix")
                return(m)
        }
        
        # if inverted matrix results do not already exist,
        # define data matrix to be inverted using get function
        # note: x$ here refers to the list of functions, of which we want to 
        # call the get() function, which is to be applied to the x matrix 
        # (as defined in the makeCacheMatrix environment)
        data<-x$get()
        
        #next, set m matrix equal to the result of applying solve function to 
        # the original x matrix (obtained via get() in step above)
        m<-solve(data,...)
        
        # next, setmatrix function is applied to the m matrix, which is the 
        # inverted matrix
        x$setmatrix(m)
        
        # display the result (inverted matrix)
        m
}

# this can be tested as follows:
# create a sample matrix A 
A<-matrix(data=c(3,4,1,2), nrow=2)

# A is original x matrix supplied to function makeCacheMatrix
# this just returns list of functions, which is saved as C
C<-makeCacheMatrix(A)

# next, the list of functions from above (C) are provided as arguments to cacheSolve
# this returns the inverted matrix of original matrix A
cacheSolve(C)

# if we run cacheSolve a second time, it returns the same result (from the cache) 
#along with the message
cacheSolve(C)

# if we wanted to run the inversion function on a different matrix, say B instead of A
# set alt matrix B
B<-matrix(data=c(1,2,3,2), nrow=2)

# set the original matrix as B
C$set(B)

# then, rerun cacheSolve, which will return inverted B
cacheSolve(C)
