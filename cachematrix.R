
# This function returns list of functions.
# Purpose of this function is to store matrix x and its inverse
# list contains following fuctions
#	* set - set matrix
#	* get - get matrix
#	* setInverse - set inverse of matrix
#	* getInverse - get inverse of marrix 
# e.g
#	y<-makeCacheMatrix(matrix(1:9, 3, 3))
#	y$get()
#	     [,1] [,2] [,3]
#	[1,]    1    4    7
#	[2,]    2    5    8
#	[3,]    3    6    9
makeCacheMatrix <- function(x = matrix()) {
	
	inverse<-NULL # to hold inverse of matrix

	# set matrixt
	set<- function(m){
		x<<-m
		inverse<-NULL
	}
	# return the matrix
	get<-function() x
	# set inverse
	setInverse<-function(inv) inverse<<-inv
	# return inverse
	getInverse<-function() inverse

	list(get=get, set=set,
		getInverse=getInverse, setInverse=setInverse)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated # (and the
# matrix has not changed), then the cacheSolve returns the inverse from the cache
# e.g.



cacheSolve <- function(x, ...) {
	# Return a matrix that is the inverse of 'x'
	inverse<-x$getInverse()
	if(!is.null(inverse)) return(inverse)
	m<-x$get()
	# computes inverse 
	inverse<-solve(m)
	#update cache
	x$setInverse(inverse)
	inverse
}
