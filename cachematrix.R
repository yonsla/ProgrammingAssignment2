## The purpose of the following two functions is to minimize computing time
## when computing the inverse of a matrix.
## This is good for situations when we repeatedly need calculations of the
## inverse of a matrix, but the matrix might occassionally change.
## The solution is, in case the inverse had already been computed, to keep track
## of the answer by caching and thus avoid repeat calculation, while
## recalculating the inverse only if the matrix had recently been changed.

## makeCacheMatrix creates a data structure, which we'll call a "cache matrix",
## for keeping track and updating both a given matrix x and its inverse s.
## More precisely, it returns a list of 4 functions, set and get
## for setting and getting the matrix x and setInv and getInv
## for setting and getting its inverse s.
## In addition, we use the NULL value for s as a flag indicating the inverse
## had not yet been calculated.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL;
    set <- function(y) {
        x <<- y;
        s <<- NULL;
    }
    get <- function() {
        x;
    }
    setInv <- function(myS) {
        s <<- myS;
    }
    getInv <- function() {
        s;
    }
    list(set=set, get=get, setInv=setInv, getInv=getInv);
}


## cacheSolve takes a "cache matrix" cacheX and returns the inverse of the
## corresponding matrix. If this had already been computed,
## it returns the computed value. Otherwise, it computes it.

cacheSolve <- function(cacheX, ...) {
    ## Return a matrix that is the inverse of 'cacheX'
    s <- cacheX$getInv();
    if(is.null(s)) {
        x <- cacheX$get();
        s <- solve(x, ...);
        cacheX$setInv(s);
    }
    s;
}
