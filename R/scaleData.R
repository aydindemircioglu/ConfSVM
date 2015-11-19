
# from the mighty internet
normalize3 <- function(mat) { 
    apply(mat,2,function(x) {xmin <- min(x); 2*(x-xmin)/(max(x)-xmin)-1})
}
 

normalize01 <- function(mat) { 
    apply(mat,2,function(x) {xmin <- min(x); (x-xmin)/(max(x)-xmin)})
}

 
scaleData <- function(dataset,
                                    transformation = "VARIANCE",
                                    verbose = TRUE)
{
    if (verbose == TRUE)
		cat("  Scaling data.\n")

    if (transformation == "VARIANCE") {
        # declare result
        nX = as.matrix(dataset$x)
    
        # scale data, apply scaling columnwise
        for (t in seq(1:ncol(nX)))
        {
            # get column
            curCol = as.matrix(nX[, t])
            u = unique(curCol)
            
            # if its unary, then we do not do anything. this really happens.
            if (length(u) == 1)
            {
                curCol = 0*curCol
            }

            # binary, we scale to 0, 1
            if (length(u) == 2)
            {
                # rescale to 0, 1
                # count the numbers
                u = unique(curCol)
                countA = sum(curCol == u[1])
                countB = sum(curCol == u[2])
                
                if (countA > countB) {
                    tmp = u[2]
                    u[2] = u[1]
                    u[1] = tmp
                } 
                curCol = (curCol-u[2])/(u[1]-u[2])
            }
            
            # anything beyond binary, we scale by its variance
            if (length(u) > 2)
            {
                curCol = curCol/sd(curCol)
            }
            
            # re insert the column
            nX[, t] = curCol
        }
        
        # scale label. we check it is binary and rescale to -1, 1
        curCol = as.matrix(as.numeric(as.character(dataset$y)))
        u = unique(curCol)
        
        # if its binary, then 
        if (length(u) != 2)
        {
            stopf("Labels are not binary!")
        }

        curCol  = normalize3 (curCol)
        ny = curCol
    }
    
    if (transformation == "LINEAR") {	
        nX = normalize01 (as.matrix(dataset$x))
        ny = normalize01 (as.matrix(as.numeric(as.character(dataset$y))))
	}

    
    if (transformation == "NORMALIZE") {
        nX = normalize3 (as.matrix(dataset$x))
        ny = normalize3 (as.matrix(as.numeric(as.character(dataset$y))))
	}
	
	# now, we have seen that there are datasets where
	# a whole row is simply missing. this will make problems.
	# we try to remedy the situation by removing Nans
    nX[is.na(nX)]<-0

    return (list("x" = nX, "y" = ny))
}
 
