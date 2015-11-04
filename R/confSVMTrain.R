
#' training function for convsvm
#'
#' @param  model		model to use
#'
#' @export
confSVMTrain = function (model = "1999", gamma = 3.125, cost = 1, 
	train.x = NULL, train.y = NULL, kappa = 0.1, tau = 0.1, verbose = TRUE, ...) {
	
	doPlot = FALSE
	doTest = TRUE
	doCondNumber = FALSE
	
	if (doCondNumber== TRUE) {
		library(kernlab)
		rbf <- rbfdot (sigma = gamma)
		k_ij = kernelMatrix(rbf, train.x)
		s = svd(k_ij)
		plot(s$d, main = sprintf("before, k: %f", kappa(k_ij)))
	}
	
	
	## first round of computations, can/will use the normal SVM
	if (verbose == TRUE) {
		cat ("\n### Calling normal SVM to obtain approximation of decision boundary.\n")
	}
	svm.model = alphasvm(x = train.x, y = train.y, gamma = gamma, 
						cost = cost, probability = FALSE, type = "C-classification")
						
	# if we have a test set, we compute its accuracy
	if (is.null(test.x) == FALSE) {
		scores = predict(svm.model, test.x, decision.values = TRUE)
		accuracy = sum(diag(table(scores, test.y)))/length(test.y)
		cat (" -- Accuracy of SVM on test set: ", accuracy)
	}

	
	## compute conformal scaling
	if (verbose == TRUE) {
		cat ("\n### Computing conformal scaling with ", model, " model.\n")
	}
	# compute the conformal scaling on the whole training set
	confScaling = getConfScaling(model, svm.model, train.x, train.y, kappa = kappa, tau = tau)
	if (is.null(confScaling) == TRUE)
		stop ("Sorry, no confscaling selected.")

	# scale each train.x instead of modifying the kernel -- was ist das fuern quatsch?
# 	tmpX = train.x
# 	for (i in 1:nrow(train.x)) {
# 		tmpX[i,] = tmpX[i,] * confScaling[i]
# 	}
	
	if (doCondNumber == TRUE) {
		rbf <- rbfdot (sigma = gamma)
		khat_ij = as.matrix(kernelMatrix(rbf, tmpX))
		shat = svd(khat_ij)
		X11()
		plot(shat$d, main = sprintf("after, k: %f", kappa(khat_ij)))
	}
	
		
	## plotting conformal scaling
	if (doPlot == TRUE) {
	  if (verbose == TRUE)
	    cat ("\n### Plotting conformal scaling.\n")
	  
	  library(ggplot2)
		# TODO: adapt grid size to data.
		plotdata = expand.grid(x=seq(-1,1,0.1), y= seq(-1,1,0.1))
	#	  plotdata = expand.grid(x=seq(-2,2,0.05), y= seq(-2,2,0.05))
		plotDensity = NULL
		plotDensity  = getConfScaling (model, svm.model, plotdata, kappa = 1, tau = 1)

		plotdata = cbind(plotdata, plotDensity)
		colnames(plotdata)=c("x","y", "plotDensity")
		
		midpoint = 0
		myPlot = ggplot() + 
			xlab ("x") +
			ylab ("y") +
			ggtitle("density") +   
			layer(
				data=plotdata,
				mapping = aes(x = x, y = y, fill = (plotDensity)),
				geom="tile" 
			) 
		
		print (myPlot)
	}	

	
	
	# compute scaling on test set 
	if (is.null(test.x) == FALSE) {
	  
	  ## compute conformal scaling on test set
	  if (verbose == TRUE)
	    cat ("\n### Computing conformal scaling on test set.\n")
	  testScaling = getConfScaling(model, svm.model, test.x, kappa = kappa, tau = tau)
	}

	
	# second round of computations, can/will use the normal SVM
	if (verbose == TRUE)
	  cat ("\n### Training conformally scaled SVM\n")
	newsvm.model = alphasvm(x = train.x, y = train.y, gamma = gamma, 
						confscaling = confScaling, fitted = FALSE,
						cost = cost, probability = FALSE)

	# need now the confscaling (from the first round) ON the new supportvectors
	# as the kernel is the old kernel. confScaling is now the size of the SVs of the second model.
	confScaling = getConfScaling(model, svm.model, newsvm.model$SV, kappa = kappa, tau = tau)

	if (is.null(test.x) == FALSE) {
		preds = predict(newsvm.model, 
						test.x, 
						probability = FALSE, 
						confscaling = confScaling, 
						testscaling = testScaling)

		score = sum(diag(table(preds, test.y)))/length(test.y)
		cat("\n --- ", score, "\n")
	}

	model = list()
	return (model)
}
