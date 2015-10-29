
#' training function for convsvm
#'
#' @param  model		model to use
#'
#' @export
confSVMTrain = function (model = "1999", gamma = 3.125, cost = 1, 
	train.x = NULL, train.y = NULL, kappa = 0.1, tau = 0.1, ...) {
	
	doPlot = TRUE
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
	cat ("\n\n### first round.\n")
	svm.model = alphasvm(x = train.x, y = train.y, gamma = gamma, 
						cost = cost, probability = FALSE, type = "C-classification")
	if (is.null(test.x) == FALSE) {
		scores = predict(svm.model, test.x, decision.values = TRUE)
		accuracy = sum(diag(table(scores, test.y)))/length(test.y)
		cat (" -- Accuracy of first round on test set: ", accuracy)
	}

	
	## compute conformal scaling
	cat ("\n\n### computing conformal scaling.\n")
	confScaling = getConfScaling(model, svm.model, train.x, train.y, kappa = kappa, tau = tau)
	if (is.null(confScaling) == TRUE)
		stop ("Sorry, no confscaling selected.")

	# scale each train.x instead of modifying the kernel
	tmpX = train.x
	for (i in 1:nrow(train.x)) {
		tmpX[i,] = tmpX[i,] * confScaling[i]
	}
	
	if (doCondNumber == TRUE) {
		rbf <- rbfdot (sigma = gamma)
		khat_ij = as.matrix(kernelMatrix(rbf, tmpX))
		shat = svd(khat_ij)
		X11()
		plot(shat$d, main = sprintf("after, k: %f", kappa(khat_ij)))
	}
	
		
	## plotting conformal scaling
	cat ("\n\n### plotting conformal scaling.\n")
	if (doPlot == TRUE) {
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

	
	## compute conformal scaling on test set
	cat ("\n\n### computing conformal scaling.\n")
	
	
	# compute scaling on test set 
	if (is.null(test.x) == FALSE) {
		testScaling = getConfScaling(model, svm.model, test.x, kappa = kappa, tau = tau)
	}

	# second round of computations, can/will use the normal SVM
	newsvm.model = alphasvm(x = train.x, y = train.y, gamma = gamma, 
						confscaling = confScaling, fitted = FALSE,
						cost = cost, probability = FALSE)

	# need now the confscaling (from the first round) ON the new supportvectors
	# as the kernel is the old kernel.

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
