	require(SwarmSVM)
	require(e1071)
	library(lattice)
	require(ggplot2)

	setwd("~/lab/gsoc/SwarmSVM/")
	source ("./R/generateSinusData.R")
	source("./R/unfactorize.R")


	
getConfScaling = function (model, svm.model, data, labels = NULL, kappa = 1, tau = 1) {
	confScaling = NULL
	
	# 2002 model
	if (model == "2002") {
		cat ("--- 2002 model.\n")
		confScaling = rep(1, nrow(data))
	}

	# 1999 model
	if (model == "1999") {
		cat ("--- 1999 model.\n")
		# optimize later
		confScaling = rep(0, nrow(data))
#		h = -as.numeric(as.character(labels[svm.model$index]))*svm.model$coefs 
		h = (svm.model$coefs)
		for (i in 1:nrow(data)) {
			for (a in 1:nrow(svm.model$SV)) {
				v = (data[i,] - svm.model$SV[a,]) 
				n = -sum(v*v)/(2*tau^2)
				confScaling[i] = confScaling[i] + h[a]*exp(n)
			}
		}
	}
	
	if (model == "williams") {
		cat ("--- williams model.\n")
		scores = predict(svm.model, data, decision.values = TRUE)
		decisionValues = attr(scores, "decision.values")
		if (is.null(kappa) == TRUE) {
			kappa = 1/max(abs(decisionValues)) 
			cat (" -- Setting Kappa to:",kappa, "\n")
		}
		confScaling = exp(-kappa*decisionValues^2)
	}
	
	# set kappa!
	
	return (confScaling)
}


	
	
	model = "1999"
#	model = "williams"
	doPlot = FALSE
	
	N = 1000

	# but we take here cost = 4 -->  0.94 = 0.06 error
	gamma = 3.125
	cost = 1
#	cost = 20
	#gamma = 1/0.6

	# confscaling parameters
	kappa = 0.11
	tau = 0.2

	set.seed(43)

	# generate data
	d = generateSinusData(N)
	train.x = d$x
	train.y = as.factor(d$y)

	d = generateSinusData(N)
	test.x = d$x
	test.y = as.factor(d$y)


	## make sure confSVM works as expected first

	# first round of computations, can/will use the normal SVM
	svm.model = alphasvm(x = train.x, y = train.y, gamma = gamma, 
						cost = cost, probability = FALSE, type = "C-classification")
	scores = predict(svm.model, test.x, decision.values = TRUE)
	accuracy = sum(diag(table(scores, test.y)))/length(test.y)
	print(accuracy)

	# next we compute with constant confscaling and expect the same
	confScaling = 0*train.x + 1
	svm.model = alphasvm(x = train.x, y = train.y, gamma = gamma, 
		confScaling = confScaling,
		cost = cost, probability = FALSE, type = "C-classification")
	confScaling = 0*svm.model$index + 1
	testScaling = 0*as.numeric(test.y) + 1
	scores = predict(svm.model, test.x, 
		confscaling = confScaling, 
		testscaling = testScaling,
		decision.values = TRUE)
	caccuracy = sum(diag(table(scores, test.y)))/length(test.y)
	print(accuracy)

	if (abs(accuracy - caccuracy) >= 1/length(test.y)) {
		stop ("ConfSVM does not work as expected")
	}

	
	
	## first round of computations, can/will use the normal SVM
	cat ("\n\n### first round.\n")
	svm.model = alphasvm(x = train.x, y = train.y, gamma = gamma, 
						cost = cost, probability = FALSE, type = "C-classification")
	scores = predict(svm.model, test.x, decision.values = TRUE)
	accuracy = sum(diag(table(scores, test.y)))/length(test.y)
	print(accuracy)


	
	## compute conformal scaling
	cat ("\n\n### computing conformal scaling.\n")

	confScaling = getConfScaling(model, svm.model, train.x, train.y, kappa = kappa, tau = tau)
	if (is.null(confScaling) == TRUE)
		stop ("Sorry, no confscaling selected.")

		
		
		
	## plotting conformal scaling
	cat ("\n\n### plotting conformal scaling.\n")
	
	#print (confScaling)
	if (doPlot == TRUE) {
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
	testScaling = getConfScaling(model, svm.model, test.x, kappa = kappa, tau = tau)


	# second round of computations, can/will use the normal SVM
	newsvm.model = alphasvm(x = train.x, y = train.y, gamma = gamma, 
						confscaling = confScaling, fitted = FALSE,
						cost = cost, probability = FALSE)

	# need now the confscaling (from the first round) ON the new supportvectors
	# as the kernel is the old kernel.

	confScaling = getConfScaling(model, svm.model, newsvm.model$SV, kappa = kappa, tau = tau)

	preds = predict(newsvm.model, 
					test.x, 
					probability = FALSE, 
					confscaling = confScaling, 
					testscaling = testScaling)

	score = sum(diag(table(preds, test.y)))/length(test.y)
	cat("\n --- ", score, "\n")

