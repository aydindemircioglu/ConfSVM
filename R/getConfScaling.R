	
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

