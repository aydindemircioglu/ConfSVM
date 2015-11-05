	
# get D(.) of a fixed set. is usally used with a) the set of all training points,
# so we can modify the kernel by just precomputing and scaling (other values of D
# will not be needed during training)-- or b) the set of all test points to make
# testing if not faster than easier to read. 
getConfScaling = function (model, svm.model, data, labels = NULL, kappa = 1, tau = 1) {
	confScaling = NULL
	
	# 2002 model with a single tau instead of a SV-dependent
	if (model == "2002") {
		cat ("--- 2002 model.\n")
	  confScaling = rep(0, nrow(data))
	  for (i in 1:nrow(data)) {
		  for (a in 1:nrow(svm.model$SV)) {
		    v = (data[i,] - svm.model$SV[a,]) 
		    n = -sum(v*v)/(2*tau^2)
		    confScaling[i] = confScaling[i] + exp(n)
		  }
		}
	}

	# 2002 model as speciied 
	if (model == "2002full") {
	  cat ("--- 2002 model.\n")
	  
	  tau = rep(0,nrow(svm.model$SV)) 
	  for (a in 1:nrow (svm.model$SV)) {
	    # find the nearest neighbours of a
	    M = 7 # M nearest ones
	    N = c() # neighbors
	    s = 0
	    for (i in N) {
	      v = (svm.model$SV[i,] - svm.model$SV[a,]) 
	      s = s + sum(v*v)
	    }
	    tau[a] = s/M
	  }
	  
	  confScaling = rep(0, nrow(data))
	  for (i in 1:nrow(data)) {
	    for (a in 1:nrow(svm.model$SV)) {
	      v = (data[i,] - svm.model$SV[a,]) 
	      n = -sum(v*v)/(2*tau[a]^2)
	      confScaling[i] = confScaling[i] + exp(n)
	    }
	  }
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

