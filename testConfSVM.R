
library(ConfSVM)

	model = "1999"
	doPlot = FALSE
	
	N = 100

	# but we take here cost = 4 -->  0.94 = 0.06 error
	gamma = 3.125
	cost = 1
	
	# confscaling parameters
	kappa = 0.11
	tau = 0.4

	set.seed(43)
	
	### covtype
	library(SVMBridge)
	covtype = readSparseData (file = "./tmp/codrna")
	covtype$X = covtype$X[1:50000,]
	covtype$Y = covtype$Y[1:50000,]

	trainInd =  sample (seq_len(nrow(covtype$X)), size = floor (0.75*nrow(covtype$X)))

	train.x = covtype$X[trainInd,]
	train.y = as.factor(covtype$Y[trainInd])
	
	test.x = covtype$X[-trainInd,]
	test.y = as.factor(covtype$Y[-trainInd])

	confSVMTrain (model = model, gamma = gamma, train.x = train.x, train.y = train.y,
		test.x = test.x, test.y = test.y, kappa = kappa, tau = tau)
	
