	
	model = "1999"
	doPlot = FALSE
	
	N = 100

	# but we take here cost = 4 -->  0.94 = 0.06 error
	gamma = 3.125
	cost = 1
	
	# confscaling parameters
	kappa = 0.11
	tau = 0.25

	set.seed(43)

	# generate data
	d = generateSinusData(N)
	train.x = d$x
	train.y = as.factor(d$y)

	d = generateSinusData(N)
	test.x = d$x
	test.y = as.factor(d$y)

	confSVMTrain (model = model, gamma = gamma, train.x = train.x, train.y = train.y,
		test.x = test.x, test.y = test.y)
	
