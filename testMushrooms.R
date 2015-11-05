
devtools::load_all(".")
#library(ConfSVM)
library(microbenchmark)

#X11()
	model = "williams"
	doPlot = FALSE
	
	# but we take here cost = 4 -->  0.94 = 0.06 error
	gamma = 2.73
	cost = 0.18
	
	# confscaling parameters
	kappa = 10
	tau = 0.1

	set.seed(42)
	
	library(SVMBridge)
	dataset = "protein"
	data = readSparseData (file = paste0("./data/", dataset, ".train"))
	train.x = data$X
	train.y = as.factor(data$Y)
	
	data = readSparseData (file = paste0("./data/", dataset, ".test"))
	test.x = data$X
	test.y = as.factor(data$Y)
	
	test.x = test.x [1:5000,]
	test.y = test.y [1:5000]
	train.x = train.x [1:5000,]
	train.y = train.y [1:5000]

if (1 == 0) {	
	cat ("### Testing ConfSVM.\n")
	time.conf = microbenchmark (
	  confSVMTrain (confScalingModel = model, gamma = gamma, train.x = train.x, train.y = train.y,
		  test.x = test.x, test.y = test.y, kappa = kappa, tau = tau), 
  times = 1)
}
	cat ("### Testing DCSVM with 4 levels.\n")
	time.confDC = microbenchmark (
	  confDCSVMTrain (confScalingModel = model, gamma = gamma, x = train.x, y = train.y, m = 5000,
	  	valid.x = test.x, valid.y = test.y, kappa = kappa, tau = tau,
		  pre.scale = FALSE, k = 6, max.levels = 2, early = 2),
	times = 1)

	print (time.conf)
	print (time.confDC)
	
	stop ()
	
	cat ("### Testing DCSVM with 4 levels and early stopping.\n")
	confDCSVMTrain (model = model, gamma = gamma, x = train.x, y = train.y, m = 1000,
		test.x = test.x, test.y = test.y, kappa = kappa, tau = tau,
		pre.scale = FALSE, k = 10, max.levels = 1, early = 0)	
	
