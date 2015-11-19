
devtools::load_all(".")
#library(ConfSVM)
library(microbenchmark)


# do for all datasets



# load dataset
# load best SVM parameters from database

# do 5-CV: loop over all sigma/tau
#    

#X11()
	model = "williams"
	doPlot = FALSE
	
	# but we take here cost = 4 -->  0.94 = 0.06 error
	gamma = 2.73
	cost = 0.18
	
	# confscaling parameters
	kappa = 10
	tau = 0.1

	data.path = "~/lab/data/"
	
	set.seed(42)
	
	library(SVMBridge)

	dataset = "ijcnn1/ijcnn1.tr"
	data = readSparseData (file = file.path (data.path, dataset))
	train = list (x = data$X, y = as.factor(data$Y))
	train = scaleData (train, transformation = "LINEAR")
	train.x = train$x
	train.y = train$y
	writeSparseData (X = train.x, Y = train.y, file = "/home/drunkeneye/ijcnn.train")
	
	dataset = "ijcnn1/ijcnn1.t"
	data = readSparseData (file = file.path (data.path, dataset))
	test = list (x = data$X, y = as.factor(data$Y))
	test = scaleData (test, transformation = "LINEAR")
	test.x = test$x
	test.y = test$y
	writeSparseData  (X = test.x, Y = test.y, file = "/home/drunkeneye/ijcnn.test")
	
# 	print (head(train.x))
# 	print (head(test.x))
# 	stop ("A")
	
	cat ("### Testing ConfSVM.\n")
	cost = 2
	gamma = 2
	time.conf = microbenchmark (
	  confSVMTrain (confScalingModel = model, cost = cost, gamma = gamma, train.x = train.x, train.y = train.y,
		  test.x = test.x, test.y = test.y, kappa = kappa, tau = tau), 
	times = 1)

stop()	
	
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
	
