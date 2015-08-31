library(ConfSVM)
context("confSVM")


test_that ("confSVM works as expected when no conf scaling is provided.") 
{
	# first round of computations, can/will use the normal SVM
	svm.model = alphasvm(x = train.x, y = train.y, gamma = gamma, 
						cost = cost, probability = FALSE, type = "C-classification")
	scores = predict(svm.model, test.x, decision.values = TRUE)
	accuracy = sum(diag(table(scores, test.y)))/length(test.y)
#	print(accuracy)

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
#	print(accuracy)

	expect_equal (accuracy, caccuracy) 
# 	if (abs(accuracy - caccuracy) >= 1/length(test.y)) {
# 		stop ("ConfSVM does not work as expected")
# 	}
}
