
#clusterMatrix 



getClusterAnchestor = function (c, t) {
  # given t, it is enough to know to which cluster it will be assigned
  # on the lowest level. then we can deduce the rest from the cluster matrix,
  # as (theoretically?), the lower clusters' tree is known (it will only)
  # get merged with other clusters until its the big cluster.
  clusterMatrix = c (1, 7, 17, 32, 675)
  nc = clusterMatrix [which (clusterMatrix == c)+1]
  #print (nc)
#  if (is.na (nc)) {
#  }
  return (nc)
}


getModel = function (c) {
  #cat ("Get model for c")
#  model = paste0("model ", as.character (c))
  model = ""
  return (model)
}


getD = function (c, x) {
  MODEL = "williams"
  MODEL = "2002"
  
  if (MODEL == "williams") {
    getModel (c)
    expand = TRUE
    if (expand == TRUE) {
      s = paste0 ("\\exp(-", getf(c, x), x, ")")
    } else { 
      s = paste0 ("D^{", c, "}(", x, ")")
    }
  } 
  
  if (MODEL == "2002") {
    s = paste0 ("\\sum_{j \\in ", c, "} exp(- ||", x, "-s_j^{", c, "}||^2/\tau_i)")
  }
  return (s)
}


getK = function (c, x, y) {
  if (is.na(c) == TRUE) {
    s = paste0 ("K(", x, ",", y, ")")
    return (s)
  }

  nc = getClusterAnchestor (c, x)  
  d1 = getD (c, x)
  d2 = getD (c, y)
  Knc = getK (nc, x, y)
  s = paste0(d1, d2, Knc)
  return (s)
}


getf = function (c, x) {
  m = getModel (c)
  nc = getClusterAnchestor (c, x) 
  if (is.na(nc) == TRUE) {
    # ...
  }
  s = paste0 ("\\sum_{i \\in ", c, "} \\alpha_i^{", c, "}(")
  t = getK (nc, paste0("s_i^{", c, "}"), paste0 (x) )
  s = paste0 (s, t, ")")
  return (s)
}



clusterMatrix = c (1, 7, 17, 32, 675)
print (getf (1, "X") )
  






#' 
#' Predictions with Divide-Conquer Support Vector Machines
#' 
#' The function applies a model produced by the 
#'  \code{dcSVM} function to every row of a data matrix and returns the model predictions.
#' 
#' @param object Object of class "dcSVM", created by \code{dcSVM}.
#' @param newdata An n x p matrix containing the new input data. Could be a matrix or a sparse matrix object.
#' @param ... other parameters passing to \code{predict.svm}
#' 
#' @method predict dcSVM
#' 
#' @note confscaling is actually part of the model, so we do not need to pass it 
#' 
#' @export
#' 
confDCSVMTest = function(object, newdata,
                         probability = FALSE,
                         ...) {
  
  checkmate::assertClass(object, 'dcSVM')
  
  if (missing(newdata))
    return(fitted(object$svm))
  
  # assertMatrix(newdata, min.rows = 1)
  assertInt(nrow(newdata), lower = 1)
  assertInt(ncol(newdata), lower = 1)
  if (testClass(newdata, "data.frame"))
    newdata = data.matrix(newdata)
  scale.info = object$scale
  if (!testNull(scale.info$scale)) {
    assertInteger(scale.info$scale)
    newdata[, scale.info$scale] = scaleBySD(newdata[, scale.info$scale],
                                            scale.info$x.scale)
  }
  
  # Assign label
  if (object$early > 0) {
    new.result = rep(1, nrow(newdata))
    for (i in 1:object$early) {
      #i = object$early
      cluster.object.list = object$cluster.tree[[i]]
      new.label = rep(0,nrow(newdata))
      k = 0
      for (cid in 1:length(cluster.object.list)) {
        ind = which(new.result == cid)
        if (length(ind) > 0) {
          new.label[ind] = k + object$cluster.predict(newdata[ind,, drop = FALSE], 
                                                      cluster.object.list[[cid]])
          k = max(new.label[ind])
        }
      }
      new.result = new.label
    }
    # new.result = object$cluster.predict(newdata,object$cluster.object)
    k = max(new.result)
    preds = rep(0, nrow(newdata))
    for (i in 1:k) {
      ind = which(new.result == i)
      if (length(ind)>0) {
        # we need our confscaling first 
        
        confScaling = getConfScaling(confScalingModel, object$svm[[i]], newsvm.model$SV, kappa = kappa, tau = tau)
        testScaling = getConfScaling(confScalingModel, svm.model, test.x, kappa = kappa, tau = tau)
        
        preds[ind] = predict.alphasvm(object$svm[[i]], newdata[ind,, drop = FALSE], 
                                      confScaling = confscaling, 
                                      testScaling = testscaling, 
                                      ...)
      }
    }
  } else {
    preds = predict(object$svm, newdata, 
                    confScaling = confscaling, 
                    testScaling = testscaling, 
                    ...)
  }
  return(preds)
}
