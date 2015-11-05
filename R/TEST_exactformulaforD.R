
#clusterMatrix 

MODEL = "williams"
#MODEL = "2002"


#\usepackage{nath}
#\delimgrowth=1


getClusterAnchestor = function (c, t) {
  # given t, it is enough to know to which cluster it will be assigned
  # on the lowest level. then we can deduce the rest from the cluster matrix,
  # as (theoretically?), the lower clusters' tree is known (it will only)
  # get merged with other clusters until its the big cluster.
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



clusterMatrix = c (1, 7, 17, 32, 675, 1872, 11111)
MODEL = "williams"
print (getf (7, "X") )
cat ("\n===========\n\n")
MODEL = "2002"
print (getf (7, "X") )


