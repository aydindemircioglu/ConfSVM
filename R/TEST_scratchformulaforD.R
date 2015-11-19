
# instead of defining $K^i = D_i D_i K^{i-1}$
# we define here $K^i = D_i D_i K$. So we actually
# believe that $D^i$ on the upper level already 
# make things better, so the $D^i$ are not 'updating'
# the kernel, but are being updated. We do not
# modify the kernel in each step, but the modification.
# this makes things more computable.

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


getD = function (c, x, mode = "SCRATCH", model = "2002") {
  
  s = paste0 ("D^{", c, "}(", x, ")")
  return (s)
  if (model == "williams") {
    getModel (c)
    expand = TRUE
    if (expand == TRUE) {
      s = paste0 ("\\exp(-", getf(c, x, mode = mode, model = model), x, ")")
    } else { 
      s = paste0 ("D^{", c, "}(", x, ")")
    }
  } 
  
  if (model == "2002") {
    s = paste0 ("\\sum_{j \\in ", c, "} exp(- ||", x, "-s_j^{", c, "}||^2 \\tau_i)")
  }
  return (s)
}


getK = function (c, x, y, mode = "SCRATCH", model = "2002") {
  if (is.na(c) == TRUE) {
    s = paste0 ("K(", x, ",", y, ")")
    return (s)
  }
  
  nc = getClusterAnchestor (c, x)  
  d1 = getD (c, x, mode = mode, model = model)
  d2 = getD (c, y, mode = mode, model = model)
  if (mode == "SCRATCH") {
    Knc = paste0 ("K(", x, ",", y, ")")
  } else {
    Knc = getK (nc, x, y, mode = mode, model = model)
  }
  s = paste0(d1, d2, Knc)
  return (s)
}


getf = function (c, x, mode = "SCRATCH", model = "2002") {
  m = getModel (c)
  nc = getClusterAnchestor (c, x) 
  if (is.na(nc) == TRUE) {
    # ...
  }
  s = paste0 ("\\sum_{i \\in ", c, "} \\alpha_i^{", c, "}(")
  t = getK (nc, paste0("s_i^{", c, "}"), paste0 (x), mode = mode, model = model )
  s = paste0 (s, t, ")")
  return (s)
}



clusterMatrix = c (1, 7, 17, 32, 675, 1872, 11111)
cat ("\n=========== williams FULL ==== \n\n")
cat ("$$", getf (1, "X", mode = "FULL", model = "williams"), "$$" )
cat ("\n=========== williams SCRATCH ==== \n\n")
cat ("$$", getf (1, "X", mode = "SCRATCH", model = "williams"), "$$" )

cat ("\n\n=========== 2002 FULL ==== \n")
cat ("$$", getf (1, "X", mode = "FULL", model = "2002"), "$$" )
cat ("\n\n=========== 2002 SCRATCH ==== \n")
cat ("$$", getf (1, "X", mode = "SCRATCH", model = "2002"), "$$" )


