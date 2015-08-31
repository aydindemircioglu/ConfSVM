
#' generate sinus data
#'
#' @export
generateSinusData = function (N = 1000) {
  x = matrix( runif(2*N, min = -1, max = 1), ncol = 2)
  y = as.numeric(x[,2] < sin (pi*x[,1]))
  y = 2*y - 1
  return (list(x = x, y = y))
}


#' generate and plot sinus data
#'
#' @export
plotSinusData = function (d) {
  library(ggplot2)
  
  d = generateSinusData (1000)
  data = data.frame ( x = d$x[,1], y = d$x[,2], c = d$y)
  
  normalPlot = ggplot() + 
    xlab ("x") +
    ylab ("y") +
    ggtitle("sinus data") +   
    layer(
      data = data, 
      mapping = aes(x = x, y = y, colour = c),
      geom="point", 
      geom_params=list (size = 4)#, pch = 16) 
    )    
  
  print (normalPlot)
}
