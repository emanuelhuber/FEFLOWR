
#' Write mesh nodes at an ideal distance from a well
#' 
#' @param xy [\code{numeric(2)}] Well coordinates
#' @param r [\code{numeric(1)}] Well radius
#' @param n [\code{integer}] Number of points
#' @param fPath [\code{character(1)}] File names for exporting the points
# example
# xy <- c(651645, 225560)
# r <- 0.108
# n <- 6
# 
# xy_nodes <- wellMeshNodes(xy = c(651645, 225560), r = 0.108, n = 6)
# 
# plot(xy_nodes, col = "blue", asp = 1)
# points(t(xy), col = "red", pch = 20)
# 
# writeWellMeshNodes("test.pnt", xy = c(651645, 225560), r = 0.108, n = 6)

wellMeshNodes <- function(xy = c(0,0), r = 1 , n = 6){
  dist_factor <- exp(2*pi / (n * tan(pi/n)))
  node_dist <- dist_factor * r
  
  theta <- ((1:n) - 1 ) * 2 * pi / n
  
  xy_nodes <- matrix(nrow = n, ncol = 2)
  xy_nodes[, 1] <- xy[1] + node_dist * cos(theta)  
  xy_nodes[, 2] <- xy[2] + node_dist * sin(theta)  
  return(xy_nodes)
}

writeWellMeshNodes <- function(fPath, xy = c(0,0), r = 1 , n = 6){
  xy_nodes <- wellMeshNodes(xy = xy, r = r, n = n)
  cat(paste(1:n, xy_nodes[, 1], xy_nodes[, 2], 
            paste("\"nodes", 1:n, "\""), 
            collapse = "\n", sep = "\t"), 
      file = fPath, append = FALSE ,sep="\n")
  cat("END", file = fPath, append = TRUE, sep="\n" )
}
 


