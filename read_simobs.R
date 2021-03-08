library(RColorBrewer )
library(signal)


fname  <- c("output_simobs.txt")
fname  <- c("output_simobs_sse3.txt")
fname  <- c("output_simobs_sse5.txt")
x <- read.table(fname, header = TRUE)

dim(x)


# split QL by profile
xL <- by(x, x$ID, function(x) x)
lab <- sapply(xL, function(x) x$Curve[1])
# xL[[1]]

n <- length(xL)

xlim <- range(x$X)
ylim <- range(x$Y)
lty <- c(rep(1, n/2), rep(3, n/2))
col <- rep(brewer.pal(n/2, "Set1"), 2)
col[2] <- "dodgerblue"
col[2 + n/2] <- col[2]

plot(0, type = "n", xlim = xlim, ylim = ylim, ylab = "Druckhöhe (m)", xlab = "Zeit (Tage)")
for(i in seq_along(xL)){
  lines(xL[[i]][,2:3], lty = lty[i], col = col[i], lwd = 2)
}
legend("bottomright", col = c(col[1:(n/2)], "black", "black"), 
       legend = c(lab[1:(n/2)], "beobachtet", "simuliert"), bty = "n", 
       lwd = rep(2, n/2), lty = c(rep(2, n/2), 3, 1))



#------------ relative drawdown
relH <- list()
for(i in seq_along(xL)){
  relH[[i]] <- xL[[i]][,2:3]
  relH[[i]][, 2] <- xL[[i]][,3] - xL[[i]][1,3]
  # res[[i]][, 2] <- xL[[n/2 + i]][,3] -  xL[[i]][1:nrow(res[[i]]),3] 
}
ylim <- range(sapply(relH, function(x) range(x[, 2], na.rm = TRUE)))
plot(0, type = "n", xlim = xlim, ylim = ylim, ylab = "Absenkung (m)", xlab = "Zeit (Tage)")
for(i in seq_along(relH)){
  lines(relH[[i]], lty = lty[i], col = col[i], lwd = 2)
}
legend("bottomright", col = c(col[1:(n/2)], "black", "black"), 
       legend = c(lab[1:(n/2)], "beobachtet", "simuliert"), bty = "n", 
       lwd = rep(2, n/2), lty = c(rep(2, n/2), 3, 1))



#------------ residuals (based on relativ drawdown)
res <- list()
for(i in 1:(n/2)){
  res[[i]] <- relH[[n/2 + i]]
  res[[i]][, 2] <- relH[[n/2 + i]][,2] -  relH[[i]][1:nrow(res[[i]]),2] 
  print(paste(lab[i], ": ", round(max(abs(res[[i]][,2]), na.rm = TRUE), 2)))
}

ylim <- range(sapply(res, function(x) range(x[, 2], na.rm = TRUE)))
plot(0, type = "n", xlim = xlim, ylim = ylim, ylab = "Residuen - Druckhöhe (m)", xlab = "Zeit (Tage)")
for(i in seq_along(res)){
  lines(res[[i]][,1:2], lty = lty[i], col = col[i])
}
legend("bottomright", col = c(col[1:(n/2)], "black", "black"), 
       legend = c(lab[1:(n/2)], "beobachtet", "simuliert"), bty = "n", 
       lwd = rep(2, n/2), lty = c(rep(2, n/2), 3, 1))


#------------ hist
xmax <- max(abs(range(sapply(res, function(x) range(x[,2])))))
breaks <- seq(- xmax, to = xmax, length.out = 20)
par(mfrow = c(1, 4))
for(i in seq_along(res)){
  hist(res[[i]][,2], main = lab[i],
       xlab = "Residuen (m)", xlim = c(-1, 1) * xmax, breaks = breaks)
}



#------------ Copper Jakob
xlim <- range(log10(relH[[i]][-1,1]))
ylim <- c(-0.05, 0.7)
plot(0, type = "n", xlim = xlim, ylim = ylim, ylab = "Absenkung (m)", xlab = "Zeit (Tage)")
for(i in seq_len(n/2)){
  lines(log10(relH[[i]][,1]), -relH[[i]][, 2], type = "o", pch = 20, col = col[i])
  lines(log10(relH[[i]][-1,1]), -diff(relH[[i]][, 2]))
}




for(i in 1:(n/2)){
  res_rel <- relH[[n/2 + i]][,2] -  relH[[i]][1:nrow(relH[[n/2 + i]]),2] 
  print(paste(lab[i]), ": ", max(abs(res_rel), na.rm = TRUE))
}

sapply(relH, function(x) max(abs(x), na.rm = TRUE))


#------------ hist
res_centered <- lapply(res, function(x) x[, 2] - mean(x[, 2]))
xmax <- max(abs(range(sapply(res_centered, function(x) range(x)))))
breaks <- seq(- xmax, to = xmax, length.out = 50)
par(mfrow = c(1, 4))
for(i in seq_along(res)){
  hist(res_centered[[i]], main = lab[i],
       xlab = "Residuen (m)", xlim = c(-1, 1) * max(abs(xlim)), breaks = breaks)
}
