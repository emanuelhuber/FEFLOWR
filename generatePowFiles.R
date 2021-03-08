library(units)
library(signal)
library(lubridate)

formatTS <- function(fName, x, xd, start_date, end_date, delta_t, 
                     x_unit = "l/min", out_unit = "m3/d", id = 1000,
                     id_name = "obs", append = FALSE, thatsall = TRUE){
  
  if(x_unit != out_unit){
    units(x) <-  as_units(x_unit)
    units(x) <- as_units(out_unit)
    x <- as.numeric(x)
  }
  
  tt <- seq(from = start_date, by = delta_t, to = end_date)
  
  x_tt <- signal::interp1(as.numeric(xd), x, as.numeric(tt), method = "linear")
  
  plot(tt, x_tt, type = "o", pch = 20, main = id_name)
  
  # x_tt_all <- apply(cbind(as.character(tt), x_tt), 1, paste)
  
  tt_rel <- (as.numeric(tt) - as.numeric(tt[1]))/69/60/24
  
  
  cat(paste0("#", id), file = fName, sep="\n", append = append)
  cat(paste0("! ", id_name), file = fName, append = TRUE, sep="\n" )	
  cat(paste0("#", id), file = fName, append = TRUE, sep="\n")	
  cat(paste(as.character(tt_rel), x_tt, collapse = "\n"), file = fName, append = TRUE ,sep="\n")	
  cat("END", file = fName, append = TRUE, sep="\n" )	
  if(thatsall) cat("END", file = fName, append = TRUE, sep="\n" )	
}

writeObsPow <- function(X, fName = "obs.pow", id = 2000, start_date, end_date, delta_t,
                        idate = c(1,1,1,1), ival = c(4, 2, 2, 2)){
  append <- FALSE
  for(i in seq_along(X)){
    formatTS(fName = fName,
             x = X[[i]][, ival[i]],
             xd = lubridate::dmy_hm(X[[i]][, idate[i]]),
             start_date = start_date,
             end_date  = end_date,
             delta_t = delta_t,
             x_unit = "m",
             out_unit = "m",
             id = id + i - 1,
             id_name = names(X)[i],
             append = append,
             thatsall = FALSE)
    
    append <- TRUE
  }
  cat("END", file = fName, append = TRUE, sep="\n" )	
}

setwd("P:/Projekte/23 Horw/2020/2320 297 Schenkon, GWF Zellfeld West/50_Auswertung/Berechnungen und Modellierungen/Grundwassermodellierung/01_Phase_Modell_Verifizierung")


FILES <- c("RB_05_20", "RB_06_20", "RB_07_20", "RB_08_20")
X <- lapply(file.path( "pumpversuch", paste0(FILES, ".csv")), read.csv, sep = ";")
names(X) <- FILES

writeObsPow(X, 
            id = 2005,
            start_date = lubridate::dmy_hm("25.11.2020 09:45"),
            end_date  = lubridate::dmy_hm("02.12.2020 12:00"),
            delta_t = 15 * 60, 
            idate = c(1,1,1,1), 
            ival = c(5, 2, 2, 2))



# parameter for the time-series interpolation
start_date <- lubridate::dmy_hm("25.11.2020 09:45")
end_date  <- lubridate::dmy_hm("02.12.2020 12:00")
delta_t <- 15 * 60   # 15 min

# data unit
x_unit <- "l/min"

x <- read.csv(file = "pumpversuch/RB_05_20.csv", sep = ";")



formatTS(fName = "pumpingrate.pow",
         x = x[, 4],
         xd = lubridate::dmy_hm(x[,1]),
         start_date = lubridate::dmy_hm("25.11.2020 09:45"),
         end_date  = lubridate::dmy_hm("02.12.2020 12:00"),
         delta_t = 15 * 60,
         x_unit = "l/min",
         out_unit = "m3/d",
         id = 1000)

formatTS(fName = "obs_B_05_20.pow",
         x = x[, 5],
         xd = lubridate::dmy_hm(x[,1]),
         start_date = lubridate::dmy_hm("25.11.2020 09:45"),
         end_date  = lubridate::dmy_hm("02.12.2020 12:00"),
         delta_t = 15 * 60,
         x_unit = "m",
         out_unit = "m",
         id = 2005,
         id_name = "B_05_20")


x <- read.csv(file = "pumpversuch/RB_06_20.csv", sep = ";")[, c(1, 2)]

formatTS(fName = "obs_B_06_20.pow",
         x = x[, 2],
         xd = lubridate::dmy_hm(x[,1]),
         start_date = lubridate::dmy_hm("25.11.2020 09:45"),
         end_date  = lubridate::dmy_hm("02.12.2020 12:00"),
         delta_t = 15 * 60,
         x_unit = "m",
         out_unit = "m",
         id = 2006,
         id_name = "B_06_20")

x <- read.csv(file = "pumpversuch/RB_07_20.csv", sep = ";")[, c(1, 2)]

formatTS(fName = "obs_B_07_20.pow",
         x = x[, 2],
         xd = lubridate::dmy_hm(x[,1]),
         start_date = lubridate::dmy_hm("25.11.2020 09:45"),
         end_date  = lubridate::dmy_hm("02.12.2020 12:00"),
         delta_t = 15 * 60,
         x_unit = "m",
         out_unit = "m",
         id = 2007,
         id_name = "B_07_20")

x <- read.csv(file = "pumpversuch/RB_08_20.csv", sep = ";")[, c(1, 2)]

formatTS(fName = "obs_B_08_20.pow",
         x = x[, 2],
         xd = lubridate::dmy_hm(x[,1]),
         start_date = lubridate::dmy_hm("25.11.2020 09:45"),
         end_date  = lubridate::dmy_hm("02.12.2020 12:00"),
         delta_t = 15 * 60,
         x_unit = "m",
         out_unit = "m",
         id = 2008,
         id_name = "B_08_20")


# xs <- xts::xts(x[,2], order.by = date_time)
# plot(xs)
h <- x[, 2]

units(h) <-  as_units(x_unit)

h <- as.numeric(set_units(h, "m3/d"))

tt <- seq(from = start_date, by = delta_t, to = end_date)

h_tt <- signal::interp1(as.numeric(date_time), h, as.numeric(tt), method = "pchip")

plot(tt, h_tt, type = "o", pch = 20)

h_tt_all <- apply(cbind(as.character(tt), h_tt), 1, paste)

tt_rel <- (as.numeric(tt) - as.numeric(tt[1]))/69/60/24


cat("#1000", file = "pumpingrate.pow",sep="\n")
cat("! RB 05/20", file = "pumpingrate.pow", append = TRUE, sep="\n" )	
cat("#1000", file = "pumpingrate.pow", append = TRUE, sep="\n")	
cat(paste(as.character(tt_rel), h_tt, collapse = "\n"), file = "pumpingrate.pow", append = TRUE ,sep="\n")	
cat("END", file = "pumpingrate.pow", append = TRUE, sep="\n" )	
cat("END", file = "pumpingrate.pow", append = TRUE, sep="\n" )	



?cat


xy <- c(651645, 225560)
well_radius <- 0.108
n_nodes <- 6
dist_factor <- exp(2*pi / (n_nodes * tan(pi/n_nodes)))
node_dist <- dist_factor * well_radius

theta <- ((1:n_nodes) - 1 ) * 2 * pi / n_nodes

xy_nodes <- matrix(nrow = n_nodes, ncol = 2)
xy_nodes[, 1] <- xy[1] + node_dist * cos(theta)  
xy_nodes[, 2] <- xy[2] + node_dist * sin(theta)  

plot(xy_nodes, col = "blue")
points(t(xy), col = "red", pch = 20)


cat(paste(1:n_nodes, xy_nodes[, 1], xy_nodes[, 2], paste("\"nodes", 1:n_nodes, "\""), collapse = "\n", sep = "\t"), 
    file = "well_additional_nodes.pnt", append = FALSE ,sep="\n")
cat("END", file = "well_additional_nodes.pnt", append = TRUE, sep="\n" )

