# Load Packages 
library(sf)
library(ggplot2)
library(ggspatial)
library(dplyr)
library(rEDM)

load(file='~/BRITE/tentmap_del.rda')
load(file='~/BRITE/block_3sp.rda')


#testing the rEDM package 
data(tentmap_del)
str(tentmap_del)
data(block_3sp)
str(block_3sp)


ts <- tentmap_del
lib <- c(1, 100)
lib_b <- c(1, NROW(block_3sp))

pred <- c(201, 500)
pred_b <- c(1, NROW(block_3sp))

cols_b <- c(1,2,4)
target_b <- 1

simplex_output <- simplex(ts,lib,pred)
str(simplex_output)

par(mar = c(4,4,1,1), mgp = c(2.5,1,0)) # set up margins for plotting
plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", ylab = "Forecast Skill (rho)")


#simplex_output <- simplex(ts,lib,pred,E=2,tp=3)
#par(mar = c(4, 4, 1, 1))
#plot(simplex_output$tp, simplex_output$rho, type = "l", xlab = "Time to Prediction (tp)", ylab = "Forecast Skill (rho)")


smap_output <- s_map(ts, lib, pred, E = 2)
par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", 
     ylab = "Forecast Skill (rho)")

ts <- ts + rnorm(length(ts), sd = sd(ts) * 0.2)
smap_output <- s_map(ts, lib, pred, E = 2)
par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", 
     ylab = "Forecast Skill (rho)")

block_lnlp_output <- block_lnlp(block_3sp, lib = lib_b, pred = pred_b, columns = cols_b, 
                                target_column = target_b, stats_only = FALSE, first_column_time = TRUE)












# Processes: Shape file pulled from (http://www.biodiversity.bz/)
belize_waters <- st_read('~/BRITE/Belize_Nationalwaters/Belize_Nationalwaters.shp')
Plot_BC_csv <- read.csv('~/BRITE/Morrissette_et_al_2023_plots.csv')
Biomass_BC_csv <- read.csv('~/BRITE/BC_Biomass.csv')

# filter plot data for lat-long coords
Plot_coords <- Plot_BC_csv %>% select(core_id, latitude,longitude)

# fill biomass data with plot data lat-long coords
