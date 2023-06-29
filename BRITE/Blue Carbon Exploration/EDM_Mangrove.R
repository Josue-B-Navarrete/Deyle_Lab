## read in the data 
Morreissette_plot_csv <- read.csv('~/BRITE/Morrissette_et_al_2023_plots.csv')

## Make a date column 
Morreissette_plot_csv$date <- as.Date(paste(Morreissette_plot_csv$year,Morreissette_plot_csv$month,Morreissette_plot_csv$day, sep = "-"))
## Make df with height_x 
species_var <- Morreissette_plot_csv %>% select(date, salinity, ORP, tree_count)

## rename col
species_var <- species_var %>% rename("salinity_x_t" = "salinity", 
                                      "ORP_y_t" = "ORP",
                                      "tree_count_z_t"="tree_count")
# create lags for each varible/species
# N = 1
species_var$salinity_x_t_1 <- lag(species_var$salinity_x_t, n=1)
species_var$ORP_y_t_1 <- lag(species_var$ORP_y_t, n=1)
species_var$tree_count_z_t_1 <- lag(species_var$tree_count_z_t, n=1)
# N = 2
species_var$salinity_x_t_2 <- lag(species_var$salinity_x_t, n=2)
species_var$ORP_y_t_2 <- lag(species_var$ORP_y_t, n=2)
species_var$tree_count_z_t_2 <- lag(species_var$tree_count_z_t, n=2)

# rearrange cols make dataframe into tibble first 
species_var <- as_tibble(species_var)
species_var <- species_var[, c(1,2,5,8,3,6,9,4,7,10)]

# add a time col
species_var$date <- 1:nrow(species_var)
## rename date col
species_var <- species_var %>% rename("time" = "date")




