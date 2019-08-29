message('  <<< LOADING PREDICTION UTILITIES >>> ')

library(raster)
library(dplyr)
library(reshape2)

##############################################################################
## Define input files and directories

DATA.DIR <- file.path("data")

PREDICTION.DATA.DIR <- file.path(DATA.DIR, "prediction")

COVARIATES.LIST <- c("underweight_2017", "bsgmi_2018", "dirtyfuel_2017", 
                     "kpk", "sindh", "punjab", "balochistan", 
                     "ajk", "islamabad", "fana")

SHAPE.ADM1.FILE.NAME <- file.path(DATA.DIR, "shapefiles", "alhasan-pk-adm-1-with-clusters.shp")

## POP.DIR Directory where all population rasters are located. This directory 
##     is expected to have files `pop_<sex>_<age>_2018.tif` for:
##     - `<sex>` in [`f`, `m`] and 
##     - `age` in [`15`,`20`,...,`80`]
POP.DIR = file.path(PREDICTION.DATA.DIR, "population_5km")

##############################################################################
## Build the prediction frame

build.prediction.frame <- function() {
    message("| Build the prediction frame")
    
    message("|--| Step 1/5: Load the age-sex frequencies from population rasters")
    
    raster.stack.normed <- raster::stack(file.path(POP.DIR, "pop_all_2018"))
    
    message("|--| Step 2/5: Format the raster stack as a data.frame and aggregate strata")
    
    ## Format raster stack as data.frame
    pred.df <- as.data.frame(raster.stack.normed)
    
    ## Add the coordinates
    pred.df$LONGITUDE <- coordinates(raster.stack.normed)[,1]
    pred.df$LATITUDE <- coordinates(raster.stack.normed)[,2]
    
    pred.df[['female']] <- 0
    for (age in seq(15,80,5)) {
        pred.df[['female']] <- pred.df[['female']] + pred.df[[sprintf("pop_f_%s_2018", age)]]
        
        group_name <- sprintf("age_%d%s", age, ifelse(age==80, "up", age+4))
        pred.df[[group_name]] <- 0
        for (sex in c("f", "m")) {
            name <- sprintf("pop_%s_%s_2018", sex, age)
            pred.df[[group_name]] <- pred.df[[group_name]] + pred.df[[name]]
        }
    }
    
    message("|--| Step 3/5: Add the covariates")
    
    ## Set the covariate directory name
    ## `cov.dir` Directory where all covariates raster are located.
    ## Each raster is expected to be named as `<cov.name>.tif` where 
    ## <cov.name> is the name of a covariate (e.g. povMPI, ...)
    cov.dir = file.path(PREDICTION.DATA.DIR, "covariates_5km")
    
    ## Add covariates
    for(cov.name in COVARIATES.LIST) {
        if ((cov.name != 'age') & (cov.name != 'sex_m')) {
            
            raster.path <- file.path(cov.dir, sprintf("%s.tif", cov.name))
            
            message(sprintf("|--|--| Add covariate `%s` from raster `%s`", cov.name, raster.path))
            
            cov.raster <- raster(raster.path)
            
            ## Sanity check
            if(!all(coordinates(cov.raster) == coordinates(raster.stack.normed))){
                stop(sprintf("Coordinates of covariate `%s` do not match the population rasters !!", cov.name))
            }
            
            pred.df[[cov.name]] <- unlist(as.data.frame(cov.raster[[cov.name]]))
        }
    }
    
    message("|--| Step 4/5: Add the total population")
    
    for (year in c(2010, 2018)) {
        name <- sprintf("pop_tot_%s", year)
        
        pop.tot.raster <- raster(file.path(POP.DIR, sprintf("%s.tif", name)))
        
        ## Sanity check
        if(!all(coordinates(pop.tot.raster) == coordinates(raster.stack.normed))){
            stop("Coordinates of total population raster do not match the population rasters !!")
        }
        
        pred.df[[name]] <- unlist(as.data.frame(pop.tot.raster[[name]]))
    }
    
    
    message("|--| Step 5/5: Filter out rows outside of country borders")
    
    ## Filter out rows with NA values (this may happen at country boundary in covariate rasters)
    row.has.na <- apply(pred.df, 1, function(x) any(is.na(x)))
    pred.df <- pred.df[!row.has.na,]
    
    return(pred.df)
}
