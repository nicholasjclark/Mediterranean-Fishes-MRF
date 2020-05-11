### Function to fit Generalised Additive Models (GAMs) to 
# assess relationships between SST and spatial coordinates with 
# predicted network modularity
fit_mod_gams = function(climate_data, modularity_data, n_cores = 2){
  library(dplyr)
  library(mgcv)
  library(visreg)
  
  #### Process supplied data into a dataframe for fitting GAMs ####
  cat('Building dataset for GAM modelling...\n')
  exponent <- function(a, pow) (abs(a)^pow)*sign(a)
  gam.dat <- climate_data %>%
    dplyr::group_by(Latitude,Longitude) %>%
    dplyr::mutate(Summer_SST = mean(July, August, September),
                  Winter_SST = mean(January, February, March)) %>%
    dplyr::ungroup() %>% 
    dplyr::select(Latitude, Longitude, Summer_SST, Winter_SST) %>%
    dplyr::bind_cols(data.frame(modularity = modularity_data)) %>%
    dplyr::mutate(modularity = exponent(modularity, pow = (1/3)))
  
  # Calculate a variogram on the raw data to inspect spatial autocorrelation
  data.var = data.frame(gam.dat)
  sp::coordinates(data.var) = ~Longitude+Latitude
  var.mod.raw <- variogram(modularity ~ Longitude+Latitude, data = data.var)
  var.fit.raw = fit.variogram(var.mod.raw, model = vgm("Sph"))
  
  #### Fit GAM using the default thin plate regression spline for 
  # the spatial term ####
  cat('Fitting GAM using thin plate spatial regression splines...\n')
  mod.gam <- mgcv::bam(modularity ~ s(Summer_SST, k = 50) + 
                          s(Winter_SST, k = 50) + 
                          s(Longitude, Latitude, k = 100), 
                        data = gam.dat, discrete = T)
  
  ##### Now fit GAMs using Gaussian process spatial splines (for comparison) ####
  cat('Fitting GAM using Gaussian Process spatial regression splines...\n')
  mod.gam.gp <- mgcv::bam(modularity ~ s(Summer_SST, k = 50) + 
                             s(Winter_SST, k = 50) + 
                             s(Longitude, Latitude, 
                               bs = 'gp', k = 100), 
                           data = gam.dat, discrete = T)

  ##### Use deviance scores to identify the best-fitting model 
  # for each metric ####
  if(which.min(c(deviance(mod.gam),
                 deviance(mod.gam.gp))) == 2){
    mod.best <- 'Gaussian Process'
  } else {
    mod.best <- 'Thin Plate'
  }
  cat('Best-fitting spatial spline is', 
      mod.best, 
      '...\n')
  
  # Remove the un-needed model to save memory
  if(mod.best == 'Gaussian Process'){
    best.mod.gam <- mod.gam.gp
    rm(mod.gam, mod.gam.gp)
  } else {
    best.mod.gam <- mod.gam
    rm(mod.gam, mod.gam.gp)
  }
  
  #### Process best-fitting models for each metric ####
  # Calculate spatial autocorrelation on the fitted model residuals
  data.var.gam = data.frame(resids = residuals(best.mod.gam),
                            Longitude = gam.dat$Longitude,
                            Latitude = gam.dat$Latitude)
  sp::coordinates(data.var.gam) = ~Longitude+Latitude
  var.mod <- variogram(resids ~ Longitude+Latitude, data = data.var.gam)
  var.fit = fit.variogram(var.mod, model = vgm("Sph"))
  
  # Nugget:sill ratio
  # Low ratio = large part of the variance introduced spatially, implying a strong spatial dependence
  # A high ratio often indicates weak spatial dependency.
  raw.spatial.autocorr <- var.fit.raw$psill[1] / sum(var.fit.raw$psill) # without the model
  resid.spatial.autocorr <- var.fit$psill[1] / sum(var.fit$psill) # after modelling
  
  # Extract model summaries
  mod.summary <- summary(best.mod.gam)
  
  # Check for possible concurvity issues
  cat('Calculating pairwise concurvity among predictors...\n')
  mod.concurv <- concurvity(best.mod.gam, full = FALSE)$estimate
  
  # Calculate partial effects of smooth variables for nicer plotting
  cat('Calculating smooth predictor partial effects...\n')
  
  # Function to extract partial effects for plotting
  extract_partials = function(gam_mod){
    plotdata <- visreg::visreg(gam_mod, 
                               type = "conditional", 
                               plot = FALSE)
    smooths <- plyr::ldply(plotdata, function(part)   
      data.frame(Variable = part$meta$x, 
                 x=part$fit[[part$meta$x]], 
                 smooth=part$fit$visregFit, 
                 lower=part$fit$visregLwr, 
                 upper=part$fit$visregUpr))
    
    residuals <- plyr::ldply(plotdata, function(part)
      data.frame(Variable = part$meta$x, 
                 x=part$res[[part$meta$x]], 
                 y=part$res$visregRes))
    
    # remove lat and long plots as they are less interpretable
    smooths %>%
      dplyr::mutate(char.Variable = as.character(Variable)) %>%
      dplyr::filter(char.Variable %in% c('Summer_SST','Winter_SST')) -> smooths
    
    smooths$char.Variable <- as.factor(smooths$Variable)
    levels(smooths$char.Variable) <- c("Summer SST",
                                               "Winter SST",
                                       "Latitude", "Longitude")
    residuals %>%
      dplyr::mutate(char.Variable = as.character(Variable)) %>%
      dplyr::filter(char.Variable %in% c('Summer_SST','Winter_SST')) -> residuals

    residuals$char.Variable <- as.factor(residuals$Variable)
    levels(residuals$char.Variable) <- c("Summer SST",
                                       "Winter SST",
                                       "Latitude", "Longitude")
    list(smooths = smooths, residuals = residuals)
      
  }
  
  mod.partials <- extract_partials(best.mod.gam)
  
  return(list(mod_gam_spatial = mod.best,
              mod_gam_summary = mod.summary,
              mod_gam_concurvity = mod.concurv,
              mod_gam_effects = mod.partials,
              mod_gam_model = best.mod.gam,
              raw_autocorrelation = raw.spatial.autocorr,
              modelled_autocorrelation = resid.spatial.autocorr))
}