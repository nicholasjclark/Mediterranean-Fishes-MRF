### Function to fit Generalised Additive Models (GAMs) to 
# assess relationships between SST and spatial coordinates with 
# predicted functional and phylogenetic diversity metrics
fit_div_gams = function(climate_data, phylo_div, functional_div, n_cores = 2, thin_plate = FALSE){
  library(dplyr)
  library(mgcv)
  library(ggplot2)
  
  #### Process supplied data into a dataframe for fitting GAMs ####
  cat('Building dataset for GAM modelling...\n')
  gam.dat <- climate_data %>%
    dplyr::group_by(Latitude,Longitude) %>%
    dplyr::mutate(Summer_SST = mean(July, August, September),
                  Winter_SST = mean(January, February, March)) %>%
    dplyr::ungroup() %>% 
    dplyr::select(Latitude, Longitude, Summer_SST, Winter_SST) %>%
    dplyr::bind_cols(data.frame(pdiv = phylo_div,
                                fdiv = functional_div))

  # Calculate a variogram on the raw functional diversity data to inspect spatial autocorrelation
  data.var = data.frame(gam.dat)
  sp::coordinates(data.var) = ~Longitude+Latitude
  var.mod.raw <- variogram(functional_div ~ Longitude+Latitude, data = data.var)
  var.fit.raw = fit.variogram(var.mod.raw, model = vgm("Sph"))
  
  if(thin_plate){
  #### Fit GAMs using the default thin plate regression spline for 
  # the spatial term ####
  cat('Fitting GAMs using thin plate spatial regression splines...\n')
  fdiv.gam <- mgcv::bam(fdiv ~ s(Summer_SST, k = 50) + 
                          s(Winter_SST, k = 50) + 
                          s(Longitude, Latitude, k = 100), 
                        data = gam.dat, discrete = T)
  
  pdiv.gam <- mgcv::bam(pdiv ~ s(Summer_SST, k = 50) + 
                          s(Winter_SST, k = 50) + 
                          s(Longitude, Latitude, k = 100), 
                        data = gam.dat, discrete = T)
}
  ##### Now fit GAMs using Gaussin process spatial splines (for comparison) ####
  cat('Fitting GAMs using Gaussian Process spatial regression splines...\n')
  fdiv.gam.gp <- mgcv::bam(fdiv ~ s(Summer_SST, k = 50) + 
                          s(Winter_SST, k = 50) + 
                          s(Longitude, Latitude, 
                            bs = 'gp', k = 100), 
                        data = gam.dat, discrete = T)
  
  pdiv.gam.gp <- mgcv::bam(pdiv ~ s(Summer_SST, k = 50) + 
                             s(Winter_SST, k = 50) + 
                             s(Longitude, Latitude, 
                               bs = 'gp', k = 100), 
                           data = gam.dat, discrete = T)
  
  ##### Use deviance scores to identify the best-fitting model 
  # for each metric ####
  if(thin_plate){
  if(which.min(c(deviance(fdiv.gam),
              deviance(fdiv.gam.gp))) == 2){
    fdiv.best <- 'Gaussian Process'
  } else {
    fdiv.best <- 'Thin Plate'
  }
  cat('Best-fitting spatial spline for functional diversity is', 
      fdiv.best, 
      '...\n')

  # Remove the un-needed model to save memory
  if(fdiv.best == 'Gaussian Process'){
    best.fdiv.gam <- fdiv.gam.gp
    rm(fdiv.gam, fdiv.gam.gp)
  } else {
    best.fdiv.gam <- fdiv.gam
    rm(fdiv.gam, fdiv.gam.gp)
  }
  
  if(which.min(c(deviance(pdiv.gam),
                 deviance(pdiv.gam.gp))) == 2){
    pdiv.best <- 'Gaussian Process'
  } else {
    pdiv.best <- 'Thin Plate'
  }
  
  cat('Best-fitting spatial spline for phylogenetic diversity is', 
      pdiv.best, 
      '...\n')
  
  if(pdiv.best == 'Gaussian Process'){
    best.pdiv.gam <- pdiv.gam.gp
    rm(pdiv.gam, pdiv.gam.gp)
  } else {
    best.pdiv.gam <- pdiv.gam
    rm(pdiv.gam, pdiv.gam.gp)
  }
  
  } else {
    best.fdiv.gam <- fdiv.gam.gp
    best.pdiv.gam <- pdiv.gam.gp
  }
  
  #### Process best-fitting models for each metric ####
  # Calculate spatial autocorrelation on the fitted fdiv model residuals
  data.var.gam = data.frame(resids = residuals(best.fdiv.gam),
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
  fdiv.summary <- summary(best.fdiv.gam)
  pdiv.summary <- summary(best.pdiv.gam)
  
  # Check for possible concurvity issues
  cat('Calculating pairwise concurvity among predictors...\n')
  fdiv.concurv <- concurvity(best.fdiv.gam, full = FALSE)$estimate
  pdiv.concurv <- concurvity(best.pdiv.gam, full = FALSE)$estimate
  
  # Calculate partial effects of smooth variables for nicer plotting
  cat('Calculating smooth predictor partial effects...\n')
  
  # Function to format x axis in degrees
  scale_x_degrees <- function(xmin = 8, xmax = 30, step = 1, ...) {
    xbreaks <- seq(xmin, xmax, step)
    xlabels <- unlist(lapply(xbreaks, function(x) parse(text = paste0(x,"^o", "*C")), x))
    return(scale_x_continuous("", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
  }
  
  # Function to extract partial effects for plotting
  extract_partials = function(gam_mod, div_metric){
    plotdata <- visreg::visreg(gam_mod, 
                               type = "conditional", 
                               plot = FALSE)
    smooths <- plyr::ldply(plotdata, function(part)   
      data.frame(Variable = part$meta$x, 
                 x = part$fit[[part$meta$x]], 
                 smooth = part$fit$visregFit, 
                 lower = part$fit$visregLwr, 
                 upper = part$fit$visregUpr))
    
    residuals <- plyr::ldply(plotdata, function(part)
      data.frame(Variable = part$meta$x, 
                 x = part$res[[part$meta$x]], 
                 y = part$res$visregRes))
    
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
    
    list(smooths = smooths,
         residuals = residuals)
    }

  fdiv.partials <- extract_partials(gam_mod = best.fdiv.gam, div_metric = 'fdiv')
  pdiv.partials <- extract_partials(best.pdiv.gam, 'pdiv')
  
  return(list(Fdiv_gam_summary = fdiv.summary,
              Fdiv_gam_concurvity = fdiv.concurv,
              Fdiv_gam_effects = fdiv.partials,
              Fdiv_gam_model = best.fdiv.gam,
              fdiv_raw_autocorrelation = raw.spatial.autocorr,
              fdiv_modelled_autocorrelation = resid.spatial.autocorr,
              Pdiv_gam_summary = pdiv.summary,
              Pdiv_gam_concurvity = pdiv.concurv,
              Pdiv_gam_effects = pdiv.partials,
              Pdiv_gam_model = best.pdiv.gam))
}