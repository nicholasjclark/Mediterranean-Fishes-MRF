#### Function to map model predictions for past and future projections
plot_composite_map = function(coords, past_obs, future_obs, 
                              metric = 'richness'){
  library(ggplot2)
  library(viridis)
  
  # Organise coordinates, past (1980) and future (2040) predictions into a dataframe
  points.dat <- coords
  points.dat$Past <- as.vector(past_obs)
  if(metric == 'richness'){
    future_obs[future_obs < 0] <- 0
  }
  points.dat$Future <- as.vector(future_obs)
  
  # Download a world map and crop to the Mediterranean region
  med_map <- map_data("world", xlim = c(-10, 50), ylim = c(25, 45))
  
  # General mapping theme
  obs_legend_theme = function(){ theme(legend.title = element_text(size = 6.5,
                                                                   hjust = 0.5),
                                       legend.text = element_text(size = 6.25),
                                       #legend.key.size = unit(0.3, "cm"),
                                       legend.key.size = unit(0.2, 'cm'),
                                       legend.justification = c(1, 1), 
                                       legend.position = c(0.99, 0.99),
                                       legend.key = element_rect(fill = "gray85"),
                                       legend.background = element_rect(fill = 'gray85', 
                                                                        colour = 'gray85',
                                                                        size = 0.05),
                                       legend.box.background = element_rect(colour = "gray85",
                                                                            size = 0.05),
                                       legend.margin = margin(-0.01, 0.04, 0.02, 0.04,
                                                              unit = 'cm'),
                                       plot.margin = margin(1, 0.5, -12.5, -8),
                                       axis.text.x = element_text(size = 5.5),
                                       axis.text.y = element_text(size = 5.5,
                                                                  angle = 90,
                                                                  vjust = 0,
                                                                  hjust = 0.7),
                                       panel.background = element_rect(fill = 'slategray1',
                                                                       colour = 'black'))}
  
  # Functions to format axis labels
  scale_x_longitude <- function(xmin=-180, xmax=180, step=1, ...) {
    xbreaks <- seq(xmin,xmax,step)
    xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(-x,"^o", "*W")), 
                                                         ifelse(x > 0, parse(text=paste0(x,"^o", "*E")),x))))
    return(scale_x_continuous("", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
  }
  
  scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
    ybreaks <- seq(ymin,ymax,step)
    ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(-x,"^o", "*S")), 
                                                         ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))))
    return(scale_y_continuous("", breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
  } 
  
  # Breaks for background rectangles
  rects <- data.frame(xstart = c(-6, 26.5), xend = c(3, 36.25), 
                      ystart = c(42, 39), yend = c(45.75, 45.75))
  
  # Prepare the datasets by cutting predictions into breaks
  if(metric == 'richness'){
    # Create a raster for plotting
    empty_raster <- raster::raster(nrow = 105, ncols = 275,  
                                   ext = raster::extent(range(points.dat$Longitude),
                                                        range(points.dat$Latitude)))
    spdf <- sp::SpatialPointsDataFrame(coords = points.dat %>%
                                         dplyr::select(Longitude, Latitude), 
                                       data = points.dat,
                                       proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
   
    # Plot future change projections
    points.dat$Change <- points.dat$Future - points.dat$Past
    sst_raster <- raster::rasterize(spdf, empty_raster, points.dat$Change, mean)
    
    sst_raster_spdf <- as(sst_raster, "SpatialPixelsDataFrame")
    sst_raster_df <- as.data.frame(sst_raster_spdf)
    colnames(sst_raster_df) <- c("Change", "Longitude", "Latitude")
    
    sst_raster_df$Col_break_change <- cut(sst_raster_df$Change, 
                                          breaks = c(-100, -20, -5, 5, 20,
                                                     100),
                                          labels = c('- >20','- 5-20',
                                                     'no change', 
                                                     '+ 5-20',
                                                     '+ >20'))
    cols <- pals::kovesi.diverging_linear_bjr_30_55_c53(5)
    names(cols) <- c('- >20','- 5-20',
                     'no change', 
                     '+ 5-20',
                     '+ >20')
    
    # Plot predicted change in richness using diverging colour scheme
    change_map <- ggplot() + coord_fixed() + xlab("") + ylab("") + 
      theme_bw() +
      geom_tile(data = sst_raster_df, aes(fill = Col_break_change,
                                          x = Longitude, y = Latitude),
                size = 0.85) +
      geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                                  ymin = ystart, ymax = yend),
                fill = 'gray85', colour = 'gray85') +
      geom_polygon(data = med_map, aes(x = long, y = lat, 
                                       group = group), 
                   colour = "gray85", fill = "gray85",
                   size = 0.25) +
      scale_fill_manual(name = expression(paste(Delta~Richness)),
                         values = cols, drop = FALSE) +
      theme(axis.text = element_text(size = 7)) +
      scale_x_longitude(xmin = -3, xmax = 35, step = 7) +
      scale_y_latitude(ymin = 30, ymax = 49, step = 5) +
      labs(x = "", y = "") +
      coord_map(projection = "rectangular", lat0 = 0,
                xlim = c(-6, 36.25), ylim = c(30, 45.75)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      obs_legend_theme() + 
      guides(fill = guide_legend(nrow = 5, reverse = T, title.vjust = 0,
                                title.hjust = 1,
                                override.aes = list(size = 1.3,
                                                    alpha = 1)))
    
  } 
  if(metric == 'temp'){
    empty_raster <- raster::raster(nrow = 105, ncols = 275,  
                                   ext = raster::extent(range(points.dat$Longitude),
                                                        range(points.dat$Latitude)))
    spdf <- sp::SpatialPointsDataFrame(coords = points.dat %>%
                                         dplyr::select(Longitude, Latitude), 
                                       data = points.dat,
                                       proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    points.dat$Change <- points.dat$Future - points.dat$Past
    sst_raster <- raster::rasterize(spdf, empty_raster, points.dat$Change, mean)
    
    sst_raster_spdf <- as(sst_raster, "SpatialPixelsDataFrame")
    sst_raster_df <- as.data.frame(sst_raster_spdf)
    colnames(sst_raster_df) <- c("Change", "Longitude", "Latitude")
    
    sst_raster_df$Col_break_change <- cut(sst_raster_df$Change, 
                                          breaks = c(min(sst_raster_df$Change) - 1,
                                                     0.25, 0.75, 1.25, 1.75,
                                                     max(sst_raster_df$Change)),
                                          labels = c('<0.25','0.25-0.75', '0.76-1.25', 
                                                     '1.26-1.75','>1.75'))
    darkred <- pals::brewer.reds(10)[10]
    cols <- c('white', pals::brewer.ylorrd(6)[c(2,4,5)], darkred)
    names(cols) <- c('<0.25','0.25-0.75', '0.76-1.25', 
                     '1.26-1.75','>1.75')

    change_map <- ggplot() + coord_fixed() + xlab("") + ylab("") + 
      theme_bw() +
      geom_tile(data = sst_raster_df, aes(fill = Col_break_change,
                                          x = Longitude, y = Latitude),
                size = 0.85) +
      geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                                  ymin = ystart, ymax = yend),
                fill = 'gray85', colour = 'gray85') +
      geom_polygon(data = med_map, aes(x = long, y = lat, 
                                       group = group), 
                   colour = "gray85", fill = "gray85",
                   size = 0.25) +
      scale_fill_manual(name = expression(paste(Delta~SST, ~degree, 'C')),
                        values = cols, drop = FALSE) +
      theme(axis.text = element_text(size = 7)) +
      scale_x_longitude(xmin = -3, xmax = 35, step = 7) +
      scale_y_latitude(ymin = 30, ymax = 49, step = 5) +
      labs(x = "", y = "") +
      coord_map(projection = "rectangular", lat0 = 0,
                xlim = c(-6, 36.25), ylim = c(30, 45.75)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      obs_legend_theme() +
      guides(fill = guide_legend(nrow = 5, reverse = T, title.vjust = 0,
                                 title.hjust = 1,
                                 override.aes = list(size = 1.3,
                                                     alpha = 1)))
    
  }
  if(metric %in% c('diversity', 'modularity')){
    points.dat$Past.sc <- (points.dat$Past - mean(points.dat$Past)) / 
      sd(points.dat$Past)
    empty_raster <- raster::raster(nrow = 105, ncols = 275,  
                                   ext = raster::extent(range(points.dat$Longitude),
                                                        range(points.dat$Latitude)))
    spdf <- sp::SpatialPointsDataFrame(coords = points.dat %>%
                                         dplyr::select(Longitude, Latitude), 
                                       data = points.dat,
                                       proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    
    points.dat$Future.sc <- (points.dat$Future - mean(points.dat$Future)) / 
      sd(points.dat$Future)
    points.dat$Change.sc <- points.dat$Future.sc - points.dat$Past.sc
    sst_raster <- raster::rasterize(spdf, empty_raster, points.dat$Change.sc, mean)
    
    sst_raster_spdf <- as(sst_raster, "SpatialPixelsDataFrame")
    sst_raster_df <- as.data.frame(sst_raster_spdf)
    colnames(sst_raster_df) <- c("Change.sc", "Longitude", "Latitude")
    
    sst_raster_df$Col_break_change <- cut(sst_raster_df$Change.sc, 
                                          breaks = c(min(sst_raster_df$Change.sc) - 1,
                                                     -1, -0.25, 0.25, 1,
                                                     max(sst_raster_df$Change.sc)),
                                          labels = c('--','-', 'no change', 
                                                     '+','++    '))
    
    cols <- pals::kovesi.diverging_linear_bjr_30_55_c53(5)
    names(cols) <- c('--','-', 'no change', 
                     '+','++    ')
    
    if(metric == 'diversity'){
      change_map <- ggplot() + coord_fixed() + xlab("") + ylab("") + 
        theme_bw() +
        geom_tile(data = sst_raster_df, aes(fill = Col_break_change,
                                            x = Longitude, y = Latitude),
                  size = 0.85) +
        geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                                    ymin = ystart, ymax = yend),
                  fill = 'gray85', colour = 'gray85') +
        geom_polygon(data = med_map, aes(x = long, y = lat, 
                                         group = group), 
                     colour = "gray85", fill = "gray85",
                     size = 0.25) +
        scale_fill_manual(name = expression(paste(Delta~Diversity)),
                          values = cols, drop = FALSE) +
        theme(axis.text = element_text(size = 7)) +
        scale_x_longitude(xmin = -3, xmax = 35, step = 7) +
        scale_y_latitude(ymin = 30, ymax = 49, step = 5) +
        labs(x = "", y = "") +
        coord_map(projection = "rectangular", lat0 = 0,
                  xlim = c(-6, 36.25), ylim = c(30, 45.75)) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        obs_legend_theme() + 
        guides(fill = guide_legend(nrow = 5, reverse = T, title.vjust = 0,
                                   title.hjust = 1,
                                   override.aes = list(size = 1.3,
                                                       alpha = 1)))
    } else {
      change_map <- ggplot() + coord_fixed() + xlab("") + ylab("") + 
        theme_bw() +
        geom_tile(data = sst_raster_df, aes(fill = Col_break_change,
                                            x = Longitude, y = Latitude),
                  size = 0.85) +
        geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                                    ymin = ystart, ymax = yend),
                  fill = 'gray85', colour = 'gray85') +
        geom_polygon(data = med_map, aes(x = long, y = lat, 
                                         group = group), 
                     colour = "gray85", fill = "gray85",
                     size = 0.25) +
        scale_fill_manual(name = expression(paste(Delta~Modularity)),
                          values = cols, drop = FALSE) +
        theme(axis.text = element_text(size = 7)) +
        scale_x_longitude(xmin = -3, xmax = 35, step = 7) +
        scale_y_latitude(ymin = 30, ymax = 49, step = 5) +
        labs(x = "", y = "") +
        coord_map(projection = "rectangular", lat0 = 0,
                  xlim = c(-6, 36.25), ylim = c(30, 45.75)) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        obs_legend_theme() + 
        guides(fill = guide_legend(nrow = 5, reverse = T, title.vjust = 0,
                                   title.hjust = 1,
                                   override.aes = list(size = 1.3,
                                                       alpha = 1)))
    }
    
  } 
  
  return(change_map)
  
}





