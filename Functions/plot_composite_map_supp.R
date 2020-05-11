#### Function to map model predictions for past and future projections
plot_composite_map_supp = function(coords, past_obs, future_obs, 
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
  obs_legend_theme = function(){ theme(legend.title = element_text(size = 0.1,
                                                                   hjust = 0.5),
                                       legend.title.align = 1,
                                       legend.text = element_text(size = 6),
                                       legend.key.size = unit(0.15, "cm"),
                                       legend.justification = c(1, 1), 
                                       legend.position = c(0.99, 0.975),
                                       legend.key = element_rect(fill = "gray85"),
                                       legend.background = element_rect(fill = 'gray85', 
                                                                        colour = 'gray85',
                                                                        size = 0.05),
                                       legend.box.background = element_rect(colour = "gray85",
                                                                            size = 0.05),
                                       legend.margin = margin(-0.01, 0.04, 0.02, 0.04,
                                                              unit = 'cm'),
                                       plot.margin = margin(1, 0.5, -16.5, -12.75),
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
    sst_raster <- raster::rasterize(spdf, empty_raster, points.dat$Past, mean)
    
    sst_raster_spdf <- as(sst_raster, "SpatialPixelsDataFrame")
    sst_raster_df <- as.data.frame(sst_raster_spdf)
    colnames(sst_raster_df) <- c("Past", "Longitude", "Latitude")

    sst_raster_df$Col_break <- cut(sst_raster_df$Past, 
                                breaks = c(-1, 40, 70, 90, 110,
                                           max(points.dat$Past)),
                                labels = c('<40','40-70',
                                           '71-90', 
                                           '91-110 ',
                                           '>110'))
    
    # Set colours
    cols <- rev(c(viridis::inferno(10)[c(5,7)],viridis::viridis(10)[c(10,8)],
                  viridis::plasma(5)[1]))
    names(cols) <- c('<40','40-70',
                     '71-90', 
                     '91-110 ',
                     '>110')
    
    # Create a map of the 1980 predictions using viridis colour scheme
    obs_map <- ggplot() + coord_fixed() + xlab("") + ylab("") + 
      theme_bw() +
      geom_tile(data = sst_raster_df, aes(fill = Col_break,
                                          x = Longitude, y = Latitude),
                size = 0.85) +
      #geom_point(data = points.dat, aes(x = Longitude, y = Latitude, 
                                       # color = Col_break),
                 #size = 0.15, alpha = I(0.7),
                 #shape = 15) +
      geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                                  ymin = ystart, ymax = yend),
                fill = 'gray85', colour = 'gray85') +
      geom_polygon(data = med_map, aes(x = long, y = lat, 
                                       group = group), 
                   colour = "gray85", fill = "gray85",
                   size = 0.25) +
      scale_fill_manual(values = cols, drop = FALSE, name = NULL) +
      theme(axis.text = element_text(size = 7)) +
      scale_x_longitude(xmin = -3, xmax = 35, step = 7) +
      scale_y_latitude(ymin = 30, ymax = 49, step = 5) +
      labs(x = "", y = "") +
      coord_map(projection = "rectangular", lat0 = 0,
                xlim = c(-6, 36.25), ylim = c(30, 45.75)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      obs_legend_theme() +
      guides(fill = guide_legend(nrow = 6, reverse = T,
                                title.vjust = 0,
                                override.aes = list(size = 1.3,
                                                   alpha = 1)))
    
    # Repeat for future projections
    empty_raster <- raster::raster(nrow = 105, ncols = 275,  
                                   ext = raster::extent(range(points.dat$Longitude),
                                                        range(points.dat$Latitude)))
    spdf <- sp::SpatialPointsDataFrame(coords = points.dat %>%
                                         dplyr::select(Longitude, Latitude), 
                                       data = points.dat,
                                       proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    sst_raster <- raster::rasterize(spdf, empty_raster, points.dat$Future, mean)
    
    sst_raster_spdf <- as(sst_raster, "SpatialPixelsDataFrame")
    sst_raster_df <- as.data.frame(sst_raster_spdf)
    colnames(sst_raster_df) <- c("Future", "Longitude", "Latitude")
    
    sst_raster_df$Col_break <- cut(sst_raster_df$Future, 
                                   breaks = c(-1, 40, 70, 90, 110,
                                              max(points.dat$Future)),
                                   labels = c('<40','40-70',
                                              '71-90', 
                                              '91-110 ',
                                              '>110'))
    
    # Set colours
    cols <- rev(c(viridis::inferno(10)[c(5,7)],viridis::viridis(10)[c(10,8)],
                  viridis::plasma(5)[1]))
    names(cols) <- c('<40','40-70',
                         '71-90', 
                         '91-110 ',
                         '>110')
    
    # Create a map of the 1980 predictions using viridis colour scheme
    future_map <- ggplot() + coord_fixed() + xlab("") + ylab("") + 
      theme_bw() +
      geom_tile(data = sst_raster_df, aes(fill = Col_break,
                                          x = Longitude, y = Latitude),
                size = 0.85) +
      #geom_point(data = points.dat, aes(x = Longitude, y = Latitude, 
      # color = Col_break),
      #size = 0.15, alpha = I(0.7),
      #shape = 15) +
      geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                                  ymin = ystart, ymax = yend),
                fill = 'gray85', colour = 'gray85') +
      geom_polygon(data = med_map, aes(x = long, y = lat, 
                                       group = group), 
                   colour = "gray85", fill = "gray85",
                   size = 0.25) +
      scale_fill_manual(values = cols, drop = FALSE, name = NULL) +
      theme(axis.text = element_text(size = 7)) +
      scale_x_longitude(xmin = -3, xmax = 35, step = 7) +
      scale_y_latitude(ymin = 30, ymax = 49, step = 5) +
      labs(x = "", y = "") +
      coord_map(projection = "rectangular", lat0 = 0,
                xlim = c(-6, 36.25), ylim = c(30, 45.75)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      obs_legend_theme() +
      guides(fill = guide_legend(nrow = 6, reverse = T,
                                 title.vjust = 0,
                                 override.aes = list(size = 1.3,
                                                     alpha = 1)))
    
    # Repeat for the future projections
    points.dat$Change <- points.dat$Future - points.dat$Past
    sst_raster <- raster::rasterize(spdf, empty_raster, points.dat$Change, mean)
    
    sst_raster_spdf <- as(sst_raster, "SpatialPixelsDataFrame")
    sst_raster_df <- as.data.frame(sst_raster_spdf)
    colnames(sst_raster_df) <- c("Change", "Longitude", "Latitude")
    
    sst_raster_df$Col_break_change <- cut(sst_raster_df$Change, 
                                          breaks = c(-100, -20, -10, 10, 20,
                                                     100),
                                          labels = c('- >20','- 10-20',
                                                     '+/-', 
                                                     '+ 10-20',
                                                     '+ >20'))
    cols <- pals::kovesi.diverging_linear_bjr_30_55_c53(5)
    names(cols) <- c('- >20','- 10-20',
                     '+/-', 
                     '+ 10-20',
                     '+ >20')
    
    # Plot predicted change in richness using diverging colour scheme
    change_map <- ggplot() + coord_fixed() + xlab("") + ylab("") + 
      theme_bw() +
      geom_tile(data = sst_raster_df, aes(fill = Col_break_change,
                                          x = Longitude, y = Latitude),
                size = 0.85) +
      #geom_point(data = points.dat, aes(x = Longitude, y = Latitude, 
       #                                 color = Col_break_change),
        #         size = 0.15, alpha = I(0.7),
         #        shape = 15) +
      geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                                  ymin = ystart, ymax = yend),
                fill = 'gray85', colour = 'gray85') +
      geom_polygon(data = med_map, aes(x = long, y = lat, 
                                       group = group), 
                   colour = "gray85", fill = "gray85",
                   size = 0.25) +
      scale_fill_manual(name = NULL,
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
    sst_raster <- raster::rasterize(spdf, empty_raster, points.dat$Past.sc, mean)
    
    sst_raster_spdf <- as(sst_raster, "SpatialPixelsDataFrame")
    sst_raster_df <- as.data.frame(sst_raster_spdf)
    colnames(sst_raster_df) <- c("Past.sc", "Longitude", "Latitude")
    
    sst_raster_df$Col_break <- cut(sst_raster_df$Past.sc, 
                                breaks = c(min(sst_raster_df$Past.sc) - 1,
                                           -0.85, -0.25, 0.25, 0.85,
                                           max(sst_raster_df$Past.sc)),
                                labels = c('very low','low', 'moderate', 
                                           'high','very high'))
    #darkred <- pals::brewer.reds(10)[10]
    #cols <- c(darkred, pals::kovesi.linear_kry_5_95_c72(7)[3:6])
    #cols <- c(viridis::viridis(10)[c(10,9)], pals::ocean.speed(7)[5:7])
    #names(cols) <- c('very low','low', 'moderate', 
                         #'high','very high')
    cols <- rev(c(viridis::inferno(10)[c(5,7)],viridis::viridis(10)[c(10,8)],
                  viridis::plasma(5)[1]))
    names(cols) <- c('very low','low', 'moderate', 
                     'high','very high')
    obs_map <- ggplot() + coord_fixed() + xlab("") + ylab("") + 
      theme_bw() +
      geom_tile(data = sst_raster_df, aes(fill = Col_break,
                                          x = Longitude, y = Latitude),
                size = 0.85) +
     # geom_point(data = points.dat, aes(x = Longitude, y = Latitude, 
      #                                  color = Col_break),
       #          size = 0.15, alpha = I(0.7),
        #         shape = 15) +
      geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                                  ymin = ystart, ymax = yend),
                fill = 'gray85', colour = 'gray85') +
      geom_polygon(data = med_map, aes(x = long, y = lat, 
                                       group = group), 
                   colour = "gray85", fill = "gray85",
                   size = 0.25) +
      scale_fill_manual(name = NULL,
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
                                override.aes = list(size = 1.3,
                                                    alpha = 1)))
    
    # Repeat for the future map
    points.dat$Future.sc <- (points.dat$Future - mean(points.dat$Future)) / 
      sd(points.dat$Future)
    empty_raster <- raster::raster(nrow = 105, ncols = 275,  
                                   ext = raster::extent(range(points.dat$Longitude),
                                                        range(points.dat$Latitude)))
    spdf <- sp::SpatialPointsDataFrame(coords = points.dat %>%
                                         dplyr::select(Longitude, Latitude), 
                                       data = points.dat,
                                       proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    sst_raster <- raster::rasterize(spdf, empty_raster, points.dat$Future.sc, mean)
    
    sst_raster_spdf <- as(sst_raster, "SpatialPixelsDataFrame")
    sst_raster_df <- as.data.frame(sst_raster_spdf)
    colnames(sst_raster_df) <- c("Future.sc", "Longitude", "Latitude")
    
    sst_raster_df$Col_break <- cut(sst_raster_df$Future.sc, 
                                   breaks = c(min(sst_raster_df$Future.sc) - 1,
                                              -0.85, -0.25, 0.25, 0.85,
                                              max(sst_raster_df$Future.sc)),
                                   labels = c('very low','low', 'moderate', 
                                              'high','very high'))
    cols <- rev(c(viridis::inferno(10)[c(5,7)],viridis::viridis(10)[c(10,8)],
                  viridis::plasma(5)[1]))
    names(cols) <- c('very low','low', 'moderate', 
                     'high','very high')
    future_map <- ggplot() + coord_fixed() + xlab("") + ylab("") + 
      theme_bw() +
      geom_tile(data = sst_raster_df, aes(fill = Col_break,
                                          x = Longitude, y = Latitude),
                size = 0.85) +
      # geom_point(data = points.dat, aes(x = Longitude, y = Latitude, 
      #                                  color = Col_break),
      #          size = 0.15, alpha = I(0.7),
      #         shape = 15) +
      geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                                  ymin = ystart, ymax = yend),
                fill = 'gray85', colour = 'gray85') +
      geom_polygon(data = med_map, aes(x = long, y = lat, 
                                       group = group), 
                   colour = "gray85", fill = "gray85",
                   size = 0.25) +
      scale_fill_manual(name = NULL,
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
                                 override.aes = list(size = 1.3,
                                                     alpha = 1)))
    
    # Repeat for the change map
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
                                          labels = c('--','-', '-/+', 
                                                     '+','++    '))
    
    cols <- pals::kovesi.diverging_linear_bjr_30_55_c53(5)
    names(cols) <- c('--','-', '-/+', 
                     '+','++    ')
    
    change_map <- ggplot() + coord_fixed() + xlab("") + ylab("") + 
      theme_bw() +
      geom_tile(data = sst_raster_df, aes(fill = Col_break_change,
                                          x = Longitude, y = Latitude),
                size = 0.85) +
      #geom_point(data = points.dat, aes(x = Longitude, y = Latitude, 
       #                                 color = Col_break_change),
        #         size = 0.15, alpha = I(0.7),
         #        shape = 15) +
      geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                                  ymin = ystart, ymax = yend),
                fill = 'gray85', colour = 'gray85') +
      geom_polygon(data = med_map, aes(x = long, y = lat, 
                                       group = group), 
                   colour = "gray85", fill = "gray85",
                   size = 0.25) +
      scale_fill_manual(name = NULL,
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
      #theme(legend.position = 'none')
  } 
  # if(metric == 'modularity') {
  #   points.dat$Past.sc <- (points.dat$Past - mean(points.dat$Past)) / 
  #     sd(points.dat$Past)
  #   points.dat$Col_break <- cut(points.dat$Past.sc, 
  #                               breaks = c(min(points.dat$Past.sc) - 1,
  #                                          -0.85, -0.25, 0.25, 0.85,
  #                                          max(points.dat$Past.sc)),
  #                               labels = c('very low','low', 'moderate', 
  #                                          'high','very high'))
  #   
  #   obs_map <- ggplot() + coord_fixed() + xlab("") + ylab("") + 
  #     theme_bw() +
  #     geom_point(data = points.dat, aes(x = Longitude, y = Latitude, 
  #                                       color = Col_break),
  #                size = 0.15, alpha = I(0.7),
  #                shape = 15) +
  #     geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
  #                                 ymin = ystart, ymax = yend),
  #               fill = 'gray85', colour = 'gray85') +
  #     geom_polygon(data = med_map, aes(x = long, y = lat, 
  #                                      group = group), 
  #                  colour = "gray85", fill = "gray85",
  #                  size = 0.25) +
  #     scale_color_viridis(name = NULL, 
  #                         discrete = T, drop = FALSE) +
  #     theme(axis.text = element_text(size = 7)) +
  #     scale_x_longitude(xmin = -3, xmax = 35, step = 7) +
  #     scale_y_latitude(ymin = 30, ymax = 49, step = 5) +
  #     labs(x = "", y = "") +
  #     coord_map(projection = "rectangular", lat0 = 0,
  #               xlim = c(-6, 36.25), ylim = c(30, 45.75)) +
  #     theme(panel.grid.major = element_blank(),
  #           panel.grid.minor = element_blank()) +
  #     obs_legend_theme() +
  #     guides(col = guide_legend(nrow = 5, reverse = T, title.vjust = 0,
  #                               override.aes = list(size = 1.3,
  #                                                   alpha = 1)))
  #   
  #   points.dat$Future.sc <- (points.dat$Future - mean(points.dat$Future)) / 
  #     sd(points.dat$Future)
  #   points.dat$Col_break_change <- cut(points.dat$Future.sc, 
  #                                      breaks = c(min(points.dat$Past.sc) - 1,
  #                                                 -0.85, -0.25, 0.25, 0.85,
  #                                                 max(points.dat$Past.sc)),
  #                                      labels = c('very low','low', 'moderate', 
  #                                                 'high','very high'))
  #   
  #   change_map <- ggplot() + coord_fixed() + xlab("") + ylab("") + 
  #     theme_bw() +
  #     geom_point(data = points.dat, aes(x = Longitude, y = Latitude, 
  #                                       color = Col_break_change),
  #                size = 0.15, alpha = I(0.7),
  #                shape = 15) +
  #     geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
  #                                 ymin = ystart, ymax = yend),
  #               fill = 'gray85', colour = 'gray85') +
  #     geom_polygon(data = med_map, aes(x = long, y = lat, 
  #                                      group = group), 
  #                  colour = "gray85", fill = "gray85",
  #                  size = 0.25) +
  #     scale_color_manual(name = NULL,
  #                        values = viridis(5), drop = FALSE) +
  #     theme(axis.text = element_text(size = 7)) +
  #     scale_x_longitude(xmin = -3, xmax = 35, step = 7) +
  #     scale_y_latitude(ymin = 30, ymax = 49, step = 5) +
  #     labs(x = "", y = "") +
  #     coord_map(projection = "rectangular", lat0 = 0,
  #               xlim = c(-6, 36.25), ylim = c(30, 45.75)) +
  #     theme(panel.grid.major = element_blank(),
  #           panel.grid.minor = element_blank()) +
  #     obs_legend_theme() + 
  #     theme(legend.position = 'none')
  #   
  #   
  # }
  
  list(obs_map, future_map, change_map)
  
}





