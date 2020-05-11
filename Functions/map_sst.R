#### Function to map projected change in SST for summer and winter seasons ####
map_sst = function(obs_climate_dat, proj_climate_dat, summer = TRUE){
  library(viridis)
  library(ggplot2)
  
# Calculate predicted change in mean summer and winter SST
SST_changes <- obs_climate_dat %>%
  dplyr::group_by(Latitude, Longitude) %>%
  dplyr::mutate(obs_Summer_SST = mean(July, August, September),
                obs_Winter_SST = mean(January, February, March)) %>%
  dplyr::ungroup() %>% 
  dplyr::select(Latitude, Longitude, obs_Summer_SST, obs_Winter_SST) %>%
  dplyr::left_join(proj_climate_dat %>%
                     dplyr::group_by(Latitude, Longitude) %>%
                     dplyr::mutate(proj_Summer_SST = mean(July, August, September),
                                   proj_Winter_SST = mean(January, February, March)) %>%
                     dplyr::ungroup() %>% 
                     dplyr::select(Latitude, Longitude, proj_Summer_SST, proj_Winter_SST)) %>%
  dplyr::mutate(change_Summer_SST = proj_Summer_SST - obs_Summer_SST,
                change_Winter_SST = proj_Winter_SST - obs_Winter_SST)

# Extract coordinates
points.dat <- SST_changes %>%
  dplyr::select(Latitude, Longitude)

# Download a world map and crop to the Mediterranean region
med_map <- map_data("world", xlim = c(-10, 50), ylim = c(25, 45))

# General mapping theme
obs_legend_theme = function(){ theme(legend.title = element_text(size = 7,
                                                                 hjust = 0.5),
                                     legend.text = element_text(size = 6.6),
                                     #legend.key.size = unit(0.3, "cm"),
                                     legend.key.size = unit(0.25, 'cm'),
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

# Create a map of the summer SST predictions
# Breaks for background rectangles
rects <- data.frame(xstart = c(-6, 26.5), xend = c(3, 36.25), 
                    ystart = c(42, 39), yend = c(45.75, 45.75))

if(summer){
  # Create a raster for plotting
  empty_raster <- raster::raster(nrow = 105, ncols = 275, 
                      ext = raster::extent(range(SST_changes$Longitude),
                                           range(SST_changes$Latitude)))
  spdf <- sp::SpatialPointsDataFrame(coords = SST_changes %>%
                                       dplyr::select(Longitude, Latitude), 
                                     data = SST_changes,
                                     proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  sst_raster <- raster::rasterize(spdf, empty_raster, SST_changes$change_Summer_SST, mean)
  
  sst_raster_spdf <- as(sst_raster, "SpatialPixelsDataFrame")
  sst_raster_df <- as.data.frame(sst_raster_spdf)
  colnames(sst_raster_df) <- c("change_Summer_SST", "Longitude", "Latitude")
  rm(spdf, empty_raster, sst_raster, sst_raster_spdf)
  
  # Set breaks for the plot colour categories
  sst_raster_df$summer_break <- cut(sst_raster_df$change_Summer_SST, 
                                    breaks = c(-1, 0, 0.5, 1, 1.5, 2, 2.5, 
                                               max(SST_changes$change_Winter_SST)),
                                    labels = c('< 0.00','0.01-0.50', '0.51-1.00', 
                                               '1.01-1.50',
                                               '1.51-2.00', '2.01-2.50',
                                               '> 2.50'))
  
  # Set colours
  #devtools::install_github("kwstat/pals")
  #sst_raster1.cols <- pals::brewer.rdbu(10)[c(1:3, 7:10)]; use rev
  darkred <- pals::brewer.reds(10)[10]
  cols <- c('white', pals::brewer.ylorrd(6)[2:6], darkred)
  names(cols) <- c('< 0.00','0.01-0.50', '0.51-1.00', 
                       '1.01-1.50',
                       '1.51-2.00', '2.01-2.50',
                       '> 2.50')
  
  # Create the plot
plot_map <- ggplot() + coord_fixed() +
  theme_bw() +
  geom_tile(data = sst_raster_df, aes(fill = summer_break,
                                x = Longitude, y = Latitude),
            size = 0.85) +
  #geom_point(data = SST_changes, aes(x = Longitude, y = Latitude, 
                                    #color = summer_break),
             #size = 0.25, alpha = I(0.7), shape = 15) +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                              ymin = ystart, ymax = yend),
            fill = 'gray85', colour = 'gray85') +
  geom_polygon(data = med_map, aes(x = long, y = lat, 
                                   group = group), 
               colour = "gray85", fill = "gray85",
               size = 0.25) +
  scale_fill_manual(values = cols,
                       name = expression(paste(Delta~SST, ~degree,'C (1980-2040)')),
                    drop = FALSE) +
  #scale_color_viridis(name = expression(paste(Delta~SST, ' (Celsius)')),
                      #option = 'plasma',
                      #discrete = T,
                      #drop = FALSE) +
  theme(axis.text = element_text(size = 7)) +
  scale_x_longitude(xmin = -3, xmax = 35, step = 7) +
  scale_y_latitude(ymin = 30, ymax = 49, step = 5) +
  labs(x = "", y = "") +
  coord_map(projection = "rectangular", lat0 = 0,
            xlim = c(-6, 36.25), ylim = c(30, 45.75)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  obs_legend_theme() +
  #guides(col = guide_legend(nrow = 7, title.vjust = 0, reverse = T,
                         #   override.aes = list(size = 1.5,
                                               # alpha = 1))) +
  guides(fill = guide_legend(nrow = 7, title.vjust = 0, reverse = T,
                            override.aes = list(size = 0.8,
                                                alpha = 1)))
} else {
  empty_raster <- raster::raster(nrow = 105, ncols = 275,  
                                 ext = raster::extent(range(SST_changes$Longitude),
                                                      range(SST_changes$Latitude)))
  spdf <- sp::SpatialPointsDataFrame(coords = SST_changes %>%
                                       dplyr::select(Longitude, Latitude), 
                                     data = SST_changes,
                                     proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  sst_raster <- raster::rasterize(spdf, empty_raster, SST_changes$change_Winter_SST, mean)
  
  sst_raster_spdf <- as(sst_raster, "SpatialPixelsDataFrame")
  sst_raster_df <- as.data.frame(sst_raster_spdf)
  colnames(sst_raster_df) <- c("change_Winter_SST", "Longitude", "Latitude")
  rm(spdf, empty_raster, sst_raster, sst_raster_spdf)
  sst_raster_df$winter_break <- cut(sst_raster_df$change_Winter_SST, 
                              breaks = c(-1, 0, 0.5, 1, 1.5, 2, 2.5, 
                                         max(SST_changes$change_Winter_SST)),
                              labels = c('< 0.00','0.01-0.50', '0.51-1.00', 
                                         '1.01-1.50',
                                         '1.51-2.00', '2.01-2.50',
                                         '> 2.50'))
  
  # Create Winter SST map
  #sst_raster1.cols <- pals::brewer.rdbu(10)[c(1:3, 7:10)]; use rev
  darkred <- pals::brewer.reds(10)[10]
  cols <- c('white', pals::brewer.ylorrd(6)[2:6], darkred)
  names(cols) <- c('< 0.00','0.01-0.50', '0.51-1.00', 
                   '1.01-1.50',
                   '1.51-2.00', '2.01-2.50',
                   '> 2.50')
  plot_map <- ggplot() + coord_fixed() +
    theme_bw() +
    geom_tile(data = sst_raster_df, aes(fill = winter_break,
                                  x = Longitude, y = Latitude),
              size = 0.85) +
    #geom_point(data = SST_changes, aes(x = Longitude, y = Latitude, 
    #color = summer_break),
    #size = 0.25, alpha = I(0.7), shape = 15) +
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                                ymin = ystart, ymax = yend),
              fill = 'gray85', colour = 'gray85') +
    geom_polygon(data = med_map, aes(x = long, y = lat, 
                                     group = group), 
                 colour = "gray85", fill = "gray85",
                 size = 0.25) +
    scale_fill_manual(values = cols,
                      name = expression(paste(Delta~SST, ~degree,'C (1980-2040)')),
                      drop = FALSE) +
    #scale_color_viridis(name = expression(paste(Delta~SST, ' (Celsius)')),
    #option = 'plasma',
    #discrete = T,
    #drop = FALSE) +
    theme(axis.text = element_text(size = 7)) +
    scale_x_longitude(xmin = -3, xmax = 35, step = 7) +
    scale_y_latitude(ymin = 30, ymax = 49, step = 5) +
    labs(x = "", y = "") +
    coord_map(projection = "rectangular", lat0 = 0,
              xlim = c(-6, 36.25), ylim = c(30, 45.75)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    obs_legend_theme() +
    #theme(legend.position = 'none')
    guides(fill = guide_legend(nrow = 7, title.vjust = 0, reverse = T,
                               override.aes = list(size = 0.8,
                                                   alpha = 1)))
  
}

plot_map
}
