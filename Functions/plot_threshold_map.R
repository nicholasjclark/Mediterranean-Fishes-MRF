#### Function to plot sites based on their threshold categories ####
plot_threshold_map = function(SST_data){
  library(ggplot2)
  library(viridis)
  
  # Download a world map and crop to the Mediterranean region
  med_map <- map_data("world", xlim = c(-10, 50), ylim = c(25, 45))
  
  # General mapping theme
  obs_legend_theme = function(){ theme(legend.title = element_text(size = 7.5,
                                                                   hjust = 0.5),
                                       legend.text = element_text(size = 7.5),
                                       legend.key.size = unit(0.35, "cm"),
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
  
  # Create a raster for plotting
  empty_raster <- raster::raster(nrow = 105, ncols = 275,  
                                 ext = raster::extent(range(SST_data$Longitude),
                                                      range(SST_data$Latitude)))
  spdf <- sp::SpatialPointsDataFrame(coords = SST_data %>%
                                       dplyr::select(Longitude, Latitude), 
                                     data = SST_data,
                                     proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  # Calculate the most common class in each raster cell (the mode)
  getmode <- function(v, na.rm = T) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  sst_raster <- raster::rasterize(spdf, empty_raster, 
                                  as.numeric(factor(SST_data$cross_threshold,
                                                    levels = c('stays below',
                                                               'crosses',
                                                               'stays above'))), getmode)
  
  # Convert the raster to a dataframe and reorganise the variable to a factor
  sst_raster_spdf <- as(sst_raster, "SpatialPixelsDataFrame")
  sst_raster_df <- as.data.frame(sst_raster_spdf)
  colnames(sst_raster_df) <- c("cross_threshold", "Longitude", "Latitude")
  sst_raster_df$cross_threshold <- as.factor(sst_raster_df$cross_threshold)
  sst_raster_df %>%
    dplyr::mutate(cross_threshold = dplyr::case_when(
      cross_threshold == '1' ~ 'stays below',
      cross_threshold == '2' ~ 'crosses',
      cross_threshold == '3' ~ 'stays above',
    )) -> sst_raster_df
  sst_raster_df$cross_threshold <- factor(sst_raster_df$cross_threshold,
                                          levels = c('stays below',
                                              'crosses',
                                              'stays above'))

  # Set colours for plotting
  cols <- pals::kovesi.diverging_linear_bjr_30_55_c53(3)
  names(cols) <- c('stays below',
                   'crosses',
                   'stays above')
  
  # Create a map of categories
  threshold_map <- ggplot() + coord_fixed() + xlab("") + ylab("") + 
    theme_bw() +
    geom_tile(data = sst_raster_df, aes(fill = cross_threshold,
                                        x = Longitude, y = Latitude),
              size = 0.85) +
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                                ymin = ystart, ymax = yend),
              fill = 'gray85', colour = 'gray85') +
    geom_polygon(data = med_map, aes(x = long, y = lat, 
                                     group = group), 
                 colour = "gray85", fill = "gray85",
                 size = 0.25) +
    scale_fill_manual(name = expression(paste('13'~degree,"C",' winter SST ', 'threshold')),
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
    guides(col = guide_legend(nrow = 3, reverse = T,
                              title.vjust = 0,
                              override.aes = list(size = 1.3,
                                                  alpha = 1)))
  
  # Generate the boxplots of proportional change for each habitat category
  plot_theme = function(){theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 7),
    legend.position = 'none',
    aspect.ratio = 0.75)}
  
  # Set colours for plotting
  cols <- pals::kovesi.diverging_linear_bjr_30_55_c53(3)
  names(cols) <- c('stays below',
                   'crosses',
                   'stays above')
  
  change_rich <- ggplot(SST_data, 
                        aes(factor(cross_threshold, levels = c('stays below',
                                                               'crosses',
                                                               'stays above')), 
                            richness.2040_sim / richness.1980_sim)) +
    theme_bw() +
    geom_hline(yintercept = 1, size = 0.5, col = 'black',
               linetype = 'dashed') +
    geom_boxplot(aes(fill = factor(cross_threshold, 
                                   levels = c('stays below',
                                              'crosses',
                                              'stays above'))),
                 outlier.alpha = 0.3, outlier.size = 0.5,
                 color = 'gray30') +
    scale_fill_manual(name = 'Threshold category',
                      values = cols,
                      drop = FALSE) +
    plot_theme() +
    ylab('Change in richness\n(proportional)') +
    xlab('')
  
  change_div <- ggplot(SST_data, 
                       aes(factor(cross_threshold, levels = c('stays below',
                                                              'crosses',
                                                              'stays above')),
                           fdiv.2040_sim - fdiv.1980_sim)) +
    theme_bw() +
    geom_hline(yintercept = 0, size = 0.5, col = 'black',
               linetype = 'dashed') +
    geom_boxplot(aes(fill = factor(cross_threshold, 
                                   levels = c('stays below',
                                              'crosses',
                                              'stays above'))),
                 outlier.alpha = 0.3, outlier.size = 0.5,
                 color = 'gray30') +
    scale_fill_manual(name = 'Threshold category',
                      values = cols,
                      drop = FALSE) +
    plot_theme() + 
    ylab('Change in diversity') +
    xlab('')
  
  change_mod <- ggplot(SST_data, 
                       aes(x = factor(cross_threshold, levels = c('stays below',
                                                                  'crosses',
                                                                  'stays above')), 
                           y = (modularities.2040_sim - modularities.1980_sim))) +
    theme_bw() +
    geom_hline(yintercept = 0, size = 0.5, col = 'black',
               linetype = 'dashed') +
    geom_boxplot(aes(fill = factor(cross_threshold, 
                                   levels = c('stays below',
                                              'crosses',
                                              'stays above'))),
                 outlier.alpha = 0.3, outlier.size = 0.5,
                 color = 'gray30') +
    scale_fill_manual(name = 'Threshold category',
                      values = cols,
                      drop = FALSE) +
    plot_theme() + 
    ylab('Change in modularity') + xlab('')
  
  # Put the plots together as a cowplot object and return
  cowplot::plot_grid(threshold_map,
                     cowplot::plot_grid(change_rich, change_div,change_mod, nrow = 1),
                     ncol = 1, rel_heights = c(1, 0.6),
                     labels = c('a','b'),
                     label_size = 9,
                     hjust = c(-4, -3.5),
                     vjust = c(3, 0))
  
}