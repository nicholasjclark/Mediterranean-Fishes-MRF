#### Function to plot GAM prediction results ####
generate_gam_plot = function(smooths, ylabel, y_decimal, season_labels = TRUE){
  library(ggplot2)
  
  # General plotting parameters
  axis_degree = function(x) parse(text = paste0(x,"^o", "*C"))
  winter_col <- 'blue'
  summer_col <- 'red'
  
  if(y_decimal){
    y_label_fun = function(x){stringr::str_pad(sprintf("%.1f", x), 4, pad = " ")}
  } else {
    y_label_fun = function(x){stringr::str_pad(x, 4, pad = " ")}
  }
  
  if(season_labels){
  sst_names <- c(`Summer SST` = "summer",
                 `Winter SST` = "winter")
  } else {
    sst_names <- c(`Summer SST` = "",
                   `Winter SST` = "")
  }

  my_breaks <- function(x) { if (min(x) < 15) seq(10, 19, 3) else seq(20, 26, 3) }
  
  # Smooth the summer SST upper and lower predictions
  g1.SST <- ggplot(smooths %>%
                     filter(char.Variable == 'Summer SST'), aes(x)) +
    stat_smooth(aes(y = lower), size = 0.05, colour = summer_col,
                method = "loess", se = FALSE) +
    stat_smooth(aes(y = upper), size = 0.05, colour = summer_col,
                method = "loess", se = FALSE) +
    stat_smooth(aes(y = smooth), size = 0.5, colour = "black",
                method = "loess", se = FALSE)
  built.SST <- ggplot_build(g1.SST)
  SST.smooth <- data.frame(x = built.SST$data[[1]]$x,
                           y = built.SST$data[[3]]$y,
                           ymin = built.SST$data[[1]]$y,
                           ymax = built.SST$data[[2]]$y)
  
  # Repeat for winter SST predictions
  g1.WST <- ggplot(smooths %>%
                     filter(char.Variable == 'Winter SST'), aes(x)) +
    stat_smooth(aes(y = lower), size = 0.05, colour = winter_col,
                method = "loess", se = FALSE) +
    stat_smooth(aes(y = upper), size = 0.05, colour = winter_col,
                method = "loess", se = FALSE) +
    stat_smooth(aes(y = smooth), size = 0.5, colour = "black",
                method = "loess", se = FALSE)
  built.WST <- ggplot_build(g1.WST)
  WST.smooth <- data.frame(x = built.WST$data[[1]]$x,
                           y = built.WST$data[[3]]$y,
                           ymin = built.WST$data[[1]]$y,
                           ymax = built.WST$data[[2]]$y)
  
  # Generate final plot as a ggplot object
  SST.smooth$char.Variable <- 'Summer SST'
  WST.smooth$char.Variable <- 'Winter SST'
  both.smooths  <- rbind(SST.smooth, WST.smooth)
  
  ggplot(both.smooths, aes(x, fill = as.factor(char.Variable))) + 
    theme_bw() +
    geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax),
                alpha = 0.5) + 
    geom_line(aes(x = x, y = y)) +
    facet_wrap(~char.Variable, scales = 'free_x',
               labeller = as_labeller(sst_names),
               ncol = 1) +
    scale_x_continuous(label = axis_degree,
                       breaks = my_breaks) +
    scale_fill_manual('', values = c(summer_col, winter_col)) +
    scale_y_continuous(label = y_label_fun) +
    ylab(ylabel) + 
    theme(aspect.ratio = 0.58) +
    xlab('SST') + 
    theme(panel.grid.major = element_blank(),
          panel.spacing.y = unit(0, "lines"),
          legend.position = 'none',
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 5.5),
          axis.title = element_text(size = 7),
          strip.background = element_rect(fill = "white",
                                          colour = NA),
          strip.text = element_text(size = 7,
                                    margin = margin(b = 0, 
                                                    t = 0),
                                    hjust = 0))
}

