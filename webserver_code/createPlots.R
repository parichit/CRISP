library(plotly)
library(datasets)
library(reshape2)


create_3D_plots <- function(input_data, real_mdl, imag_mdl, output_dir){
  
  
  grid.lines <- 50
  grid_x <- seq(min(input_data$Volt), max(input_data$Volt), length.out = grid.lines)
  grid_y <- seq(min(input_data$Freq), max(input_data$Freq), length.out = grid.lines)
  gridXY <- expand.grid("Volt" = grid_x, "Freq" = grid_y, KEEP.OUT.ATTRS = F)
  
  gridXY$"Zreal_pred" <- predict(real_mdl, newdata = gridXY)
  gridXY$"Zimag_pred" <- predict(imag_mdl, newdata = gridXY)
  
  gridXYreal <- acast(gridXY, Volt~Freq, value.var = "Zreal_pred")
  gridXYimag <- acast(gridXY, Volt~Freq, value.var = "Zimag_pred")
  
  x_text = c(round(unique(input_data$Volt), 2))
  y_text = c(seq(2000, 10000, 2000))
  zreal_z_text = c(seq(0, 1, 0.2))
  
  #### Plot for real impedance
  
  real_plot <- plot_ly(input_data, 
                       x = input_data$Volt, 
                       y = input_data$Freq, 
                       z = input_data$Zreal,
                       type = "scatter3d", mode = "markers", 
                       marker = list(color = ~Volt, color = input_data$colors,
                                     colorscale = list(c(0, rgb(200, 50, 0, max = 255)), 
                                                       c(1, rgb(0, 255, 100, max = 255))), 
                                     colorbar = list(title = "<b>Cell potentail (V)</b>", len=0.3, thickness=20),
                                     showscale = TRUE))
  
  real_plot <- add_surface(p=real_plot, z = gridXYreal,
                           x = grid_x,
                           y = grid_y, showscale=TRUE,
                           type = "surface",
                           colorscale = list(c(0, 1), c("#0072B2", "#D55E00")),
                           colorbar = list(title = "<b>Z<sub>r</sub></b>", len=0.4, thickness=20,
                                           orientation="h"))
  
  real_plot <- real_plot %>% layout(title=list(text="<b>Regression surface for real impedance</b>"), 
                                    scene = list(
                                      xaxis=list(title='<b><i>Cell potential (Volt)</i></b>',
                                                 ticktext = paste0("<b>", x_text, "</b>"),
                                                 tickvals = x_text),
                                      yaxis=list(title='<b><i>Frequency (Hz)</i></b>',
                                                 ticktext = paste0("<b>", y_text, "</b>"),
                                                 tickvals = y_text),
                                      zaxis=list(title=paste("<b><i>Z<sub>r</sub></i></b>"),
                                                 ticktext = paste0("<b>", zreal_z_text, "</b>"),
                                                 tickvals = zreal_z_text)))
  
  
  ### Save the html format
  htmlwidgets::saveWidget(real_plot, file.path(output_dir, "real_impedance.html"))
  

  
  #### Plot for imaginary impedance
  
  zimag_z_text = c(seq(0, 14, 2))
  
  imag_plot <- plot_ly(input_data, 
                       x = input_data$Volt, 
                       y = input_data$Freq, 
                       z = input_data$Zimag,
                       type = "scatter3d", mode = "markers", 
                       marker = list(color = ~Volt, color = input_data$colors,
                                     colorscale = list(c(0, rgb(200, 50, 0, max = 255)), 
                                                       c(1, rgb(0, 255, 100, max = 255))), 
                                     colorbar = list(title = "<b>Cell potentail (V)</b>", len=0.3, thickness=20),
                                     showscale = TRUE))
  
  imag_plot <- add_surface(p=imag_plot, z = gridXYimag,
                           x = grid_x,
                           y = grid_y, showscale=TRUE,
                           type = "surface",
                           colorscale = list(c(0, 1), c("#0072B2", "#D55E00")),
                           colorbar = list(title = "<b>Z<sub>j</sub></b>", len=0.4, thickness=20,
                                           orientation="h"))
  
  imag_plot <- imag_plot %>% layout(title=list(text="<b>Regression surface for imaginary impedance</b>"), 
                                    scene = list(
                                      xaxis=list(title='<b><i>Cell potential (Volt)</i></b>',
                                                 ticktext = paste0("<b>", x_text, "</b>"),
                                                 tickvals = x_text),
                                      yaxis=list(title='<b><i>Frequency (Hz)</i></b>',
                                                 ticktext = paste0("<b>", y_text, "</b>"),
                                                 tickvals = y_text),
                                      zaxis=list(title=paste("<b><i>Z<sub>j</sub></i></b>"),
                                                 ticktext = paste0("<b>", zimag_z_text, "</b>"),
                                                 tickvals = zimag_z_text)))
  
  ### Save the html format
  htmlwidgets::saveWidget(imag_plot, file.path(output_dir, "imag_impedance.html"))
    
  }
















