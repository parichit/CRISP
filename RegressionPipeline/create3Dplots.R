library(plotly)
library(datasets)
library(reshape2)
library(grid)
library(gridExtra)

###### Fit the best model to the data and save the results for 
###### further plotting
source("regenerateData.R")

real_comp_best_models = c("Rborist", "ranger")
imag_comp_best_models = c("kknn", "qrf")
number = 5
repeats = 5
set_seed = 142
file_name_string = "lowFreq"

out <- regenerate_data(train_data, test_data, real_comp_best_models, imag_comp_best_models, 
                set_seed, number, repeats, file_name_string)


real_ensemble <- out$real
imag_ensemble <- out$imag



train_data_copy = train_data
train_data_copy <- scale(train_data_copy, scale=TRUE, center=TRUE)
train_data_copy <- as.data.frame(train_data_copy)


# Create the grid of predictors for the 3d plot
# grid.lines = 60
# grid_x <- seq(min(train_data$Volt), max(train_data$Volt), length.out = grid.lines)
# grid_y <- seq(min(train_data$Freq), max(train_data$Freq), length.out = grid.lines)
# gridXY <- expand.grid("Volt" = grid_x, "Freq" = grid_y)
# 
# Zreal_pred <- matrix(predict(real_ensemble, newdata = gridXY), nrow = grid.lines, ncol = grid.lines)
# Zimag_pred <- matrix(predict(imag_ensemble, newdata = gridXY), nrow = grid.lines, ncol = grid.lines)
# 
# gridXY$"Zreal_pred" = Zreal_pred
# gridXY$"Zimag_pred" =


# zreal_fit <- predict(real_ensemble, newdata = train_data_copy[, c(1,2)])
# zimag_fit <- predict(imag_ensemble, newdata = train_data_copy[, c(1,2)])
# 
# 
# 
# p1 <- scatter3D(train_data$Freq, train_data$Volt, train_data_copy$Zreal, pch = 19, cex = 1,colvar = NULL, 
#           col="red", theta = 0, phi = -40, bty="b2", ticktype="detailed",
#           ylab = "Cell Potential", xlab = "Frequency", zlab = "Impedance (Z_real)",  
#           surf = list(x = grid_y, y = grid_x, z = Zreal_pred ,  
#                       facets = TRUE, fit = zreal_fit, 
#                       col=ramp.col(col = c("dodgerblue3","seagreen2"), n = 300, alpha=0.7), 
#                       border="black"), main = "Regression Surface")
# 
# p2 <- scatter3D(train_data$Freq, train_data$Volt, train_data_copy$Zimag, pch = 19, cex = 1,colvar = NULL, 
#                 col="red", theta = -10, phi = -30, bty="b2", ticktype="detailed",
#                 ylab = "Cell Potential", xlab = "Frequency", zlab = "Impedance (Z_real)",  
#                 surf = list(x = grid_y, y = grid_x, z = Zimag_pred ,  
#                             facets = TRUE, fit = zimag_fit, 
#                             col=ramp.col(col = c("dodgerblue3","seagreen2"), n = 300, alpha=0.7), 
#                             border="black"), main = "Regression Surface")


# data(iris)
# data <- iris
# petal_lm <- lm(Petal.Length ~ 0 + Sepal.Length + Sepal.Width, data = data)
# graph_reso <- 0.05
# 
# #Setup Axis
# axis_x <- seq(min(data$Sepal.Length), max(data$Sepal.Length), by = graph_reso)
# axis_y <- seq(min(data$Sepal.Width), max(data$Sepal.Width), by = graph_reso)

#Sample points
# petal_lm_surface <- expand.grid(Sepal.Length = axis_x, Sepal.Width = axis_y, KEEP.OUT.ATTRS = F)
# petal_lm_surface$Petal.Length <- predict.lm(petal_lm, newdata = petal_lm_surface)
# petal_lm_surface <- acast(petal_lm_surface, Sepal.Width ~ Sepal.Length, value.var = "Petal.Length") #y ~ x
# 
# 
# hcolors=c("red","blue","green")[data$Species]
# iris_plot <- plot_ly(data, 
#                      x = ~Sepal.Length, 
#                      y = ~Sepal.Width, 
#                      z = ~Petal.Length,
#                      text = ~Species, # EDIT: ~ added
#                      type = "scatter3d", 
#                      mode = "markers",
#                      marker = list(color = hcolors))
# 
# iris_plot <- add_trace(p = iris_plot,
#                        z = petal_lm_surface,
#                        x = axis_x,
#                        y = axis_y,
#                        type = "surface")
# 
# iris_plot






grid.lines <- 40
grid_x <- seq(min(train_data_copy$Volt), max(train_data_copy$Volt), length.out = grid.lines)
grid_y <- seq(min(train_data_copy$Freq), max(train_data_copy$Freq), length.out = grid.lines)
gridXY <- expand.grid("Volt" = grid_x, "Freq" = grid_y, KEEP.OUT.ATTRS = F)
dim(gridXY)

gridXY$"Zreal_pred" <- predict(real_ensemble, newdata = gridXY)
gridXY$"Zimag_pred" <- predict(imag_ensemble, newdata = gridXY)


gridXYreal <- acast(gridXY, Volt~Freq, value.var = "Zreal_pred")
gridXYimag <- acast(gridXY, Volt~Freq, value.var = "Zimag_pred")

x_text = c(round(unique(train_data_copy$Volt), 2))
y_text = c(seq(2000, 10000, 2000))
zreal_z_text = c(seq(0, 1, 0.2))


# train_data_copy$Zreal -

real_plot <- plot_ly(train_data, 
                     x = train_data_copy$Volt, 
                     y = train_data_copy$Freq, 
                     z = train_data_copy$Zreal,
                     type = "scatter3d", mode = "markers", 
                     marker = list(color = ~Volt, color = train_data_copy$colors,
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
                                    tickvals = zreal_z_text)
                                    ))

real_plot




zimag_z_text = c(seq(0, 14, 2))

imag_plot <- plot_ly(train_data, 
                     x = train_data_copy$Volt, 
                     y = train_data_copy$Freq, 
                     z = train_data_copy$Zimag,
                     type = "scatter3d", mode = "markers", 
                     marker = list(color = ~Volt, color = train_data_copy$colors,
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
                                               tickvals = zimag_z_text)
                                  ))

imag_plot





