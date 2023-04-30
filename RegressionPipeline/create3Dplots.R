library(plot3D)
library(rgl)
library(plotly)
library(ggplot2)
library(ggrepel)

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


real_ensemble <- out[[1]]
imag_ensemble <- out[[2]]





train_data_copy = train_data

train_data_copy <- scale(train_data_copy, scale=TRUE, center=TRUE)

# train_data_copy$Volt <- scale(train_data_copy$Volt, scale=TRUE, center=TRUE)
# train_data_copy$Freq <- scale(train_data_copy$Freq, scale=TRUE, center=TRUE)
# 
# cen = attr(train_data_copy, "scaled:center")
# sc = attr(train_data_copy, "scaled:scale")
# vv = (3.879 - cen) / sc
# 
train_data_copy <- as.data.frame(train_data_copy)
# 
# train_data_copy = train_data_copy[train_data_copy$Volt == vv,]



# Create the grid of predictors for the 3d plot
grid.lines = 30
grid_x <- seq(min(train_data$Volt), max(train_data$Volt), length.out = grid.lines)
grid_y <- seq(min(train_data$Freq), max(train_data$Freq), length.out = grid.lines)
gridXY <- expand.grid("Volt" = grid_x, "Freq" = grid_y)

Zreal_pred <- matrix(predict(real_ensemble, newdata = gridXY), nrow = grid.lines, ncol = grid.lines)
Zimag_pred <- matrix(predict(imag_ensemble, newdata = gridXY), nrow = grid.lines, ncol = grid.lines)


zreal_fit <- predict(real_ensemble, newdata = train_data_copy[, c(1,2)])
zimag_fit <- predict(imag_ensemble, newdata = train_data_copy[, c(1,2)])



scatter3D(train_data$Freq, train_data$Volt, train_data_copy$Zreal, pch = 19, cex = 1,colvar = NULL, 
          col="red", theta = 0, phi = -40, bty="b2", ticktype="detailed",
          ylab = "Cell Potential", xlab = "Frequency", zlab = "Impedance (Z_real)",  
          surf = list(x = grid_y, y = grid_x, z = Zreal_pred ,  
                      facets = TRUE, fit = zreal_fit, 
                      col=ramp.col(col = c("dodgerblue3","seagreen2"), n = 300, alpha=0.7), 
                      border="black"), main = "Regression Surface")

plot3d( 
  x=train_data$Freq, y=train_data$Volt, z=train_data_copy$Zreal, 
  type = 's', 
  radius = .1,
  xlab="Sepal Length", ylab="Sepal Width", zlab="Petal Length")








