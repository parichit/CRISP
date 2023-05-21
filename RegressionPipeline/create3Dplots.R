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

train_data <- train_data[-which(train_data$Freq < 0.0001), ]


grid.lines <- 60

# l <- seq(6.310e-03, 9.990e-01, length.out=500)
# u <- seq(1.267e+00, 1.008e+04, length.out=grid.lines)
# grid_y = c(l, u)
# grid_y <- seq(min(train_data$Freq), max(train_data$Freq), by = 0.05)
grid_y <- seq(min(train_data$Freq), max(train_data$Freq), length.out = grid.lines)
grid_y = c(l, grid_y)

grid_x <- seq(min(train_data$Volt), max(train_data$Volt), length.out = grid.lines)

gridXY <- expand.grid("Volt" = grid_x, "Freq" = grid_y, KEEP.OUT.ATTRS = F)
dim(gridXY)

gridXY$"Zreal_pred" <- predict(real_ensemble, newdata = gridXY)
gridXY$"Zimag_pred" <- predict(imag_ensemble, newdata = gridXY)


gridXYreal <- acast(gridXY, Volt~Freq, value.var = "Zreal_pred")
gridXYimag <- acast(gridXY, Volt~Freq, value.var = "Zimag_pred")

x_text = c(round(unique(train_data$Volt), 2))
y_text = c(seq(2000, 10000, 2000))
zreal_z_text = c(seq(0, 1, 0.2))


# train_data_copy$Zreal -

real_plot <- plot_ly(train_data, 
                     x = train_data$Volt, 
                     y = train_data$Freq, 
                     z = train_data$Zreal,
                     type = "scatter3d", mode = "markers", 
                     marker = list(color = ~Volt, color = train_data$colors,
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
                     x = train_data$Volt, 
                     y = train_data$Freq, 
                     z = train_data$Zimag,
                     type = "scatter3d", mode = "markers", 
                     marker = list(color = ~Volt, color = train_data$colors,
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

imag_plot





