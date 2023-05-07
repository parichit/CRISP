suppressMessages(library("caretEnsemble"))
library(ggplot2)
library(dplyr)
library(kknn)
library(Metrics)
library(kknn)
source("createPlots.R")


mdl_predict <- function(input_file_path, output_dir){
  
    # The directory for saved models
    MDL_DIR = file.path(getwd(), "saved_models")
    
    
    # File for saving prediction statistics
    prediction_result = file.path(output_dir, "pred_stat.txt")
  
  
    # Function for loading input data
    load_input_data <- function(input_file_path){
      data <- read.csv2(file=input_file_path, sep=",", stringsAsFactors = FALSE)
      data <- as.data.frame(apply(data, 2, as.numeric))
      return(data)
    }
      
    # Load the data
    data <- load_input_data(input_file_path)
    
    real_id = which(colnames(data) == "Zreal")
    imag_id = which(colnames(data) == "Zimag")
    
    real_data <- data[, -imag_id]
    imag_data <- data[, -real_id]
    
    print("#######################")
    print("1. Data loading complete")
    print("#######################")
    
    
    
    print("#######################")
    print("2. Starting model predictions")
    print("#######################")
    
    # Load the model for real impedance
    real_mdl <- readRDS(file.path(MDL_DIR, "real_mdl.rds"))
    
    # Load the model for imaginary impedance
    imag_mdl <- train.kknn(Zimag ~ ., imag_data, ks = c(2),  kernel = "inv", distance = 1)
    
  
    result <- data.frame()
  
    # Save prediction statistics
    if (("Zreal" %in% colnames(data)) & ("Zimag" %in% colnames(data))){
    
        real_preds <- predict(real_mdl, real_data)
        real_rmse <- rmse(real_data$Zreal, real_preds)
        real_mae <- mae(real_data$Zreal, real_preds)
        real_rsq <- (cor(real_data$Zreal, real_preds))^2
        
        imag_preds <- as.numeric(unlist(imag_mdl$fitted.values))
        imag_rmse <- rmse(imag_data$Zimag, imag_preds)
        imag_mae <- mae(imag_data$Zimag, imag_preds)
        imag_rsq <- (cor(imag_data$Zimag, imag_preds))^2
        
        data <- cbind(data, "Zreal_preds"=real_preds, "Zimag_preds"=imag_preds)
        temp <- data.frame("Zreal_rmse"=real_rmse, "Zreal_mae"=real_mae, "Zreal_rsq"=real_rsq, 
                           "Zimag_rmse"=imag_rmse, "Zimag_mae"=imag_mae, "Zimag_rsq"=imag_rsq)
  
    } else if (("Zimag" %in% colnames(data)) & (!"Zreal" %in% colnames(data))){
    
        imag_preds <- fitted(mdl)
        imag_rmse <- rmse(imag_data$Zimag, imag_preds)
        imag_mae <- mae(imag_data$Zimag, imag_preds)
        imag_rsq <- (cor(imag_data$Zimag, imag_preds))^2
        
        data <- cbind(data, "Zimag_preds"=imag_preds)
        temp <- data.frame("Zimag_rmse"=imag_rmse, "Zimag_mae"=imag_mae, "Zimag_rsq"=imag_rsq)
        
    } else if ((!"Zimag" %in% colnames(data)) & ("Zreal" %in% colnames(data))){
    
      real_preds <- predict(real_mdl, data)
      real_rmse <- rmse(real_data$Zreal, real_preds)
      real_mae <- mae(real_data$Zreal, real_preds)
      real_rsq <- (cor(real_data$Zreal, real_preds))^2
      
      data <- cbind(data, "Zreal_preds"=real_preds)
      temp <- data.frame("Zreal_rmse"=real_rmse, "Zreal_mae"=real_mae, "Zreal_rsq"=real_rsq)
      
    }
    
      print("#######################")
      print("3. Model predictions done.")
      print("#######################")
      
      # Save  the regenerated data  
      write.table(data, file=file.path(output_dir, "Regenerated_data.csv"), row.names = FALSE, sep=",")
      
      # Save the stats
      write.table(temp, file=file.path(output_dir, "Pred_stat.csv"), sep=",", row.names = FALSE)
      
      
      print("#######################")
      print("4. Creating the plots")
      print("#######################")
      
      # Create the visualization plots
      create_3D_plots(data, real_mdl, imag_mdl, output_dir)
      
  }


###### Example of how to run this code:

# To generate the output:  Call this script with two command line arguments 

# Arg 1 is the path to the input file (a sample input file (raw_data.csv) is already given in the webserver_code folder)
# Arg 2 is the path to the output folder where all the output will be saved. A sample output folder (test) can be seen and other output folder
# will have the same files with same names but actual output values will differ depending on the input data.

# The input file will be uploaded by the user to the webserver (make sure to check the column names of the input file). An input file 
# must have the same names as shown in the sample input. If not then, show appropriate message to the user and guide them on how to correctly format the
# input file.

## The output folder should be different for each run (decide how to name each output folder)

## When the run completes, display the output (Regenerated_data.csv) to the user in nice format, for ex: either they can see the entire data or.
## they can filter the outout by different Volt (show output only for 3.14V or 4.93 V) etc. Output can also be seen by different columns - for example:
## show the predictions only for Zreal or or Zimag. In Regenerated_data.csv - Zreal_pred.csv is the prediction made by the mode whereas Zimag is the 
## actual value input by the user in their file. 

## Some ideas : You can show rows of Zreal - Zreal_preds in different colors depending on how much difference is there in each row, closer values
## can be shown in some light green color and rows with large difference can be shown in some shade of red etc.

## The plots need to be displayed too. The output folder also has the plots.

## The stats (Pred_stats.csv) contains some basic statistics that also needs to be shown to the user. Use rcan choose which stats to see
## i.e. all/real/imag

## First of all - get the basic version setup and running and then work on filtering the output data.


args = commandArgs(trailingOnly=TRUE)

if (length(args) < 2){
  print("Provide both input file path and the path to output directory. Exiting.")
  stop(exiting)
}

type_pred = args[1]
already_running = args[2]


# input = "raw_data.csv"
# out = "test"


# The following line call the funciton and generate the output
mdl_predict(input, out)



