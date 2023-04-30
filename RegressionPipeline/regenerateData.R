suppressMessages(library("caretEnsemble"))
library(ggplot2)


regenerate_data <- function(train_data, test_data, real_comp_best_models, imag_comp_best_models, 
                            set_seed, number, repeats, file_name_string){


    run_ensemble <- function(pred_type, train_data, number, repeats, set_seed){
    
          set.seed(set_seed)
          control <- trainControl(method = "repeatedcv", 
                                  number = number, repeats = repeats, 
                                  savePredictions = "final",
                                  index = createResample(train_data[, 1], number*repeats))
          
          if (pred_type == "real"){
            model_list <- caretList(Zreal~., data=train_data, trControl=control, 
                                    methodList=real_comp_best_models, preProcess= c("center","scale"))
          } else if (pred_type == "imag"){
            model_list <- caretList(Zimag~., data=train_data, trControl=control, 
                                    methodList=imag_comp_best_models, preProcess= c("center","scale"))
          }
          
          return(model_list)
      }
  
    
    real_data <- train_data[, -4]
    imag_data <- train_data[, -3]
  
    real_model_list <- run_ensemble("real", real_data, number, repeats, set_seed)
    ensemble_real_data <- caretEnsemble(real_model_list, metric="RMSE")

    imag_model_list <- run_ensemble("imag", imag_data, number, repeats, set_seed)
    ensemble_imag_data <- caretEnsemble(imag_model_list, metric="RMSE")
    
    real_data <- rbind(real_data, test_data[, -4])
    imag_data <- rbind(imag_data, test_data[, -3])

    real_preds = predict(ensemble_real_data, newdata = real_data[, c(1,2)])
    imag_preds = predict(ensemble_imag_data, newdata = imag_data[, c(1,2)])
    
    real_data = as.data.frame(cbind(real_data, "Zreal_pred"=real_preds))
    imag_data = as.data.frame(cbind(imag_data, "Zimag_pred"=imag_preds))

    regenerated_data <- as.data.frame(cbind(real_data[, c(1, 2, 3)], "Zimag"=imag_data$Zimag, 
                                        "Zreal_pred"=real_data$Zreal_pred, "Zimag_pred"=imag_data$Zimag_pred))
    
    write.table(regenerated_data, file=paste("Regenerated_", file_name_string, "_data.csv", sep=""), row.names = FALSE, sep=",")
    
    out = list("real"=ensemble_real_data, "imag"=ensemble_imag_data)
    return(out)


# orig_volt = 3.45
# some_volt = orig_volt


# temp = regenerated_data[regenerated_data$Volt == some_volt, ]
# 
# temp1 <- temp[, c(4, 5)]
# temp2 = temp[, c(6, 7)]
# colnames(temp2) <- c("Zreal", "Zimag")
# 
# ggplot(data=temp1, aes(x=Zreal, y=Zimag)) + geom_point() + geom_point(data=temp2, colour="blue")



# new_d = read.csv2(file=file.path(base_path,"RegressionPipeline","Regenerated_data.csv"), sep=",")
# new_d$Soc <- as.numeric(new_d$Soc)
# new_d$Volt <- as.numeric(new_d$Volt)
# new_d$Freq <- as.numeric(new_d$Freq)
# new_d$Z_r_orig <- as.numeric(new_d$Z_r_orig)
# new_d$Z_i_orig <- as.numeric(new_d$Z_i_orig)
# new_d$Z_r_pred <- as.numeric(new_d$Z_r_pred)
# new_d$Z_i_pred <- as.numeric(new_d$Z_i_pred)

# temp = new_d[new_d$Volt == orig_volt, ]
# temp1 <- temp[, c(4, 5)]
# temp2 = temp[, c(6, 7)]
# colnames(temp2) <- c("Z_r_orig", "Z_i_orig")
# ggplot(data=temp1, aes(x=Z_r_orig, y=Z_i_orig)) + geom_point() + geom_point(data=temp2, colour="blue")

}




