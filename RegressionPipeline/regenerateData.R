suppressMessages(library("caretEnsemble"))
library(ggplot2)
library(dplyr)
library(kknn)


regenerate_data <- function(train_data, test_data, real_comp_best_models, imag_comp_best_models, 
                            set_seed, number, repeats, file_name_string){
    
  
    get_new_data <- function(){
      
      train_new_data <- read.csv2(file="/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/DSAA_23/data/LowFreqData/synthetic_data.csv", 
                                  sep = ",", stringsAsFactors = FALSE)
      
      train_new_data$Volt <- as.numeric(train_new_data$Volt)
      train_new_data$Freq <- as.numeric(train_new_data$Freq)
      train_new_data$Zreal <- as.numeric(train_new_data$Zreal)
      train_new_data$Zimag <- as.numeric(train_new_data$Zimag)
      
      train_new_data = as.data.frame(train_new_data)
      colnames(train_new_data) <- c("Volt", "Freq", "Zreal", "Zimag")
      
      
      set.seed(9)
      train_indices <- createDataPartition(y = as.factor(train_new_data$Volt), p = 0.85, list = FALSE)
      training_data <- train_new_data[train_indices, ]
      test_data <- train_new_data[-train_indices, ]
      
      return(list(training_data, test_data))
    }


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
  
    
    real_comp_best_models = c("Rborist", "ranger", "ctree")
    imag_comp_best_models = c("kknn")
    number = 5
    repeats = 5
    set_seed = 142
    file_name_string = "lowFreq"
    
    # train_data <- train_data[-which(train_data$Freq>4000), ]
    
    train_data <- train_data[-which(train_data$Freq < 0.00001), ]
    real_data <- train_data[, -4]
    imag_data <- train_data[, -3]
    

    real_model_list <- run_ensemble("real", real_data, number, repeats, set_seed)
    ensemble_real_data <- caretEnsemble(real_model_list, metric="RMSE")
    
    # rs <- Rborist::Rborist(real_data[, c(1,2)], real_data$Zreal, nTree=50, nLevel=10)
    # RMSE(real_data$Zreal, as.numeric(unlist(rs$prediction)))
    
    # imag_model_list <- run_ensemble("imag", imag_data, number, repeats, set_seed)
    # ensemble_imag_data <- caretEnsemble(imag_model_list, metric="RMSE")
    
    # new_test = rbind(imag_data, test_data[, -3])
    # mdl <- kknn(Zimag~., train=imag_data, test=test_data[, -3], distance=1, kernel="inv", k=2)
    tr_mdl <- kknn(Zimag ~ ., imag_data, test=imag_data[, -3], k = 2,  kernel="inv", distance = 1)
    test_mdl <- kknn(Zimag ~ ., imag_data, test=test_data[, -3], k = 2,  kernel="inv", distance = 1)
    
    tr_fit <- fitted(tr_mdl)
    RMSE(imag_data$Zimag, as.numeric(tr_fit))
    
    test_fit <- fitted(test_mdl)
    RMSE(test_data$Zimag, as.numeric(test_fit))
    
    summary(ensemble_real_data)
    
    # train_data <- rbind(train_data, test_data)
    # u_volts = unique(train_data$Volt)
    # 
    # r_sd <- sd(train_data$Zreal)
    # i_sd <- sd(train_data$Zimag)
    # 
    # for (volt in u_volts){
    #   
    #   temp <- train_data[train_data$Volt == volt, ]
    #   
    #   # t_r_sd <- r_sd - mean(temp$Zreal)
    #   # t_i_sd <- i_sd - mean(temp$Zimag)
    #   
    #   t_r_sd <-  sd(temp$Zreal)
    #   t_i_sd <- sd(temp$Zimag)
    #   
    #   r_c = 0
    #   i_c = 0
    #   
    #   r_d = c()
    #   i_d = c()
    #   
    #   temp <- temp[order(temp$Zreal, decreasing = FALSE), ]
    #   
    #   for (i in 1:nrow(temp)){
    #     if (i < nrow(temp)){
    #     diff <- temp[i+1, ]$Zreal - temp[i, ]$Zreal
    #       if (diff > t_r_sd){
    #         r_c = r_c + 1
    #         r_d = c(r_d, diff)
    #       }
    #     }
    #   }
    #   
    #   temp <- train_data[train_data$Volt == volt, ]
    #   temp <- temp[order(temp$Zimag, decreasing = FALSE), ]
    #   
    #   for (i in 1:nrow(temp)){
    #     if (i < nrow(temp)){
    #     diff <- temp[i+1, ]$Zimag - temp[i, ]$Zimag
    #       if (diff > t_i_sd){
    #         i_c = i_c + 1
    #         i_d = c(i_d, diff)
    #       }
    #     }
    #   }
    # 
    #   print(paste("Volt: ", volt, "Real: ", r_c, " Imag: ", i_c, "r_sd:", t_r_sd, "i_sd: ", t_i_sd))
    #   print(r_d)
    #   print(i_d)
    # }
    
    ### Diff in values between training real and predicted values
    # print(sum(abs(predict(real_model_list, real_data) - real_data$Zreal)))
    # print(sum(abs(predict(real_model_list, test_data[, -4]) - test_data$Zreal)))
    
    print(sum(abs(predict(ensemble_real_data, real_data) - real_data$Zreal)))
    print(sum(abs(predict(ensemble_real_data, test_data[, -4]) - test_data$Zreal)))

    # print(sum(abs(predict(imag_model_list, imag_data) - imag_data$Zimag)))
    # print(sum(abs(predict(imag_model_list, test_data[, -3]) - test_data$Zimag)))
    # 
    # print(sum(abs(predict(ensemble_imag_data, imag_data) - imag_data$Zimag)))
    # print(sum(abs(predict(ensemble_imag_data, test_data[, -3]) - test_data$Zimag)))
    
    
    saveRDS(ensemble_real_data, "real_mdl.rds")
    saveRDS(tr_mdl, "imag_mdl.rds")
    
    real_data <- rbind(real_data, test_data[, -4])
    imag_data <- rbind(imag_data, test_data[, -3])
    
    real_preds = predict(ensemble_real_data, newdata = real_data[, c(1,2)])
    # imag_preds = predict(imag_model_list, newdata = imag_data[, c(1,2)])

    real_data = as.data.frame(cbind(real_data, "Zreal_pred"=real_preds[1:length(real_preds)]))
    imag_data = as.data.frame(cbind(imag_data, "Zimag_pred"=as.numeric(c(tr_fit, test_fit))))
    
    colnames(imag_data) <- c("Volt", "Freq", "Zimag", "Zimag_pred")

    regenerated_data <- as.data.frame(cbind(real_data[, c(1, 2, 3)], "Zimag"=imag_data$Zimag, 
                                        "Zreal_pred"=real_data$Zreal_pred, "Zimag_pred"=imag_data$Zimag_pred))
    
    
    v <- unique(regenerated_data$Volt)
    v = c(3.177)
    c = 0
    
    for (i in v){
      temp <-  train_data[train_data$Volt == i, ]
      temp <- temp[order(temp$Freq, decreasing = FALSE), ]
      
      # f_sd = sd(temp[temp$Freq <1, 2])
      # f_m = mean(temp[temp$Freq <1, 2])
      
      f_sd = sd(temp$Freq)
      f_m = mean(temp$Freq)
      
      print(f_sd)
      print(f_m)
      
      n = temp[temp$Freq < (f_m-f_sd), ]
      print(nrow(n))
      
      # for (j in 1:nrow(temp)){
      #   
      #   if (j+1 < nrow(temp)){
      #     
      #     if (temp[j, ]$Freq < 1){
      #       
      #         if ((temp[j+1, ]$Freq - temp[j, ]$Freq) >= f_m){
      #           print(temp[j, ])
      #         # print(temp[j+1, ])
      #           c=c+1
      #         }
      #     }
      #   }
      # }
      print(paste("Total size: ", nrow(temp), "less than 1: ", c))
    }
    
    
    
    write.table(regenerated_data, file=paste("Regenerated_", file_name_string, "_data.csv", sep=""), row.names = FALSE, sep=",")
    # 
    # out = list("real"=ensemble_real_data, "imag"=ensemble_imag_data)
    # return(out)


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




