require("caret")
require("readxl")
require("stringr")

load_data <- function(base_data_path){

# Read the raw data
# Format of input data
# A excel (XLSX) file with multiple sheets
# A sheet correspond to a specific combinations of Voltage and SoC values
# The sheets MUST be names as follows 1.25V_10%SoC 
# WHERE: 1.25V is the voltage and 10% is the SoC separated by single "_" character.
# 
read_data <- function(base_data_path){
  
  sheet_names = excel_sheets(base_data_path)
  Inputdata = data.frame()
  
  for (sh in sheet_names){
    temp <- as.data.frame(read_excel(base_data_path, sheet = sh))
    volt = as.numeric(noquote(strsplit(sh, "mV")))
    
    Volt = rep(volt, nrow(temp))
    Volt = Volt/1000
    
    temp = cbind("Volt"=Volt, temp)
    Inputdata = rbind(Inputdata, temp)
  }
  
  Inputdata[, 1] <- as.numeric(Inputdata[, 1])
  Inputdata[, 2] <- as.numeric(Inputdata[, 2])
  Inputdata[, 3] <- as.numeric(Inputdata[, 3])
  Inputdata[, 4] <- as.numeric(Inputdata[, 4])
  
  colnames(Inputdata) <- c("Volt", "Freq", "Zreal", "Zimag")
  

  return(Inputdata)
}

# for(i in list_files){
#     temp <- read.table(file=i, header = TRUE, stringsAsFactors = FALSE)
#     soc_values <- rep(tools::file_path_sans_ext(basename(i), nrow(temp)))
#     temp = cbind("soc"=as.numeric(soc_values), temp)
#     Inputdata <- rbind(Inputdata, temp)
# }

Inputdata <- read_data(base_data_path)

# print(unique(Inputdata$Volt))
# print(dim(Inputdata))
# print(head(Inputdata))

# Create training and test data
set.seed(9)
train_indices <- createDataPartition(y = as.factor(Inputdata$Volt), p = 0.85, list = FALSE)
training_data <- Inputdata[train_indices, ]
test_data <- Inputdata[-train_indices, ]

print("Data read-in successfully")
print(paste("Rows:", nrow(Inputdata), " Cols:", ncol(Inputdata)))
print(paste("Train set: ", dim(training_data)[1], dim(training_data)[2]))
print(paste("test set: ", dim(test_data)[1], dim(test_data)[2]))

out = list("train_data" = training_data, "test_data" = test_data)

return(out)

}

# a = load_data("/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/ML_Battery_Data/data/LowFreqData/EIS_ExtraLowFreq_Data.xlsx")

