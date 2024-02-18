################################################################################
## Clean Memory
################################################################################
rm(list = ls())
gc(full = TRUE)

################################################################################
## Libraries
################################################################################
library(readxl   )     ## Read excel files
library(writexl  )     ## Write excel files
library(caret    )     ## Machine Learning Functions
library(nnet     )     ## Logistic Regression
library(lmtest   )     ## Coef test
library(ggplot2  )     ## Plots
library(tidyverse)     ## Data Manipulation

################################################################################
## Paths
################################################################################
## Library for automatic path
library(this.path)

## Get path
path <- this.dir()
path <- strsplit(path, "/")[[1]]
path <- paste(rev(rev(path)[-1]), collapse = "/")

## Other paths
path_data    <- paste0(path, "/Data/"   )
path_results <- paste0(path, "/Results/")

## Disable scientif notation
options(scipen = 999)

## Seed
set.seed(100)

################################################################################
## Read Dataset
################################################################################
## Path
setwd(path_data)

## Read data
base <- read_excel("Final Dataset (Revision).xlsx", sheet = "Reg")

################################################################################
## Data manipulation
################################################################################
## Translate
base$GENDER <- ifelse(base$GENDER == "MASCULINO", "Male", "Female")

## Factors
base$CP     <- factor(base$CP    , levels = c("NAO" , "SIM"   ))
base$GENDER <- factor(base$GENDER, levels = c("Male", "Female"))

## Change names
colnames(base) <- c("CP"           ,
                    "BMI"          ,
                    "AGE"          ,
                    "GENDER"       ,
                    "CLIMBER_EXP"  ,
                    "N_LESIONS"    ,
                    "N_RECURRENCES")

################################################################################
## Logistic Regression Validation
################################################################################
## Symbolic Description
frl <- CP ~ BMI + AGE + GENDER + log(CLIMBER_EXP) + N_LESIONS + N_RECURRENCES

## Predicted value
pred_val <- c()

## Leave One Out cross validation
eval_time <- system.time({
  for(i in 1:nrow(base)){
    
    ## Split train and validation data
    data_train <- base %>% slice(-i)
    data_val   <- base %>% slice( i)
    
    ## Train
    reg <- multinom(frl, data = data_train, maxit = 1000, trace = FALSE)
    
    ## Validate
    pred_val <- c(pred_val, as.character(predict(reg, data_val)))
  }
})

## Confusion Matrix
cm_loocv <- confusionMatrix(table(base$CP, pred_val))

## Coerce to data frame
df_loocv <- data.frame(c(cm_loocv$overall, cm_loocv$byClass))

## Add names
df_loocv <- data.frame(Metrics   = rownames(df_loocv), 
                       Results   = df_loocv[,1]      , 
                       row.names = NULL              )

################################################################################
## Logistic Regression Train
################################################################################
## Train
reg <- multinom(frl, data = base, maxit = 1000)

## Coef test
df_coef_test <- data.frame(coeftest(reg)[,])

## Add Variables
df_coef_test <- data.frame(Variables    = rownames(df_coef_test)       , 
                           df_coef_test                                , 
                           Significance = df_coef_test$Pr...z.. <= 0.05,
                           row.names    = NULL                         )

## Names
colnames(df_coef_test) <- c("Variables"    ,
                            "Coef."        ,
                            "SE"           ,
                            "z value"      ,
                            "p-value"      ,
                            "Significative")

################################################################################
## Save results
################################################################################
## path
setwd(path_results)

## Result list
list_res <- list(Coefs = df_coef_test, LOOCV = df_loocv)

## Save excel file
write_xlsx(list_res, "Logistic Regression Results.xlsx")
