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

################################################################################
## Read Dataset
################################################################################
## Path
setwd(path_data)

## Read data
base <- read_excel("Final Dataset (Revision).xlsx", sheet = "Desc_Stats")

################################################################################
## General Descriptive Statistics
################################################################################
## Age
mean(base$AGE, na.rm = TRUE)
sd  (base$AGE, na.rm = TRUE)

## weight
mean(base$WEIGHT, na.rm = TRUE)
sd  (base$WEIGHT, na.rm = TRUE)

## height
mean(base$HEIGHT, na.rm = TRUE)
sd  (base$HEIGHT, na.rm = TRUE)

## climbing experience
mean(base$`CLIMBING EXPERIENCE`, na.rm = TRUE)
sd  (base$`CLIMBING EXPERIENCE`, na.rm = TRUE)

################################################################################
## Create Data for Descriptive Statistics
################################################################################
## symbolic description
frl <- cbind(AGE                               , 
             WEIGHT                            ,
             HEIGHT                            ,
             BMI                               , 
             `CLIMBING EXPERIENCE`             ,
             `CLIMBING FREQUENCY (INDOOR)`     ,
             `CLIMBING FREQUENCY (SPORT)`      ,
             `CLIMBING FREQUENCY (BOULDER)`    ,
             `CLIMBING FREQUENCY (TRADITIONAL)`,
             `CLIMBING FREQUENCY (MULTIPITCH)` ,
             `CLIMBING FREQUENCY (OTHERS)`     ,
             `NUMBER OF LESIONS`               ,   
             `NUMBER OF LESIONS RECURRENCES`   ) ~ GENDER



## Descriptive Stats
df_stats <- rbind(
  data.frame(Type = "Min" , aggregate(frl, base, min , na.rm = TRUE)),
  data.frame(Type = "Max" , aggregate(frl, base, max , na.rm = TRUE)),
  data.frame(Type = "Mean", aggregate(frl, base, mean, na.rm = TRUE)),
  data.frame(Type = "SD  ", aggregate(frl, base, sd  , na.rm = TRUE))
)

## Save Descriptive Stats
setwd(path_results)
write_xlsx(df_stats, "Descriptive Statistics - Table 01.xlsx")
