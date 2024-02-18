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
library(ggplot2  )     ## Plots
library(ggpubr   )     ## Arrange plots
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

## Remove scientific notation
options(scipen = 999)

## Seed
set.seed(100)

################################################################################
## Read Dataset
################################################################################
## Path
setwd(path_data)

## Read data
base <- read_excel("Final Dataset (Revision).xlsx", sheet = "CP")

################################################################################
## Data manipulation
################################################################################
## Cut Variables into categories (Age)
base <- base %>% mutate(
  AGE = cut(x              = AGE                                         ,
            right          = FALSE                                       ,
            ordered_result = TRUE                                        , 
            breaks         = c(-Inf, 18, 28, 37, 46, Inf)                ,
            labels         = c("< 18", "18-27", "28-36", "37-45", "> 45"))
)

## Cut Variables into categories (Climbing Experience)
base <- base %>% mutate(
  `CLIMBING EXPERIENCE` = cut(x              = `CLIMBING EXPERIENCE`           ,
                              right          = FALSE                           ,
                              ordered_result = TRUE                            , 
                              breaks         = c(-Inf, 12, 37, 73, Inf)        ,
                              labels         = c("< 12","12-36","37-72","> 72"))
)

## Weekly frequency per climbing
fn_fq <- function(x){
  x[is.na(x)]             <- "RARE OR NEVER"
  x[x == "RARO OU NUNCA"] <- "RARE OR NEVER"
  x[x == "MENSAL"       ] <- "1X (MONTH)"
  x[x == "15/15 DIAS"   ] <- "2X (MONTH)"
  x[x == "1X NA SEMANA" ] <- "1X (WEEK)"
  x[x == "2X NA SEMANA" ] <- "2X (WEEK)"
  x[x == "3X NA SEMANA" ] <- "3X (WEEK)"
  x[x == "4X NA SEMANA" ] <- "4X (WEEK)"
  x[x == "5X NA SEMANA" ] <- "5X (WEEK)"
  x[x == "6X NA SEMANA" ] <- "6X (WEEK)"
  x[x == "TODO DIA"     ] <- "7X (WEEK)" 
  x <- factor(x, levels = c("RARE OR NEVER",
                            "1X (MONTH)"   ,
                            "2X (MONTH)"   ,
                            "1X (WEEK)"    ,
                            "2X (WEEK)"    ,
                            "3X (WEEK)"    ,
                            "4X (WEEK)"    ,
                            "5X (WEEK)"    ,
                            "6X (WEEK)"    ,
                            "7X (WEEK)"    ))
  return(x)
} 

## Factors
base$GENDER <- factor(base$GENDER, levels = c("MASCULINO", "FEMININO"))
base$CP     <- factor(base$CP    , levels = c("SIM"      , "NAO"     ))   

## Frequencies
base <- base %>% mutate(
  `CLIMBING FREQUENCY (INDOOR)`     = fn_fq(`CLIMBING FREQUENCY (INDOOR)`     ),
  `CLIMBING FREQUENCY (SPORT)`      = fn_fq(`CLIMBING FREQUENCY (SPORT)`      ),
  `CLIMBING FREQUENCY (BOULDER)`    = fn_fq(`CLIMBING FREQUENCY (BOULDER)`    ),
  `CLIMBING FREQUENCY (TRADITIONAL)`= fn_fq(`CLIMBING FREQUENCY (TRADITIONAL)`),
  `CLIMBING FREQUENCY (MULTIPITCH)` = fn_fq(`CLIMBING FREQUENCY (MULTIPITCH)` ),
  `CLIMBING FREQUENCY (OTHERS)`     = fn_fq(`CLIMBING FREQUENCY (OTHERS)`     )
)

################################################################################
## General Descriptive Statistics
################################################################################
## Number of CP yes responses
sum (base$CP == "SIM")
mean(base$CP == "SIM")

################################################################################
## Association Tests
################################################################################
## Function to perform test
fn_tests <- function(x, y){
  ## Total
  df <- na.omit(data.frame(x, y))
  
  ## Relative
  tot_rel <- data.frame(table(df$x))
  rel     <- data.frame(table(df  )) %>% filter(y == "SIM")
  
  ## Data frame with results
  df_res <- data.frame(Levels  = levels(x)         ,
                       Total   = nrow(df)          ,
                       n       = sum(df$y == "SIM"),
                       Tot_Rel = tot_rel$Freq      ,
                       Rel     = rel$Freq          ,
                       Odd     = NA                ,
                       Lower   = NA                , 
                       Upper   = NA                ,
                       pvalue  = NA                )
  
  for(i in 1:length(levels(x))){
    l <- levels(x)[i]
    if(mean(x == l, na.rm = TRUE) == 1 | mean(x == l, na.rm = TRUE) == 0) next
    
    res              <- fisher.test(x == l, y, simulate.p.value = TRUE)
    df_res$Odd   [i] <- as.numeric(res$estimate   )
    df_res$Lower [i] <- as.numeric(res$conf.int[1])
    df_res$Upper [i] <- as.numeric(res$conf.int[2])
    df_res$pvalue[i] <- as.numeric(res$p.value    )
  }
  
  ## Tests
  test_odds_ratio  <- fisher.test(x, y, simulate.p.value = TRUE)
  test_chi_squared <- chisq.test (x, y, simulate.p.value = TRUE)
  
  ## Save results
  df_res$fisher_pvalue      <- test_odds_ratio$p.value
  df_res$chi_squared        <- test_chi_squared$statistic
  df_res$chi_squared_pvalue <- test_chi_squared$p.value
  
  ## Return data
  return(df_res)
}

## List with results
list_df <- list(
  GENDER      = fn_tests(base$GENDER                            , base$CP),
  AGE         = fn_tests(base$AGE                               , base$CP),
  Climb_Exp   = fn_tests(base$`CLIMBING EXPERIENCE`             , base$CP),
  INDOOR      = fn_tests(base$`CLIMBING FREQUENCY (INDOOR)`     , base$CP),
  SPORT       = fn_tests(base$`CLIMBING FREQUENCY (SPORT)`      , base$CP),
  BOULDER     = fn_tests(base$`CLIMBING FREQUENCY (BOULDER)`    , base$CP),
  TRADITIONAL = fn_tests(base$`CLIMBING FREQUENCY (TRADITIONAL)`, base$CP),
  MULTIPITCH  = fn_tests(base$`CLIMBING FREQUENCY (MULTIPITCH)` , base$CP),
  OTHERS      = fn_tests(base$`CLIMBING FREQUENCY (OTHERS)`     , base$CP)
)

## Path
setwd(path_results)

## Save result
write_xlsx(list_df, "CP Tests Results.xlsx")


################################################################################
## Plots 
################################################################################
fn_plot <- function(x, y, res, vx){
  
  ## Data frame for plot
  df_plot <- data.frame(table(x, y))
  
  ## Create Percentages
  df_plot <- split(df_plot, ~x)
  df_plot <- lapply(df_plot, function(x) mutate(x, Perc = Freq/sum(Freq)))
  df_plot <- do.call(rbind, df_plot)
  df_plot <- data.frame(df_plot, row.names = NULL)
  
  ## Add pvalue
  df_plot <- merge(x    = df_plot                                        , 
                   y    = select(res, Levels, pvalue, chi_squared_pvalue),
                   by.x = "x"                                            ,
                   by.y = "Levels"                                       )
  

  ## If vx is gender, translate names
  if(toupper(vx) == "GENDER"){
    df_plot$x  <- ifelse(df_plot$x      == "FEMININO", "Female", "Male")
    res$Levels <- ifelse(res    $Levels == "FEMININO", "Female", "Male")
  } 
  
  ## Add pvalue on names
  df_plot$x <- paste(df_plot$x,
                     round(df_plot$pvalue, digits = 4),
                     sep = "\np-value: ")
  
  res$Levels <- paste(res$Levels,
                      round(res$pvalue, digits = 4),
                      sep = "\np-value: ")
  
  ## Remove NA
  df_plot$x      <- gsub("p-value: NA", "p-value: -", df_plot$x     )
  res    $Levels <- gsub("p-value: NA", "p-value: -", res    $Levels)
  
  ## Change CP names
  df_plot <- mutate(df_plot, y = as.character(y)                   )
  df_plot <- mutate(df_plot, y = ifelse(y == "SIM", "Yes", "No")   )
  df_plot <- mutate(df_plot, y = factor(y, levels = c("Yes", "No")))
  
  ## Filter only CP Yes
  df_plot <- df_plot %>% filter(y == "Yes")
  
  ## Change names
  colnames(df_plot) <- c("x", "y", "Freq", "Perc", "pvalue", "test_pvalue")
  
  ## Subtitle
  subtitle <- paste("p-value:", round(df_plot$test_pvalue[1], digits = 4))
  
  ## Same order as res
  df_plot$x <- factor(df_plot$x, levels = res$Levels)
  
  ## Plot
  ggplot(df_plot) +
    geom_col(aes(x = x, y = 100*Perc), position = "dodge") + 
    #geom_label(aes(x = x, y = 100*Perc, label = round(100*Perc, digits = 2))) + 
    xlab("") + 
    ylab("CP Lifetime Prevalence\n% of gender total") + 
    scale_fill_grey("", start = 0.35, end = 0.7) +
    ggtitle(vx, subtitle = subtitle) + 
    theme_bw() + 
    coord_flip()
}

p1 <- fn_plot(base$GENDER,
              base$CP,
              list_df$GENDER,
              "Gender")

p2 <- fn_plot(base$AGE,
              base$CP,
              list_df$AGE,
              "Age")

p3 <- fn_plot(base$`CLIMBING EXPERIENCE`,
              base$CP,
              list_df$Climb_Exp,
              "Climbing Experience")


p4 <- fn_plot(base$`CLIMBING FREQUENCY (INDOOR)`,
              base$CP,
              list_df$INDOOR,
              "Climbing Frequency (Indoor)")

p5 <- fn_plot(base$`CLIMBING FREQUENCY (SPORT)`,
              base$CP,
              list_df$SPORT,
              "Climbing Frequency (Sport)")

p6 <- fn_plot(base$`CLIMBING FREQUENCY (BOULDER)`,
              base$CP,
              list_df$BOULDER,
              "Climbing Frequency (Boulder)")

p7 <- fn_plot(base$`CLIMBING FREQUENCY (TRADITIONAL)`,
              base$CP,
              list_df$TRADITIONAL,
              "Climbing Frequency (Traditional)")

p8 <- fn_plot(base$`CLIMBING FREQUENCY (MULTIPITCH)`,
              base$CP,
              list_df$MULTIPITCH,
              "Climbing Frequency (Multi Pitch)")

p9 <- fn_plot(base$`CLIMBING FREQUENCY (OTHERS)`,
              base$CP,
              list_df$OTHERS,
              "Climbing Frequency (Others)")

################################################################################
## Save plot
################################################################################
## Path
setwd(path_results)

## Save plot 1
pdf("Figure 1.pdf", width = 12, height = 5)
ggarrange(plotlist = list(p1, p2, p3), ncol = 3, nrow = 1)
dev.off()

## Save plot 1 as tiff
ggarrange(plotlist = list(p1, p2, p3), ncol = 3, nrow = 1)
ggsave("Figure 1.tiff", width = 12, height = 5, device = 'tiff', dpi = 600)

## Save plot 2
pdf("Figure 2.pdf", width = 10, height = 13)
ggarrange(plotlist = list(p4, p5, p6, p7, p8, p9), ncol = 2, nrow = 3)
dev.off()

## Save plot 2 as tiff
ggarrange(plotlist = list(p4, p5, p6, p7, p8, p9), ncol = 2, nrow = 3)
ggsave("Figure 2.tiff", width = 10, height = 13, device = 'tiff', dpi = 600)

