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
base <- read_excel("Final Dataset (Revision).xlsx", sheet = "Lesions_Sites")

################################################################################
## Data manipulation
################################################################################
## Correct site
base$SITES                                   <- as.character(base$SITES) 
base$SITES[base$SITES == "SPINE (VERVICAL)"] <- "SPINE (CERVICAL)"
base$SITES[base$SITES == "FIST"            ] <- "WRIST"

## Factors
base$GENDER      <- factor(base$GENDER     , c("MASCULINO", "FEMININO"))
base$SITES       <- factor(base$SITES                                  )
base$MACRO_SITES <- factor(base$MACRO_SITES                            )

################################################################################
## Test for Lesions Sites
################################################################################
fn_tests <- function(x, y){
  
  ## Count data
  df_count     <- data.frame(table(x, y))
  count_male   <- df_count %>% filter(y == "MASCULINO")
  count_female <- df_count %>% filter(y != "MASCULINO")
  
  ## Order data
  count_male   <- count_male   %>% arrange(levels(x))
  count_female <- count_female %>% arrange(levels(x))
  
  ## Data frame with results
  df_res <- data.frame(Levels = levels(x)        ,
                       Male   = count_male$Freq  ,
                       Female = count_female$Freq,
                       Odd    = NA               ,
                       Lower  = NA               , 
                       Upper  = NA               ,
                       pvalue = NA               )
  
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
  SITES       = fn_tests(base$SITES      , base$GENDER),
  MACRO_SITES = fn_tests(base$MACRO_SITES, base$GENDER)
)

## Path
setwd(path_results)

## Save result
write_xlsx(list_df, "Lesions Tests Results.xlsx")

################################################################################
## Plot A
################################################################################
## Data frame for plot
df_plot <- data.frame(table(base$SITES, base$GENDER))

## Create Percentages
df_plot <- split(df_plot, ~Var2)
df_plot <- lapply(df_plot, function(x) mutate(x, Perc = Freq/sum(Freq)))
df_plot <- do.call(rbind, df_plot)
df_plot <- data.frame(df_plot, row.names = NULL)

## Add pvalue
df_plot <- merge(x    = df_plot                              , 
                 y    = select(list_df$SITES, Levels, pvalue),
                 by.x = "Var1"                               ,
                 by.y = "Levels"                             )

## Format some sites names
df_plot$Var1 <- gsub(" \\(", "\n\\(", as.character(df_plot$Var1))

## Add pvalue on names
df_plot$Var1 <- paste(df_plot$Var1                     ,
                      round(df_plot$pvalue, digits = 3),
                      sep = "\np-value: "              )

## Order to arrange plot
order_var <- df_plot %>% filter(Var2 == "MASCULINO") %>% arrange(Perc)
order_var <- as.character(order_var$Var1)

## Change gender names
df_plot <- mutate(df_plot, Var2 = as.character(Var2)                           )
df_plot <- mutate(df_plot, Var2 = ifelse(Var2 == "MASCULINO", "Male", "Female"))

## Factors to arrange plot
df_plot <- mutate(df_plot                                          ,
                  Var1 = factor(Var1, levels = order_var)          ,
                  Var2 = factor(Var2, levels = c("Male", "Female")))

## Change names
colnames(df_plot) <- c("Sites", "Gender", "Freq", "Perc")

## Plot
p1 <- ggplot(df_plot) + 
  geom_col(aes(x = Sites, y = 100*Perc, fill = Gender), position = "dodge") + 
  xlab("Reported Sites of CP") + 
  ylab("% of gender total") + 
  scale_fill_grey("", start = 0.35, end = 0.7) +
  ggtitle("A", subtitle = "p-value: 0.025") + 
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

################################################################################
## Plot B
################################################################################
## Data frame for plot
df_plot <- data.frame(table(base$MACRO_SITES, base$GENDER))

## Create Percentages
df_plot <- split(df_plot, ~Var2)
df_plot <- lapply(df_plot, function(x) mutate(x, Perc = Freq/sum(Freq)))
df_plot <- do.call(rbind, df_plot)
df_plot <- data.frame(df_plot, row.names = NULL)

## Add pvalue
df_plot <- merge(x    = df_plot                                    , 
                 y    = select(list_df$MACRO_SITES, Levels, pvalue),
                 by.x = "Var1"                                     ,
                 by.y = "Levels"                                   )

## Add pvalue on names
df_plot$Var1 <- paste(df_plot$Var1                     ,
                      round(df_plot$pvalue, digits = 4),
                      sep = "\np-value: "              )


## Change gender names
df_plot <- mutate(df_plot, Var2 = as.character(Var2)                           )
df_plot <- mutate(df_plot, Var2 = ifelse(Var2 == "MASCULINO", "Male", "Female"))
df_plot <- mutate(df_plot, Var2 = factor(Var2, levels = c("Male", "Female"))   )

## Change names
colnames(df_plot) <- c("Macro_Sites", "Gender", "Freq", "Perc")

## Plot
p2 <- ggplot(df_plot) +
  facet_wrap(~Macro_Sites, ncol = 1, scales = "free_y") +
  geom_col(aes(x = Gender, y = 100*Perc, fill = Gender), position = "dodge") + 
  xlab("Reported Sites of CP") + 
  ylab("% of gender total") + 
  scale_fill_grey("", start = 0.35, end = 0.7) +
  ggtitle("B", subtitle = "p-value: 0.001") + 
  theme_bw()

################################################################################
## Save plot
################################################################################
## Path
setwd(path_results)

## Save plot
pdf("Figure 3.pdf", width = 13, height = 8)
ggarrange(plotlist = list(p1, p2), widths = c(2, 1))
dev.off()

## Save plot as tiff
ggarrange(plotlist = list(p1, p2), widths = c(2, 1))
ggsave("Figure 3.tiff", width = 13, height = 8, device = 'tiff', dpi = 600)
