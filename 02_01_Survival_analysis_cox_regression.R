library(dplyr)
library(tidyverse)
library(data.table)
library(knitr)
library(kableExtra)
library(naniar)
library(curl)
library(MASS)
library(survey)
library(cobalt)
library(broom)
library(ggrepel)
library(GGally)
library(ggforce)
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(survminer)

HRtable <- WD_stone%>%
  mutate(Re_OS = as.numeric(gsub(".*?([0-9]+).*", "\\1", Overall_Mortality)),
         Re_CSS = as.numeric(gsub(".*?([0-9]+).*", "\\1", Cancer_specific)),
         Re_DFS = 1- as.numeric(gsub(".*?([0-9]+).*", "\\1", Disease_Free)),
         Re_BRFS = as.numeric(gsub(".*?([0-9]+).*", "\\1", Bladder_Recurrence)),
         .after = Bladder_Recurrence)

#The function to construct uni-variate/multivariate survival
HR_table <- function(DF = DF,# complete data frame
                     survival_analysis = "Surv(Followup, Mortality)", # time-to-event
                     variables = c(),
                     variations = "multi",#multi/uni
                     HR1 = 1
){
  library(finalfit)
  library(stringi)
  DF_HR <- DF%>%
    finalfit(survival_analysis , variables)%>%
    as.data.frame()%>%
    mutate(HR = NA, P = NA)

  if(variations == "multi"){
    lev <- nrow(DF_HR)

    for(i in 1:lev){
      if(DF_HR$`HR (multivariable)`[i]=="-"){
        DF_HR[i,6:7] <- c(HR1, NA)
      }else{
        HR_info <- stri_remove_empty (strsplit(DF_HR$`HR (multivariable)`[i],
                                               split = "[-/(/,/)/p=/p/ ]",
                                               fixed = FALSE)[[1]])
        DF_HR[i,6:7] <- c(paste(HR_info[1], " (", HR_info[2], ", ", HR_info[3], ")", sep = ""),
                          ifelse(HR_info[4]=="<0.001", "<0.001*",
                                 ifelse(as.numeric(HR_info[4])<0.01, paste(HR_info[4], "*", sep=""),
                                        ifelse(as.numeric(HR_info[4])<0.05, paste(HR_info[4], "*", sep=""), HR_info[4]))))
      }
    }
    DF_HR[,c(1,2,6:7)]
  }else if(variations == "uni"){
    lev <- nrow(DF_HR)

    for(i in 1:lev){
      if(DF_HR$`HR (univariable)`[i]=="-"){
        DF_HR[i,6:7] <- c(HR1, NA)
      }else{
        HR_info <- stri_remove_empty (strsplit(DF_HR$`HR (univariable)`[i],
                                               split = "[-/(/,/)/p=/p/ ]",
                                               fixed = FALSE)[[1]])
        DF_HR[i,6:7] <- c(paste(HR_info[1], " (", HR_info[2], ", ", HR_info[3], ")", sep = ""),
                          ifelse(HR_info[4]=="<0.001", "<0.001*",
                                 ifelse(as.numeric(HR_info[4])<0.01, paste(HR_info[4], "*", sep=""),
                                        ifelse(as.numeric(HR_info[4])<0.05, paste(HR_info[4], "*", sep=""), HR_info[4]))))
      }
    }
    DF_HR[,c(1,2,6:7)]
  }else{
    stop("Error: wrong analysis method, please use 'multi' or 'uni' in variations")
  }
}

#Predict variables
Var <- colnames(WD_stone)[1:26]

#Overall Survival
HR_OSuni <- HR_table(DF = HRtable, survival_analysis = "Surv(Followup_OS.CSS, Re_OS)", variables = Var, variations = "uni")

#Cancer-Specific Survival
HR_CSSuni <- HR_table(DF = HRtable, survival_analysis = "Surv(Followup_OS.CSS, Re_CSS)", variables = Var, variations = "uni")

#Disease-Free Survival
HR_DFSuni <- HR_table(DF = HRtable, survival_analysis = "Surv(Followup_BDFS.DFS, Re_DFS)", variables = Var, variations = "uni")

#Bladder-Recurrence-Free Survival
HR_BRFSuni <- HR_table(DF = HRtable, survival_analysis = "Surv(Followup_BDFS.DFS, Re_BRFS)", variables = Var, variations = "uni")


