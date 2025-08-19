library(tidyverse)
library(dplyr)
library(survival)
library(ggfortify)
library(survminer)
library(forestploter)

# Fine-Gray Hazard Model for one-variable survival
FineGray_Model <- function(Survival_Analysis,#Survival_Analysis = "Surv(Time, Event)": as.character(), Time \in \mathbb{R}, and Event = 0 if event doesn't occurs; 1 if event occurs.
                           Variable,# vector of variables
                           Variable_name = Variable,#named the variables in final table
                           DF,#Data (data.frame)
                           Hazard_Ratio = 1#references in final table, it will be 1, "-", "Reference", etc.
){
  # survival formula and fine-gray model formula
  FML <- paste(Survival_Analysis, "~", Variable)
  FML_FG <- paste("Surv(fgstart, fgstop, fgstatus) ~ ", Variable)

  #fine-gray hazard model data frame
  FG <- finegray(formula = as.formula(FML),
                 data = DF,
                 na.action= na.pass)

  #Fine-Gray Hazard Model in Cox
  Model_result <- coxph(as.formula(FML_FG), data=FG)%>%
    fit2df(estimate_suffix = "_0.95CI")%>%
    mutate(HR_CI = paste(stri_remove_empty(strsplit(HR_0.95CI, split = "[-/(/,/)/p=/p/ ]", fixed = FALSE)[[1]])[1], " (",
                         stri_remove_empty(strsplit(HR_0.95CI, split = "[-/(/,/)/p=/p/ ]", fixed = FALSE)[[1]])[2], ", ",
                         stri_remove_empty(strsplit(HR_0.95CI, split = "[-/(/,/)/p=/p/ ]", fixed = FALSE)[[1]])[3], ")", sep = ""),
           P_value = stri_remove_empty(strsplit(HR_0.95CI, split = "[-/(/,/)/p=/p/ ]", fixed = FALSE)[[1]])[4])%>%
    mutate(P_value = ifelse(P_value=="<0.001", "<0.001*",
                            ifelse(as.numeric(P_value)<0.05, paste(P_value, "*", sep=""),
                                   ifelse(as.numeric(P_value)>=0.2, round(as.numeric(P_value), digits = 1), P_value))),
           HR_ = as.numeric(stri_remove_empty(strsplit(HR_0.95CI, split = "[-/(/,/)/p=/p/ ]", fixed = FALSE)[[1]])[1]),
           HRlower_ = as.numeric(stri_remove_empty(strsplit(HR_0.95CI, split = "[-/(/,/)/p=/p/ ]", fixed = FALSE)[[1]])[2]),
           HRupper_ = as.numeric(stri_remove_empty(strsplit(HR_0.95CI, split = "[-/(/,/)/p=/p/ ]", fixed = FALSE)[[1]])[3]),
           SE = (log(HRupper_) - log(HR_)) / 1.96)

  #Nl: the numbers of levels for the predictor variable
  Variable_class <- as.matrix(DF)[,Variable]%>%as.factor()
  Nl <- Variable_class%>%nlevels()

  if(Nl>10){
    data.frame(
      `Variables` = Variable_name,
      `Levels` = "",
      `HR_95CI` = Model_result$HR_CI,
      `P` = Model_result$P_value)%>%
      mutate(
        ` ` = paste(rep(" ", 25), collapse = " "),
        HR = Model_result$HR_,
        HR_lower = Model_result$HRlower_,
        HR_upper = Model_result$HRupper_,
        SE = Model_result$SE
      )%>%
      dplyr::select(Variables, Levels, HR_95CI, P, ` `, HR, HR_lower, HR_upper, SE)
  }else{
    data.frame(
      `Variables` = c(Variable_name, rep("", Nl-1)),
      `Levels` = Variable_class%>%levels(),
      `HR_95CI` = c(Hazard_Ratio, Model_result$HR_CI),
      `P` = c("", Model_result$P_value))%>%
      mutate(
        ` ` = paste(rep(" ", 25), collapse = " "),
        HR = c(1, Model_result$HR_),
        HR_lower = c(NA, Model_result$HRlower_),
        HR_upper = c(NA, Model_result$HRupper_),
        SE = c(0.000001, Model_result$SE)
      )%>%
      dplyr::select(Variables, Levels, HR_95CI, P, ` `, HR, HR_lower, HR_upper, SE)
  }
}

#Fine-Gray Hazard Model for Uni-variate Survival Analysis
FineGrayUni_Model <- function(Survival_Analysis,#Survival_Analysis = "Surv(Time, Event)": as.character(), Time \in \mathbb{R}, and Event = 0 if event doesn't occurs; 1 if event occurs.
                              Variables,# vector of variables
                              Variables_name = Variables,#named the variables in final table
                              DF,#Data (data.frame)
                              Hazard_Ratio = 1#references in final table, it will be 1, "-", "Reference", etc.
){
  Model_result_final <- c()

  for(v in 1:length(Variables)){
    Model_result_final <- rbind(Model_result_final,
                                FineGray_Model(Survival_Analysis = Survival_Analysis,
                                               Variable = Variables[v],
                                               Variable_name = Variables_name[v],
                                               DF = DF, Hazard_Ratio))
  }
  colnames(Model_result_final) <- c("Variables", "Levels", "HR (95% CI)", "P", " ", "HR", "HR_lower", "HR_upper", "SE")
  Model_result_final
}

#Fine-Gray Hazard Model for Multi-variate Survival Analysis
FineGrayMulti_Model <- function(Survival_Analysis,#Survival_Analysis = "Surv(Time, Event)": as.character(), Time \in \mathbb{R}, and Event = 0 if event doesn't occurs; 1 if event occurs.
                                Variables,# vector of variables
                                Variables_name = Variables,#named the variables in final table
                                DF,#Data (data.frame)
                                Hazard_Ratio = 1#references in final table, it will be 1, "-", "Reference", etc.
){
  #predict variables into formula
  VARs <- paste(Variables, collapse = "+")

  #formula
  FML <- paste(Survival_Analysis, "~", VARs)
  FML_FG <- paste("Surv(fgstart, fgstop, fgstatus) ~ ", VARs)

  FG <- finegray(formula = as.formula(FML),
                 data = DF,
                 na.action= na.pass)

  Model_result <- coxph(as.formula(FML_FG), data=FG)%>%
    fit2df(estimate_suffix = "_0.95CI")

  Model_result_final <- c()

  for(i in 1:length(Variables)){
    Model_result01 <- Model_result[i,]%>%
      mutate(HR_CI = paste(stri_remove_empty(strsplit(HR_0.95CI, split = "[-/(/,/)/p=/p/ ]", fixed = FALSE)[[1]])[1], " (",
                           stri_remove_empty(strsplit(HR_0.95CI, split = "[-/(/,/)/p=/p/ ]", fixed = FALSE)[[1]])[2], ", ",
                           stri_remove_empty(strsplit(HR_0.95CI, split = "[-/(/,/)/p=/p/ ]", fixed = FALSE)[[1]])[3], ")", sep = ""),
             P_value = stri_remove_empty(strsplit(HR_0.95CI, split = "[-/(/,/)/p=/p/ ]", fixed = FALSE)[[1]])[4])%>%
      mutate(P_value = ifelse(P_value=="<0.001", "<0.001*",
                              ifelse(as.numeric(P_value)<0.05, paste(P_value, "*", sep=""),
                                     ifelse(as.numeric(P_value)>=0.2, round(as.numeric(P_value), digits = 1), P_value))),
             HR_ = as.numeric(stri_remove_empty(strsplit(HR_0.95CI, split = "[-/(/,/)/p=/p/ ]", fixed = FALSE)[[1]])[1]),
             HRlower_ = as.numeric(stri_remove_empty(strsplit(HR_0.95CI, split = "[-/(/,/)/p=/p/ ]", fixed = FALSE)[[1]])[2]),
             HRupper_ = as.numeric(stri_remove_empty(strsplit(HR_0.95CI, split = "[-/(/,/)/p=/p/ ]", fixed = FALSE)[[1]])[3]),
             SE = (log(HRupper_) - log(HR_)) / 1.96)

    #Nl: the numbers of levels for the predictor variable
    Variable_class <- as.matrix(DF[,Variables])[,i]%>%as.factor()
    Nl <- Variable_class%>%nlevels()

    if(Nl>10){
      Model_result00 <- data.frame(
        `Variables` = Variables_name[i],
        `Levels` = "",
        `HR_95CI` = Model_result01$HR_CI,
        `P` = Model_result01$P_value)%>%
        mutate(
          ` ` = paste(rep(" ", 25), collapse = " "),
          HR = Model_result01$HR_,
          HR_lower = Model_result01$HRlower_,
          HR_upper = Model_result01$HRupper_,
          SE = Model_result01$SE
        )%>%
        dplyr::select(Variables, Levels, HR_95CI, P, ` `, HR, HR_lower, HR_upper, SE)
    }else{
      Model_result00 <- data.frame(
        `Variables` = c(Variables_name[i], rep("", Nl-1)),
        `Levels` = Variable_class%>%levels(),
        `HR_95CI` = c(Hazard_Ratio, Model_result01$HR_CI),
        `P` = c("", Model_result01$P_value))%>%
        mutate(
          ` ` = paste(rep(" ", 25), collapse = " "),
          HR = c(1, Model_result01$HR_),
          HR_lower = c(NA, Model_result01$HRlower_),
          HR_upper = c(NA, Model_result01$HRupper_),
          SE = c(0.000001, Model_result01$SE)
        )%>%
        dplyr::select(Variables, Levels, HR_95CI, P, ` `, HR, HR_lower, HR_upper, SE)
    }

    Model_result_final <- rbind(Model_result_final, Model_result00)
  }
  colnames(Model_result_final) <- c("Variables", "Levels", "HR (95% CI)", "P", " ", "HR", "HR_lower", "HR_upper", "SE")
  Model_result_final
}


#Matrix with Competing Risk
#CSS, 0 <- "no mortality"; 1 <- "cancer-specific mortality"; 2 <- "other death cause"

HRtable_FGHM <- HRtable%>%
  mutate(FG_CSS = ifelse(Re_OS==0, 0,
                         ifelse(Re_CSS==1, 1, 2)),
         .after = Re_OS)

#CSS
#Univariate
FGuni_CSS <- FineGrayUni_Model(Survival_Analysis = "Surv(Followup_OS.CSS, FG_CSS)", Variables = VAR, DF = HRtable_FGHM)

#Multivariate
FGmulti_CSS <- FineGrayMulti_Model(Survival_Analysis = "Surv(Followup_OS.CSS, FG_CSS)", Variables = VAR, DF = HRtable_FGHM)

