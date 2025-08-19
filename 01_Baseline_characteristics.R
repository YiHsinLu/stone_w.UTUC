library(dplyr)
library(tableone)
library(MatchIt)
library(WeightIt)
library(PSweight)

#import the row data
UTUC_database <- read.csv("csv/RowData_UTUCdatabase_2024-11-13.csv")

#select the variables
WD_stone <- UTUC_database%>%
  dplyr::select(Stone, Gender, Age, Cell_Type, Side, Location, Size, Pathological_Stage, Grade, RF.Smoking, RF.Herbal_supplements, RF.Family_hx, RF.ESRD.CRI, RF.Previous_Nephroureterectomy.UC, RF.Previous_renal_sparing_surgery.UTUC, Bladder_Cancer, CIS, LVI, Surgical_Margin, Preoperation_Urine_Cytology, Preoperative_Hydronephrosis, Tumor_Necrosis, Chemotherapy_Type_for_UTUC, NxUx_Method, NxUx_Access, Post_operation_Intravesical_CT_Instillation, Residual_Bladder_Cuff, Metastasis, Overall_Mortality, Cancer_specific, Disease_Free, Bladder_Recurrence, Followup_OS.CSS, Followup_BDFS.DFS)


#01 create the baseline characteristics
#TableOne
WD_stone_baseline <- WD_stone

vars_outcome0 <- colnames(WD_stone_baseline)[-1]
baseline_all0 <- CreateTableOne(data = WD_stone_baseline,
                                vars = vars_outcome0,
                                strata = "Stone",
                                includeNA = FALSE)

a0 <- print(baseline_all0, smd = TRUE, showAllLevels = T)
a0%>% kbl(caption = "Patient's Baseline") %>%
  kable_paper("hover",full_width=F)

#export table One as csv
write.csv(a0, "csv/baseline.csv")


#----------------------------------------------------------------#

#02 Overlap weighted

ps.approach <- Stone ~ Gender+Age+Cell_Type+Side+Location+Size+Pathological_Stage+Grade+RF.Smoking+RF.Herbal_supplements+RF.ESRD.CRI+RF.Previous_Nephroureterectomy.UC+Bladder_Cancer+CIS+LVI+Surgical_Margin+Preoperation_Urine_Cytology+Preoperative_Hydronephrosis+Tumor_Necrosis+Chemotherapy_Type_for_UTUC
bal.approach <- SumStat(ps.formula = ps.approach,
                        weight = c("IPW", "overlap"),
                        data = WD_stone_baseline)

W.out <- weightit(ps.approach,
                  data = WD_stone_baseline,
                  estimand = "ATO",
                  method = "ps")

# create pseudopopulation using overlap weighting for each patient
# make sample size same as the original (does not affect to results)
ps_model1_overlap<-WD_stone_baseline %>%
  mutate(overlap_weight = case_when(Stone=="0 No" ~ W.out$weights*((1707*2)/sum(W.out$weights)),
                                    Stone=="1 Yes" ~ W.out$weights*((1707*2)/sum(W.out$weights))))

# model
# complications
model_design <- svydesign(ids = ~1,
                          weights = ~overlap_weight,
                          data = ps_model1_overlap)

# after overlap weighting SMD
vars_result <- c("Residual_Bladder_Cuff", "Metastasis", "Overall_Mortality", "Cancer_specific", "Disease_Free", "Bladder_Recurrence", "Followup_OS.CSS", "Followup_BDFS.DFS")

model1<-svydesign(data=ps_model1_overlap,
                  ids = ~1,
                  weights = ~overlap_weight)

model1_tbl1<-svyCreateTableOne(data=model1,
                               vars=vars_outcome0,
                               strata = "Stone")

a_weighted<-print(model1_tbl1, smd = TRUE, showAllLevels = TRUE)
a_weighted%>%kbl(caption = "after overlap weighting") %>%
  kable_paper("hover",full_width=F)



# before overlap weighting SMD
baseline_all_unw <- CreateTableOne(data = WD_stone_baseline,
                                   vars = vars_outcome0,
                                   strata = "Stone",
                                   includeNA = TRUE)

a_unweighted <- print(baseline_all_unw, smd = TRUE, showAllLevels = T)
a_unweighted%>% kbl(caption = "before overlap weighting") %>%
  kable_paper("hover",full_width=F)


ipw_weight <- as.data.frame(bal.approach$IPW.sumstat)
ipw_weight <- ipw_weight%>%
  mutate(SMD_ipw = abs(`Mean 0 No`-`Mean 1 Yes`)/sqrt(`Weighted SD 0 No`^2+`Weighted SD 1 Yes`^2-`Weighted SD 0 No`*`Weighted SD 1 Yes`))
overlap_weight <- as.data.frame(bal.approach$overlap.sumstat)
overlap_weight <- overlap_weight%>%
  mutate(SMD_overlap = abs(`Mean 0 No`-`Mean 1 Yes`)/sqrt(`Weighted SD 0 No`^2+`Weighted SD 1 Yes`^2-`Weighted SD 0 No`*`Weighted SD 1 Yes`))

SMD <- data.frame(variable_levels = rownames(unweighted),
                  unweighted = unweighted$SMD_unweighted,
                  iptw = ipw_weight$SMD_ipw,
                  overlap = overlap_weight$SMD_overlap)

#Love Plot
ggplot(data = SMD)+
  geom_vline(xintercept = 0.1, linetype="dashed", size=0.5)+
  geom_vline(xintercept = 0, size=0.5)+
  geom_point(mapping = aes(x = unweighted, y = variable_levels, color = "unweighted"),
             shape = 0, size = 2)+
  geom_point(mapping = aes(x = overlap, y = variable_levels, color = "overlap"),
             shape = 17, size = 2)+
  geom_point(mapping = aes(x = iptw, y = variable_levels, color = "iptw"),
             shape = 19, size = 2)+
  xlim(0,0.15)+
  xlab("Standardized Mean Difference")+
  ylab("")+
  ggtitle("Create love plot to visualize balancing")+
  theme(axis.text.y = element_text(size = 10))

#Baseline Table in overlap
write.csv(a_unweighted, "csv/Baseline_unweighted.csv")
write.csv(a_weighted, "csv/Baseline_Overlap.csv")
