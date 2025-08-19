library(dplyr)
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(survminer)

SuvCurve <- HRtable

#Kaplan-Meier Methods
fitOS <- survfit(Surv(Followup_OS.CSS, Re_OS) ~ Stone, data = SuvCurve)
# overlap weighted
# fitOS <- survfit(Surv(Followup_OS.CSS, Re_OS) ~ Stone, data = SuvCurve, weights = ps_model1_overlap$overlap_weight)

#Survival Curve
#OS
ggsurvplot(fitOS,
           data = SuvCurve,
           surv.median.line = "hv", # Add medians survival

           # Change legends: title & labels
           legend.title = "Stone",
           legend.labs = c("0 No", "1 Yes"),

           # Add p-value and tervals
           pval = TRUE,
           conf.int = TRUE,

           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           risk.table.title = "Number at Risk",

           # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
           # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
           palette = c("#E7B800", "#2E9FDF"),
           ggtheme = theme_bw(), # Change ggplot2 theme

           # 5-years survival
           xlim = c(0,60),
           # breaks as a year (12 monthes)
           break.x.by = 12,
           xlab = "Time in month",
           title = "Overall Survival Curve"
)

#CSS
fitCSS<- survfit(Surv(Followup_OS.CSS, Re_CSS) ~ Stone, data = SuvCurve)
ggsurvplot(fitCSS,
           data = SuvCurve,
           surv.median.line = "hv", # Add medians survival

           # Change legends: title & labels
           legend.title = "Stone",
           legend.labs = c("0 No", "1 Yes"),

           # Add p-value and tervals
           pval = TRUE,
           conf.int = TRUE,

           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           risk.table.title = "Number at Risk",

           # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
           # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
           palette = c("#E7B800", "#2E9FDF"),
           ggtheme = theme_bw(), # Change ggplot2 theme

           # 5-years survival
           xlim = c(0,60),
           # breaks as a year (12 monthes)
           break.x.by = 12,
           xlab = "Time in month",
           title = "Cancer-Specific Survival Curve"
)

#DFS
fitDFS<- survfit(Surv(Followup_BDFS.DFS, Re_DFS) ~ Stone, data = SuvCurve)
ggsurvplot(fitDFS,
           data = SuvCurve,
           surv.median.line = "hv", # Add medians survival

           # Change legends: title & labels
           legend.title = "Stone",
           legend.labs = c("0 No", "1 Yes"),

           # Add p-value and tervals
           pval = TRUE,
           conf.int = TRUE,

           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           risk.table.title = "Number at Risk",

           # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
           # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
           palette = c("#E7B800", "#2E9FDF"),
           ggtheme = theme_bw(), # Change ggplot2 theme

           # 5-years survival
           xlim = c(0,60),
           # breaks as a year (12 monthes)
           break.x.by = 12,
           xlab = "Time in month",
           title = "Disease-Free Survival Curve"
)

#BRFS
fitBRFS <- survfit(Surv(Followup_BDFS.DFS, Re_BRFS) ~ Stone, data = SuvCurve)
ggsurvplot(fitBRFS,
           data = SuvCurve,
           surv.median.line = "hv", # Add medians survival

           # Change legends: title & labels
           legend.title = "Stone",
           legend.labs = c("0 No", "1 Yes"),

           # Add p-value and tervals
           pval = TRUE,
           conf.int = TRUE,

           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           risk.table.title = "Number at Risk",

           # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
           # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
           palette = c("#E7B800", "#2E9FDF"),
           ggtheme = theme_bw(), # Change ggplot2 theme

           # 5-years survival
           xlim = c(0,60),
           # breaks as a year (12 monthes)
           break.x.by = 12,
           xlab = "Time in month",
           title = "Bladder-Free Survival Curve"
)
