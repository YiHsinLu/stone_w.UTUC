# Impact of Kidney Stone History on Survival Outcomes in Upper Tract Urothelial Carcinoma: A Multi-Institutional Propensity Score Weighted Analysis

Bor-En Jong, Hsi-Chin Wu, Wen-Chi Chen, Wen-Jeng Wu, Ching-Chia Li, Yi-Hsin Lu, and Yao-Chou Tsai

##Abstract

Upper tract urothelial carcinoma (UTUC) is an uncommon but aggressive malignancy characterized by high recurrence rates and a marked propensity for metastasis. Although previous research suggests that urinary tract stones may heighten the risk of UTUC, their impact on patient survival has been underexplored. This multi-institutional retrospective study evaluated 3,414 patients who underwent radical nephroureterectomy for UTUC; 169 had documented urinary stones, and 3,245 did not. Demographic, clinical, and tumor-related data were gathered, and univariate and multivariate Cox proportional hazards models were used to assess oncological outcomes. Patients with a history of urinary stones showed significantly worse oncological indicators, including higher metastatic rates, increased UTUC-specific mortality, and reduced disease-free survival (DFS). After adjusting for confounding variables such as pathological stage, tumor size, and lymphovascular invasion (LVI), a history of stone disease remained an independent predictor of poorer cancer-specific survival (hazard ratio [HR] = 1.83, 95% CI: 1.35–2.47, p < 0.001) and DFS (HR = 1.69, 95% CI: 1.29–2.21, p < 0.001). These findings point to a more aggressive tumor phenotype in patients with a history of stones, potentially driven by chronic inflammation, mechanical irritation, and delayed tumor detection. Overall, this study underlines the need for closer surveillance and targeted strategies for UTUC patients with urinary stone disease, and it supports further investigation into underlying molecular mechanisms and optimal therapeutic approaches for this high-risk population.

## R source code

### [1 Baseline](https://github.com/YiHsinLu/stone_w.UTUC/blob/main/01_Baseline_characteristics.R)

* table One for baseline characteristics
* overlap weighted for balancing and love plot

### [2.1 Univariate/Multivariate Cox Regression Model](https://github.com/YiHsinLu/stone_w.UTUC/blob/main/02_01_Survival_analysis_cox_regression.R)

* functions to construct univariate and multivariate analyses for overall, cancer-specific, disease-free, and bladder-free survival

### [2.2 Fine-Gray_Hazard_Model](https://github.com/YiHsinLu/stone_w.UTUC/blob/main/02_02_Fine-Gray_Hazard_Model_competing_risk.R)

* for the competing risk in cancer-specific and bladder-free survival, fine-gray hazard is the method we used

### [3 Kaplan-Meier Survival Curve](https://github.com/YiHsinLu/stone_w.UTUC/blob/main/03_Survival_analysis_KaplanMeier_Survival_Curve.R)

* survival curve for overall, cancer-specific, disease-free, and bladder-free survival
