library(RcmdrPlugin.KMggplot2) # GUI for KM plot
library(tidyverse)
library(lubridate)

dataset_FIXED # full analysis set

# FORMAT VARIABLES
dataset_ANALYSIS = dataset_FIXED %>% select(Identifier, TimeToProgression, TimeToProgression.iRECIST, Progression, Progression.iRECIST, Progression.Binary, Progression.iRECIST.Binary, FollowUp, # survival data 1
                                            TimeToIRAE, TimeToProgression.Dichotmized.Median, TimeToProgression.iRECIST.Dichotmized.Median, TimeToProgression.Dichotmized.Median.Categorized, TimeToProgression.iRECIST.Dichotmized.Median.Categorized, # survival data 2
                                            Deceased, Deceased.Binary, Best.Response, Best.Response.iRECIST, Objective.Response, Objective.Response.Binary, Objective.Response.iRECIST, Objective.Response.iRECIST.Binary, Pseudoprogression, Reclassification, # clinical outcome data
                                            Age, Age.Dichotomized.Median, Smoking, Smoking.Status, Smoking.Duration, Smoking.Duration.Group, Smoking.Duration.Dichotomized.Median, Gender, BMI, BMI.Dichotomized.Median, Karnofsky.PS, PS.Dichotomized.80, ECOG.PS, ECOG.Score, Primary.Location, Metastasis.Category, Refractory.Lesion.Chemotherapy, Objective.Response.Chemotherapy, # patient data
                                            Refractory.Lesion.Chemotherapy, RLC.Category, # chemotherapy efficacy
                                            Brain, Bone, Lung, Liver, # metastasis data
                                            irAE, AE.Strata.Manifestation, irAE.AfterProgression.iRECIST, Chemotherapy.Delivered, Platinum, PD1.Setting.ForMetastaticDis, Number.Prior.Chemotherapy, Patient.Condition, # clinical data
                                            Neu.Baseline, Lym.Baseline, Mo.Baseline, Eo.Baseline, NLR.Baseline, Hb.Baseline, DeRitis.Baseline, LDH.Baseline, CRP.Baseline, Bacteriuria, Pyuria, Variant, # laboratory data
                                            Neu.Dichotomized.Median, Lym.Dichotomized.Median, NLR.Dichotomized.Median, Mo.Dichotomized.Median, Eo.Dichotomized.Median, Hb.Dichotomized.Median, NLR.Baseline.CUT3, Mo.Baseline.ULN, Eo.Baseline.ULN,
                                            De.Ritis.Dichotomized.Median, LDH.Dichotomized.Median, CRP.Dichotomized.Median, # dichotomized laboratory data
                                            NLR.Change, Eo.Change, Mo.Change, # immune cell dynamics
                                            Bellmunt.Factor, LIPI.Factor, Urinary.Diversion.Category, Statin.Concurrent, Radical.Surgery, De.Novo.M1Disease, 
                                            Analgesics.Concurrent, Glucocortocoid.Concurrent, Opioid.Concurrent, PPI.Concurrent, PPI.Detail, Acid.Blocker) 

labelled::var_label(dataset_ANALYSIS) <- list(FollowUp = "Follow-up period, month",
                                              Progression = "Radiographic progression",
                                              Progression.iRECIST = "Radiographic progression according to iRECIST",
                                              Best.Response = "RECIST-defined best overall response",
                                              Best.Response.iRECIST = "iRECIST-defined best overall response",
                                              TimeToProgression = "Time to RECIST-defined progression, month",
                                              TimeToProgression.iRECIST = "Time to iRECIST-defined progression, month",
                                              Objective.Response = "Objective response (CR + PR)",
                                              Objective.Response.iRECIST = "Objective response (immune CR + immune PR)",
                                              Pseudoprogression = "Pseudo-progression",
                                              Age = "Age, year", 
                                              Smoking = "Smoking history", 
                                              Smoking.Status = "Smoking status",
                                              Smoking.Duration = "Smoking duration, pack-year",
                                              Gender = "Gender",
                                              BMI = "Body mass index",
                                              Karnofsky.PS = "Karnofsky performance status",
                                              PS.Dichotomized.80 = "Karnosfky performance status (<80 or >=80)",
                                              ECOG.Score = "ECOG performance status (0 or >=1)", 
                                              Primary.Location = "Primary location of tumor (lower or upper tract)",
                                              Metastasis.Category = "Location of metastases",
                                              Chemotherapy.Delivered = "Context of most recent therapy",
                                              Patient.Condition = "Reasons for chemotherapy discontinuation",
                                              Number.Prior.Chemotherapy = "Number of prior chemotherapy",
                                              PD1.Setting.ForMetastaticDis = "Line of therapy",
                                              Platinum = "Previous platinum therapy",
                                              Bellmunt.Factor = "Bellmunt risk factor",
                                              irAE = "Immune-related adverse event",
                                              irAE.AfterProgression.iRECIST = "Immune-related adverse event after iRECIST-defined progression",
                                              Neu.Baseline = "Neutrophil count, per uL",
                                              Lym.Baseline = "Lymphocyte count, per uL",
                                              Mo.Baseline = "Monocyte count, per uL",
                                              Eo.Baseline = "Eosinophil count, per uL",
                                              NLR.Baseline = "Neutrophil-to-lymphocyte ratio",
                                              Hb.Baseline = "Hemoglobin concentration, g/dL",
                                              DeRitis.Baseline = "De ritis ratio",
                                              LDH.Baseline = "Lactate dehydrogenase, IU/L",
                                              CRP.Baseline = "C-reactive protein, g/dL",
                                              Bacteriuria = "Bacteriuria",
                                              Refractory.Lesion.Chemotherapy = "Progressive lesion during chemotherapy", 
                                              Objective.Response.Chemotherapy = "Objective response during chemotherapy", 
                                              PPI.Concurrent = "Concurrent PPI use", 
                                              Analgesics.Concurrent = "Any analgesics use", 
                                              Opioid.Concurrent = "Opioid snalgesic use")

# CREATE BASELINE CHARACTERISRICS TABLE
# overall population
FactorVariables <- c("Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy", "Smoking.Duration.Group") # factor varables
AllVariables <- c("Age", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy", "Smoking.Status", "Smoking.Duration.Group", "Smoking.Duration") # numeric and factor variables

tableOne <- tableone::CreateTableOne(vars = AllVariables, factorVars = FactorVariables, data = dataset_ANALYSIS)
results <- print(tableOne, nonnormal = c("Age", "Smoking.Duration"), catDigits = 1, contDigits = 1, pDigits = 3, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE) # calculate P value by Fisher exact test

# analysis of patients according to factor of interest
FactorVariables <- c("Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy") # factor varables
AllVariables <- c("Age", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy", "Smoking.Status") # numeric and factor variables

tableOne <- tableone::CreateTableOne(vars = AllVariables, factorVars = FactorVariables, strata = "Smoking.Duration.Group", data = dataset_ANALYSIS)
results <- print(tableOne, catDigits = 1, contDigits = 1, pDigits = 3, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE,
                 nonnormal = c("Age"), # show descriptive statistics in median (IQR)
                 exact = c("Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy", "Smoking.Status")) # calculate P value by Fisher exact test

write.csv(results, "C:/Users/wfuku/OneDrive/research data/JIKEI/ICI/export.csv") # export table

# BOR SUMMARY
tableOne <- tableone::CreateTableOne(vars = "Best.Response.iRECIST", factorVars = "Best.Response.iRECIST", 
                                     strata = "Smoking.Duration.Group", data = dataset_ANALYSIS)
results <- print(tableOne, catDigits = 1, contDigits = 1, pDigits = 3, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE,
                 exact = "Best.Response.iRECIST") # calculate P value by Fisher exact test

# histogram plot 
Histogram.All <- ggpubr::gghistogram(dataset_ANALYSIS, x = "Smoking.Duration",
                                     color = "Smoking.Status", fill = "Smoking.Status",
                                     palette = "nejm", bins = 20) + 
  theme_classic(base_size = 14) + 
  labs(x = "Smoking exposure, pack-year", 
       y = "Number of patients",
       caption = "") +
  theme(legend.position = "top") 
Histogram.All <- ggpubr::ggpar(Histogram.All, legend.title = "Smoking history")
Histogram.All

# density plot 
Density.All <- ggpubr::ggdensity(dataset_ANALYSIS, x = "Smoking.Duration",
                                 color = "Smoking.Status", fill = "Smoking.Status",
                                 palette = "nejm", alpha = 0.25) + 
  theme_classic(base_size = 18) + 
  labs(x = "Smoking exposure, pack-year", 
       y = "Number of patients",
       caption = "") +
  theme(legend.position = "top") 
Density.All <- ggpubr::ggpar(Density.All, legend.title = "Smoking history")
Density.All

# histogram and density plot
HistoDensity.All <- ggplot(dataset_ANALYSIS, aes(x = Smoking.Duration, 
                                                 y = ..density.., 
                                                 fill = Smoking)) + 
  geom_histogram(bins = 20, alpha = 0.2, color = "black") + 
  geom_density(alpha = 0.2) + 
  theme_classic() + 
  theme_classic(base_size = 18) + 
  labs(x = "Smoking exposure, pack-year", 
       y = "Density",
       title = "", 
       caption = "") +
  theme(legend.position = "none") +
  ggsci::scale_color_lancet() + ggsci::scale_fill_lancet()
HistoDensity.All

# IMMUNE-RELATED ADVERSE EVENT SUMMARY
dataset_irAE <- dataset_FIXED %>% filter(irAE == "Yes")
tableOne <- tableone::CreateTableOne(vars = "AE.Detail", factorVars = "AE.Detail", data = dataset_irAE)
results <- print(tableOne, catDigits = 1, contDigits = 1, pDigits = 3, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE,
                 exact = "AE.Detail") # calculate P value by Fisher exact test

# OBJECTIVE RESPONSE RATE FIGURE
# stacked bar chart for objective response rate
dataset_ANALYSIS %>% count(Smoking.Duration.Group, Best.Response.iRECIST) # calculate number of patients according to the responses
prop.res <- dataset_ANALYSIS %>% group_by(Smoking.Duration.Group, Best.Response.iRECIST) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)*100)
prop.res$Response <- factor(prop.res$Best.Response.iRECIST, levels = c("iCR", "iPR", "iSD", "iUPD", "iCPD")) # change order
bar <- ggplot2::ggplot() + 
  ggplot2::theme_classic(base_size = 16) +
  ggplot2::geom_bar(ggplot2::aes(y = freq, x = Smoking.Duration.Group, fill = Response), data = prop.res, stat = "identity") + 
  ggsci::scale_fill_npg() + 
  ggplot2::labs(x = "Smoking exposure", y = "Proportion of patients with the best overall response")
bar # show stacked bar chart

# ASSOCIATION BETWEEN OBJECTIVE RESPONSE AND VARIABLES
# compare treatment response and the emergence of irAEs
fisher.test(dataset_irAE$Smoking.Duration.Group, dataset_irAE$irAE.AfterProgression.iRECIST)
fisher.test(dataset_ANALYSIS$Smoking.Duration.Group, dataset_ANALYSIS$Objective.Response.iRECIST)

# logistic regression analysis
Explanatory = c("Gender", 
                "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy",
                "Smoking.Status", "Smoking.Duration.Group")
Dependent = "Objective.Response.iRECIST.Binary"

#3 analyze in univariable logistic regression model
results <- dataset_ANALYSIS %>% 
  finalfit::glmuni(Dependent, Explanatory) %>% # Univariable Cox regression analysis
  finalfit::fit2df(explanatory_name = "Variable", 
                   estimate_name = "OR (95% CI, P value)", 
                   p_name = "P", 
                   digits = c(2, 2, 3), 
                   confint_sep = "-")
results # show results
#4 analyze in multivariable logistic regression model
results <- dataset_ANALYSIS %>% 
  finalfit::glmmulti(Dependent, Explanatory) %>% # Multivariable Cox regression analysis
  finalfit::fit2df(explanatory_name = "Variable", 
                   estimate_name = "OR (95% CI, P value)", 
                   p_name = "P", 
                   digits = c(2, 2, 3), 
                   confint_sep = "-")
results # show results

# logistic regression analysis for predicting objective response
tableone::CreateTableOne(vars = "Objective.Response.iRECIST", data = dataset_ANALYSIS) # describe OR rate summary
tableone::CreateTableOne(vars = "Best.Response.iRECIST", data = dataset_ANALYSIS, includeNA = FALSE) # describe raw data of the radiographic response
Logistic <- glm(Objective.Response.Binary ~ Age + Gender + ECOG.Score + Primary.Location + 
                  Metastasis.Category + Number.Prior.Chemotherapy +  
                  Smoking.Status + Smoking.Duration, data = dataset_ANALYSIS, family = binomial("logit"))
results <- tableone::ShowRegTable(Logistic, digits = 2, pDigits = 2)

# TIME-TO-EVENT ANALYSIS
# overall population
Survfunc.Followup <- survival::survfit(data = dataset_ANALYSIS, 
                                       survival::Surv(FollowUp, Deceased.Binary) ~ 
                                         1, type = "kaplan-meier", conf.int = 0.95) # baseline -> death
Survfunc.Followup # events, median survival time (95% confidence interval)

Survfunc.Followup <- survival::survfit(data = dataset_ANALYSIS, 
                                       survival::Surv(TimeToProgression.iRECIST, Progression.iRECIST.Binary) ~ 
                                         1, type = "kaplan-meier", conf.int = 0.95) # baseline -> death
Survfunc.Followup # events, median survival time (95% confidence interval)

Survfunc.Followup <- survival::survfit(data = dataset_ANALYSIS, 
                                       survival::Surv(FollowUp, Deceased.Binary) ~ 
                                         Smoking.Duration.Group, type = "kaplan-meier", conf.int = 0.95) # baseline -> death
Survfunc.Followup # events, median survival time (95% confidence interval)

Survfunc.Followup <- survival::survfit(data = dataset_ANALYSIS, 
                                       survival::Surv(TimeToProgression.iRECIST, Progression.iRECIST.Binary) ~ 
                                         Smoking.Duration.Group, type = "kaplan-meier", conf.int = 0.95) # baseline -> death
Survfunc.Followup # events, median survival time (95% confidence interval)

# calculate the median follow-up
library(pec)
quantile(prodlim::prodlim(Hist(FollowUp, Deceased.Binary) ~ 1, data = dataset_ANALYSIS, reverse = TRUE))

# pairwise survival difference
pfs.pairwise <- survminer::pairwise_survdiff(survival::Surv(TimeToProgression.iRECIST, Progression.iRECIST.Binary) ~ Smoking.Status, data = dataset_ANALYSIS)
pfs.pairwise

# multivariable Cox regression analysis using finalfit
#1 prepare survival function
dependent_os = "Surv(FollowUp, Deceased.Binary)" # overall survival (all-cause mortality)
dependent_pfs = "Surv(TimeToProgression.iRECIST, Progression.iRECIST.Binary)" # progression-free survival
#2 group variable for univariable analysis
Variables = c("Age", "Gender", "ECOG.Score",
              "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy",
              "Smoking.Duration")

#3 analyze in univariable Cox regression model
results <- dataset_ANALYSIS %>% 
  finalfit::coxphmulti(dependent_pfs, Variables) %>% # Univariable Cox regression analysis
  finalfit::fit2df(explanatory_name = "Variable", 
                   estimate_name = "HR (95% CI, P value)", 
                   p_name = "P", 
                   digits = c(2, 2, 3), 
                   confint_sep = "-")
results # show results

Variables = c("Age", "Gender", 
              "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy",
              "Smoking.Duration")

#4 analyze in multivariable Cox regression model
results <- dataset_ANALYSIS %>% 
  finalfit::coxphmulti(dependent_os, Variables) %>% # Multivariable Cox regression analysis
  finalfit::fit2df(explanatory_name = "Variable", 
                   estimate_name = "HR (95% CI, P value)", 
                   p_name = "P", 
                   digits = c(2, 2, 3), 
                   confint_sep = "-")
results # show results

write.csv(results, "C:/Users/wfuku/OneDrive/research data/JIKEI/ICI/result.csv") # export table

# test the proportional hazard asumption for a Cox regression model
fit = survival::coxph(survival::Surv(FollowUp, Deceased.Binary) ~ 
                        TimeToProgression.iRECIST, data = dataset_ANALYSIS)
ftest_pfs <- survival::cox.zph(fit)
hazard <- survminer::ggcoxzph(ftest_pfs)
hazard

# calculate Harrell Concordance index
compareC::compareC(dataset_ANALYSIS$FollowUp, dataset_ANALYSIS$Deceased.Binary, # set survival function
                   dataset_ANALYSIS$Objective.Response.Binary, dataset_ANALYSIS$Objective.Response.iRECIST.Binary)

# DECISION CURVE ANALYSIS COMPARING iRECIST AND RECIST
library(survival)
dataset_ANALYSIS <- as.data.frame(dataset_ANALYSIS) # dca package cannot handle tibble
Cox.RECIST = survival::coxph(survival::Surv(dataset_ANALYSIS$FollowUp, dataset_ANALYSIS$Deceased.Binary)
                             ~ Objective.Response, data = dataset_ANALYSIS)
Cox.iRECIST = survival::coxph(survival::Surv(dataset_ANALYSIS$FollowUp, dataset_ANALYSIS$Deceased.Binary)
                              ~ Objective.Response.iRECIST, data = dataset_ANALYSIS)
dataset_ANALYSIS$OR_RECIST <- c(1 - (summary(survival::survfit(Cox.RECIST, newdata = dataset_ANALYSIS), times = 6)$surv))
dataset_ANALYSIS$OR_iRECIST <- c(1 - (summary(survival::survfit(Cox.iRECIST, newdata = dataset_ANALYSIS), times = 6)$surv))
result = dca::stdca(data = dataset_ANALYSIS, outcome = "Deceased.Binary", ttoutcome = "FollowUp", 
                    timepoint = 6, predictors = c("OR_RECIST", "OR_iRECIST"), xstop = 0.3, smooth = TRUE)

# L1 penalized Cox regression analysis
library(ggfortify)
library(ggrepel)

dataset_ANALYSIS = dataset_ANALYSIS %>% mutate(
  Gender.01 = case_when(Gender == "Male" ~ 0, Gender == "Female" ~ 1),
  Smoking.01 = case_when(Smoking == "No" ~ 0, TRUE ~ 1),
  Primary.Location.01 = case_when(Primary.Location == "Lower tract" ~ 0, TRUE ~ 1),
  Metastasis.Category.01 = case_when(Metastasis.Category == "LN metastasis" ~ 0, Metastasis.Category == "Visceral metastasis" ~ 1, TRUE ~ 2))

dataset_PENALIZED = dataset_ANALYSIS %>% 
  select(FollowUp, Deceased.Binary, Age, Gender.01, ECOG.Score, BMI, Smoking.01, Primary.Location.01, Metastasis.Category.01, Number.Prior.Chemotherapy, 
         Hb.Baseline, Neu.Baseline, Lym.Baseline, NLR.Baseline, Mo.Baseline, Eo.Baseline, DeRitis.Baseline, LDH.Baseline, CRP.Baseline, Pyuria)
dataset_PENALIZED <- na.omit(dataset_PENALIZED)

x <- dataset_PENALIZED %>% 
  select(Age, Gender.01, ECOG.Score, BMI, Smoking.01, Primary.Location.01, Metastasis.Category.01, Number.Prior.Chemotherapy, 
         Hb.Baseline, Neu.Baseline, Lym.Baseline, NLR.Baseline, Mo.Baseline, Eo.Baseline, DeRitis.Baseline, LDH.Baseline, CRP.Baseline, Pyuria) %>% as.matrix()
y <- survival::Surv(dataset_PENALIZED$FollowUp, dataset_PENALIZED$Deceased.Binary)
Cox.GLMnet <- glmnet::glmnet(x, y, family = "cox")
autoplot(Cox.GLMnet, xvar = "lambda", 
         size = 1.5, label.size = 5) + theme_classic() 
cvfit = glmnet::cv.glmnet(x, y, family = "cox")

# HAZARD PLOT
library(rms)

dataset_ANALYSIS <- dataset_ANALYSIS %>% filter(Smoking == "Yes")

dd = datadist(dataset_ANALYSIS)
dd$limits$Smoking.Duration[2] <- 25 # define the reference
options(datadist = "dd")

# hazard plot for progression
dataset_ANALYSIS <- as.data.frame(dataset_ANALYSIS) # rwl package cannot handle tibble
CPH.Survival <- cph(Surv(TimeToProgression.iRECIST, Progression.iRECIST.Binary) ~ Age + Gender + ECOG.Score + Primary.Location + 
                      Metastasis.Category + Number.Prior.Chemotherapy + rcs(Smoking.Duration, 3),
                    data = dataset_ANALYSIS, x = TRUE, y = TRUE)
pdata <- Predict(CPH.Survival, Smoking.Duration, ref.zero = TRUE, fun = exp, conf.int = 0.95)

HR.Progression <- ggplot(data = pdata, aes(x = Smoking.Duration, y = yhat), rug = TRUE) +
  geom_line(size = 1) + 
  geom_ribbon(data = pdata, aes(ymin = lower, ymax = upper), alpha = 0.3, linetype = 0, fill = '#4A699099') +
  geom_hline(data = pdata, aes(yintercept = 1), linetype = 3) +
  theme_classic(base_size = 14) +
  labs(x = "Smoking exposure, pack-year", 
       y = "HR (95% CI) for progression",
       caption = "")
HR.Progression

# hazard plot for all-cause mortality
dataset_ANALYSIS <- as.data.frame(dataset_ANALYSIS) # rwl package cannot handle tibble
CPH.Survival <- cph(Surv(FollowUp, Deceased.Binary) ~ Age + Gender + Primary.Location + 
                      Metastasis.Category + Number.Prior.Chemotherapy + rcs(Smoking.Duration, 3),
                    data = dataset_ANALYSIS, x = TRUE, y = TRUE)
pdata <- Predict(CPH.Survival, Smoking.Duration, ref.zero = TRUE, fun = exp, conf.int = 0.95)

HR.Deceased <- ggplot(data = pdata, aes(x = Smoking.Duration, y = yhat), rug = TRUE) +
  geom_line(size = 1) + 
  geom_ribbon(data = pdata, aes(ymin = lower, ymax = upper), alpha = 0.3, linetype = 0, fill = '#4A699099') +
  geom_hline(data = pdata, aes(yintercept = 1), linetype = 3) +
  theme_classic(base_size = 14) +
  labs(x = "Smoking exposure, pack-year", 
       y = "HR (95% CI) for all-cause mortality",
       caption = "")
HR.Deceased

# histogram plot for distribution of progression
Histogram.Progression <- ggpubr::gghistogram(dataset_ANALYSIS, x = "Smoking.Duration",
                                             color = "Progression.iRECIST", fill = "Progression.iRECIST",
                                             palette = "nejm", bins = 15) + 
  theme_classic(base_size = 14) + 
  labs(x = "Smoking exposure, pack-year", 
       y = "Number of \n patients",
       caption = "") +
  theme(legend.position = "top") 
Histogram.Progression <- ggpubr::ggpar(Histogram.Progression, legend.title = "Progression")
Histogram.Progression

# histogram plot for distribution of progression
Histogram.Deceased <- ggpubr::gghistogram(dataset_ANALYSIS, x = "Smoking.Duration",
                                          color = "Deceased", fill = "Deceased",
                                          palette = "nejm", bins = 15) + 
  theme_classic(base_size = 14) + 
  labs(x = "Smoking exposure, pack-year", 
       y = "Number of \n patients",
       caption = "") +
  theme(legend.position = "top") 
Histogram.Deceased <- ggpubr::ggpar(Histogram.Deceased, legend.title = "All-cause mortality")
Histogram.Deceased

# density plot according to progression with marginal rug
Density.Progression <- ggpubr::ggdensity(dataset_ANALYSIS, x = "Smoking.Duration", 
                                         fill = "Progression.iRECIST", 
                                         color = "Progression.iRECIST",
                                         palette = "nejm",
                                         rug = TRUE) + 
  theme_classic(base_size = 14) + 
  labs(x = "Smoking exposure, pack-year", 
       y = "Density",
       caption = "") +
  theme(legend.position = "none") 
Density.Progression

# density plot according to all-cause mortality with marginal rug
Density.Deceased <- ggpubr::ggdensity(dataset_ANALYSIS, x = "Smoking.Duration", 
                                      fill = "Deceased", 
                                      color = "Deceased",
                                      palette = "nejm",
                                      rug = TRUE) + 
  theme_classic(base_size = 14) + 
  labs(x = "", 
       y = "Density",
       caption = "") +
  theme(legend.position = "none") 
Density.Deceased

ggpubr::ggarrange(Histogram.Progression, Histogram.Deceased, HR.Progression, HR.Deceased,
                  ncol = 2, nrow = 2, heights = c(2, 3))