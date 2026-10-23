# ========================== MULTINOMIAL LOGISTIC REGRESSION (MLR) MODEL: PERF =========================
# Fitting an MLR-model towards finalizing its input space in modelling the transition rate: 
# Performing to Performing (baseline), Default, Settlement, Write-off
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Default risk term-structure modelling using Markov-models
# SCRIPT AUTHOR(S): Dr Arno Botha (AB)
# ------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R
#   - 3b.Data_Fusion2.R
#
# -- Inputs:
#   - datCredit_train | Training set, created from subsampled set from 3b
#   - datCredit_valid | Validation set, created from subsampled set from 3b
#
# -- Outputs:
#   - <Analytics> | Input space
# ------------------------------------------------------------------------------------------------------





# ------ 1. Preliminaries

# - Confirm that required data objects are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)

# - Extreme down-sample the training data, used only for stepwise forward selection procedure and final confirmation
# of statistical significance
smp_size <- 50000 # Number of keys/loans during the subsampling step
# Implied sampling fraction for the downsampling step
smp_perc <- smp_size/datCredit_train[Counter==1,.N]
set.seed(6,kind="Mersenne-Twister")
# Training Key population
# Get unique subject IDs or keys from the full dataset
datKeys <- datCredit_train %>% subset(Counter==1, c("Date", "LoanID", "Date_Origination"))
# Use stratified random sampling to select at random some keys from which the training set will be populated 
datKeys_sampled <- datKeys %>% group_by(Date_Origination) %>% slice_sample(prop=smp_perc)
# Obtain the associated loan records in creating the subsampled dataset
datCredit_train_sub <- copy(datCredit_train %>% subset(LoanID %in% datKeys_sampled$LoanID))

# - Distributional analyses
describe(datCredit_train[MarkovStatus=="Perf",Target_FromP]) # Dominant class: Performing
describe(datCredit_train[MarkovStatus=="Def",Target_FromD]) # Dominant class: Default
describe(datCredit_train_sub[MarkovStatus=="Perf",Target_FromP]) # Dominant class: Performing
describe(datCredit_train_sub[MarkovStatus=="Def",Target_FromD]) # Dominant class: Default

# - Fit an empty model as a performance gain, used within some diagnostic functions
modMLR_base_perf <- multinom(Target_FromP ~  1, 
                        data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
modMLR_base_perf_sub <- multinom(Target_FromP ~  1, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)





# ------ 2a. Modelling theme: Portfolio-level delinquency-themed variables

# --- Single-factor models: g0_Delinq_Any_Aggr_Prop and lags
# Where, relevant, we build single-factor models to determine which lag-orders are best
# - g0_Delinq_Any_Aggr_Prop
modMLR <- multinom(Target_FromP ~  g0_Delinq_Any_Aggr_Prop,
                  data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC:  1,164,537; McFadden R^2:  0.29%; AUC:  58.96%

# - g0_Delinq_Any_Aggr_Prop_Lag_1
modMLR <- multinom(Target_FromP ~  g0_Delinq_Any_Aggr_Prop_Lag_1,
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC:  1,164,554; McFadden R^2:  0.29%; AUC:  58.81%

# - g0_Delinq_Any_Aggr_Prop_Lag_2
modMLR <- multinom(Target_FromP ~  g0_Delinq_Any_Aggr_Prop_Lag_2,
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC:  1,164,661; McFadden R^2:  0.28%; AUC:  58.38%

# - g0_Delinq_Any_Aggr_Prop_Lag_3
modMLR <- multinom(Target_FromP ~  g0_Delinq_Any_Aggr_Prop_Lag_3, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC:  1,164,852; McFadden R^2:  0.26% ; AUC:  58.01%

# - g0_Delinq_Any_Aggr_Prop_Lag_4
modMLR <- multinom(Target_FromP ~  g0_Delinq_Any_Aggr_Prop_Lag_4,
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,164,942; McFadden R^2:  0.25%; AUC:  57.78%

# - g0_Delinq_Any_Aggr_Prop_Lag_5
modMLR <- multinom(Target_FromP ~  g0_Delinq_Any_Aggr_Prop_Lag_5,
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,052; McFadden R^2:  0.24%; AUC:  57.56%

# - g0_Delinq_Any_Aggr_Prop_Lag_6
modMLR <- multinom(Target_FromP ~  g0_Delinq_Any_Aggr_Prop_Lag_6,
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,116; McFadden R^2:  0.24%; AUC:  57.50%

# - g0_Delinq_Any_Aggr_Prop_Lag_9
modMLR <- multinom(Target_FromP ~  g0_Delinq_Any_Aggr_Prop_Lag_9,
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,291; McFadden R^2:  0.22%; AUC:  57.61%

# - g0_Delinq_Any_Aggr_Prop_Lag_12
modMLR <- multinom(Target_FromP ~  g0_Delinq_Any_Aggr_Prop_Lag_12,
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,523; McFadden R^2:  0.20%; AUC:  57.46%
### FINAL: The highest-ranked variables across all fit statistics:
# g0_Delinq_Any_Aggr_Prop; g0_Delinq_Any_Aggr_Prop_Lag_1
# There appears to be a monotonic decreasing trend across all fit statistics as the lag-order increases; earlier is better



# --- Single-factor models: DefaultStatus1_Aggr_Prop and lags

# - DefaultStatus1_Aggr_Prop
modMLR <- multinom(Target_FromP ~  DefaultStatus1_Aggr_Prop, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC:  1,165,715; McFadden R^2:  0.19%; AUC:  53.63%

# - DefaultStatus1_Aggr_Prop_Lag_1
modMLR <- multinom(Target_FromP ~  DefaultStatus1_Aggr_Prop_Lag_1,
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,797; McFadden R^2:  0.18%; AUC:  52.73%

# - DefaultStatus1_Aggr_Prop_Lag_2
modMLR <- multinom(Target_FromP ~  DefaultStatus1_Aggr_Prop_Lag_2,
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,816; McFadden R^2:  0.18%; AUC:  51.95%

# - DefaultStatus1_Aggr_Prop_Lag_3
modMLR <- multinom(Target_FromP ~  DefaultStatus1_Aggr_Prop_Lag_3,
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,798 ; McFadden R^2:  0.18%; AUC:  51.38%

# - DefaultStatus1_Aggr_Prop_Lag_4
modMLR <- multinom(Target_FromP ~  DefaultStatus1_Aggr_Prop_Lag_4,
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,818; McFadden R^2:  0.18%; AUC:  50.95%

# - DefaultStatus1_Aggr_Prop_Lag_5
modMLR <- multinom(Target_FromP ~  DefaultStatus1_Aggr_Prop_Lag_5,
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,827; McFadden R^2:  0.18%; AUC:  50.50%

# - DefaultStatus1_Aggr_Prop_Lag_6
modMLR <- multinom(Target_FromP ~  DefaultStatus1_Aggr_Prop_Lag_6,
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,921; McFadden R^2:  0.17%; AUC:  49.99%

# - DefaultStatus1_Aggr_Prop_Lag_9
modMLR <- multinom(Target_FromP ~  DefaultStatus1_Aggr_Prop_Lag_9,
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,208; McFadden R^2:  0.14% ; AUC:  51.66%

# - DefaultStatus1_Aggr_Prop_Lag_12
modMLR <- multinom(Target_FromP ~  DefaultStatus1_Aggr_Prop_Lag_12, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,293; McFadden R^2:  0.14%; AUC:  53.56%
### FINAL: The highest-ranked variables across all fit statistics:
# DefaultStatus1_Aggr_Prop; DefaultStatus1_Aggr_Prop_Lag_1
# There appears to be a monotonic decreasing trend across all fit statistics as the lag-order increases; earlier is better
# Relatively weaker variable than g0_Delinq_Any_Aggr_Prop.



# --- Single-factor models: Various delinquency-themed portfolio-level variables

# - g0_Delinq_Ave
modMLR <- multinom(Target_FromP ~  g0_Delinq_Ave, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,164,454; McFadden R^2:  0.29%; AUC: 59.17%

# - CuringEvents_Aggr_Prop
modMLR <- multinom(Target_FromP ~  CuringEvents_Aggr_Prop, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,286; McFadden R^2:  0.14%; AUC:  50.08%

# - ArrearsToBalance_Aggr_Prop
modMLR <- multinom(Target_FromP ~  ArrearsToBalance_Aggr_Prop, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,164,738; McFadden R^2:  0.27%; AUC: 58.61%
### FINAL: The highest-ranked variables across all fit statistics:
# g0_Delinq_Ave; ArrearsToBalance_Aggr_Prop





# ------ 2b. Combining insights: Delinquency-themed portfolio-level variables

# --- Full-model
modMLR_full <- multinom(Target_FromP ~ ArrearsToBalance_Aggr_Prop + CuringEvents_Aggr_Prop + g0_Delinq_Ave + 
                     g0_Delinq_Any_Aggr_Prop + g0_Delinq_Any_Aggr_Prop_Lag_1 + 
                       DefaultStatus1_Aggr_Prop + DefaultStatus1_Aggr_Prop_Lag_1, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,161,848; McFadden R^2:  0.52%; AUC: 56.19%

# - Statistical significance: Wald-test
modMLR_full_sum <- summary(modMLR_full)
(z <- modMLR_full_sum$coefficients  / modMLR_full_sum$standard.errors) # test statistic
(p <- (1 - pnorm(abs(z), 0, 1)) * 2) # 2-tailed z test p-value
### RESULTS: Insignificant variables for state (l): g0_Delinq_Ave  (S); g0_Delinq_Any_Aggr_Prop (S)
# g0_Delinq_Any_Aggr_Prop_Lag_1  (W)
# Wald statistic is known to be inappropriate for large sample sizes since everything becomes significant.



# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ ArrearsToBalance_Aggr_Prop +  CuringEvents_Aggr_Prop + g0_Delinq_Ave + 
                          g0_Delinq_Any_Aggr_Prop + g0_Delinq_Any_Aggr_Prop_Lag_1 + 
                          DefaultStatus1_Aggr_Prop + DefaultStatus1_Aggr_Prop_Lag_1, 
                        data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 401,065; McFadden R^2:  0.56%; AUC: 56.16%
# Agrees very closely (aside from the AIC) with full-sample model
# Henceforth, we shall use only the subsampled variant as an expediency

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_perf_sub, scope = list(lower = modMLR_base_perf_sub, upper = modMLR_full_sub), 
                          direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 45m
### RESULTS: AIC: 401,081; McFadden R^2:  0.55%; AUC:  56.17%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 4m
### RESULTS: All variables are significant
### FINAL: Selected variables: g0_Delinq_Ave; DefaultStatus1_Aggr_Prop; g0_Delinq_Any_Aggr_Prop;





# ------ 3a. Modelling theme: Other portfolio-level variables

# --- Single-factor models: InterestRate_Margin_Aggr_Med and lags

# - InterestRate_Margin_Aggr_Med
modMLR <- multinom(Target_FromP ~  InterestRate_Margin_Aggr_Med, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,359; McFadden R^2:  0.13%; AUC:  57.05%

# - InterestRate_Margin_Aggr_Med_1
modMLR <- multinom(Target_FromP ~  InterestRate_Margin_Aggr_Med_1, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,387; McFadden R^2:  0.13%; AUC:  56.98%

# - InterestRate_Margin_Aggr_Med_2
modMLR <- multinom(Target_FromP ~  InterestRate_Margin_Aggr_Med_2, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,398; McFadden R^2:  0.13% ; AUC:  56.84%

# - InterestRate_Margin_Aggr_Med_3
modMLR <- multinom(Target_FromP ~  InterestRate_Margin_Aggr_Med_3, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC:  1,166,430; McFadden R^2:  0.12%; AUC:  56.72%

# - InterestRate_Margin_Aggr_Med_9
modMLR <- multinom(Target_FromP ~  InterestRate_Margin_Aggr_Med_9, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,720; McFadden R^2:  0.10%; AUC:  55.94%
### FINAL: The highest-ranked variables across all fit statistics:
# InterestRate_Margin_Aggr_Med; InterestRate_Margin_Aggr_Med_1;
# There appears to be a monotonic decreasing trend across all fit statistics as the lag-order increases; earlier is better



# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ InterestRate_Margin_Aggr_Med + InterestRate_Margin_Aggr_Med_1 + InterestRate_Margin_Aggr_Med_2 +
                              InterestRate_Margin_Aggr_Med_3 + InterestRate_Margin_Aggr_Med_9, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 402,400; McFadden R^2:  0.23%; AUC: 56.70%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_perf_sub, scope = list(lower = modMLR_base_perf_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 1.3h
### RESULTS: Selected variables: InterestRate_Margin_Aggr_Med; InterestRate_Margin_Aggr_Med_2; InterestRate_Margin_Aggr_Med_9;
# AIC:  402,427; McFadden R^2:  0.22%; AUC:  56.69%



# --- Single-factor models: Various portfolio-level variables

# - InstalmentToBalance_Aggr_Prop
modMLR <- multinom(Target_FromP ~  InstalmentToBalance_Aggr_Prop, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,372; McFadden R^2:  0.13%; AUC:  54.73%

# - AgeToTerm_Aggr_Mean
modMLR <- multinom(Target_FromP ~  AgeToTerm_Aggr_Mean, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,066; McFadden R^2:  0.24%; AUC: 57.61%

# - PerfSpell_Maturity_Aggr_Mean
modMLR <- multinom(Target_FromP ~  PerfSpell_Maturity_Aggr_Mean, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,167,585; McFadden R^2:  0.03%; AUC:  51.52%

# - CreditLeverage_Aggr
modMLR <- multinom(Target_FromP ~  CreditLeverage_Aggr, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,429; McFadden R^2:  0.21%; AUC:  57.84%

# - Ave_Margin_Aggr
modMLR <- multinom(Target_FromP ~  Ave_Margin_Aggr, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,297; McFadden R^2:  0.14%; AUC:  57.30%

# - NewLoans_Aggr_Prop
modMLR <- multinom(Target_FromP ~  NewLoans_Aggr_Prop, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,573; McFadden R^2:  0.11%; AUC:  52.87%
### FINAL: The highest-ranked variables across all fit statistics:
# CreditLeverage_Aggr; AgeToTerm_Aggr_Mean;





# ------ 3b. Combining insights: Other portfolio-level variables
# ---- Combining insights: Other portfolio-level variables

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ InterestRate_Margin_Aggr_Med + InterestRate_Margin_Aggr_Med_2 + InterestRate_Margin_Aggr_Med_9 +
                              InstalmentToBalance_Aggr_Prop + AgeToTerm_Aggr_Mean + PerfSpell_Maturity_Aggr_Mean + 
                              CreditLeverage_Aggr + Ave_Margin_Aggr + NewLoans_Aggr_Prop , 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 401,288; McFadden R^2:  0.51%; AUC:  54.96%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_perf_sub, scope = list(lower = modMLR_base_perf_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 1.9h
### RESULTS: AIC: 401,326; McFadden R^2:  0.50%; AUC:  54.81%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime: 13m
### RESULTS: All variables are significant
### FINAL: Selected variables: InterestRate_Margin_Aggr_Med_2; InstalmentToBalance_Aggr_Prop; 
# AgeToTerm_Aggr_Mean; NewLoans_Aggr_Prop; CreditLeverage_Aggr ; PerfSpell_Maturity_Aggr_Mean
# It is strange that the lagged variant of InterestRate_Margin_Aggr_Med was chosen over its non-lagged variant,
# even though the latter had better fit statistics in single-factor models





# ------ 3c. Combining insights: Complete set of portfolio-level variables

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + g0_Delinq_Any_Aggr_Prop +
                              InterestRate_Margin_Aggr_Med + InterestRate_Margin_Aggr_Med_2 + InstalmentToBalance_Aggr_Prop + 
                              AgeToTerm_Aggr_Mean + NewLoans_Aggr_Prop + Ave_Margin_Aggr + CreditLeverage_Aggr +
                              PerfSpell_Maturity_Aggr_Mean, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 400,976; McFadden R^2:  0.59%; AUC:  56.59%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_perf_sub, scope = list(lower = modMLR_base_perf_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 1.6h
### RESULTS: AIC: 401,006; McFadden R^2:  0.57%; AUC:  56.25%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime: 8m
### RESULTS: All variables are significant
# Selected variables: g0_Delinq_Ave; DefaultStatus1_Aggr_Prop; g0_Delinq_Any_Aggr_Prop; InstalmentToBalance_Aggr_Prop
# Additional variables manually selected using expert judgment: 
#   AgeToTerm_Aggr_Mean; CreditLeverage_Aggr; InterestRate_Margin_Aggr_Med



# --- Model 1, subsampled set
modMLR_sub_cand <- multinom(Target_FromP ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + g0_Delinq_Any_Aggr_Prop +
                              InstalmentToBalance_Aggr_Prop + AgeToTerm_Aggr_Mean + 
                              CreditLeverage_Aggr + InterestRate_Margin_Aggr_Med, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_sub_cand, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 400,990; McFadden R^2:  0.58%; AUC:  56.73%

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_sub_cand, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime: 13.5m
### RESULTS: Insignificant variables include: InstalmentToBalance_Aggr_Prop; AgeToTerm_Aggr_Mean; InterestRate_Margin_Aggr_Med;



# --- Model 2, subsampled set
modMLR_sub_cand <- multinom(Target_FromP ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + g0_Delinq_Any_Aggr_Prop +
                                CreditLeverage_Aggr + InterestRate_Margin_Aggr_Med, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_sub_cand, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 401,013; McFadden R^2:  0.57%; AUC:  56.62%

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_sub_cand, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime: 7.3m
### RESULTS: All variables are significant
### FINAL: Selected variables: g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + g0_Delinq_Any_Aggr_Prop +
# CreditLeverage_Aggr + InterestRate_Margin_Aggr_Med





# ------ 4a. Modelling theme: Loan-level delinquency-themed variables

# --- Single-factor models: Various information theoretic forms of delinquency
# - TimeInPerfSpell
modMLR <- multinom(Target_FromP ~  TimeInPerfSpell, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,163,054; McFadden R^2:  0.41%; AUC:  60.30%

# - g0_Delinq_fac
modMLR <- multinom(Target_FromP ~  g0_Delinq_fac, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 939,524; McFadden R^2:  19.55%; AUC:  96.86%

# - g0_Delinq
modMLR <- multinom(Target_FromP ~  g0_Delinq, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 940,128; McFadden R^2:  19.50%; AUC:  96.86%

# - g0_Delinq_Num
modMLR <- multinom(Target_FromP ~  g0_Delinq_Num, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,149,217; McFadden R^2: 1.60% ; AUC:  86.90%

# - TimeInDelinqState
modMLR <- multinom(Target_FromP ~  TimeInDelinqState, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,080,924; McFadden R^2:  7.45%; AUC:  93.21%

# - slc_acct_arr_dir_3
modMLR <- multinom(Target_FromP ~  slc_acct_arr_dir_3, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,049,220; McFadden R^2:  10.16%; AUC:  89.66%

# - slc_acct_roll_ever_24_imputed_mean
modMLR <- multinom(Target_FromP ~  slc_acct_roll_ever_24_imputed_mean, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,097,030; McFadden R^2:  6.07%; AUC:  91.54%
### FINAL: The highest-ranked variables across all fit statistics: 
# g0_Delinq_fac (over g0_Delinq) + slc_acct_arr_dir_3 + TimeInDelinqState + slc_acct_roll_ever_24_imputed_mean + g0_Delinq_Num 
# It is remarkable the good fit of g0_Delinq_fac compared to other delinquency-themed variables, even though
# these loan-level variables are overall better fitting than the portfolio-level ones so far.



# --- Single-factor models: speed at which delinquency changes across various window lengths
# - g0_Delinq_SD_4
modMLR <- multinom(Target_FromP ~  g0_Delinq_SD_4, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,037,235; McFadden R^2:  11.19%; AUC:  91.81%

# - g0_Delinq_SD_5
modMLR <- multinom(Target_FromP ~  g0_Delinq_SD_5, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,032,944; McFadden R^2:  11.55%; AUC:  92.93%

# - g0_Delinq_SD_6
modMLR <- multinom(Target_FromP ~  g0_Delinq_SD_6, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,031,489; McFadden R^2:  11.68%; AUC:  93.56%

# - g0_Delinq_SD_9
modMLR <- multinom(Target_FromP ~  g0_Delinq_SD_9, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,063,495; McFadden R^2:  8.94% ; AUC:  93.81%

# - g0_Delinq_SD_12
modMLR <- multinom(Target_FromP ~  g0_Delinq_SD_12, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,088,662; McFadden R^2:  6.78%; AUC:   93.25%
### FINAL: The highest-ranked variables across all fit statistics:
# g0_Delinq_SD_6 + g0_Delinq_SD_5 + g0_Delinq_SD_4
# There appears to be a monotonic increasing trend across all fit statistics as the window length increases; 
# longer is better, but only up to 6-months, whereafter most fit statistics start to decrease again





# ------ 4b. Combining insights: Loan-level delinquency-themed variables

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + 
                              slc_acct_roll_ever_24_imputed_mean + g0_Delinq_Num + 
                              g0_Delinq_SD_6 + g0_Delinq_SD_5 + g0_Delinq_SD_4, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 321,735; McFadden R^2:  20.24%; AUC:  96.08%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_perf_sub, scope = list(lower = modMLR_base_perf_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 2.8h
### RESULTS: AIC: 321,762; McFadden R^2:  20.23%; AUC:  96.13%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 22.5m
### RESULTS: All variables are significant
# Selected variables: g0_Delinq_fac + g0_Delinq_SD_6 + g0_Delinq_SD_4 + TimeInDelinqState + slc_acct_arr_dir_3 + 
# g0_Delinq_Num 



# --- Model 1, subsampled set
modMLR_sub_cand <- multinom(Target_FromP ~ g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + 
                              slc_acct_roll_ever_24_imputed_mean + g0_Delinq_Num + g0_Delinq_SD_6, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_sub_cand, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 322,057; McFadden R^2:  20.16%; AUC:  96.23%

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_sub_cand, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 22.7m
### RESULTS: All variables are significant
### FINAL: Selected variables: 0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + 
# slc_acct_roll_ever_24_imputed_mean + g0_Delinq_Num + g0_Delinq_SD_6





# ------ 5a. Modelling theme: Loan-level time-fixed variables

# --- Single-factor models: various time-fixed variables
# - Principal_Real
modMLR <- multinom(Target_FromP ~  Principal_Real, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,594; McFadden R^2:  0.11%; AUC:  55.70%

# - InterestRate_Margin
modMLR <- multinom(Target_FromP ~  InterestRate_Margin, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,156,069; McFadden R^2:   1.01%; AUC:  56.69%

# - LN_TPE
modMLR <- multinom(Target_FromP ~  LN_TPE, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,167,597; McFadden R^2:  0.02%; AUC:  48.51%

# - pmnt_method_grp
modMLR <- multinom(Target_FromP ~  pmnt_method_grp, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,143,838; McFadden R^2:  2.06%; AUC:  71.71%
### FINAL: The highest-ranked variables across all fit statistics:
# pmnt_method_grp + InterestRate_Margin





# ------ 5b. Modelling theme: Loan-level time-varying variables

# --- Single-factor models: various time-varying variables
# - Balance_Real
modMLR <- multinom(Target_FromP ~  Balance_Real, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,160,013; McFadden R^2:  0.67%; AUC:  50.86%

# - Instalment_Real
modMLR <- multinom(Target_FromP ~  Instalment_Real, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,867; McFadden R^2:  0.09%; AUC:  52.52%

# - InterestRate_Nom
modMLR <- multinom(Target_FromP ~  InterestRate_Nom, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,162,897; McFadden R^2:  0.43%; AUC:  53.98%

# - AgeToTerm
modMLR <- multinom(Target_FromP ~  AgeToTerm, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,167,733; McFadden R^2:  0.01% ; AUC:  51.67%

# - BalanceToPrincipal
modMLR <- multinom(Target_FromP ~  BalanceToPrincipal, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,141,588; McFadden R^2:  2.25%; AUC:  59.66%

# - slc_acct_pre_lim_perc_imputed_med
modMLR <- multinom(Target_FromP ~  slc_acct_pre_lim_perc_imputed_med, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,155,845; McFadden R^2:  1.03%; AUC:  33.62%
### FINAL: The highest-ranked variables across all fit statistics:
# BalanceToPrincipal + slc_acct_pre_lim_perc_imputed_med + InterestRate_Nom





# ------ 5c. Combining insights: Loan-level variables

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ pmnt_method_grp + InterestRate_Margin + 
                              BalanceToPrincipal + slc_acct_pre_lim_perc_imputed_med + InterestRate_Nom, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 375,999; McFadden R^2:  6.78%; AUC: 70.76%


# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_perf_sub, scope = list(lower = modMLR_base_perf_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 1.3h
### RESULTS: AIC: 375,999; McFadden R^2:  6.78%; AUC:  70.76%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 14.5m
### RESULTS: All variables are significant
### FINAL: Selected variables: BalanceToPrincipal + pmnt_method_grp + slc_acct_pre_lim_perc_imputed_med + 
# InterestRate_Margin + InterestRate_Nom





# ------ 5d. Modelling theme: Loan-level State spell variables

# --- Single-factor models: various state spell variables
# - Prev_Spell_Age
modMLR <- multinom(Target_FromP ~  Prev_Spell_Age, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,163,430; McFadden R^2:  0.38%; AUC:  39.25%

# - StateSpell_Num
modMLR <- multinom(Target_FromP ~  StateSpell_Num, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,157,167; McFadden R^2:  0.92%; AUC:  39.45%

# - StateSpell_Num_Total
modMLR <- multinom(Target_FromP ~  StateSpell_Num_Total, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,157,026; McFadden R^2:  0.93%; AUC:  39.17%

# - TimeInStateSpell
modMLR <- multinom(Target_FromP ~  TimeInStateSpell, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,163,392; McFadden R^2:  0.38% ; AUC:  60.13%
### FINAL: The highest-ranked variables across all fit statistics:
# TimeInStateSpell + StateSpell_Num_Total





# ------ 5e. Combining insights: A complete set of loan-level non-delinquency variables

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ BalanceToPrincipal + pmnt_method_grp + slc_acct_pre_lim_perc_imputed_med + 
                              InterestRate_Margin + InterestRate_Nom + TimeInStateSpell + StateSpell_Num_Total, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 381,919; McFadden R^2:  5.31%; AUC:  72.03%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_perf_sub, scope = list(lower = modMLR_base_perf_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 2.5h
### RESULTS: AIC: 374,262; McFadden R^2:  7.21%; AUC:  71.39%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 25.6m
### RESULTS: All variables are significant
### FINAL: Selected variables: BalanceToPrincipal + pmnt_method_grp + slc_acct_pre_lim_perc_imputed_med + 
# InterestRate_Margin + InterestRate_Nom + TimeInStateSpell + StateSpell_Num_Total





# ------ 5f. Combining insights: A complete set of loan-level variables

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ BalanceToPrincipal + pmnt_method_grp + slc_acct_pre_lim_perc_imputed_med + 
                              InterestRate_Margin + TimeInStateSpell + StateSpell_Num_Total + 
                              g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + 
                              slc_acct_roll_ever_24_imputed_mean + g0_Delinq_Num + g0_Delinq_SD_6, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 312,900; McFadden R^2:  22.44%; AUC:  96.17%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_perf_sub, scope = list(lower = modMLR_base_perf_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 9.3h
### RESULTS: AIC: 305,071; McFadden R^2:  24.37%; AUC:  96.20%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 63m
### RESULTS: All variables are significant
### FINAL: Selected variables: BalanceToPrincipal + pmnt_method_grp + slc_acct_pre_lim_perc_imputed_med + 
# InterestRate_Margin + StateSpell_Num_Total + g0_Delinq_fac + TimeInDelinqState + slc_acct_roll_ever_24_imputed_mean + 
# g0_Delinq_Num + g0_Delinq_SD_6





# ------ 6a. Modelling theme: Repo rate

# --- Single-factor base-model, subsampled set | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromP ~ M_Repo_Rate, 
                              data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 402,407; McFadden R^2:  0.22%; AUC:  52.97%

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ M_Repo_Rate + M_Repo_Rate_1 + M_Repo_Rate_2 + M_Repo_Rate_3 +
                              M_Repo_Rate_6 + M_Repo_Rate_9 + M_Repo_Rate_12, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 401,798; McFadden R^2:  0.38%; AUC:  553.42%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 26m
### RESULTS: AIC: 401,839; McFadden R^2:  0.36%; AUC:  53.37%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 3.8m
### RESULTS: All variables are significant
### FINAL: Selected variables: M_Repo_Rate + M_Repo_Rate_12  + M_Repo_Rate_2   



# --- Single-factor models: various lag orders
# - M_Repo_Rate_2
modMLR <- multinom(Target_FromP ~  M_Repo_Rate_2, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,287; McFadden R^2:  0.22% ; AUC:  52.10%

# - M_Repo_Rate_12
modMLR <- multinom(Target_FromP ~  M_Repo_Rate_12, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,747; McFadden R^2:  0.18%; AUC:  52.10%
# 'Strongest' variable: M_Repo_Rate




# ------ 6b. Modelling theme: Inflation growth

# --- Single-factor base-model, subsampled set | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromP ~ M_Inflation_Growth, 
                              data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 402,611; McFadden R^2:  0.17%; AUC:  52.89%

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ M_Inflation_Growth + M_Inflation_Growth_1 + M_Inflation_Growth_2 + M_Inflation_Growth_3 +
                              M_Inflation_Growth_6 + M_Inflation_Growth_9 + M_Inflation_Growth_12, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 402,118; McFadden R^2:  0.30%; AUC:  53.08%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 34m
### RESULTS: AIC: 402,10; McFadden R^2:  0.30%; AUC:  53.06%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 3.8m
### RESULTS: All variables are significant
### FINAL: Selected variables:  M_Inflation_Growth + M_Inflation_Growth_2 + M_Inflation_Growth_12



# --- Single-factor models: various lag orders
# - M_Inflation_Growth_2
modMLR <- multinom(Target_FromP ~  M_Inflation_Growth_2, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,753; McFadden R^2:  0.18%; AUC:  52.42%

# - M_Inflation_Growth_12
modMLR <- multinom(Target_FromP ~  M_Inflation_Growth_12, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,446; McFadden R^2:  0.12; AUC:  50.69%
# 'Strongest' variable: M_Inflation_Growth_2 + M_Inflation_Growth_12




# ------ 6c. Modelling theme: RealGDP growth

# --- Single-factor base-model, subsampled set | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromP ~ M_RealGDP_Growth, 
                              data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 403,033; McFadden R^2:  0.06%; AUC:  53.05%

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ M_RealGDP_Growth + M_RealGDP_Growth_1 + M_RealGDP_Growth_2 + M_RealGDP_Growth_3 +
                              M_RealGDP_Growth_6 + M_RealGDP_Growth_9 + M_RealGDP_Growth_12, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 402,306; McFadden R^2:  0.25%; AUC:  53.64%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 31m
### RESULTS: AIC: 402,352; McFadden R^2:  0.24%; AUC:  53.56%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 3.8m
### RESULTS: All variables are significant
### FINAL: Selected variables:  M_RealGDP_Growth + M_RealGDP_Growth_9 + M_RealGDP_Growth_12



# --- Single-factor models: various lag orders
# - M_RealGDP_Growth_9
modMLR <- multinom(Target_FromP ~  M_RealGDP_Growth_9, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,411; McFadden R^2:  0.13%; AUC:  53.15%

# - M_RealGDP_Growth_12
modMLR <- multinom(Target_FromP ~  M_RealGDP_Growth_12, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,936; McFadden R^2:  0.17%; AUC:  53.43%
# 'Strongest' variable: M_RealGDP_Growth_12




# ------ 6d. Modelling theme: RealIncome growth

# --- Single-factor base-model, subsampled set | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromP ~ M_RealIncome_Growth, 
                              data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 402,919; McFadden R^2:  0.09%; AUC:  52.62%

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ M_RealIncome_Growth + M_RealIncome_Growth_1 + M_RealIncome_Growth_2 + M_RealIncome_Growth_3 + 
                              M_RealIncome_Growth_6 + M_RealIncome_Growth_9 + M_RealIncome_Growth_12, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 402,124; McFadden R^2:  0.30% ; AUC:  53.47%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 47m
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 7.7m
### RESULTS: All variables are significant
### FINAL: Selected variables:  M_RealIncome_Growth + M_RealIncome_Growth_1 + M_RealIncome_Growth_9 + M_RealIncome_Growth_12



# --- Single-factor models: various lag orders
# - M_RealIncome_Growth_1
modMLR <- multinom(Target_FromP ~  M_RealIncome_Growth_1,
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,167,061; McFadden R^2:  0.07%; AUC:  52.56%

# - M_RealIncome_Growth_9
modMLR <- multinom(Target_FromP ~  M_RealIncome_Growth_9, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,333; McFadden R^2:  0.13%; AUC:  53.08%

# - M_RealIncome_Growth_12
modMLR <- multinom(Target_FromP ~  M_RealIncome_Growth_12, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,801; McFadden R^2:  0.18%; AUC:  53.30%
# 'Strongest' variable: M_RealIncome_Growth_12




# ------ 6e. Modelling theme: DTI growth

# --- Single-factor base-model, subsampled set | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromP ~ M_DTI_Growth, 
                              data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 402,342; McFadden R^2:  0.24%0.24%; AUC:  53.24%

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ M_DTI_Growth + M_DTI_Growth_1 + M_DTI_Growth_2 + M_DTI_Growth_3 + 
                              M_DTI_Growth_6 + M_DTI_Growth_9 + M_DTI_Growth_12, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 401,831; McFadden R^2:  0.37%; AUC:  53.58%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 40m
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 4.5m
### RESULTS: All variables are significant
### FINAL: Selected variables:  M_DTI_Growth + M_DTI_Growth_1 + M_DTI_Growth_9 + M_DTI_Growth_12



# --- Single-factor models: various lag orders
# - M_DTI_Growth_1
modMLR <- multinom(Target_FromP ~  M_DTI_Growth_1, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,450; McFadden R^2:  0.21%; AUC:  53.06%

# - M_DTI_Growth_9
modMLR <- multinom(Target_FromP ~  M_DTI_Growth_9, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,165,733; McFadden R^2:  0.18%; AUC:  52.10%

# - M_DTI_Growth_12
modMLR <- multinom(Target_FromP ~  M_DTI_Growth_12, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,141; McFadden R^2:  0.15%; AUC:  51.08%
# 'Strongest' variable: M_DTI_Growth + M_DTI_Growth_1





# ------ 6f. Modelling theme: Employment growth

# --- Single-factor base-model, subsampled set | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromP ~ M_Emp_Growth, 
                              data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 403,078; McFadden R^2:  0.05%; AUC:  52.82%

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ M_Emp_Growth + M_Emp_Growth_1 + M_Emp_Growth_2 + M_Emp_Growth_3 + 
                              M_Emp_Growth_6 + M_Emp_Growth_9 + M_Emp_Growth_12, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 402,493; McFadden R^2:  0.21%; AUC:  53.62%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 32.9m
### RESULTS: AIC: 402,510; McFadden R^2:  0.20%; AUC:  53.57%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 3.2m
### RESULTS: All variables are significant
### FINAL: Selected variables:  M_Emp_Growth + M_Emp_Growth_1 + M_Emp_Growth_12



# --- Single-factor models: various lag orders
# - M_Emp_Growth_1
modMLR <- multinom(Target_FromP ~  M_Emp_Growth_1, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,167,443; McFadden R^2:  0.04%; AUC:  52.75%

# - M_Emp_Growth_12
modMLR <- multinom(Target_FromP ~  M_Emp_Growth_12, 
                   data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 1,166,547; McFadden R^2:  0.11%; AUC:  53.34%
# 'Strongest' variable: M_Emp_Growth_12 + M_Emp_Growth





# ------ 6g. Combining insights: A complete set of macroeconomic variables

# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ M_Repo_Rate + M_Repo_Rate_12  + M_Repo_Rate_2 + 
                              M_Inflation_Growth + M_Inflation_Growth_2 + M_Inflation_Growth_12 + 
                              M_RealGDP_Growth + M_RealGDP_Growth_12 + 
                              M_RealIncome_Growth + M_RealIncome_Growth_12 + 
                              M_DTI_Growth + M_DTI_Growth_1 + 
                              M_Emp_Growth_12 + M_Emp_Growth, 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 401,170; McFadden R^2:  0.55%; AUC:  54.04%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_base_perf_sub, scope = list(lower = modMLR_base_perf_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 2.8h
### RESULTS: AIC: 401,285; McFadden R^2:  0.51%; AUC:  53.61%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 14m
### RESULTS: All variables are significant
### FINAL: Selected variables: M_Inflation_Growth_2 + M_Inflation_Growth_12 + M_Repo_Rate + 
# M_Emp_Growth + M_DTI_Growth + M_RealIncome_Growth
# Removed using expert judgement: M_Repo_Rate_2 + M_Repo_Rate_12





# ------ 7. Combining insights: A complete set of all input variables

# --- Base-level null model, subsampled set | Stepwise forward selection procedure
# Specifying the best-in-class variables per theme as the minimum model
modMLR_single_sub <- multinom(Target_FromP ~ g0_Delinq_fac + g0_Delinq_Ave, 
                              data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 324,246; McFadden R^2:  19.60%; AUC:  64.91%



# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromP ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + g0_Delinq_Any_Aggr_Prop +
                              CreditLeverage_Aggr + InterestRate_Margin_Aggr_Med + 
                              BalanceToPrincipal + pmnt_method_grp + slc_acct_pre_lim_perc_imputed_med + 
                              InterestRate_Margin + StateSpell_Num_Total + g0_Delinq_fac + TimeInDelinqState + slc_acct_roll_ever_24_imputed_mean + 
                              g0_Delinq_Num + g0_Delinq_SD_6 + 
                              M_Inflation_Growth_2 + M_Inflation_Growth_12 + M_Repo_Rate + 
                              M_Emp_Growth + M_DTI_Growth + M_RealIncome_Growth
                              , 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 304,389; McFadden R^2:  24.56%; AUC:  76.69%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_sub_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full_sub), 
                                    direction = "both", k=log(datCredit_train_sub[MarkovStatus=="Perf",.N]), maxit=50)
evalMLR(modMLR_full_sub_stepwise, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 20.6h
### RESULTS: AIC: 304,511; McFadden R^2:  24.52%; AUC:  76.64%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_sub_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 1.6h
### RESULTS: All variables are significant
### FINAL: Selected variables:  g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + 
# BalanceToPrincipal + slc_acct_pre_lim_perc_imputed_med + InterestRate_Margin +
# g0_Delinq_Num + g0_Delinq_SD_6 + TimeInDelinqState +  g0_Delinq_fac + pmnt_method_grp + 
# StateSpell_Num_Total + slc_acct_roll_ever_24_imputed_mean + M_Emp_Growth



# --- Model 1, subsampled set
modMLR_sub_cand <- multinom(Target_FromP ~ g0_Delinq_Ave +  DefaultStatus1_Aggr_Prop + 
                              BalanceToPrincipal + slc_acct_pre_lim_perc_imputed_med + InterestRate_Margin + 
                              g0_Delinq_Num + g0_Delinq_SD_6 + TimeInDelinqState +  g0_Delinq_fac + pmnt_method_grp +
                              StateSpell_Num_Total + slc_acct_roll_ever_24_imputed_mean + 
                              M_Emp_Growth + M_Inflation_Growth_2 + M_Repo_Rate , 
                            data = datCredit_train_sub[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_sub_cand, modMLR_base_perf_sub, datCredit_train_sub[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 304,509; McFadden R^2:  24.52%; AUC:  76.65%

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_sub_cand, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 2.1h
### RESULTS: All variables are significant



# --- Model 1b, training set
modMLR_perf <- multinom(Target_FromP ~ g0_Delinq_Ave +  DefaultStatus1_Aggr_Prop + 
                              BalanceToPrincipal + slc_acct_pre_lim_perc_imputed_med + InterestRate_Margin + 
                              g0_Delinq_Num + g0_Delinq_SD_6 + TimeInDelinqState +  g0_Delinq_fac + pmnt_method_grp +
                              StateSpell_Num_Total + slc_acct_roll_ever_24_imputed_mean + 
                              M_Emp_Growth + M_Inflation_Growth_2 + M_Repo_Rate , 
                            data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_perf, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 904,657; McFadden R^2:  22.55%; AUC:  76.65%

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_perf, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 7.5h
### RESULTS: Following variables are insignificant: slc_acct_pre_lim_perc_imputed_med + TimeInDelinqState + M_Inflation_Growth_2



# --- Model 2, training set
modMLR_perf <- multinom(Target_FromP ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + 
                              BalanceToPrincipal + InterestRate_Margin + 
                              g0_Delinq_Num + g0_Delinq_SD_6 + g0_Delinq_fac + pmnt_method_grp +
                              StateSpell_Num_Total + slc_acct_roll_ever_24_imputed_mean + 
                              M_Emp_Growth + M_Inflation_Growth_2 + M_Repo_Rate , 
                            data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_perf, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 897,469; McFadden R^2:  23.16%; AUC:  76.65%

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_perf, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 4.8h
### RESULTS: All variables are significant


# --- Model 3a, training set
modMLR_perf <- multinom(Target_FromP ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + CreditLeverage_Aggr + 
                              BalanceToPrincipal + InterestRate_Margin + 
                              g0_Delinq_Num + g0_Delinq_SD_6 + g0_Delinq_fac + pmnt_method_grp +
                              StateSpell_Num_Total + slc_acct_roll_ever_24_imputed_mean + 
                              M_Emp_Growth + M_Inflation_Growth_2 + M_Repo_Rate + 
                              slc_acct_pre_lim_perc_imputed_med, 
                            data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_perf, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 884,41; McFadden R^2:  24.28%; AUC:  76.66%



# --- Model 3b, training set, with natural splines, applied interactively using expert judgement
modMLR_perf <- multinom(Target_FromP ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + ns(CreditLeverage_Aggr,3) +
                          ns(BalanceToPrincipal,3) + ns(InterestRate_Margin,3) + 
                          ns(g0_Delinq_Num,5) + g0_Delinq_SD_6 + g0_Delinq_fac + pmnt_method_grp +
                          StateSpell_Num_Total + ns(slc_acct_roll_ever_24_imputed_mean,5) + 
                          M_Emp_Growth + M_Inflation_Growth_2 + ns(M_Repo_Rate,3) + 
                          ns(slc_acct_pre_lim_perc_imputed_med,4), 
                        data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_perf, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 853,108; McFadden R^2:  26.97% ; AUC:  81.72%

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_perf, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 8.1h
### RESULTS: All variables are significant




# ------ 8. Fit final model 
# - Confirm that required data objects are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)

# - Fit an empty model as a performance gain, used within some diagnostic functions
modMLR_base_perf <- multinom(Target_FromP ~  1, 
                             data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)

# - Fit model
modMLR_perf <- multinom(Target_FromP ~ g0_Delinq_Ave + DefaultStatus1_Aggr_Prop + ns(CreditLeverage_Aggr,3) +
                          ns(BalanceToPrincipal,3) + ns(InterestRate_Margin,3) + 
                          ns(g0_Delinq_Num,5) + g0_Delinq_SD_6 + g0_Delinq_fac + pmnt_method_grp +
                          StateSpell_Num_Total + ns(slc_acct_roll_ever_24_imputed_mean,5) + 
                          M_Emp_Growth + M_Inflation_Growth_2 + ns(M_Repo_Rate,3) + 
                          ns(slc_acct_pre_lim_perc_imputed_med,4), 
                        data = datCredit_train[MarkovStatus=="Perf",],maxit=1000)
evalMLR(modMLR_perf, modMLR_base_perf, datCredit_train[MarkovStatus=="Perf",], targetFld="Target_FromP", predClass="Perf")
### RESULTS: AIC: 853,108; McFadden R^2:  26.97% ; AUC:  81.72%


# --- Cleanup
rm(datCredit_train, datCredit_train_sub, datCredit_valid, datKeys, datKeys_sampled, 
   modLR_Result, modMLR, modMLR_base_perf, modMLR_base_perf_sub, modMLR_full, modMLR_full_sub,
   modMLR_single_sub, modMLR_sub_cand, modMLR_full_sub_stepwise, modMLR_perf); gc()
