# ========================== MULTINOMIAL LOGISTIC REGRESSION (MLR) MODEL: DEF =========================
# Fitting an MLR-model towards finalizing its input space in modelling the transition rate: 
# Default to Performing, Default (baseline), Settlement, Write-off
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
describe(datCredit_train[MarkovStatus=="Perf",Target_FromD]) # Dominant class: Performing
describe(datCredit_train[MarkovStatus=="Def",Target_FromD]) # Dominant class: Default
describe(datCredit_train_sub[MarkovStatus=="Perf",Target_FromD]) # Dominant class: Performing
describe(datCredit_train_sub[MarkovStatus=="Def",Target_FromD]) # Dominant class: Default

# - Fit an empty model as a performance gain, used within some diagnostic functions
modMLR_base_def <- multinom(Target_FromD ~  1, 
                        data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
modMLR_base_sub <- multinom(Target_FromD ~  1, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)





# ------ 2a. Modelling theme: Portfolio-level delinquency-themed variables

# --- Single-factor models: g0_Delinq_Any_Aggr_Prop and lags
# Where, relevant, we build single-factor models to determine which lag-orders are best
# - g0_Delinq_Any_Aggr_Prop
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,920; McFadden R^2:  0.07% ; AUC:  52.52%

# - g0_Delinq_Any_Aggr_Prop_Lag_1
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_1,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,913; McFadden R^2:  0.07%; AUC:  52.81%

# - g0_Delinq_Any_Aggr_Prop_Lag_2
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_2,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,906; McFadden R^2:  0.08% ; AUC:  52.94%

# - g0_Delinq_Any_Aggr_Prop_Lag_3
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_3, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,914; McFadden R^2:  0.07%; AUC:  52.85%

# - g0_Delinq_Any_Aggr_Prop_Lag_4
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_4,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,916; McFadden R^2:  0.07%; AUC:  52.66%

# - g0_Delinq_Any_Aggr_Prop_Lag_5
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_5,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,922; McFadden R^2:   0.07%; AUC:  52.35%

# - g0_Delinq_Any_Aggr_Prop_Lag_6
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_6,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,930; McFadden R^2:   0.07%; AUC:  51.72%

# - g0_Delinq_Any_Aggr_Prop_Lag_9
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_9,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,875; McFadden R^2:   0.09%; AUC:  51.35%

# - g0_Delinq_Any_Aggr_Prop_Lag_12
modMLR <- multinom(Target_FromD ~  g0_Delinq_Any_Aggr_Prop_Lag_12,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,778 ; McFadden R^2:  0.13%0.13%; AUC:  51.74%
### FINAL: The highest-ranked variables across all fit statistics:
# g0_Delinq_Any_Aggr_Prop_Lag_12 + g0_Delinq_Any_Aggr_Prop_Lag_9 + g0_Delinq_Any_Aggr_Prop_Lag_2
# No definitive trend detected, except at the tail end of lag orders (which appears to be slightly better fitting)



# --- Single-factor models: DefaultStatus1_Aggr_Prop and lags

# - DefaultStatus1_Aggr_Prop
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  254,919; McFadden R^2:  0.46%; AUC:  49.86%

# - DefaultStatus1_Aggr_Prop_Lag_1
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_1,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  254,848; McFadden R^2:  0.49%; AUC:  49.50%

# - DefaultStatus1_Aggr_Prop_Lag_2
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_2,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  254,781; McFadden R^2:  0.52%; AUC:  49.09%

# - DefaultStatus1_Aggr_Prop_Lag_3
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_3,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  254,724 ; McFadden R^2:  0.54%; AUC:  48.73%

# - DefaultStatus1_Aggr_Prop_Lag_4
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_4,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  254,682; McFadden R^2:  0.55%; AUC:  51.60%

# - DefaultStatus1_Aggr_Prop_Lag_5
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_5,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  254,645; McFadden R^2:  0.57%; AUC:  51.83%

# - DefaultStatus1_Aggr_Prop_Lag_6
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_6,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  254,646; McFadden R^2:  0.57%; AUC:  51.96%

# - DefaultStatus1_Aggr_Prop_Lag_9
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_9,
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  254,756; McFadden R^2:  0.53%; AUC:  52.04%

# - DefaultStatus1_Aggr_Prop_Lag_12
modMLR <- multinom(Target_FromD ~  DefaultStatus1_Aggr_Prop_Lag_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  254,910; McFadden R^2:  0.47%; AUC:  52.58%
### FINAL: The highest-ranked variables across all fit statistics:
# DefaultStatus1_Aggr_Prop_Lag_5 + DefaultStatus1_Aggr_Prop_Lag_6 + DefaultStatus1_Aggr_Prop_Lag_9
# There appears to be a non-linear increasing/decreasing trend across all fit statistics as the lag-order increases; middle is better
# Relatively weaker variable than g0_Delinq_Any_Aggr_Prop.



# --- Single-factor models: Various delinquency-themed portfolio-level variables

# - g0_Delinq_Ave
modMLR <- multinom(Target_FromD ~  g0_Delinq_Ave, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,908; McFadden R^2:  0.08%; AUC:  52.64%

# - CuringEvents_Aggr_Prop
modMLR <- multinom(Target_FromD ~  CuringEvents_Aggr_Prop, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,115; McFadden R^2:  0.38%; AUC:  53.41%

# - ArrearsToBalance_Aggr_Prop
modMLR <- multinom(Target_FromD ~  ArrearsToBalance_Aggr_Prop, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,954; McFadden R^2:  0.06%; AUC:  52.79%
### FINAL: The highest-ranked variables across all fit statistics:
# CuringEvents_Aggr_Prop + g0_Delinq_Ave + ArrearsToBalance_Aggr_Prop





# ------ 2b. Combining insights: Delinquency-themed portfolio-level variables

# --- Full-model
modMLR_full <- multinom(Target_FromD ~ ArrearsToBalance_Aggr_Prop + CuringEvents_Aggr_Prop + g0_Delinq_Ave + 
                          g0_Delinq_Any_Aggr_Prop_Lag_12 + g0_Delinq_Any_Aggr_Prop_Lag_9 + g0_Delinq_Any_Aggr_Prop_Lag_2 + 
                          DefaultStatus1_Aggr_Prop_Lag_5 + DefaultStatus1_Aggr_Prop_Lag_6 + DefaultStatus1_Aggr_Prop_Lag_9, 
                        data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  253,980; McFadden R^2:  0.85%; AUC: 55.17% 



# --- Full-model, subsampled set | Stepwise forward selection procedure
modMLR_full_sub <- multinom(Target_FromD ~ ArrearsToBalance_Aggr_Prop + CuringEvents_Aggr_Prop + g0_Delinq_Ave + 
                              g0_Delinq_Any_Aggr_Prop_Lag_12 + g0_Delinq_Any_Aggr_Prop_Lag_9 + g0_Delinq_Any_Aggr_Prop_Lag_2 + 
                              DefaultStatus1_Aggr_Prop_Lag_5 + DefaultStatus1_Aggr_Prop_Lag_6 + DefaultStatus1_Aggr_Prop_Lag_9, 
                            data = datCredit_train_sub[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_sub, modMLR_base_sub, datCredit_train_sub[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  87,590; McFadden R^2:  0.87%; AUC:  55.35%
# Agrees very closely (aside from the AIC) with full-sample model
# Given much smaller sample sizes than the MarkovStatus=="Perf" set, we shall stick to the full set instead of the subsampled set

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_base_def, scope = list(lower = modMLR_base_def, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 8.5m
### RESULTS: AIC: 254,044 ; McFadden R^2:  0.81%; AUC:  55.16%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; <1m
### RESULTS: All variables are significant
### FINAL: Selected variables: DefaultStatus1_Aggr_Prop_Lag_5 + DefaultStatus1_Aggr_Prop_Lag_9 + g0_Delinq_Any_Aggr_Prop_Lag_12 + 
# CuringEvents_Aggr_Prop





# ------ 3a. Modelling theme: Other portfolio-level variables

# --- Single-factor models: InterestRate_Margin_Aggr_Med and lags

# - InterestRate_Margin_Aggr_Med
modMLR <- multinom(Target_FromD ~  InterestRate_Margin_Aggr_Med, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,612; McFadden R^2:  0.19%; AUC:  51.09%

# - InterestRate_Margin_Aggr_Med_1
modMLR <- multinom(Target_FromD ~  InterestRate_Margin_Aggr_Med_1, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,608; McFadden R^2:  0.19% ; AUC:  51.04%

# - InterestRate_Margin_Aggr_Med_2
modMLR <- multinom(Target_FromD ~  InterestRate_Margin_Aggr_Med_2, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,603; McFadden R^2:  0.19%; AUC:  50.99%

# - InterestRate_Margin_Aggr_Med_3
modMLR <- multinom(Target_FromD ~  InterestRate_Margin_Aggr_Med_3, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,595; McFadden R^2:  0.20% ; AUC:  50.93%

# - InterestRate_Margin_Aggr_Med_9
modMLR <- multinom(Target_FromD ~  InterestRate_Margin_Aggr_Med_9, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,582; McFadden R^2:  0.20%; AUC:  50.91%
### FINAL: The highest-ranked variables across all fit statistics:
# InterestRate_Margin_Aggr_Med_9 + InterestRate_Margin_Aggr_Med_3 + InterestRate_Margin_Aggr_Med
# Except for AUC, there appears to be a monotonic increasing trend across all fit statistics as the lag-order increases; later is better



# --- Full-model | Stepwise forward selection procedure
modMLR_full <- multinom(Target_FromD ~ InterestRate_Margin_Aggr_Med + InterestRate_Margin_Aggr_Med_1 + InterestRate_Margin_Aggr_Med_2 +
                              InterestRate_Margin_Aggr_Med_3 + InterestRate_Margin_Aggr_Med_9, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:   255,555; McFadden R^2:  0.22%; AUC:  51.63%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_base_def, scope = list(lower = modMLR_base_def, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; <1m
### RESULTS: Selected variables: InterestRate_Margin_Aggr_Med_9;
### RESULTS: AIC:  255,582; McFadden R^2:  0.20% ; AUC:  50.91%



# --- Single-factor models: Various portfolio-level variables

# - InstalmentToBalance_Aggr_Prop
modMLR <- multinom(Target_FromD ~  InstalmentToBalance_Aggr_Prop, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,831; McFadden R^2:  0.11%; AUC:  51.54%

# - AgeToTerm_Aggr_Mean
modMLR <- multinom(Target_FromD ~  AgeToTerm_Aggr_Mean, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,870; McFadden R^2:  0.09%; AUC:  52.09%

# - PerfSpell_Maturity_Aggr_Mean
modMLR <- multinom(Target_FromD ~  PerfSpell_Maturity_Aggr_Mean, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,744; McFadden R^2:  0.14%; AUC:  50.43%

# - CreditLeverage_Aggr
modMLR <- multinom(Target_FromD ~  CreditLeverage_Aggr, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,768; McFadden R^2:  0.13%; AUC:  52.29%

# - Ave_Margin_Aggr
modMLR <- multinom(Target_FromD ~  Ave_Margin_Aggr, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,698; McFadden R^2:  0.16%; AUC:  51.72%

# - NewLoans_Aggr_Prop
modMLR <- multinom(Target_FromD ~  NewLoans_Aggr_Prop, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,397; McFadden R^2:  0.27%; AUC:  50.03%
### FINAL: The highest-ranked variables across all fit statistics:
# Ave_Margin_Aggr + CreditLeverage_Aggr + NewLoans_Aggr_Prop





# ------ 3b. Combining insights: Other portfolio-level variables
# ---- Combining insights: Other portfolio-level variables

# --- Full-model | Stepwise forward selection procedure
modMLR_full <- multinom(Target_FromD ~ InterestRate_Margin_Aggr_Med_9 + InterestRate_Margin_Aggr_Med_3 + InterestRate_Margin_Aggr_Med +
                              Ave_Margin_Aggr + CreditLeverage_Aggr + NewLoans_Aggr_Prop, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC: 254,701; McFadden R^2:  0.56%; AUC:  53.03%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_base_def, scope = list(lower = modMLR_base_def, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 11.4m
### RESULTS: AIC:  87,898; McFadden R^2:  0.48%; AUC:  52.41%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime: 1.2m
### RESULTS: All variables are significant
### FINAL: Selected variables: InterestRate_Margin_Aggr_Med + InterestRate_Margin_Aggr_Med_9 + NewLoans_Aggr_Prop + CreditLeverage_Aggr + Ave_Margin_Aggr





# ------ 3c. Combining insights: Complete set of portfolio-level variables

# --- Full-model | Stepwise forward selection procedure
modMLR_full <- multinom(Target_FromD ~ DefaultStatus1_Aggr_Prop_Lag_5 + DefaultStatus1_Aggr_Prop_Lag_9 + g0_Delinq_Any_Aggr_Prop_Lag_2 +
                          CuringEvents_Aggr_Prop + g0_Delinq_Ave + ArrearsToBalance_Aggr_Prop + 
                          InterestRate_Margin_Aggr_Med + InterestRate_Margin_Aggr_Med_9 + NewLoans_Aggr_Prop + CreditLeverage_Aggr + Ave_Margin_Aggr, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  253,907; McFadden R^2:  0.88%; AUC:  55.38%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_base_def, scope = list(lower = modMLR_base_def, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 12.4m
### RESULTS: AIC:  253,927; McFadden R^2:  0.86%; AUC:  55.34%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime: 1.2m
### RESULTS: All variables are significant
# Selected variables: g0_Delinq_Ave; DefaultStatus1_Aggr_Prop_Lag_5 + DefaultStatus1_Aggr_Prop_Lag_9 + 
# CreditLeverage_Aggr + g0_Delinq_Ave + CuringEvents_Aggr_Prop
# Additional variables manually selected using expert judgement: 
#   InterestRate_Margin_Aggr_Med



# --- Model 1, subsampled set
modMLR_full_cand <- multinom(Target_FromD ~ DefaultStatus1_Aggr_Prop_Lag_5 + DefaultStatus1_Aggr_Prop_Lag_9 + 
                               CreditLeverage_Aggr + g0_Delinq_Ave + CuringEvents_Aggr_Prop + InterestRate_Margin_Aggr_Med, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_cand, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  253,971; McFadden R^2:  0.84%; AUC:  55.03%

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_cand, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime: 11.4m
### RESULTS: Insignificant variables include: InterestRate_Margin_Aggr_Med





# ------ 4a. Modelling theme: Loan-level delinquency-themed variables

# --- Single-factor models: Various information theoretic forms of delinquency
# - TimeInPerfSpell
modMLR <- multinom(Target_FromD ~  TimeInDefSpell, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,481; McFadden R^2:  0.24%; AUC: 47.00% 

# - g0_Delinq_fac
modMLR <- multinom(Target_FromD ~  g0_Delinq_fac, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  210,853; McFadden R^2:  17.67%; AUC:  91.87%

# - g0_Delinq
modMLR <- multinom(Target_FromD ~  g0_Delinq, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  219,155; McFadden R^2:  14.43%; AUC:  91.87%

# - g0_Delinq_Num
modMLR <- multinom(Target_FromD ~  g0_Delinq_Num, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  254,068; McFadden R^2:  0.79%; AUC:  65.87%

# - TimeInDelinqState
modMLR <- multinom(Target_FromD ~  TimeInDelinqState, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  245,227; McFadden R^2:  4.25%; AUC:  67.10%

# - slc_acct_arr_dir_3
modMLR <- multinom(Target_FromD ~  slc_acct_arr_dir_3, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  252,152; McFadden R^2:  1.55%; AUC:  57.98%

# - slc_acct_roll_ever_24_imputed_mean
modMLR <- multinom(Target_FromD ~  slc_acct_roll_ever_24_imputed_mean, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,671; McFadden R^2:  0.17%; AUC:  50.59%
### FINAL: The highest-ranked variables across all fit statistics: 
# g0_Delinq_fac (over g0_Delinq) + TimeInDelinqState + slc_acct_arr_dir_3 + g0_Delinq_Num + slc_acct_roll_ever_24_imputed_mean
# It is remarkable the good fit of g0_Delinq_fac compared to other delinquency-themed variables, even though
# these loan-level variables are overall better fitting than the portfolio-level ones so far.



# --- Single-factor models: speed at which delinquency changes across various window lengths
# - g0_Delinq_SD_4
modMLR <- multinom(Target_FromD ~  g0_Delinq_SD_4, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  253,404; McFadden R^2:  1.05%; AUC:  48.80%

# - g0_Delinq_SD_5
modMLR <- multinom(Target_FromD ~  g0_Delinq_SD_5, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  253,135; McFadden R^2:  1.16%; AUC:  50.32%

# - g0_Delinq_SD_6
modMLR <- multinom(Target_FromD ~  g0_Delinq_SD_6, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  252,846; McFadden R^2:  1.27%; AUC:  49.50%

# - g0_Delinq_SD_9
modMLR <- multinom(Target_FromD ~  g0_Delinq_SD_9, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  235,958; McFadden R^2:  7.87%; AUC:  80.44%

# - g0_Delinq_SD_12
modMLR <- multinom(Target_FromD ~  g0_Delinq_SD_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  240,808; McFadden R^2:  5.97%; AUC:  75.67%
### FINAL: The highest-ranked variables across all fit statistics:
# g0_Delinq_SD_9 + g0_Delinq_SD_12
# There appears to be a monotonic increasing trend across all fit statistics as the window length increases; later is better





# ------ 4b. Combining insights: Loan-level delinquency-themed variables

# --- Full-model | Stepwise forward selection procedure
modMLR_full <- multinom(Target_FromD ~ g0_Delinq_fac + TimeInDelinqState + slc_acct_arr_dir_3 + g0_Delinq_Num + 
                          slc_acct_roll_ever_24_imputed_mean + g0_Delinq_SD_9 + g0_Delinq_SD_12, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  189,787; McFadden R^2:  25.92%; AUC:  96.01%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_base_def, scope = list(lower = modMLR_base_def, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 31.5m
### RESULTS: AIC:  189,787; McFadden R^2:  25.92%; AUC:  96.01%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 4.9m
### RESULTS: All variables are significant
# Selected variables: g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + g0_Delinq_Num + g0_Delinq_SD_9 + g0_Delinq_SD_12 + 
# slc_acct_roll_ever_24_imputed_mean



# --- Model 1 | Stepwise forward selection procedure
modMLR_full_cand <- multinom(Target_FromD ~ g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + g0_Delinq_Num + 
                               g0_Delinq_SD_9 + slc_acct_roll_ever_24_imputed_mean, 
                        data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full_cand, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  190,129; McFadden R^2:  25.78%; AUC:  95.99%

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_cand, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 4.3m
### RESULTS: All variables are significant and finally chosen.





# ------ 5a. Modelling theme: Loan-level time-fixed variables

# --- Single-factor models: various time-fixed variables
# - Principal_Real
modMLR <- multinom(Target_FromD ~  Principal_Real, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,108; McFadden R^2:  0.39%; AUC:  52.88%  

# - InterestRate_Margin
modMLR <- multinom(Target_FromD ~  InterestRate_Margin, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  254,249; McFadden R^2:  0.72%; AUC:  50.90%

# - LN_TPE
modMLR <- multinom(Target_FromD ~  LN_TPE, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,941; McFadden R^2:  0.06%; AUC:  50.78%

# - pmnt_method_grp
modMLR <- multinom(Target_FromD ~  pmnt_method_grp, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  250,920; McFadden R^2:  2.03%; AUC:  62.14%
### FINAL: The highest-ranked variables across all fit statistics:
# pmnt_method_grp + InterestRate_Margin + Principal_Real





# ------ 5b. Modelling theme: Loan-level time-varying variables

# --- Single-factor models: various time-varying variables
# - Balance_Real
modMLR <- multinom(Target_FromD ~  Balance_Real, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  254,018; McFadden R^2:  0.81%; AUC:  53.52%

# - Instalment_Real
modMLR <- multinom(Target_FromD ~  Instalment_Real, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,672; McFadden R^2:  0.17%; AUC:  50.94%

# - InterestRate_Nom
modMLR <- multinom(Target_FromD ~  InterestRate_Nom, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,352; McFadden R^2:  0.29%; AUC:  53.73%

# - AgeToTerm
modMLR <- multinom(Target_FromD ~  AgeToTerm, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,662; McFadden R^2:  0.17%; AUC:  53.64%

# - BalanceToPrincipal
modMLR <- multinom(Target_FromD ~  BalanceToPrincipal, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  252,438; McFadden R^2:  1.43%; AUC: 58.76% 

# - slc_acct_pre_lim_perc_imputed_med
modMLR <- multinom(Target_FromD ~  slc_acct_pre_lim_perc_imputed_med, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  256,012; McFadden R^2:  0.03%; AUC:  50.41%
### FINAL: The highest-ranked variables across all fit statistics:
# BalanceToPrincipal + AgeToTerm + InterestRate_Nom





# ------ 5c. Combining insights: Loan-level variables

# --- Full-model | Stepwise forward selection procedure
modMLR_full <- multinom(Target_FromD ~ pmnt_method_grp + InterestRate_Margin + Principal_Real + 
                                       BalanceToPrincipal + AgeToTerm + InterestRate_Nom, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  245,667; McFadden R^2:  4.09%; AUC:  63.70%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_base_def, scope = list(lower = modMLR_base_def, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 19.4m
### RESULTS: AIC:  245,667; McFadden R^2:  245,667; AUC:  63.70%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 2.7m
### RESULTS: All variables are significant
### FINAL: Selected variables: pmnt_method_grp + InterestRate_Margin + Principal_Real + 
# BalanceToPrincipal + AgeToTerm + InterestRate_Nom





# ------ 5d. Modelling theme: Loan-level State spell variables

# --- Single-factor models: various state spell variables
# - Prev_Spell_Age
modMLR <- multinom(Target_FromD ~  Prev_Spell_Age, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,523; McFadden R^2:  0.23%; AUC:  52.40%

# - StateSpell_Num
modMLR <- multinom(Target_FromD ~  StateSpell_Num, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,660; McFadden R^2:  0.17%; AUC:  53.60%

# - StateSpell_Num_Total
modMLR <- multinom(Target_FromD ~  StateSpell_Num_Total, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,615; McFadden R^2:  0.19%; AUC:  54.11%

# - TimeInStateSpell
modMLR <- multinom(Target_FromD ~  TimeInStateSpell, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  254,729; McFadden R^2:  0.54%; AUC:  46.14%
### FINAL: The highest-ranked variables across all fit statistics:
# StateSpell_Num_Total + TimeInStateSpell + Prev_Spell_Age





# ------ 5e. Combining insights: A complete set of loan-level non-delinquency variables

# --- Full-model | Stepwise forward selection procedure
modMLR_full <- multinom(Target_FromD ~ pmnt_method_grp + InterestRate_Margin + Principal_Real + 
                            BalanceToPrincipal + AgeToTerm + InterestRate_Nom + 
                            StateSpell_Num_Total + TimeInStateSpell + Prev_Spell_Age, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  244,206; McFadden R^2:  4.67%; AUC:  64.22%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_base_def, scope = list(lower = modMLR_base_def, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 49m
### RESULTS: AIC:  244,206; McFadden R^2:  4.67%; AUC:  64.22%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 
### RESULTS: All variables are significant
### FINAL: Selected variables: pmnt_method_grp + InterestRate_Margin + Principal_Real +  
# BalanceToPrincipal + AgeToTerm + InterestRate_Nom + StateSpell_Num_Total + TimeInStateSpell + Prev_Spell_Age





# ------ 5f. Combining insights: A complete set of loan-level variables

# --- Full-model | Stepwise forward selection procedure
modMLR_full <- multinom(Target_FromD ~ g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + g0_Delinq_Num + 
                          g0_Delinq_SD_9 + slc_acct_roll_ever_24_imputed_mean + pmnt_method_grp + InterestRate_Margin + Principal_Real +  
                          BalanceToPrincipal + AgeToTerm + InterestRate_Nom + StateSpell_Num_Total + TimeInStateSpell + Prev_Spell_Age
                          , data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  184,479; McFadden R^2:  28.01%; AUC: 96.24%  

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_base_def, scope = list(lower = modMLR_base_def, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 3.2h
### RESULTS: AIC:  184,677; McFadden R^2:  27.93%; AUC:  96.21%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 14.6m
### RESULTS: All variables are significant
### FINAL: Selected variables: g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + g0_Delinq_Num + 
# g0_Delinq_SD_9 + slc_acct_roll_ever_24_imputed_mean + pmnt_method_grp + InterestRate_Margin + Principal_Real + 
# BalanceToPrincipal + AgeToTerm + TimeInStateSpell





# ------ 6a. Modelling theme: Repo rate

# --- Single-factor base-model | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromD ~ M_Repo_Rate, 
                              data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,902; McFadden R^2:  0.08%; AUC:  52.02%

# --- Full-model| Stepwise forward selection procedure
modMLR_full <- multinom(Target_FromD ~ M_Repo_Rate + M_Repo_Rate_1 + M_Repo_Rate_2 + M_Repo_Rate_3 +
                              M_Repo_Rate_6 + M_Repo_Rate_9 + M_Repo_Rate_12, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,129; McFadden R^2:  0.39% ; AUC:  52.99%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 2.7m
### RESULTS: AIC:  255,158; McFadden R^2:  0.37%; AUC:  52.62%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; <1m
### RESULTS: All variables are significant
### FINAL: Selected variables: M_Repo_Rate + M_Repo_Rate_12  + M_Repo_Rate_6   



# --- Single-factor models: various lag orders
# - M_Repo_Rate_6
modMLR <- multinom(Target_FromD ~  M_Repo_Rate_6, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,934; McFadden R^2:  0.07%; AUC:  52.44%

# - M_Repo_Rate_12
modMLR <- multinom(Target_FromD ~  M_Repo_Rate_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,809; McFadden R^2:  0.11%; AUC:  52.16%
# 'Strongest' variable: M_Repo_Rate_12 + M_Repo_Rate





# ------ 6b. Modelling theme: Inflation growth

# --- Single-factor base-model | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromD ~ M_Inflation_Growth, 
                              data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,977; McFadden R^2:  0.03%; AUC:  51.80%

# --- Full-model | Stepwise forward selection procedure
modMLR_full <- multinom(Target_FromD ~ M_Inflation_Growth + M_Inflation_Growth_1 + M_Inflation_Growth_2 + M_Inflation_Growth_3 +
                              M_Inflation_Growth_6 + M_Inflation_Growth_9 + M_Inflation_Growth_12, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,423; McFadden R^2:  0.28% ; AUC:  52.76%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 2.9m
### RESULTS: AIC:  255,420; McFadden R^2:  0.27%; AUC:  52.69%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; <1m
### RESULTS: All variables are significant
### FINAL: Selected variables:  M_Inflation_Growth + M_Inflation_Growth_6 + M_Inflation_Growth_12



# --- Single-factor models: various lag orders
# - M_Inflation_Growth_6
modMLR <- multinom(Target_FromD ~  M_Inflation_Growth_6, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,989; McFadden R^2:  0.04% ; AUC:  51.27%

# - M_Inflation_Growth_12
modMLR <- multinom(Target_FromD ~  M_Inflation_Growth_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,743; McFadden R^2:  0.14% ; AUC:  50.39%
# 'Strongest' variable: M_Inflation_Growth_6 + M_Inflation_Growth_12





# ------ 6c. Modelling theme: RealGDP growth

# --- Single-factor base-model | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromD ~ M_RealGDP_Growth, 
                              data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,911; McFadden R^2:  0.07%; AUC: 51.44% 

# --- Full-model | Stepwise forward selection procedure
modMLR_full <- multinom(Target_FromD ~ M_RealGDP_Growth + M_RealGDP_Growth_1 + M_RealGDP_Growth_2 + M_RealGDP_Growth_3 +
                              M_RealGDP_Growth_6 + M_RealGDP_Growth_9 + M_RealGDP_Growth_12, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,704; McFadden R^2:  0.17%; AUC:  53.34%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 2.5m
### RESULTS: AIC:  255,710 ; McFadden R^2:  0.16%; AUC:  53.10%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; <1m
### RESULTS: All variables are significant
### FINAL: Selected variables:  M_RealGDP_Growth + M_RealGDP_Growth_1 + M_RealGDP_Growth_12



# --- Single-factor models: various lag orders
# - M_RealGDP_Growth_1
modMLR <- multinom(Target_FromD ~  M_RealGDP_Growth_1, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,920; McFadden R^2:  0.07%; AUC:  51.80%

# - M_RealGDP_Growth_12
modMLR <- multinom(Target_FromD ~  M_RealGDP_Growth_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,908; McFadden R^2:  0.08%; AUC:  52.95%
# 'Strongest' variable: M_RealGDP_Growth_12 + M_RealGDP_Growth_1 + M_RealGDP_Growth





# ------ 6d. Modelling theme: RealIncome growth

# --- Single-factor base-model | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromD ~ M_RealIncome_Growth, 
                              data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,760; McFadden R^2:  0.13%; AUC:  51.15%

# --- Full-model | Stepwise forward selection procedure
modMLR_full <- multinom(Target_FromD ~ M_RealIncome_Growth + M_RealIncome_Growth_1 + M_RealIncome_Growth_2 + M_RealIncome_Growth_3 + 
                              M_RealIncome_Growth_6 + M_RealIncome_Growth_9 + M_RealIncome_Growth_12, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,448; McFadden R^2:  0.27%; AUC:  54.01%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 1.6m
### RESULTS: AIC: 255,487; McFadden R^2:  0.24%; AUC:  53.81%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; <1m
### RESULTS: All variables are significant
### FINAL: Selected variables:  M_RealIncome_Growth + M_RealIncome_Growth_12



# --- Single-factor models: various lag orders

# - M_RealIncome_Growth_12
modMLR <- multinom(Target_FromD ~  M_RealIncome_Growth_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,775; McFadden R^2:  0.13%; AUC:  53.56%
# 'Strongest' variable: M_RealIncome_Growth_12 + M_RealIncome_Growth




# ------ 6e. Modelling theme: DTI growth

# --- Single-factor base-model | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromD ~ M_DTI_Growth, 
                              data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,878; McFadden R^2:  0.09%; AUC:  52.27%

# --- Full-model | Stepwise forward selection procedure
modMLR_full <- multinom(Target_FromD ~ M_DTI_Growth + M_DTI_Growth_1 + M_DTI_Growth_2 + M_DTI_Growth_3 + 
                              M_DTI_Growth_6 + M_DTI_Growth_9 + M_DTI_Growth_12, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,153; McFadden R^2:  0.38%; AUC:  53.74%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: AIC:  255,151; McFadden R^2:  0.38%; AUC:  53.70%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime;
### RESULTS: All variables are significant
### FINAL: Selected variables:  M_DTI_Growth + M_DTI_Growth_6 + M_DTI_Growth_9 + M_DTI_Growth_12



# --- Single-factor models: various lag orders
# - M_DTI_Growth_6
modMLR <- multinom(Target_FromD ~  M_DTI_Growth_6, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,825; McFadden R^2:  0.11%; AUC:  50.98%

# - M_DTI_Growth_12
modMLR <- multinom(Target_FromD ~  M_DTI_Growth_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,602; McFadden R^2:  0.19% ; AUC:  50.42%
# 'Strongest' variable: M_DTI_Growth_12 + M_DTI_Growth





# ------ 6f. Modelling theme: Employment growth

# --- Single-factor base-model | Stepwise forward selection procedure
modMLR_single_sub <- multinom(Target_FromD ~ M_Emp_Growth, 
                              data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,834; McFadden R^2:  0.10%; AUC:  51.98%

# --- Full-model | Stepwise forward selection procedure
modMLR_full <- multinom(Target_FromD ~ M_Emp_Growth + M_Emp_Growth_1 + M_Emp_Growth_2 + M_Emp_Growth_3 + 
                              M_Emp_Growth_6 + M_Emp_Growth_9 + M_Emp_Growth_12, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,555; McFadden R^2:  0.23%; AUC:  53.85%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 2.7m
### RESULTS: AIC:  255,579; McFadden R^2:  0.21%; AUC:  53.66%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; <1m
### RESULTS: All variables are significant
### FINAL: Selected variables:  M_Emp_Growth + M_Emp_Growth_2 + M_Emp_Growth_12



# --- Single-factor models: various lag orders
# - M_Emp_Growth_2
modMLR <- multinom(Target_FromD ~  M_Emp_Growth_2, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,849; McFadden R^2:  0.10%; AUC:  52.68%

# - M_Emp_Growth_12
modMLR <- multinom(Target_FromD ~  M_Emp_Growth_12, 
                   data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  255,875; McFadden R^2:  0.09%; AUC:  53.69%
# 'Strongest' variable: M_Emp_Growth_12 + M_Emp_Growth_2 + M_Emp_Growth





# ------ 6g. Combining insights: A complete set of macroeconomic variables

# --- Full-model | Stepwise forward selection procedure
modMLR_full <- multinom(Target_FromD ~ M_Repo_Rate_12 + M_Repo_Rate + 
                          M_Inflation_Growth_6 + M_Inflation_Growth_12 + 
                          M_RealGDP_Growth_12 + M_RealGDP_Growth_1 + M_RealGDP_Growth + 
                          M_RealIncome_Growth_12 + M_RealIncome_Growth + 
                          M_DTI_Growth_12 + M_DTI_Growth + 
                          M_Emp_Growth_12 + M_Emp_Growth_2 + M_Emp_Growth, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  254,033; McFadden R^2:  0.84%; AUC:  55.00%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_base_def, scope = list(lower = modMLR_base_def, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 30m
### RESULTS: AIC:  254,104; McFadden R^2:  0.80%; AUC:  54.89%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 2.1m
### RESULTS: All variables are significant
### FINAL: Selected variables: M_Repo_Rate + M_Inflation_Growth_6
# M_RealGDP_Growth_12 + M_RealIncome_Growth + M_DTI_Growth + M_Emp_Growth + M_Emp_Growth_2
# Removed using expert judgement: M_Repo_Rate_12 + M_Inflation_Growth_12





# ------ 7. Combining insights: A complete set of all input variables

# --- Base-level null model | Stepwise forward selection procedure
# Specifying the best-in-class variables per theme as the minimum model
modMLR_single_sub <- multinom(Target_FromD ~ g0_Delinq_fac + g0_Delinq_Ave, 
                              data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_single_sub, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  210,769; McFadden R^2:  17.71% ; AUC:  91.95%

# --- Full-model | Stepwise forward selection procedure
modMLR_full <- multinom(Target_FromD ~ DefaultStatus1_Aggr_Prop_Lag_5 + DefaultStatus1_Aggr_Prop_Lag_9 + 
                          CreditLeverage_Aggr + g0_Delinq_Ave + CuringEvents_Aggr_Prop + 
                          g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + g0_Delinq_Num + 
                          g0_Delinq_SD_9 + slc_acct_roll_ever_24_imputed_mean + pmnt_method_grp + InterestRate_Margin + Principal_Real + 
                          BalanceToPrincipal + AgeToTerm + TimeInStateSpell + 
                          M_Repo_Rate + M_Inflation_Growth_6 + M_RealGDP_Growth_12 + M_RealIncome_Growth + 
                          M_DTI_Growth + M_Emp_Growth + M_Emp_Growth_2
                            , data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_full, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  183,624; McFadden R^2:  28.37%; AUC:  96.25%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_full_stepwise <- stepAIC(modMLR_single_sub, scope = list(lower = modMLR_single_sub, upper = modMLR_full), 
                                    direction = "both", k=log(datCredit_train[MarkovStatus=="Def",.N]), maxit=50)
evalMLR(modMLR_full_stepwise, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
proc.time() - ptm # IGNORE: elapsed runtime; 7.8h
### RESULTS: AIC:  183,715; McFadden R^2:  28.32%; AUC:  96.23%
# Selection agrees with intuition in that deselected ones had poor fit statistics

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_full_stepwise, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 27m
### RESULTS: All variables are significant
### FINAL: Selected variables:  DefaultStatus1_Aggr_Prop_Lag_5 + DefaultStatus1_Aggr_Prop_Lag_9 + CreditLeverage_Aggr + g0_Delinq_Ave + 
# g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + g0_Delinq_Num + g0_Delinq_SD_9 + slc_acct_roll_ever_24_imputed_mean + 
# pmnt_method_grp + InterestRate_Margin + Principal_Real + BalanceToPrincipal + AgeToTerm + TimeInStateSpell + 
# M_Repo_Rate + M_RealGDP_Growth_12 + M_DTI_Growth
# Removed using expert judgement: DefaultStatus1_Aggr_Prop_Lag_9
# Added using expert judgement: M_Inflation_Growth_6



# --- Model 1, training set
modMLR_def <- multinom(Target_FromD ~ DefaultStatus1_Aggr_Prop_Lag_5 + CreditLeverage_Aggr + g0_Delinq_Ave + 
                              g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + g0_Delinq_Num + g0_Delinq_SD_9 + slc_acct_roll_ever_24_imputed_mean + 
                              pmnt_method_grp + InterestRate_Margin + Principal_Real + BalanceToPrincipal + AgeToTerm + TimeInStateSpell + 
                              M_Repo_Rate + M_RealGDP_Growth_12 + M_DTI_Growth + M_Inflation_Growth_6, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_def, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:  183,712; McFadden R^2:  28.32% ; AUC:  96.24%

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_def, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 25.5m
### RESULTS: All variables are significant



# --- Model 2, training set, with natural splines, applied interactively using expert judgement
modMLR_def <- multinom(Target_FromD ~ DefaultStatus1_Aggr_Prop_Lag_5 + CreditLeverage_Aggr + g0_Delinq_Ave + 
                              g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + g0_Delinq_Num + g0_Delinq_SD_9 + 
                              ns(slc_acct_roll_ever_24_imputed_mean,6) + pmnt_method_grp + ns(InterestRate_Margin,3) + Principal_Real + 
                              BalanceToPrincipal + AgeToTerm + TimeInStateSpell + 
                              M_Repo_Rate + M_RealGDP_Growth_12 + M_DTI_Growth + M_Inflation_Growth_6, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_def, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:   183,464 ; McFadden R^2:  28.43% ; AUC:  96.24%

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modLR_Result <- dropterm(modMLR_def, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 28.9m
### RESULTS: All variables are significant




# ------ 8. Fit final model 
# - Confirm that required data objects are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)

# - Fit an empty model as a performance gain, used within some diagnostic functions
modMLR_base_def <- multinom(Target_FromD ~  1, 
                            data = datCredit_train[MarkovStatus=="Def",],maxit=1000)

# - Train model
modMLR_def <- multinom(Target_FromD ~ DefaultStatus1_Aggr_Prop_Lag_5 + CreditLeverage_Aggr + g0_Delinq_Ave + 
                         g0_Delinq_fac + slc_acct_arr_dir_3 + TimeInDelinqState + g0_Delinq_Num + g0_Delinq_SD_9 + 
                         ns(slc_acct_roll_ever_24_imputed_mean,6) + pmnt_method_grp + ns(InterestRate_Margin,3) + Principal_Real + 
                         BalanceToPrincipal + AgeToTerm + TimeInStateSpell + 
                         M_Repo_Rate + M_RealGDP_Growth_12 + M_DTI_Growth + M_Inflation_Growth_6, 
                       data = datCredit_train[MarkovStatus=="Def",],maxit=1000)
evalMLR(modMLR_def, modMLR_base_def, datCredit_train[MarkovStatus=="Def",], targetFld="Target_FromD", predClass="Perf")
### RESULTS: AIC:   183,464 ; McFadden R^2:  28.43% ; AUC:  96.24%


# --- Cleanup
rm(datCredit_train, datCredit_train_sub, datCredit_valid, datKeys, datKeys_sampled, 
   modLR_Result, modMLR, modMLR_base_def, modMLR_full, modMLR_full_sub,
   modMLR_single_sub, modMLR_def); gc()
