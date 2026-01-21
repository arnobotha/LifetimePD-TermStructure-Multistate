# ======================================== SUMMARY STATISTICS== ========================================
# Drawing summary statistics of selected input variables across the various models
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
#   - <Summary statistics>
# ------------------------------------------------------------------------------------------------------





# ------ 1. Preliminaries

# - Confirm that required data objects are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)

# - Bind data together to draw sample statistics; not necessary for a resampling scheme currently
datCredit <- rbind(data.table(datCredit_train, Sample="Training"), 
                   data.table(datCredit_valid, Sample="Validation"))

# - Cleanup; memory optimisation
rm(datCredit_train, datCredit_valid); gc()






# ------ 2. Summary statistics
lapply(describe2(datCredit$AgeToTerm_Aggr_Mean), function(x) {round(x,digits=3)} )
lapply(describe2(datCredit$BalanceToPrincipal), function(x) {round(x,digits=3)} )
lapply(describe2(datCredit$CreditLeverage_Aggr), function(x) {round(x,digits=3)} )
lapply(describe2(datCredit$DefaultStatus1_Aggr_Prop), function(x) {round(x,digits=3)} )
lapply(describe2(datCredit$g0_Delinq), function(x) {round(x,digits=3)} )
lapply(describe2(datCredit$g0_Delinq_Ave), function(x) {round(x,digits=3)} )
lapply(describe2(datCredit$InterestRate_Margin), function(x) {round(x,digits=3)} )
lapply(describe2(datCredit$M_Emp_Growth), function(x) {round(x,digits=3)} )
lapply(describe2(datCredit$M_Repo_Rate), function(x) {round(x,digits=3)} )
lapply(describe2(datCredit$slc_acct_roll_ever_24_imputed_mean), function(x) {round(x,digits=3)} )

# - Cleanup
rm(datCredit); gc()
