# =================================== CUSTOM FUNCTIONS ==================================
# Defining custom functions used across various projects
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Default risk term-structure modelling using Markov-models
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Marcel Muller (MM), Roland Breedt (RB)

# DESCRIPTION:
# This script defines various functions that are used elsewhere in this project
# or, indeed, used across other projects. Functions are grouped thematically.
# =======================================================================================
### NOTE: This script originates predominantly from another project called 'ClassifierDiagnostics'



# -------- Ternary functions
# from https://stackoverflow.com/questions/8790143/does-the-ternary-operator-exist-in-r
`%?%` <- function(x, y) list(x = x, y = y)
`%:%` <- function(xy, z) if(xy$x) xy$y else z




# -------- Utility functions

# -- Mode function (R doesn't have a built-int one)
getmode <- function(v) {
  uniqv <- unique(v);
  # discard any missingness
  uniqv <- uniqv[complete.cases(uniqv)]
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# -- Memory function using 'gdata' package
getMemUsage <- function(limit=1000){
  require(gdata); require(scales)
  # - Get list of significant object sizes occupied in memory, order ascendingly
  totUsage <- ll()
  memSize <- subset(totUsage, KB >= limit)
  memSize$MB <- memSize$KB/1000
  gc(verbose=F)
  cat("Total memory used: ", comma(sum(totUsage$KB)/1000), "MB\n")
  cat("Big objects size: ", comma(sum(memSize$MB)), "MB\n\n")
  return(  memSize[order(memSize$KB), c(1,3)])
}



# -- Custom summary function in replicating describe() due to its recent performance issues on large datasets
describe2 <- function(x) {
  #x <- dat.raw$Arrears
  result <- c(
    n = length(x),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    q = quantile(x, 0.05, na.rm = TRUE),
    q = quantile(x, 0.1, na.rm = TRUE),
    q = quantile(x, 0.25, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    q = quantile(x, 0.75, na.rm = TRUE),
    q = quantile(x, 0.9, na.rm = TRUE),
    q = quantile(x, 0.95, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    missing = sum(is.na(x))
  )
  result2 <- sort(x)[1:5]
  result3 <- rev(sort(x))[1:5]
  return(list(summary=result, lowest=result2, highest=result3))
}



# -------- Cleaning functions (Missing/extreme value treatments)

# -- Custom function that curates a main vector [x] to equal the previous/most-recent non-
# missing element in a given vector
imputeLastKnown <- function (x) {
  # -- Testing purposes
  # x <- Lookup$ZeroBal_Remain_Ind; x_lead <- Lookup$ZeroBal_Remain_Ind_lead
  # x <- c(0,0,0,1,1,1,0,1)
  # x <- c(0,0,0,1,1,1,0,NA)
  # x <- c(0,0,0,1,1,1,1,NA)
  # x <- c(0,0,0,1,NA,1,0,NA)
  # x <- c(0,NA)
  
  firstOne <- which(is.na(x))[1]
  if (!is.na(firstOne) & firstOne > 1) {
    x[firstOne] <- x[firstOne-1]
    # call function recursively to fix earlier missing-cases
    return( imputeLastKnown(x))
  } else { # no missing value found, return original vector
    return(x)
  }
}


# -- Custom function that curates a main vector [x] where x[1] is missing.
# This is achieved by finding the first non-missing element and back-filling that value
imputeFirstKnown <- function(x) {
  # -- Testing purposes
  # x <- c(NA, NA, 2,3,4)
  firstOne <- which(!is.na(x))[1]
  if (!is.na(firstOne) & firstOne > 1) {
    x[1:(firstOne-1)] <- x[firstOne]
    return(x)
  } else { # no non-missing value found, return original vector
    return(x)
  }
}


# -- Custom Function by which to adjust for inflation
# Assumes a monthly macroeconomic dataset [macro_data_hist] to exist with [Date_T] and [Inflation] fields
adjInflation <- function(g_start, g_stop) {
  compFact <- macro_data_hist[Date_T >= g_start & Date_T <= g_stop, list(Factor = prod(1 + (Inflation/100)/12))]
  return(compFact)
}


# -- Adjusting for inflation (Robust version that accepts a macroeconomic dataset)
# Generating an inflation factor for a given series of yearly inflation growth rates
# Input:  [datMacro]: The dataset containing the yearly inflation growth rate
#         [time]: Name of the time/date variable in [datMacro]
#         [g_start]:  The starting date for the series of inflation growth rates
#         [g_stop]:   The ending date for the series of inflation growth rates
# Output: A factor indicating the cumulative inflation over the period starting at [g_start] and ending [g_stop]
# --- Define custom function for computing inflation/deflation factors
adjInflation_MV <- function(datMacro, time, Inflation_Growth, g_start, g_stop) {
  # datMacro=datMV; time="Date"; g_start<-date("2015-02-28"); g_stop<-date("2022-12-31"); Inflation_Growth<-"M_Inflation_Growth"
  compFact <- as.numeric(datMacro[get(time) >= g_start & get(time) <= g_stop, list(Factor = prod(1 + (get(Inflation_Growth))/12))])
  return(compFact)
  # rm(datMacro, time, g_start, g_stop, Inflation_Growth); gc()
}
# - Unit test
# if (!exists('datMV')) unpack.ffdf(paste0(genPath,"datMV"), tempPath)
# (test <- adjInflation(datMacro=datMV, time="Date", g_start=date("2015-02-28"), g_stop=date("2022-12-31"), Inflation_Growth="M_Inflation_Growth"))
# rm(datMV, test); gc()


# -- Function to convert NaN-values or infinite values within a vector to the given value 
Treat_NaN <- function(vec, replaceVal=0) {
  vec[is.nan(vec) | is.infinite(vec)] <- replaceVal
  return (vec)
}


# -- Function for applying Winsorisation for dealing with outliers
# All values above (and or below) a certain quantile(s) are assigned the value(s) of that specific quantile(s).
# Input: [x]: A real valued vector
# Output: A vector containing values betweem the specified lower and upper quantile of the input vector.
winsorise <- function(x,lower_quant=0.025,upper_quant=0.975){
  # --- Testing purposes
  #x <- 1560*rbeta(10000, shape1=1, shape2=20); hist(x)
  #lower_quant <- 0.05; upper_quant <- 0.90
  # - Obtainnig the upper and lower quantiles of the distribution and creating indicator variables for applying the winsorisation
  wins_ind_low <- as.numeric(x<quantile(x,lower_quant))
  wins_ind_up <- as.numeric(x>quantile(x,upper_quant))
  wins_ind_bet <- abs(wins_ind_low + wins_ind_up-1)
  
  # - Applying the winsorisation
  z <- wins_ind_bet*x + wins_ind_low*quantile(x,lower_quant) + wins_ind_up*quantile(x,upper_quant)
  
  # - Returning the winsorised vector
  return(z)
  
  #hist(z); rm(x,z,lower_quant,upper_quant,wins_ind_low,wins_ind_up,wins_ind_bet)
}




# -------- Interleaving & Interpolation functions

# -- Coalescing function to facilitate data fusion between two given vectors
# Input: two scalar values (x & y) that may have selective missingness in either side (left: x; right: y)
# Output: Returns the non-missing side. If both are non-missing, then returns the (given) preference.
interleave <- function(x,y, na.value = as.integer(NA), pref='X') {
  # ensure require(dplyr)
  case_when(!is.na(x) & is.na(y) ~ x,
            is.na(x) & !is.na(y) ~ y,
            is.na(x) & is.na(y) ~ na.value,
            x == y ~ x,
            x != y & pref=='X' ~ x,
            x != y & pref=='Y' ~ y,
  )
}


# -- Missing value Treatment: Interpolate the values between two known non-missing points
# Assumes all missingness are 'encased' between two known points.
# Input: [given]: a time series possibly with some missing values for which we like to interpolate;
#     [shouldRollBackward]: If the first element is missing, should we try to fix this by 'back-interpolating'
#       from the first non-missing point found?;
#     [SilenceWarnings]: Self-explanatory;
#     [shouldRollForward]: When there is only a single non-missing element, should we simply copy that value forward?
# Output: Linearly interpolated vector
interPol <- function(given, shouldRollForward=T, shouldRollBackward=T, SilenceWarnings=T) {
  
  # -- Testing conditions
  #given <- macro_data_hist$Inflation # for testing
  #given <- as.vector(subset(macro_data, Scenario=="Historic")[order(Date_T), RealGDP_Growth_yoy])
  #unique(macro_data$Scenario)
  #given <- as.vector(subset(macro_data, Scenario=="Baseline")[order(Date_T), rbqn_rb5339q])
  # (given <- as.vector(subset(macro_data, Scenario=="SevereStress")[order(Date_T), Consumption_Level_1q]))
  
  
  # first, check if there are any non-missing element
  if (all(is.na(given))) {
    # yes, there is, so just return the same input values and throw a warning (if allowed)
    if (SilenceWarnings==F) {
      warning("All data is missing, returning NA throughout..") 
    }
    return(given)
  }
  
  # second, check if there is any missing value; if so, then exit the function
  if (all(!is.na(given))) {
    return(given)
  }
  
  # third, check if first value is missing, which can hamper our interpolation procedure
  if (is.na(given[1])) {
    # yup, so should we try to fix this by 'back-interpolating' based on the first set of 2 non-missing values in the series?
    if (shouldRollBackward == T) {
      
      start.point <- 1 # starting point for filling in interpolated vaues at the end of this procedure
      
      # find first non-missing value in the series, which will be our 'ending value' for interpolating backwards
      end.point <- which(!is.na(given))[1]-1 # position before first non-missing element
      end.val <- given[end.point+1] # first non-missing element
      
      # we need to find second non-missing value and perform an 'interim' interpolation so that we have a one-period value
      # by which to change [end.val] backwards to [start.point] at the 'same speed' (as an assumption)
      start.point2 <- which(!is.na(given))[1]+1 # position after first non-missing element
      start.val2 <- given[start.point2-1] # first non-missing element
      end.point2 <- which(!is.na(given))[2]-1 # position before second non-missing element
      end.val2 <- given[end.point2+1] # second non-missing element
      
      # interpolate across this range, including the two values as outer bounds (therefore add 2 to the interpolation length)
      # note that (end.point - start.point + 1) denotes the length of this missingness-episode
      inter.vals <- seq(from=start.val2, to=end.val2, length.out = end.point2 - start.point2 + 1 + 2)
      
      # - might as well linearly interpolate here (saving a computing cycle of the while loop later on) ..
      # delete the first and last observation (they are the outer values outside of the missingness range)
      # and assign these interpolated values to the given vector
      given[start.point2:end.point2] <- inter.vals[2:(end.point2 - start.point2 + 2)]
      
      # check if we have non-zero elements at both sides
      if (start.val2 == 0 & end.val2 == 0) {
        # yes, so by-pass this treatment and just fill with 0s
        given[start.point:end.point] <- rep(0, end.point-start.point + 1)
      } else {
        
        # get interpolation 'speed'
        speed <- diff(given[start.point2:(start.point2+1)]) / given[start.point2]
        # given[start.point2]*(1+speed) # test
        
        # 'discount' the value backwards from the first non-missing value, using the previously calculated speed as the 'discount rate'
        for (i in end.point:start.point ) {
          given[i] <- given[i+1]*(1+speed)^(-1)
        } 
      }
      
    } else {
      # no we cannot. So throw error and exit
      stop("Error: Base assumption violated - First observation is missing, cannot interpolate. Exiting ..") 
    }
  }
  
  # repeat until no more missingness in given vector
  while ( any(is.na(given)) ) {
    
    # -- testing conditions
    #given <- c(2,NA,NA,5,NA,NA,8) # works
    #given <- c(2,NA,NA,5,6, NA,NA, 9) # works
    #given <- c(2,NA,NA,5,6, NA, NA, NA)
    
    # find the indices of all missing observations
    miss.ind <- which(is.na(given))
    
    # find "episodes" of missingness in these indices, since there may be more than 1 episode in the general case,
    # for which we need to repeat this procedure.
    # 1. Do this by first isolating the cases where the lagged differences are greater than 1
    # 2. Add 1 to these found positions to move to the "initial starting points" of the next episode in succession
    # 3. Pre-fix this vector with '1' to re-include the first 'episode' that was deselected previously
    # 4. Given this vector of indices (of indices), return starting positions again
    episode.starting.times <- miss.ind[c(1, which(diff(miss.ind) > 1) + 1)]
    
    # - check if we have data points outside of the first episode from which to interpolate
    # get staring point of first episode of missingness
    start.point <- episode.starting.times[1]
    # get ending point of first episode (got to test first if we have multiple episodes and diverge logic from there)
    if (length(episode.starting.times) > 1) {
      # we have multiple episodes. Therefore, scan the series from missingness's start up to the first non-missing element, then minus 1
      # add this to the starting point, minus 1 to exclude the first missing value (otherwise we are double-counting it when adding this range)
      end.point <- start.point + (Position(function(x) {!is.na(x)}, x=given[start.point:(episode.starting.times[2]-1)] ) - 1) - 1
    } else {
      # we don't have multiple episodes. Therefore, take last known missingness index
      end.point <- miss.ind[length(miss.ind)]
    }
    
    # given the starting and ending points for the actual interpolation, test for non-missing data outside of this range from 
    # which we need to interpolate
    if (!is.na(given[start.point-1]) & !is.na(given[end.point+1])) {# returns true if we can interpolate (no missingness outside of range)
      start.val <- given[start.point-1]
      end.val <- given[end.point+1]
      # interpolate across this range, including the two values as outer bounds (therefore add 2 to the interpolation length)
      # note that (end.point - start.point + 1) denotes the length of this missingness episode
      inter.vals <- seq(from=start.val, to=end.val, length.out = (end.point - start.point + 1) + 2)
      # delete the first and last observation (they are the outer values outside of the missingness range)
      # and assign these interpolated values to the given vector
      given[start.point:end.point] <- inter.vals[2:(end.point - start.point + 2)]
      
    } else {
      # assumption violated or episode's length = 1. Check if we can simply replace NAs with last known value in either case?
      if (shouldRollForward == T){
        if (SilenceWarnings==F) {
          warning("Base assumption violated - no available data outside of missingness range from which to interpolate. Rolling values forward instead ..")
        }
        # by definition, we must have a non-missing first element (should start.point >= 2)
        start.val <- given[start.point-1]
        given[start.point:end.point] <- rep(start.val, (end.point - start.point + 1)) # just repeat for the length of the missingness episode
        
      } else {
        # no we cannot. So throw error and exit
        stop("Error: Base assumption violated - no available data outside of missingness range from which to interpolate. Exiting ..") 
      }
    }
    
  }
  
  return(given)
}



# -------- Scaling functions

# -- A few scaling functions to standardize given vectors unto a uniform scale
# Input: [given]: a real-valued vector
# Output: standardized vector
### NOTE: the shifting parameter can be useful when the given vector contains excessive zero-values.

# 1) Range-based scaler | vectors will have equal ranges (min-max)
scaler <- function(given, shift=TRUE){
  if (shift==T){
    output <- (given - min(given,na.rm=T)) / (max(given, na.rm=T) - min(given, na.rm=T))
  } else {
    output <- (given) / (max(given, na.rm=T) - min(given, na.rm=T))
  }
  return(output)
}
# 2) Z-score/normalized scaler | vectors should roughly be N(0,1) distributed
scaler.norm <- function(given, shift=TRUE){
  # (given <- as.vector(subset(macro_data_hist1, Scenario=="Baseline")$DebtToIncome_Rate)) # for testing
  if (shift==T){
    output <- (given - mean(given,na.rm=T)) / (sqrt(var(given,na.rm=T)))
  } else {
    output <- (given) / (sqrt(var(given,na.rm=T)))
  }
  # check for NaN values (which can result if there is 0 variance)
  if (all(is.na(output))) {
    # just assign the central value, in this case, 0
    output <- rep(0, length(output))
  }
  return(output)
}




# -------- Transformation functions

# -- Yeo-Johnson transformation function
# A function for applying Yeo-Johnson transformation to a given vector. The optimal transformation is selected based 
# on either a normal log-likelihood
# function of the transformed vector. The optimal transformation is thus chosen based on the best approximation to normality.
# Input: [x]: a real-valued vector
# Output:vector transformed with an optimal power transformation
transform_yj <- function(x, bound_lower=-2, bound_upper=2, lambda_inc=0.5, verbose=FALSE, plotopt=FALSE, plotqq=FALSE, norm_test=FALSE){
  # --- Unit test
  # x <- 1560*rbeta(10000, shape1=1, shape2=20); hist(x)
  # x <- rgamma(10000, shape=2, rate=5); hist(x) 
  # bound_lower<--5; bound_upper<-5; lambda_inc<-0.5; verbose<-FALSE; plotopt<-TRUE; plotqq<-TRUE; norm_test=TRUE
  
  # - Preliminaries
  require(MASS) # Ensure the requried pacakage in loaded of the Box-Cox function
  lambda_search <- seq(from=bound_lower, to=bound_upper, by=lambda_inc) # Check if lambda_search exists and if not, assign a value: This parameter specifies the search space to obatin the optimal power transformation in th boxcox function
  
  # - Ensuring the plotting area is ready for possible plots 
  par(mfcol=c(1,1))
  
  # - Selecting the optimal lambda1 parameter based on the choice of the loss function
  lambda <- boxcox((x-min(x)+0.000001)~1,lambda=seq(from=bound_lower,to=bound_upper,by=lambda_inc), plotit=FALSE)
  lambda_opt <- lambda$x[which.max(lambda$y)]
  lambda_yj <- lambda_opt
  
  # - Applying the Yeo-Johnson transformation with the optimal lambda parameter
  con1 <- as.integer(x>=0)*(lambda_yj!=0)
  con2 <- as.integer(x>=0)*(lambda_yj==0) 
  con3 <- as.integer(x<0)*(lambda_yj!=2)
  con4 <- as.integer(x<0)*(lambda_yj==2)
  
  y <- ((con1*x+1)^lambda_yj-1)/(ifelse(lambda_yj!=0,lambda_yj,1)) + log(con2*x+1) - ((-con3*x+1)^(2-lambda_yj)-1)/(2-ifelse(lambda_yj!=2,lambda_yj,1)) - log(-con4*x+1)
  
  
  # - Reporting the optimal lambda parameter, as well as the corresponding log-likelihood
  cat('\nNOTE:\tThe optimal power-transformation is lambda1 = ', lambda_yj)
  cat('\n \tThe optimal transformation has a log-likelihood =', round(max(lambda$y[which(lambda$x==lambda_yj)])), '\n')
  
  # - Plotting two qq-plots to show the normality of the given vector (x) before and after the optimal transformation 
  if(plotqq==TRUE){
    par(mfcol= c(1,2)); qqnorm(x); qqline(x, distribution = qnorm); qqnorm(y); qqline(y, distribution = qnorm);
    par(mfcol=c(1,1)) # Resetting plotting graph dimensions
  }
  
  # - Conducting a KS test for normality
  if(norm_test){
    ks_test_x <- ks.test(x, "pnorm")$p.value
    ks_test_y <- ks.test(y, "pnorm")$p.value
    
    cat('\nNOTE:\tThe KS-test for normality on the un-transformed data yields a p-value of ', ks_test_x)
    cat('\n \tThe KS-test for normality on the transformed data yields a p-value of ', ks_test_y)
  }
  
  # - Return the transformed vector
  cat('\n \n')
  return(y)
  #rm(x,y,bound_lower,bound_upper, verbose, plotopt, plotqq, lambda1, lambda2, lambda_search, norm_test)
}
# Some more testing conditions
# transform_yj(x=1560*rbeta(10000, shape1=1, shape2=20), bound_lower=4, plotopt=TRUE)
# transform_yj(x=1560*rbeta(10000, shape1=1, shape2=20),bound_lower=-2,bound_upper=2,lambda_inc=0.1, plotopt=TRUE, plotqq=TRUE, norm_test=TRUE)




# -------- Variable Importance Measures for Logit Models

# A function for measuring and rank-ordering the variable "importance" given a logit model
# Three such measures are implemented:
# 1) standardised coefficients via refitting on Z-scored input space [stdCoef_ZScores]
# 2) absolute coefficients [absCoef]
# 3) partial dependence (an explanable AI measure; see https://arxiv.org/pdf/1904.03959.pdf) [partDep]
# Regarding measures 1-2, the size of coefficients (or transforms thereof) are used in ranking the 
# variables from most to least "important". A larger/ smaller measure-value indicates a more/ less important
# Input:  [logit_model]: A logistic regression model trained using glm()
#         [method]:      "stdCoef"; "absCoef", "partDep" using the "FIRM"-technique from the vip::vi_firm() function
#         [sig_level]:   Significance level or threshold under which the variables are considered as statistically significant using p-values from the Wald-statistic
#         [impPlot]:     Switch for producing a bar chart that shows the variable importance according to the specified measure
#         [pd_plot]:     Should a partial dependence plot be created for each variable
# Output: A data table containing the variable importance information
varImport_logit <- function(logit_model, method="stdCoef_ZScores", sig_level=0.05, impPlot=F, pd_plot=F, chosenFont="Cambria", 
                            colPalette="BrBG", colPaletteDir=1, plotVersionName="", plotName=paste0(genFigPath, "VariableImportance_", method,"_", plotVersionName,".png"), 
                            limitVars=10, dpi=180, Menard_Method="Pearson"){
  
  # - Unit testing conditions:
  # unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath); unpack.ffdf(paste0(genObjPath, "Adv_Formula"), tempPath)
  # logit_model <- glm(inputs_adv, data=datCredit_train, family="binomial")
  # method <- "stdCoef_Menard"; sig_level<-0.05; impPlot<-T; pd_plot<-T; chosenFont="Cambria"; colPalette="BrBG"; colPaletteDir=1
  # plotName=paste0(genFigPath, "VariableImportance_", method,".png"); limitVars=10; Menard_Method = "Pearson"
  
  # - Safety check
  if (!any(class(logit_model) %in% c("glm", "lm"))) stop("Specified model object is not of class 'glm' or 'lm'. Exiting .. ")
  
  # --- 0. Setup
  # - Get the data the model was trained on
  datTrain1 <- subset(logit_model$data, select = names(logit_model$data)[names(logit_model$data) %in% names(model.frame(logit_model))])
  # Getting the names of the original training dataset
  datTrain1_names <- names(datTrain1)
  
  # - Initialising vectors and datasets
  coefficients_summary <- data.table(names=names(summary(logit_model)$coefficients[,4][-1]), sig=summary(logit_model)$coefficients[,4][-1],
                                     coefficient=summary(logit_model)$coefficients[,1][-1], se=summary(logit_model)$coefficients[,2][-1]) %>% arrange(names) # Names of variables in the model
  coefficients_sig_model_level <- as.list(rep(0,coefficients_summary[,.N])) # Corresponding level name of the categorical variable; NULL in the case of a numeric or integer variable
  coefficients_data <-  data.table(names=names(datTrain1)[-which(names(datTrain1) %in% names(model.frame(logit_model))[1])]) %>% arrange(names) # Names of variables training dataset
  coefficients_sig_data_index <- rep(0,coefficients_summary[,.N]) # Index showing if the variable in the model is significant or not
  coefficients_sig_data <- rep(0,coefficients_summary[,.N]) # Names of the significant variable's associated column name in the training dataset
  sig_level <- ifelse(is.na(sig_level),1,sig_level) # The significance level against which each variable must be tested
  
  # --- 1. Filtering for significant variables
  k <- 1 # Counter
  for (i in 1:length(coefficients_data$names)){ # Main loop - looping through all the relevant variables in the training dataset
    if(class(datTrain1[,get(coefficients_data$names[i])]) %in% c("numeric", "integer")){ # Do the following if variable i is numeric
      coefficients_sig_data_index[k] <- coefficients_summary$sig[k]<=sig_level
      coefficients_sig_data[k] <- as.character(coefficients_data[i,])
      coefficients_sig_model_level[[k]] <- NA 
      k<-k+1
    } else { # Do the following if variable i is categorical
      levels_n <- length(unique(datTrain1[,get(coefficients_data$names[i])]))-1
      coefficients_sig_data_index[k:(k+levels_n-1)] <- rep(ifelse(any(coefficients_summary$sig[k:(k+levels_n-2)]<=sig_level),T,F),levels_n) # Checking if any levels of this variable is significant
      coefficients_sig_data[k:(k+levels_n-1)] <- as.character(coefficients_data[i,])
      for(j in 1:levels_n){
        coefficients_sig_model_level[[k+j-1]] <- substr(coefficients_summary$names[[k+j-1]], nchar(coefficients_data$names[i]) + 1, nchar(coefficients_summary$names[[k+j-1]]))
      }
      k<-k+levels_n
    }
  }
  
  coefficients_sig_model <- coefficients_summary$names[coefficients_sig_data_index==1] # Names of variables in model (may be more than the number of variables in the training dataset due to hot one encoding)
  coefficients_sig_model_level <- coefficients_sig_model_level[!coefficients_sig_data_index==0] # The chain ensures that only levels of the significant categorical variables are chosen
  coefficients_sig_data <- coefficients_sig_data[coefficients_sig_data_index==1] # The chaining ensures that only the columns pertaining to the significant variables are chosen
  
  # - Stopping the function if there are no significant variables
  if (is.null(coefficients_data)) stop("ERROR: Variable importance not conducted since there are no significant variables.") 
  
  # - Initiating the dataset to be returned (results dataset)
  results <- list(data = data.table(Variable = coefficients_sig_model,Value = 0,Rank = 0))
  
  # - Initialise advanced counting variable (for keeping track of all variables in the loop, including categorical/dummy variables)
  k <- 1
  
  # --- 3. Calculating variable importance based on specified method
  
  if (method=='stdCoef_ZScores'){
    # -- Standardizing the input space using Z-scores, followed by refitting the logit model
    # B = \beta - mean(X) / sd(x) | For categorical variables: sd(x) = (mean(x)*(1-mean(x)))^0.5
    # NOTE: The resulting coefficients are therefore "standardized", as per Menard2011 (http://www.jstor.org/stable/41290135)
    
    # Defining a new model formula that will be able to consume categorical variables with multiple levels
    model_form_new <- paste0(as.character(terms(logit_model)[[2]]), " ~ ")
    
    # Scaling the variables
    datTrain2 <- copy(datTrain1)
    for (i in 1:length(coefficients_sig_data)){
      # Get ith variable
      x <- datTrain1[,get(coefficients_sig_data[i])]
      # Conditionally scaling the variable if it is numeric
      if (class(datTrain2[, get(coefficients_sig_data[i])]) %in% c("numeric","integer")){ # Numerical variable
        datTrain2[, coefficients_sig_data[i] := (x-mean(x,na.rm=T))/sd(x, na.rm=T)]
        model_form_new <- paste0(model_form_new, coefficients_sig_data[i], " + ")
      } else { # Categorical variable
        x <- as.numeric(x == coefficients_sig_model_level[[i]])
        # sd_x <- sqrt(mean(x) * (1-mean(x))) # Standard deviation of a Bernoulli random variable | This results in a slightly different result compared to the sd() function (if only a few decimal places)
        sd_x <- sd(x, na.rm=T)
        datTrain2[, paste0(coefficients_sig_data[i], coefficients_sig_model_level[[i]]) := (x-mean(x,na.rm=T))/sd_x]
        model_form_new <- paste0(model_form_new, coefficients_sig_data[i], coefficients_sig_model_level[[i]], " + ")
      }
    }
    # Adjusting the model formula
    model_form_new <- as.formula(substr(model_form_new, 1, nchar(model_form_new)-3))
    
    # Re-training the model on the scaled data
    suppressWarnings( logit_model <- glm(model_form_new, data=datTrain2, family="binomial") )
    
    # Calculating importance measure and preparing result set
    results$data[,Value := data.table(names=names(logit_model$coefficients[which(names(logit_model$coefficients) %in% coefficients_sig_model)]),
                                      Std_Coefficient=logit_model$coefficients[which(names(logit_model$coefficients) %in% coefficients_sig_model)]) %>% arrange(names) %>% subset(select="Std_Coefficient")]
    results$Method <- "Standardised coefficients using Z-scores"
    
  } else if (method=="stdCoef_Goodman") { 
    # -- Variable importance based on Goodman-standardised coefficients
    # See Menard2011; https://www.jstor.org/stable/41290135)
    # B = \beta / sd(\beta)
    
    # Calculating importance measure and preparing result set
    results$data <- copy(coefficients_summary)[names %in% coefficients_sig_model]
    results$data[,Value:=coefficient/se] # Compute the importance measure
    results$data[,`:=`(coefficient=NULL,se=NULL, sig=NULL)]; colnames(results$data) <- c("Variable", "Value")
    results$Method <- "Standardised coefficients: Goodman"
    
  } else if (method=="stdCoef_Menard"){ 
    # -- Variable importance based on Menard-standardised coefficients from Menard2011; https://www.jstor.org/stable/41290135)
    # B = \beta . s_x . R / s_{logit( \hat{Y} )}, where s_x is the standard deviation of X, R is the Pearson correlation between observed values Y and 
    # predicted class probabilities \hat{Y}, and s_\hat{Y} is the standard deviation of logit(\hat{Y})
    # NOTE: R can also be interpreted as a pseudo R^2, and duly estimated using McFadden R^2
    
    # Computing the standard deviations for each x (this requires a loop to ensure that categorical variables are correctly accounted for)
    ### AB: how to compute exactly for categorical? Google suggests standard error of bin proportion, Harrell suggests using "binconf" function from Hmisc-package to calculate Wilson's confidence interval
    ### see (https://stats.stackexchange.com/questions/51248/how-can-i-find-the-standard-deviation-in-categorical-distribution#:~:text=There%20is%20no%20standard%20deviation,a%20binomial%20or%20multinomial%20proportion.)
    sd_x <- rep(0, length(coefficients_sig_model))
    for (i in 1:length(coefficients_sig_model)){
      # Get ith variable
      x <- datTrain1[,get(coefficients_sig_data[i])]
      if (class(x) %in% c("numeric", "integer")){ # Compute standard deviation for numeric variables
        sd_x[i] <- sd(x, na.rm=T)
      } else { # Compute standard deviation for categorical variables
        x <- as.numeric(datTrain1[,get(coefficients_sig_data[i])] == coefficients_sig_model_level[[i]])
        # sd_x[i] <- sqrt(mean(x) * (1-mean(x))) # Standard deviation of a Bernoulli random variable | This results in a slightly different result compared to the sd() function (if only a few decimal places)
        sd_x[i] <- sd(x, na.rm=T)
      } # else
    } # for
    
    # Computing the standard deviation for each y
    y_prob <- na.omit(predict(logit_model, newdata = datTrain1, type="response")); y_logit <- log(y_prob/(1-y_prob)) # Standard deviation of predictions
    sd_y <- sd(y_logit)
    # Conditionally computing the correlation used in the estimation - Either Pearson's correlation or the R-squared
    if (Menard_Method=="Pearson"){
      # Get actual targets
      y_act <- subset(na.omit(datTrain1[, mget(all.vars(logit_model$terms))]), select=all.vars(logit_model$terms)[[1]])[[1]]
      # Compute Pearson correlation
      r2 <- cor(y_prob, y_act, method="pearson")
    } else if (Menard_Method=="R-Squared"){
      r2 <- coefDeter_glm(logit_model)[[1]]; r2 <- as.numeric(substr(r2,1,nchar(r2)-1)) # Converting the character output to numeric  
    }
    
    # Computing the variable importance
    results$data$Value <- coefficients_summary$coefficient[coefficients_sig_data_index==1] * r2 * (sd_x/sd_y)
    
    # Stamping the results dataset with the chosen method for variable importance
    results$Method <- "Standardised Coefficients: Menard"
    
  } else {stop(paste0('"', method,'" is not supported. Please use either "stcCoef_ZScores", "stdCoef_Goodman", or "stdCoef_Menard" as method.'))}# if else (method)
  
  # - Ranking the variables according to their associated importance measure values
  results$data[,Value_Abs:=abs(Value)]
  results$data <- results$data %>% arrange(desc(Value_Abs)) %>% mutate(Rank=row_number()) %>% as.data.table()
  
  # - Calculate contribution degrees to sum of importance measure across all variables
  # NOTE: These contributions are merely ancillary and for graphing purposes.
  # They should not considered too seriously, unless studied more extensively.
  sumVarImport <- sum(results$data$Value_Abs, na.rm=T)
  results$data[, Contribution := Value_Abs / sumVarImport]
  
  # - Post results to console
  #print(results$data)
  
  # --- 3. Creating a general plot of the variable importance (if desired)
  if (impPlot==T){
    # - Create graphing object based on the top [limitVars]-number of variables
    datGraph <- results$data[1:min(.N, limitVars), ]
    
    # - Cull away lengthy names that will otherwise ruin the graph
    datGraph[, Variable_Short := ifelse(str_length(Variable) >= 18, paste0(substr(Variable, 1, 18),".."), Variable)]
    
    # Generic variable importance plot
    results$plots[["Ranking"]] <- ggplot(datGraph, aes(x=reorder(Variable, Value_Abs))) + theme_minimal() + theme(text=element_text(family=chosenFont)) +  # Re-order by [Variable] because [Variable_Short] yields incorrect orderings
      geom_col(aes(y=Value_Abs, fill=Value_Abs)) + geom_label(aes(y=sumVarImport*0.05, label=paste(percent(Contribution, accuracy=0.1)), fill=Value_Abs), family=chosenFont) + 
      annotate(geom="text", x=datGraph[.N, Variable], y=datGraph$Value_Abs[1]*0.75, label=paste0("Variable Importance (sum): ", comma(sumVarImport, accuracy=0.1)), family=chosenFont, size=3) + 
      coord_flip() + scale_fill_distiller(palette=colPalette, name="Absolute value", direction=colPaletteDir) +
      scale_colour_distiller(palette=colPalette, name="Absolute value", direction=colPaletteDir) + 
      scale_x_discrete(labels =datGraph$Variable_Short[order(datGraph$Value_Abs)]) + # Use [Variable_Short] for the x-axis labeling; Ensure ordering matches the absolute variable importance values
      labs(x="Variable name", y=results$Method)
    
    
    # Show plot to current display device
    print(results$plots[["Ranking"]])
    
    cat("Saving plot of variable importance at: ", plotName, "\n")
    # - Save graph
    ggsave(results$plots[["Ranking"]] , file=plotName, width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")
    
  } # if
  
  # - Return results
  return(results)
  # rm(logit_model, datTrain1, datTrain2, method, impPlot, coefficients_sig_model, coefficients_sig_data, sumVarImport, limitVars, coefficients_data, coefficients_sig_data, coefficients_summary, coefficients_sig_model_level)
}
# - Unit test
# install.packages("ISLR"); require(ISLR)
# datTrain_simp <- data.table(ISLR::Default); datTrain_simp[, `:=`(default=as.factor(default), student=as.factor(student))]
# logit_model <- glm(default ~ student + balance + income, data=datTrain_simp, family="binomial")
# a<-varImport_logit(logit_model = logit_model, method="stdCoef_ZScores", impPlot=T)
# b<-varImport_logit(logit_model = logit_model, method="stdCoef_Goodman", impPlot=T)
# c<-varImport_logit(logit_model = logit_model, method="stdCoef_Menard", impPlot=T)




# -------- Diagnostic functions for Logit Models

# --- Pseudo R^2 measures for classifiers
# Calculate a pseudo coefficient of determination (R^2) \in [0,1] for glm assuming binary
# logistic regression as default, based on the "null deviance" in likelihoods
# between the candidate model and the intercept-only (or "empty/worst/null") model.
# NOTE: This generic R^2 is NOT equal to the typical R^2 used in linear regression, i.e., it does
# NOT explain the % of variance explained by the model; but rather it denotes the %-valued degree
# to which the candidate's fit can be deemed as "perfect".
# Implements McFadden's pseudo R^2, Cox-Snell generalised R^2, Nagelkerke's improvement upon Cox-Snell's R^2
# see https://bookdown.org/egarpor/SSS2-UC3M/logreg-deviance.html ; https://web.pdx.edu/~newsomj/cdaclass/ho_logistic.pdf; 
# https://statisticalhorizons.com/r2logistic/
# https://stats.stackexchange.com/questions/8511/how-to-calculate-pseudo-r2-from-rs-logistic-regression
coefDeter_glm <- function(model, model_base = NA) {
  
  # - Safety check
  if (!any(class(model) %in% c("glm","multinom"))) stop("Specified model object is not of class 'glm' or 'lm'. Exiting .. ")
  
  # model <- modMLR
  
  # -- Preliminaries
  require(scales) # for formatting of results
  L_full <- logLik(model) # log-likelihood of fitted model, ln(L_M)
  nobs <- attr(L_full, "nobs") # sample size, same as NROW(model$model)

  # Fit a base/empty model if not available
  if (any(is.na(model_base))) {
    orig_formula <- deparse(unlist(list(model$formula, formula(model), model$call$formula))[[1]]) # model formula
    orig_call <- model$call; calltype.char <- as.character(orig_call[1]) # original model fitting call specification, used merely for "plumbing"
    data <- model.frame(model) # data matrix used in fitting the model (model$model)
    # get weight matrix corresponding to each observation, if applicable/specified, otherwise, this defaults to just the 0/1-valued observations (Y)
    if (!is.null(model$prior.weights) & length(model$prior.weights) > 0) {
      weights <- model$prior.weights
    } else if (!is.null(data$`(weights)`) & length(data$`(weights)` > 0)) {
      weights <- data$`(weights)`
    } else weights <- NULL
    data <- data[, 1, drop=F]; names(data) <- "y"
    nullCall <- call(calltype.char, formula = as.formula("y ~ 1"), data = data, weights = weights, family = model$family, 
                     method = model$method, control = model$control, offset = model$offset)
    model_base <- eval(nullCall) # fit base/null model
  } 
  L_base <- logLik(model_base) # log-likelihood of the null model, ln(L_0)
  
  # -- Implement the McFadden pseudo R^2 measure from McFadden1974, R^2 = 1 - log(L_M)/log(L_0)
  # NOTE: null deviance L_0 plays an analogous role to the residual sum of squares in linear regression, therefore
  # McFadden's R^2 corresponds to a proportional reduction in "error variance", according to Allison2013 (https://statisticalhorizons.com/r2logistic/)
  # NOTE2: deviance (L_M) and null deviance (L_0) within a GLM-object is already the log-likelihood since deviance = -2*ln(L_M) by definition
  # https://stats.stackexchange.com/questions/8511/how-to-calculate-pseudo-r2-from-rs-logistic-regression
  if (any(class(model) == "multinom") ) {
    coef_McFadden <- 1 - (as.numeric(L_full) / as.numeric(L_base))
  } else coef_McFadden <- 1 - model$deviance / model$null.deviance
  
  if ( !all.equal(coef_McFadden, as.numeric(1 - (-2*L_full)/(-2*L_base) ) ) ) stop("ERROR: Internal function error in calculating & verifying McFadden's pseudo R^2-measure")
  
  
  # -- Implement Cox-Snell R^2 measure from Cox1983, which according to Allison2013 is more a "generalized" R^2 measure than pseudo,
  # given that its definition is an "identity" in normal-theory linear regression. Can therefore be used to other regression settings using MLE,
  # E.g., negative binomial regression for count data or Weibull regression for survival data
  # Definition: R^2 = 1 - (L_0/L_F)^(2/nobs), but equivalent to below given that L_base = ln(L0) and L_full = ln(L_full)
  # Why? Since (L_0/L_F)^(2/nobs) can be rewritten as exp[ ln( (L_0/L_F)^(2/nobs) )] which simplifies to exp[ (2/nobs) . ln( L_0/L_F )] given property ln(a^b) = b.ln(a),
  # finally becoming exp[ (2/nobs) . ( ln( L_0 ) - ln( L_F)) ]  given property ln(a/b) = ln(a) - ln(b).
  # The below is numerically expedient in avoiding "underflow" memory issues when dealing with large negative log-likelihood values that should rather not be exponentiated.
  # Source: DescTools::PseudoR2 function in DescTools package
  coef_CoxSnell <- as.numeric( 1 - exp(2/nobs * (L_base - L_full)) )
  
  
  # -- Implement Nagelkerke R^2 from Nagelkerke1991, which according to Allison2013 improves upon Cox-Snell R^2 by ensuring an upper bound of 1
  # NOTE: Cox-Snell R^2 has an upper bound of 1 - (L_0)^(2/n), which can be considerably less than 1.
  # This comes at the cost of reducing the attractive theoretical properties of the Cox-Snell R^2 
  if (any(class(model) == "multinom") ) {
    coef_Nagelkerke <- (1 - exp((model$deviance - model_base$deviance)/nobs))/(1 - exp(-model_base$deviance/nobs))
  } else {
    coef_Nagelkerke <- (1 - exp((model$deviance - model$null.deviance)/nobs))/(1 - exp(-model$null.deviance/nobs))
  }
  
  
  # -- Report results
  return( data.frame(McFadden=percent(coef_McFadden, accuracy=0.01), CoxSnell=percent(coef_CoxSnell, accuracy=0.01), Nagelkerke=percent(coef_Nagelkerke, accuracy=0.01)) )
  
  ### NOTE: All of the above were tested and confirmed to equal the results produced below:
  # DescTools::PseudoR2(model, c("McFadden", "CoxSnell", "Nagelkerke"))
  
  # - cleanup (only relevant whilst debugging this function)
  rm(model, L_full, L_base, nobs, data, nullCall, orig_formula, orig_call, weights, coef_McFadden, coef_CoxSnell, coef_Nagelkerke)
}
# - Unit test
# install.packages("ISLR"); require(ISLR)
# datTrain_simp <- data.table(ISLR::Default); datTrain_simp[, `:=`(default=as.factor(default), student=as.factor(student))]
# logit_model <- glm(default ~ student + balance + income, data=datTrain_simp, family="binomial")
# coefDeter_glm(logit_model)
### RESULTS: candidate is 46% (McFadden) better than null-model in terms of its deviance


# --- Residual Deviance measures
# Perform residual analysis for a glm-model using deviances (difference between predicted probabilities and observed proportions of success)
# A standard normal distribution approximates the residual deviance distribution for a well-fitted model (assuming logistic regression)
# Accordingly, min/max residuals should lie within [-3,3], median should be close to 0, and 1st/3rd quantiles 
# should be similarly in their absolute value.
# Deviations from these principles indicate strain in the underlying fit of the model
# see https://library.virginia.edu/data/articles/understanding-deviance-residuals ; https://web.pdx.edu/~newsomj/cdaclass/ho_diagnostics.pdf
resid_deviance_glm <- function(model, err_Median = 0.025, err_quantiles = 0.05, plotResid=T, inf_degree_significance=0.01) {
  
  # - Preliminaries
  require(scales) # formatting numbers
  require(dplyr)
  
  # - Safety check
  if (!any(class(model) == "glm")) stop("Specified model object is not of class 'glm' or 'lm'. Exiting .. ")
  
  # - testing conditions
  # model <- logit_model
  
  # -- 1a. Using built-in functionality to calculate deviance residuals and summarise them accordingly
  d_aggr1 <- quantile(residuals(model))
  
  # -- 1b. Manual calculation of the above (for verification purposes)
  # NOTE: this process uses several types of residuals, which we'll illustrate here assuming logistic regression
  
  # 0) get predictions and observations (y)
  p_hat <- predict(model, type = "response"); y <- model$y
  
  # 1) raw residuals: the difference between observed values {0,1} and predicted probabilities of belonging to a binary-valued class
  e <- residuals(model, type = "response") # or simply e = y - p_hat where y is the observed binary-valued outcome \in {0,1} and e \in [-1,1]
  
  # 2) Pearson residuals: rescaled version of raw residuals by dividing it with the standard deviation of a binomial distribution (if using logistic regression)
  r <- e / sqrt(p_hat * (1 - p_hat)) # or simply r <- residuals(model, type = "pearson")
  
  # 3) Calculate "hatvalues", which is the "degree to which observation y_i influences fitted/predicted/class-probability \hat{y}_i
  # NOTE: hatvalue h \in [0,1] is the leverage score for ith observation (related to Mahalanobis distance),defined as
  # h = x_i^T . (X^T . X)^{-1} . x_i  , where x_i is the ith observation and X is the n x p design matrix whose rows correspond to observations (input space) and columns are variables
  # see https://en.wikipedia.org/wiki/Leverage_(statistics)#Definition_and_interpretations ; https://stats.stackexchange.com/questions/551302/developing-leverage-statistics-manually-in-r
  # see https://pj.freefaculty.org/guides/stat/Regression/RegressionDiagnostics/OlsHatMatrix.pdf
  # High hat-values indicate greater leverage/influence of the associated observation relative to the mean
  # NOTE: Calculating hatvalues is conceptually easy, e.g., X <- model.matrix(model); h <- diag(X %*% solve(t(X) %*% X) %*% t(X))
  # However, singular matrices (non-invertible, i.e., where det(cov(X)) == 0) can hinder its calculation in practice, particulalry when trying to invert "t(X) %*% X" using "solve()"
  # This can be mitigated by applying a small adjustment within the "solve()"-part using ridge regression, or by the LASSO;
  # However, this is exhaustive and we rather rely on R's built-in functionality that already caters exhaustively for these issues.
  h <- hatvalues(model)
  # Influential observations can be highlighted for those cases 
  # see https://stackoverflow.com/questions/9476475/how-to-produce-leverage-stats
  inf_degree <- NROW(h[h> (3 * mean(h))]) / NROW(h)
  inf_degree_max <- max(h) / mean(h)
  
  # 4) standardised/studentized Pearson residuals: adjusting the Pearson residual for leverage (or "hat values")
  # NOTE: These residuals are usually standard normally distributed, which can be a useful diagnostic in and of itself; see Agresti2002
  rs <- r / sqrt(1 - h) # or simply rs <- rstandard(m, type = "pearson") 
  
  # 5) deviance residuals (finally): derived from the likelihood ratio test when comparing a candidate to a saturated/full/perfect model (such that p coefficients = n observations)
  d <- sign(e)*sqrt(-2*(y*log(p_hat) + (1 - y)*log(1 - p_hat)) ) # or simply as residuals(model)
  d_aggr <- quantile(d)
  # [SANITY CHECK] Distribution summary of residual deviances should agree with each other, respective to both methods by which they are calculated.
  cat( all.equal(d_aggr1, d_aggr) %?% 'SAFE: Both methods by which residual deviances are calculated agree with each other in result.\n' %:% 
         'WARNING: The methods by which residual deviances are calculated yield different results.\n')
  
  # -- 2. Reporting results
  cat( (inf_degree <= inf_degree_significance) %?% 
         paste0('SAFE: The degree of influential cases is low at ', percent(inf_degree, accuracy=0.1), ' where hatvalues h > (3*mean(h)) is true.\n---------\n') %:%
         paste0('WARNING: High degree of influential cases at ', percent(inf_degree, accuracy=0.1), ' where hatvalues h > (3*mean(h)) is true. \n\tAlso, max(hatvalue) / mean(hatvalue) = ', 
                comma(inf_degree_max), '; whereas rule-of-thumb is 3.\n---------\n') )
  cat("Residual deviance (difference between observed and predicted; smaller = better):", comma(sum(d^2)), '\n---------\n')
  # [DIAGNOSTIC] Absolute values of min and max percentiles <= 3 ?
  cat( (abs(d_aggr[1]) <= 3 & abs(d_aggr[5]) <= 3) %?% 'SAFE: Min/max residual deviances are within expected bounds (<=3 in absolute value); model fit is adequate.\n' %:%
         'WARNING: Min/max residual deviances are outside expected bounds (<=3 in absolute value); model fit is somewhat strained.\n')
  # [DIAGNOSTIC] median residual deviance close to 0 ?
  cat( (abs(d_aggr[3]) <= err_Median) %?% 
         paste0('SAFE: Median residual deviance (', comma(abs(d_aggr[3]), accuracy=0.01), ') is sufficiently close to zero; model fit is adequate.\n') %:%
         paste0('WARNING: Median residual deviance (', comma(abs(d_aggr[3]), accuracy=0.01), ') is not zero; model fit is somewhat strained.\n') )
  # [DIAGNOSTIC] 1st and 3rd percentile is relatively close to one another, indicating a symmetric distribution ?
  cat( (abs(d_aggr[2]) - abs(d_aggr[4]) <= err_quantiles) %?% 'SAFE: 1st/3rd quantiles of residual deviances are sufficiently close to each in absolute value; model fit is adequate.\n---------\n' %:%
         'WARNING: 1st/3rd quantiles of residual deviances differ substantially from each other in absolute value; model fit is somewhat strained.\n---------\n' )
  # [DIAGNOSTIC] Degree to which hat values (leverage scores) exceed a common rule of thumb (> 3 x mean(h))
  
  # -- 3. Graph distritubion of residuals
  if (plotResid) { hist(d, breaks="FD") }
  
  return(d_aggr)
  
  # -- cleanup (only relevant whilst debugging this function)
  rm(e,d,p_hat,y,r,rs,err_Median,err_quantiles,model,X,h,inf_degree_significance)
}
# - Unit test
# install.packages("ISLR"); require(ISLR)
# datTrain <- data.table(ISLR::Default); datTrain[, `:=`(default=as.factor(default), student=as.factor(student))]
# logit_model <- glm(default ~ student + balance + income, data=datTrain, family="binomial")
# summary(logit_model)
# resid_deviance_glm(logit_model)
### RESULTS: candidate's max residual > 3, which indicates some strain.
# distributional shape somewhat skew since abs(1st) != abs(3rd) quantiles


# --- Matthews Correlation Coefficient 
# This function gives the Matthews Correlation Coefficient, as calculated from the confusion matrix entries
# Input: 1) Actual values for a classifier problem in vector form, e.g., [1,1,0,1,0,0]
#        2) Corresponding probability scores for classier in vector form, e.g., [0.74, 0.92, 0.38, 0.53, 0.02, 0.6]
#        3) Cutoff value used on probability scores (Optional; default is 0.5)
# Output: Matthews Correlation Coefficient

Get_MCC<-function(Actual, Predicted, Cutoff=0.5){
  
  # - Safety Check for NA's
  if(anyNA(c(Actual,Predicted))){
    stop("Input fields are not allowed NA values, exiting...")
  }
  # - Safety Check for equal input length
  if(length(Actual)!=length(Predicted)){
    stop("Input fields are not of the same length, exiting...")
  }
  # - Safety Check for Actuals to be 0 or 1
  if(FALSE %in% (Actual==0|Actual==1)){
    stop("Actual should be in {0,1}, exiting...")
  }
  
  # - Obtain the confusion matrix entries
  TP<-0
  TN<-0
  FP<-0
  FN<-0
  
  for(k in 1:length(Actual)){
    if(Actual[k]==1){
      if(Predicted[k]<=Cutoff) {FN<-FN+1}
      else{TP<-TP+1}
    }
    else{
      if(Predicted[k]<=Cutoff) {TN<-TN+1}
      else{FP<-FP+1}
    }
  }
  
  # - Initialise the MCC, to store the result
  MCC<-NA
  
  # - Scenario when there is only one non-zero entry in the confusion matrix
  # - If TP = n then MCC=1, otherwise if TN=n then MCC=-1 (n is the sample size)
  if((TN == 0 & FN == 0 & FP == 0) | (TP == 0 & FN == 0 & FP == 0) | (TN == 0 & TP == 0 & FP == 0) | (TN == 0 & TP == 0 & FN == 0)) {
    MCC <- ifelse((TP != 0 | TN != 0), 1, -1)
    # - Scenario when there is zero rows or columns
  } else if ((FP+TN) == 0 | (TP+FN) == 0 |
             (TN+FN) == 0 | (TP+FP) == 0)
  {
    MCC <- 0
    # - Normal MCC Calculation
  } else {
    MCC <- (TP * TN - FP * FN) / (sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)))
  }
  # - Housekeeping
  rm(Actual, Predicted, Cutoff,TP ,TN ,FP ,FN)
  
  return(MCC)
}


# --- AUC By Date Function 
# - This function computes the AUC and its confidence interval for each unique date in the datatable.
### INPUT:
# - DataSet: A dataset in datatable format containing 1)Dates; 2) a Target Variable; 3) Probability Scores
# - DateName: Character String containing the name of the DATE column in the dataset
# - Target: Character String containing the name of the TARGET column in the dataset
# - Predictions: Character String containing the name of the PROBABILITY SCORES column in the dataset

### OUTPUT: 
# - A datatable which has columns --> Date: Unique Dates; AUC_Val: Estimated AUC values; AUC_LowerCI: Estimated 
#   lower CI values of the AUC values; AUC_UpperCI: Estimated upper CI values of the AUC values

AUC_overTime<-function(DataSet, DateName, Target, Predictions){
  # DataSet<-datCredit_smp
  # DateName<-"Date"
  # Target<- "DefaultStatus1_lead_12_max"
  # Predictions<- "prob_adv"
  
  # - Safety Check for missingness in predictions
  if(anyNA(DataSet[,list(get(DateName), get(Target), get(Predictions))])){
    stop("Missingness in data, Exit function...")
  }
  
  # - Get unique dates and create the data table to store results in
  UDates<-DataSet[!duplicated(get(DateName)),list(Date=get(DateName), AUC_Val=-99, AUC_LowerCI=-99, AUC_UpperCI=-99)]
  
  # - Looping over dates and obtaining AUC and CI
  counter<-1
  for(k in UDates$Date){
    TempDat<-DataSet[Date==k,list(target_var=get(Target), predicted_var=get(Predictions))]
    TempObj<-roc(response=TempDat$target_var, predictor=TempDat$predicted_var, ci=T,ci.method="delong", conf.level=0.95, trace=FALSE)
    UDates[counter, "AUC_Val"]<-TempObj$auc
    UDates[counter, "AUC_LowerCI"]<-TempObj$ci[1]
    UDates[counter, "AUC_UpperCI"]<-TempObj$ci[3]
    counter<-counter+1
  }
  # - Return the Dataset
  return(UDates[order(Date),])
}

# - Call AUC.Over.Time Function 
# AUC_overTime(datCredit_smp,"Date","DefaultStatus1_lead_12_max","prob_bas")


# # --- Degree of Missingness
# This function gives the number of NA values of each target/input field required to fit a formulated model
# Input: 1) Model_Formula - Formula of a model for which you want to test the input fields
#        2) DataSet - The dataset that you want to check the missingness on, could be the Full, Training, Validation or Test set
# Output: A data table displaying the amount of NA's present for each variable. The data table is sorted in decreasing missingness

Amt_Missing<-function(Model_Formula, DataSet){
  # - Safety Check if Model_Formula is of class "formula"
  if(class(Model_Formula)!="formula"){
    stop("Model_Formula should be of class formula.\n    Tip: use as.formula() function to create formula before fitting a model
      or alternatively use formula() on a saved model object, i.e., formula(Model). \n")
  }
  
  # - Obtain target and input variables from provided formula
  vInputs<-labels(terms(Model_Formula))
  
  # - Calculate the number of NA values per field
  DT<-data.table(InputV=vInputs,Missingness=sapply(1:NROW(vInputs),function(i,v,D){
    D[,sum(is.na(get(v[i])))]}
    ,v=vInputs,D=DataSet))
  
  # - Sort such that the variables with the most missingness are displayed on top
  DT<-DT[order(DT[,"Missingness"], decreasing=TRUE)]
  
  # - Cleanup
  rm(vInputs)
  
  # - Return results
  return(DT)
}

# Amt_Missing(inputs_adv1,datCredit_train)


# --- Wilcoxon Signed Rank Test
# - This function computes the WSR-Test to determine if two paired samples are statistically similar.
# - H0: Population median of differences between the two samples = 0
# - Ha: Population median of differences between the two samples does not equal 0

### INPUT:
# - Actuals: Actual event rate (time series)
# - Expected: Expected event rate (time series)
# - Alpha: Hypothesis test significance level

### OUTPUT: 
# - A list containing: 1) the test statistic; 2) the p-value of the hypothesis test; 3) test outcome

Wilcoxon_SR_Test<-function(Actuals, Expected, Alpha=0.05){
  # - Safety Check for NA's
  if(anyNA(c(Actuals, Expected))){
    stop("Input fields are not allowed NA values, exiting...")
  }
  # - Safety Check for equal input length
  if(length(Actuals)!=length(Expected)){
    stop("Input fields are not of the same length, exiting...")
  }
  
  # - Call R Wilcoxon Signed Rank Function
  WSR_Test <- wilcox.test(Actuals, Expected, alternative = "two.sided")
  
  # - Initialise result list
  WSR_OUTPUT <- list(test_stat=WSR_Test$statistic, p_val=WSR_Test$p.value, 
                     outcome=ifelse(WSR_Test$p.value<=Alpha, paste0("WSR-Test: rejected"), 
                                    paste0("WSR-Test: not rejected")))
  
  # - Return Output
  return(WSR_OUTPUT)
}


# -------- Kolmogorov-Smirnov Discrimination Test
# Conduct two-sample Kolmogorov-Smirnov test of score CDFs between class subpopulations
### INPUTS: 
# - class0: probability score distribution within class 0
# - class1: probability score distribution within class 1
### OUTPUTS: results from KS-test, along with interpretation using rules of thumb

KS_discimination <- function(class0, class1, alpha=0.05) {
  
  suppressWarnings(result <- ks.test(class0,class1))
  
  # - Decide on null hypothsis (that score distributions are identical)
  if (result$p.value <= alpha){
    hypoDecision <- "KS-test: null rejected"
  } else hypoDecision <- "KS-test: null not rejected"
  
  # - Interpret K (or D) statistic as maximum deviance between cumulative distributions of each class
  # Degree of discrimination/separateion between class
  if (result$statistic < 0.2) {
    KS_discrimation <- "poor"
  } else if (result$statistic < 0.4) {
    KS_discrimation <- "negligible"
  } else if (result$statistic < 0.7) {
    KS_discrimation <- "good"
  } else KS_discrimation <- "unrealistically excellent"
  
  cat(paste0("KS-statistic: ", result$statistic, "; signifying ", KS_discrimation, " discrimination between classes.\n"))
  
  # - Stitch together result set
  resultSet <- list(KS_statistic=result$statistic, KS_discrimation=KS_discrimation, KS_decision=hypoDecision)
  return( resultSet )
}




# -------- Information Measure Functions
# Calculates prevalences (actual + expected), Shannon entropy H(q), Cross-entropy H_q(p_1), 
# Kullback-Leibler (KL) Divergence D_q(p_1) and Jeffrey Divergence J(q,p_1) for a binary classifier

### INPUTS:
# - datGiven: given dataset
# - Target: Field name of target variable, i.e, actual class observations
# - TargetValue: Value in target variable that is considered as the main event
# - Prediction: Field name of prediction variable, i.e., probability score p_1(x) or dichotomised variant thereof
# - cutOff: Probability cut-off beyond which probability scores are classified into main event [TargetValue]
# - logBase: base of logarithm (default: 2) when calculating cross-entropy and divergences (KL + Jeffreys)
### OUTPUTS: a list of prevalences and divergence measures

divergences_binary <- function(datGiven, Target, TargetValue=1, Prediction, cutOff=0.5, logBase=2) {
  require(data.table)
  
  # ----- 0. Initialisation and checks
  
  # - ensure given target name does not coincide with the intended name used internally in this function
  # If not, then ensure the field doesn't already exist
  if (Target != "Target" & "Target" %in% colnames(datGiven)){
    datGiven[, Target := NULL]
  }
  
  # - ensure given Prediction name does not coincide with the intended name used internally in this function
  # If not, then ensure the field doesn't already exist
  if (Target != "Prediction" & "Prediction" %in% colnames(datGiven)){
    datGiven[, Prediction := NULL]
  }
  
  # - ensure given probability score name does not coincide with the intended name used internally in this function
  # If not, then ensure the field doesn't already exist
  if ("Prediction_score" %in% colnames(datGiven)){
    datGiven[, Prediction_score := NULL]
  }
  
  # - ensure target variable is numeric (and not factor)
  if (class(datGiven[,get(Target)]) == "factor") {
    datGiven[, Target := as.numeric(levels(get(Target)))[get(Target)]]
  } else datGiven[, Target := get(Target)]
  
  # - ensure prediction variable is numeric (and not factor)
  if (class(datGiven[,get(Prediction)]) == "factor") {
    datGiven[, Prediction := as.numeric(levels(get(Prediction)))[get(Prediction)]]
  } else datGiven[, Prediction := get(Prediction)]
  
  # - enumerate outcome space
  q_outcomes <- unique(datGiven$Target)
  
  # - ensure outcome space is only binary
  if (length(q_outcomes) > 2) {
    stop("Given classifier is polychotomous and not binary as required")
  }
  
  # - find class 0 
  NonTargetVal <- q_outcomes[q_outcomes!=TargetValue]
  
  # - Are the predictions already probability scores or should we dichotomise?
  if (!isTRUE( all.equal( datGiven$Prediction, c(0,1)) )) {
    cat("NOTE: Predictions are probability scores that can be dichotomised with the given cut-off: ",
        cutOff,"\n   However, expected prevalences and divergence measures are still calculated using the mean of these scores.\n")
    datGiven[, Prediction_score := Prediction]
    datGiven[, Prediction := ifelse(Prediction_score > cutOff, TargetValue, NonTargetVal)]
    probScore_Ind <- T
  } else probScore_Ind <- F
  
  
  # ----- 1. Prevalences
  
  # - Prevalence q (Actual) in estimating the Prior class probability of Y
  q1 <- sum(datGiven$Target==TargetValue) / datGiven[, .N]
  
  # - Expected prevalence p_1(X) in estimating the Posterior class probability of Y given X
  if (probScore_Ind) {
    p1 <- mean(datGiven$Prediction_score, na.rm=T)
    p1_1 <- mean(datGiven[Target==TargetValue, Prediction_score], na.rm=T) # Class 1 prevalence
    p1_0 <- mean(datGiven[Target==NonTargetVal, Prediction_score], na.rm=T) # Class 0 prevalence
  } else p1 <- sum(datGiven$Prediction==TargetValue) / datGiven[, .N]
  
  
  # ----- 2. Divergence measures
  
  # - Shannon entropy of q
  H_qq <- -(q1 *log(q1,base=logBase) + (1-q1)*log(1-q1,base=logBase))
  # Alternative (gives same result)
  probs <- prop.table(table(q1)); -sum(probs * log(probs,base=logBase)); #print(-sum(probs * log(probs,base=logBase)) == H_qq)
  
  # - Binary Cross-Entropy (BCE)  of p relative to q | Discrete probability distributions
  H_qp_disc <- -1*(q1 *log(p1,base=logBase) + (1-q1)*log(1-p1,base=logBase))
  
  # - Binary Cross-Entropy (BCE) of p relative to q | Continuous probability distributions
  H_qp <- -mean( ifelse(datGiven$Target==TargetValue,1,0) * log(datGiven$Prediction_score,base=logBase) +
                   (1-ifelse(datGiven$Target==TargetValue,1,0)) * log(1-datGiven$Prediction_score,base=logBase), na.rm=T )
  
  # - Kullback-Leibler (KL) divergence of p relative to q | Discrete probability distributions
  D_qp <- q1*log(q1/p1,base=logBase) + (1-q1)*log((1-q1)/(1-p1),base=logBase)
  
  # - Jeffreys' J-divergence (information value) of p relative to q | Discrete probability distributions
  J_qp <- (q1-p1)*log(q1/p1,base=logBase) + ((1-q1)-(1-p1))*log((1-q1)/(1-p1),base=logBase)
  
  cat("Shannon entropy H(q): \t\t\t\t\t\t", H_qq, "\nCross-entropy of p (continouous) relative to q, H_q(p): \t", H_qp, 
      "\nCross-entropy of p (discrete) relative to q, H_q(p): \t\t", H_qp_disc, 
      "\nKullback-Leibler D-divergence of p to q, D_q(p): \t\t", D_qp, 
      "\nJeffreys J-divergence between q and p, J(q,p): \t\t\t", J_qp, "\n\n")
  
  
  # ----- 3. Prepare result set
  # Stitch together prevalences, based on given prediction (discrete or probabilistic)
  resultSet <- list(Prevalence_Actual=q1, Prevalence_Expected=p1)
  if (probScore_Ind) resultSet <- c(resultSet, list(Prevalence_Expected_1=p1_1, Prevalence_Expected_0=p1_0))
  
  # Stitch together the divergences
  resultSet <- c(resultSet, list(ShannonEntropy=H_qq, CrossEntropy=H_qp, CrossEntropy_discrete=H_qp_disc,
                                 KullbackLeibler_divergence=D_qp, Jeffrey_divergence=J_qp))
  return(resultSet)
}

# --- Unit test
#library(entropy) # for validating Shannon entropy
#library(Metrics) # for validating Cross-Entropy ("logloss")
#library(LaplacesDemon) # for validating Kullback-Leibler and Jeffrey divergences
#require(ISLR); 
# - Load data
#dat <- data.table(ISLR::Default)
# - Prepare data
#dat[, `:=`(default=ifelse(default=="No",0,1), student=as.factor(student), Ind=1:.N)]
# - Implement basic resampling scheme, stratified by default
#set.seed(1, kind="Mersenne-Twister")
#datTrain <- dat %>% group_by(default) %>% slice_sample(prop=0.7) %>% as.data.table()
#datValid <- subset(dat, !(Ind %in% datTrain$Ind)) %>% as.data.table()
# - Train model
#logit_model <- glm(default ~ student + balance + income, data=datTrain, family="binomial")
# - Score with model
#datValid[, prob_vals := predict(logit_model, newdata = datValid, type="response")]
#cutoff_basic <- Gen_Youd_Ind(logit_model, datValid, "default", a=10) # find cut-off using Generalised Youden Index
# - Dichotomise probability score into discrete prediction with found cut-off
#datValid[, Prediction := ifelse(prob_vals > cutoff_basic$cutoff, 1, 0)]
# -- Validating divergence/information measures against those from other R-packages
#divergences_binary(datValid, Target="default", Prediction="prob_vals", cutOff=cutoff_basic$cutoff, logBase=exp(1))
# Shannon Entropy
#entropy(prop.table(table(datValid$default)), unit="log")
# Binary Cross-Entropy (BCE) of p relative to q | Discrete & continuous probability distributions
#q1 <- sum(datValid$default==1) / datvalid[,.N]
#logLoss(c(q1,1-q1), c(mean(datValid$prob_vals), mean(1-datValid$prob_vals))) # Discrete
#logLoss(c(q1,1-q1), c(mean(datValid$Prediction), mean(1-datValid$Prediction))) # Discrete (dichotomised probability score, just out of interest)
#logLoss(datValid$default, datValid$prob_vals) # continuous
# Kullback-Leibler (KL) divergence of p relative to q | Discrete probability distributions
#(D_qp <- KLD(c(q1,1-q1), c(mean(datValid$prob_vals), mean(1-datValid$prob_vals) ), base=exp(1))$sum.KLD.px.py )
# Jeffreys' J-divergence (information value) of p relative to q | Discrete probability distributions
# NOTE: first calculate Kullback-Leibler (KL) divergence of q relative to p 
#D_pq <- KLD(c(mean(datValid$prob_vals), mean(1-datValid$prob_vals) ), c(q1,1-q1), base=exp(1))$sum.KLD.px.py
#D_pq + D_qp # Jeffrey's J-divergence
### RESULTS: All information & divergence measures successfully validated
# - Clean-up
#rm(cutoff_basic, dat, datTrain, datValid, logit_model, q1, D_qp, D_pq)




# -------- Cut-off Functions for logit models

# -- Generalised Youden Index Function
# - This function runs an optimisation procedure to find the Generalised Youden Index for a trained model.
### INPUT:
# - Trained_Model: the trained classifier for which you want to obtain the optimal cutoff p_c
# - Train_DataSet: The training dataset (in datatable format) which will be used to find p_c
# - Target: Character string containing the name of the target variable (target variable should be numeric 0/1)
# - a: The cost multiple (or ratio) of a false negative relative to a false positive
### OUTPUT: 
# - The output of the optimisation procedure; i.e., the optimal cut-off p_c and other information detailing whether the 
#   algorithm converged.

Gen_Youd_Ind<-function(Trained_Model, Train_DataSet, Target, a){
  # Trained_Model<-logitMod_Adv
  # Train_DataSet<-datCredit_train[960000:965000,]
  # Target<-"DefaultStatus1_lead_12_max"
  # a<-4
  
  require(data.table, DEoptimR)
  
  Train_DataSet <- copy(Train_DataSet) # reserve copy so that we do not change the object outside of this scope
  
  # - ensure given target name does not coincide with the intended name used internally in this function
  # If not, then ensure the field doesn't already exist
  if (Target != "Target" & "Target" %in% colnames(Train_DataSet)){
    Train_DataSet[, Target := NULL]
  }
  
  # - ensure target variable is numeric (and not factor)
  if (class(Train_DataSet[,get(Target)]) == "factor") {
    Train_DataSet[, Target := as.numeric(levels(get(Target)))[get(Target)]]
  } else Train_DataSet[, Target := get(Target)]
  
  # - Calculate Prevalence Rate q1
  q1 <- mean(Train_DataSet$Target,na.rm=TRUE)
  
  # - Objective Function to be minimized (negative the function to be maximized)
  GYI_a <- function(pc){
    Train_DataSet[, prob_vals := predict(Trained_Model, Train_DataSet, type="response")] # Obtain predicted probabilities for the model
    Train_DataSet[, class_vals := ifelse(prob_vals<=pc,0,1)] # Dichotomise the probability scores according to the cutoff pc
    
    # - Safety Check for missingness in predictions
    if(anyNA(Train_DataSet[,list(prob_vals,class_vals)])){
      stop("Missingness in predicted probabilities, Exit function...")
    }
    
    # - Calculate the True Positive Rate & True Negative Rate given pc
    TPR<-sum(Train_DataSet[,class_vals]==1 & Train_DataSet[,Target==1], na.rm=TRUE)/Train_DataSet[Target==1,.N]
    TNR<-sum(Train_DataSet[,class_vals]==0 & Train_DataSet[,Target==0], na.rm=TRUE)/Train_DataSet[Target==0,.N]
    
    # - Clean Up the Created Input Fields
    Train_DataSet[, prob_vals := NULL]
    Train_DataSet[, class_vals := NULL]
    
    # - The function to minimize
    -(TPR + (1-q1)/(a*q1)*TNR - 1)
  }
  # - Run Optimisation via a Differential Evolution algorithm
  results <- JDEoptim(lower=0, upper=1, fn=GYI_a)
  #optim(par=c(0,1), fn=GYI_a, lower=0, upper=1)
  
  return(list(cutoff=results$par, value=results$value, iterations=results$iter))
}
# # - Unit test
# require(ISLR); require(OptimalCutpoints); require(DEoptimR) # Robust Optimisation Tool		
# datTrain <- data.table(ISLR::Default); datTrain[, `:=`(default=ifelse(default=="No",0,1), student=as.factor(student))]
# datTrain[, default_fac := as.factor(default)]
# logit_model <- glm(default ~ student + balance + income, data=datTrain, family="binomial")
# # - optimal.cutpoints function from OptimalCutpoints
# datTrain[, prob_vals := predict(logit_model, type="response")]
# opti<-optimal.cutpoints(X = "prob_vals", status = "default", tag.healthy = 0, methods = "Youden", data = datTrain, ci.fit = FALSE, trace = FALSE, control = control.cutpoints(CFP=1, CFN=4, generalized.Youden=T))
# summary(opti) # Optimal Cut-off = 0.2127908; Optimal Criterion = 6.6734234
# # - Custom Gen_Youd_Ind function
#Gen_Youd_Ind(logit_model,datTrain,"default",4) # Optimal Cut-off = 0.2120438; Optimal Criterion = -6.673423
#Gen_Youd_Ind(logit_model,datTrain,"default_fac",4) # Optimal Cut-off = 0.2120438; Optimal Criterion = -6.673423








# ------------------------- MARKOV-RELATED FUNCTIONS ----------------------------------

# --- First-Order Time-Homogeneous Marcov Chain TPM
# -- Function for estimating the Markov-related transition matrix given a dataset
### INPUTS:
# - DataSetMC: A panel dataset containing the relevant columns such as MarkovStatus.
# - StateSpace: The state space of the Markov model to be estimated, e.g., StateSpace=c("Perf","Def","W_Off","Set")
# - Absorbing: A vector of length = length(StateSpace), indicating whether the respective states specified in
#              StateSpace are absorbing or not.
### OUTPUTS: The estimated first-order (discrete) time-homogeneous Marcov chain transition probability matrix

Markov_TPM<-function(DataSetMC, StateSpace, Absorbing){
  
  # - Temporarily format data
  matMarkovState <- as.matrix(pivot_wider(data=DataSetMC[order(LoanID),list(LoanID,Counter,MarkovStatus)], 
                                          id_cols=Counter:Counter, names_from=LoanID, values_from=MarkovStatus))[,-1]; gc()
  
  # - Create transition matrix
  p_TransMat <- matrix(nrow=NROW(StateSpace), ncol=NROW(StateSpace), 0)
  colnames(p_TransMat) <- StateSpace; rownames(p_TransMat) <- StateSpace
  
  # - Estimate transition matrix
  for (t in 1:(NROW(matMarkovState)-1)) {
    for (state.i in 1:NROW(StateSpace)) { # From state
      for (state.j in 1:NROW(StateSpace)) { # To state
        p_TransMat[state.i, state.j] <- p_TransMat[state.i, state.j] + # Obtain transition counts
          NROW(which(matMarkovState[t,] == StateSpace[state.i] & matMarkovState[t+1,] ==StateSpace[state.j]))   
      }
    }
  }
  
  # - Obtain the MLE estimate of the TPM
  for (state.i in 1:NROW(StateSpace)) p_TransMat[state.i, ] <- p_TransMat[state.i, ] / sum(p_TransMat[state.i, ])
  
  # - Enforce absorbing probabilities (simply because there won't be accounts moving out of those states by design)
  position_absorbing<-which(Absorbing)
  for(j in 1:NROW(StateSpace)){
    if(j %in% position_absorbing){
      p_TransMat[j,j]<-1
      p_TransMat[j,(1:NROW(StateSpace))[-j]] <- 0
    }
  }
  
  # - Return the TPM
  return(round(p_TransMat*100,5))
}

# Markov_TPM(DataSetMC=datCredit_real,StateSpace=c("Perf","Def","W_Off","Set"), Absorbing=c(FALSE,FALSE,TRUE,TRUE))



# --- Function for generating the state spell number
# -- This fuction tracks the state spell number on state level.
### INPUTS:
# - vMarkovStatus: The column in the datatable specifying the state that a loan is in.
### OUTPUTS: A vector of the state visit numbers to be added back to the datatable.

Calc_StateNum<-function(vMarkovStatus){
  # Initialize a vector to store the results
  state_counts <- rep(0,length(vMarkovStatus))
  
  # Initialize a list to track the counters for each unique state visited
  unique_states <- unique(vMarkovStatus)
  state_counters <- setNames(as.list(rep(0, length(unique_states))), unique_states)
  
  # Iterate over the rows of each LoanID
  for (i in 1:length(vMarkovStatus)) { # length of vMarkovStatus
    if(i==1){
      # Initial state
      state_counters[[vMarkovStatus[i]]]<-1
    } else {
      # If the state changed, increment the state's counter
      if (vMarkovStatus[i] != vMarkovStatus[i-1]) {
        state_counters[[vMarkovStatus[i]]] <- state_counters[[vMarkovStatus[i]]] + 1
      }}
    
    # Assign the state counter value to the current row
    state_counts[i] <- state_counters[[vMarkovStatus[i]]]
  }
  return(state_counts)
}

# --- Function for generating the state spell number
# -- This fuction is used to create a lagged spell age covariate. I.e., it looks
# at the age of the previous spell and assigns it as a covariate for prediction purposes.
### INPUTS:
# - vSpell_Counter: The counter variable that tracks the number of the spell.
# - vStateSpell_Age: The total age of the current spell that will be lagged.
### OUTPUTS: The lagged variant of spell age.
Lagged_Spell_Age<-function(vSpell_Counter,vStateSpell_Age){
  prev_age<-rep(0,length(vSpell_Counter)) # Initialise return vector
  for(i in 1:length(vSpell_Counter)){ # Iterate over given vectors
    if(vSpell_Counter[i]==1){
      prev_age[i]<-0
    }else{
      if(vSpell_Counter[i]==vSpell_Counter[i-1]){
        prev_age[i]<-prev_age[i-1]
      }else{prev_age[i]<-vStateSpell_Age[i-1]}
    }
  }
  return(prev_age)  
}

