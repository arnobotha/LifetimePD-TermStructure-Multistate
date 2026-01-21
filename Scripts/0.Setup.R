# =================================== SETUP =============================================
# Setting up R environment, parameters, and function definitions
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Default risk term-structure modelling using Markov-models
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Marcel Muller (MM), Roland Breedt (RB)

# DESCRIPTION: 
# This script installs and loads various libraries and packages, compiles all
# custom functions, and set requisite parameters.
# ---------------------------------------------------------------------------------------
# -- Inputs:
#   - DelinqM.R | Delinquency measures and related functions
#   - TruEnd.R | TruEnd-procedure and related functions
#   - 0a.CustomFunctions | Multi-project library of custom & curated functions
# =======================================================================================
### NOTE: This script originates predominantly from another project called 'ClassifierDiagnostics'



# ================ 0. Library setup

# ------ Install and load packages
# - data access and big data management
require(haven) # for SAS imports
require(ETLUtils)
require(ffbase)
require(ff)
tempPath <- "C:/TempData"; options("fftempdir"=tempPath)

# for data wrangling
require(tidyr)
require(dplyr)
require(data.table)
require(lubridate)
require(readr)
require(bit64) # for very big numeric values
require(foreach); require(doParallel) # for multi-threaded computing
require(stringr) # common string operations, e.g, str_pad
require(purrr) # mapping functions from tidyverse in working with matrices, lists
require(writexl) #for exporting to Excel
require(zoo)

# for analyses & modelling
require(Hmisc)
require(survival) # for survival modelling
require(pROC); require(ROCR) # both for conducting ROC-analyses
require(ModelMetrics) # getting a confusion matrix
require(DEoptimR) # Robust Optimisation Tool									
require(betareg) # Beta regression package
require(nnet) # for multinomial logistic regression
require(MASS) # for likelihood ratio tests dropterm() appropriate for MLR-models
require(moments) # for residual analysis
require(splines) # for fitting regression models with splines

# for graphics
require(ggplot2)
require(corrplot) # For correlation plots
require(scales)
require(ggthemes)
require(ggpp) # Extensions to ggplot2, particularly geom_table
require(RColorBrewer)
require(extrafont) #remotes::install_version("Rttf2pt1", version = "1.3.8"); Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.55.0/bin/gswin32c.exe"); font_import(); loadfonts(); loadfonts(device="win")
require(survminer)
require(gridExtra)



# ================ 1. Parametrisation

# - general R options
options(scipen=999) # Suppress showing scientific notation

# - Parameters used in calculating delinquency measures
sc.Thres <- 0.9; # repayment ratio - g1
d <- 3 # default threshold for g0/g1-measures of delinquency (payments in arrears)
k <- 6 # Probation period


# -- Path variables | User-dependent

if (Sys.getenv("USERNAME") == "arnos") { # Dr Arno Botha | Kralkatorrik-machine
  # - Common path for saving large R-objects as back-up and/or as reusable checkpoints
  genPath <- "E:/DataDump/RetailMortgages-FNB/LifetimePD-TermStructure-Multistate_Data/"
  # - Common path from which raw big datasets are imported
  genRawPath <- "E:/DataDump/RetailMortgages-FNB/"
  # - Common path for sourcing R-scripts in main codebase
  path_cust <- "E:/Backupz/Google Drive/WorkLife/Analytix/R&D Codebases/LifetimePD-TermStructure-Multistate/Scripts/"
  # - Common path for storing important (but small!) R-objects as back-up
  genObjPath <- "E:/Backupz/Google Drive/WorkLife/Analytix/R&D Codebases/LifetimePD-TermStructure-Multistate/Objects/"
  # - Common path for saving important analytics and figures
  genFigPath <- "E:/Backupz/Google Drive/WorkLife/Analytix/R&D Codebases/LifetimePD-TermStructure-Multistate/Figures/"
} else if (toupper(Sys.getenv("USERNAME")) == "R5668395") { # Roland Roland | Botha-machine
  # - Common path for saving large R-objects as back-up and/or as reusable checkpoints
  genPath <- "C:/Data/Classifier-Diagnostics_Data/"
  # - Common path from which raw big datasets are imported
  genRawPath <- "C:/Data/"
  # - Common path for sourcing R-scripts in main codebase
  path_cust <- "C:/Users/R5668395/Documents/GitHub Markov/LifetimePD-TermStructure-Multistate/Scripts/"
  # - Common path for storing important (but small!) R-objects as back-up
  genObjPath <- "C:/Users/R5668395/Documents/GitHub Markov/LifetimePD-TermStructure-Multistate/Objects/"
  # - Common path for saving important analytics and figures
  genFigPath <- "C:/Users/R5668395/Documents/GitHub Markov/LifetimePD-TermStructure-Multistate/Figures/"
} else {
  stop("User-specific paths not set for current user: ", Sys.getenv("USERNAME"), ". Please fix in Setup script (0.Setup.R) before continuing")
}



# ================ 2. Custom functions

# ------ Custom function definitions
# - Load all custom functions defined in a separate R-script
source(paste0(path_cust,"0a.CustomFunctions.R"))

# - Compile Delinquency Calculation Functions (CD, MD/DoD)
source(paste0(path_cust,'DelinqM.R'))

# - Compile the TruEnd-suite of evaluation (and auxiliary) functions
source(paste0(path_cust,'TruEnd.R'))
