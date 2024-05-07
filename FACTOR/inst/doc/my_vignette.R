## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#exploratory factor analysis
# Load required library, for exploratory factor analysis psych library is required (psych for both cronbach's alpha and EFA)
new_pkg <- c("tidyverse", "dplyr", "psych", "lavaan")
not_installed <- new_pkg[!new_pkg %in% rownames(installed.packages())]
if (length(not_installed) > 0) install.packages(not_installed) 
library(psych)
library(dplyr)
library(tidyverse)

## -----------------------------------------------------------------------------
# Load data for normal hearing individuals and hearing impaired population
normal_hearing_data <- read.csv("NH_DATA.csv")
hearing_impaired_data <- read.csv("HI_DATA.csv")
usethis::use_data(normal_hearing_data, overwrite = TRUE) #creates .rda file for the data
usethis::use_data(hearing_impaired_data, overwrite = TRUE)
#Clear empty observations. After this process the observations of two data frames matches the numbers of participants (N = 50(HI), N = 70(NH))
normal_hearing_data <- normal_hearing_data %>%
  select(-X) %>%
  drop_na(Age)
hearing_impaired_data <- hearing_impaired_data %>%
  select(-X) %>%
  drop_na(Age)

# Some "visualization" of the data
str(normal_hearing_data)
str(hearing_impaired_data)
head(normal_hearing_data)
head(hearing_impaired_data)

## -----------------------------------------------------------------------------
# Define the questionnaire items for each group based on the column names
questionnaire_items <- c("E1", "E2", "S3", "E4", "S5", "S6", "E7", "S8", "E9", "S10")

# Define a function to calculate Cronbach's alpha and factor loadings for each group
calculate_alpha_and_loadings <- function(data, group_name) {
  # Select numeric variables
  numeric_data <- data[sapply(data, is.numeric)]
  
  # Perform factor analysis
  factor_analysis <- fa(numeric_data, nfactors = 1, rotate = "varimax")
  
  # Extract factor loadings
  factor_loadings <- factor_analysis$loadings
  
  # Print factor loadings
  print(paste("Factor Loadings for", group_name))
  print(factor_loadings)
  
  # Calculate Cronbach's alpha for all questionnaire items
  cronbach_alpha <- psych::alpha(numeric_data)$total$raw_alpha
  
  # Print Cronbach's alpha
  print(paste("Cronbach's Alpha for", group_name, ":", cronbach_alpha))
}

# Calculate Cronbach's alpha and factor loadings for normal hearing group
calculate_alpha_and_loadings(normal_hearing_data[questionnaire_items], "Normal Hearing")

# Calculate Cronbach's alpha and factor loadings for hearing impaired group
calculate_alpha_and_loadings(hearing_impaired_data[questionnaire_items], "Hearing Impaired")


## -----------------------------------------------------------------------------
#Perform confirmatory factor analysis
#for CFA and Cronbach's alpha, both lavaan and psych are required (Cronbach's alpha  required psych)
# Load required libraries
library(lavaan)
library(psych)

# Load data for normal hearing individuals and hearing impaired population
normal_hearing_data <- read.csv("NH_DATA.csv")
hearing_impaired_data <- read.csv("HI_DATA.csv")

#Clear empty observations. After this process the observations of two data frames matches the numbers of participants (N = 50(HI), N = 70(NH))
normal_hearing_data <- normal_hearing_data %>%
  select(-X) %>%
  drop_na()
hearing_impaired_data <- hearing_impaired_data %>%
  select(-X) %>%
  drop_na()

# Define the questionnaire items for each group based on the column names
questionnaire_items <- c("E1", "E2", "S3", "E4", "S5", "S6", "E7", "S8", "E9", "S10")

# Define a function to perform confirmatory factor analysis (CFA) and calculate Cronbach's alpha and factor loadings for each group
perform_CFA <- function(data, group_name) {
  # Define the CFA model
  model <- '
    # Define latent variable
    factor =~ E1 + E2 + S3 + E4 + S5 + S6 + E7 + S8 + E9 + S10
  '
  
  # Perform CFA
  fit <- lavaan::cfa(model, data = data, std.lv = TRUE)
  print(fit)
  # Print standardized factor loadings
  print(paste("Standardized Factor Loadings for", group_name))
  print(inspect(fit, "std")$lambda)
  
  # Extract standardized factor loadings
  factor_loadings <- inspect(fit, "std")$lambda
  
  # Calculate Cronbach's alpha for all questionnaire items
  cronbach_alpha <- psych::alpha(data)$total$raw_alpha
  
  # Print Cronbach's alpha
  print(paste("Cronbach's Alpha for", group_name, ":", cronbach_alpha))
}

# Perform confirmatory factor analysis and Cronbach's alpha for normal hearing group
perform_CFA(normal_hearing_data[questionnaire_items], "Normal Hearing")

# Perform confirmatory factor analysis and Cronbach's alpha for hearing impaired group
perform_CFA(hearing_impaired_data[questionnaire_items], "Hearing Impaired")


## ----setup--------------------------------------------------------------------
library(FACTOR)
library(dplyr)
library(tidyverse)

## -----------------------------------------------------------------------------
pick_check("KMO", hearing_impaired_data[questionnaire_items])

## -----------------------------------------------------------------------------
pick_check("Bartlett", normal_hearing_data[questionnaire_items])

## -----------------------------------------------------------------------------
normal_hearing_data <- read.csv("NH_DATA.csv")
hearing_impaired_data <- read.csv("HI_DATA.csv")
normal_hearing_data <- normal_hearing_data %>%
  select(-X) %>%
  drop_na()
hearing_impaired_data <- hearing_impaired_data %>%
  select(-X) %>%
  drop_na()

## ----warning=FALSE------------------------------------------------------------
questionnaire_items <- c("E1","E2","S3","E4","S5","S6","E7","S8", "E9", "S10")

## -----------------------------------------------------------------------------
model <- 
   " # Define latent variable
    factor =~ E1 + E2 + S3 + E4 + S5 + S6 + E7 + S8 + E9 + S10
    "
get_loadings_model(model, normal_hearing_data[questionnaire_items], "Normal Hearing")
get_loadings_model(model, hearing_impaired_data[questionnaire_items], "Hearing Impaired")

## ----warning=FALSE------------------------------------------------------------
model <- 
   " # Define latent variable
    factor =~ E1 + E2 + S3 + E4 + S5 + S6 + E7 + S8 + E9 + S10
    "
get_alpha_model(model, normal_hearing_data[questionnaire_items], "Normal Hearing")
get_alpha_model(model, hearing_impaired_data[questionnaire_items], "Hearing Impaired")

## -----------------------------------------------------------------------------
model <- 
   " # Define latent variable
    factor =~ E1 + E2 + S3 + E4 + S5 + S6 + E7 + S8 + E9 + S10
    "
handy_fit(model, normal_hearing_data[questionnaire_items])
handy_fit(model, hearing_impaired_data[questionnaire_items])

## -----------------------------------------------------------------------------
get_loadings(normal_hearing_data[questionnaire_items], 1, "Normal Hearing")
get_loadings(hearing_impaired_data[questionnaire_items], 1, "Hearing Impaired")

get_alpha(normal_hearing_data[questionnaire_items], 1, "Normal Hearing")
get_alpha(hearing_impaired_data[questionnaire_items], 1, "Hearing Imapired")

## -----------------------------------------------------------------------------
model_test <- "
    # Define latent variable
factor1 =~ E1 + E2 + S3 + E4 + S5 + S6 + E7 + S8
factor2 =~ E9 + S10
  "

## -----------------------------------------------------------------------------
compare_fit(model, model_test, hearing_impaired_data[questionnaire_items])

