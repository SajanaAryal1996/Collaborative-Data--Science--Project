#' @title FACTOR- a complete package for Exploratory and Confirmatory Factor Analysis
#' @param data the dataset you would like to conduct analysis on, must be stored as a .csv file
#' @param data_input the dataset you would like to conduct analysis on (like the 'data' param), must be stored as a .csv file
#' @param n the number of factors desired for Exploratory Factor Analysis (EFA); standard value for this is 1, but customize as needed
#' @param model_input the model used for comparison in Confirmatory Factor Analysis (CFA)
#' @param group_name title you would like to give to the dataset that you are analyzing, must be entered as a character string, in quotation marks (i.e. "Normal Hearing Data")
#' @param model_1 1st model input for comparison fit
#' @param model_2 2nd model input for comparison fit
#' @param test which test you would like to use for determining whether a dataset is suitable for factor analysis, must be entered as a character string inside quotation marks (i.e. "KMO", "Kaiser, Meyer, Olkin", "Kaiser", "Meyer", "Olkin")
#' @keywords factor analysis, cronbach's alpha
#' @export 
#' @examples: get_loadings(NH_data, 1, "Normal Hearing Data")


get_loadings <- function(data, n, group_name) {
  numeric_data <- data[sapply(data, is.numeric)]
  factor_analysis <- psych::fa(numeric_data, nfactors = n, rotate = "varimax")
  factor_loadings <- factor_analysis$loadings
  print(paste("Factor Loadings for", group_name))
  print(factor_loadings)
}

get_alpha <- function(data, n, group_name) {
  numeric_data <- data[sapply(data, is.numeric)]
  factor_analysis <- psych::fa(numeric_data, nfactors = n, rotate = "varimax")
  factor_loadings <- factor_analysis$loadings
  cronbach_alpha <- psych::alpha(numeric_data)$total$raw_alpha
  print(paste("Cronbach's Alpha for", group_name, ":", cronbach_alpha))
}

get_loadings_model <- function(model_input, data_input, group_name) {
  model <- paste(model_input)
  fit <- lavaan::cfa(model, data = data_input, std.lv = TRUE)
  print(paste("Standardized Factor Loadings for", group_name))
  print(lavaan::inspect(fit, "std")$lambda)
}

get_alpha_model <- function(model_input, data_input, group_name) {
  model <- paste(model_input)
  fit <- lavaan::cfa(model, data = data_input, std.lv = TRUE)
  factor_loadings <- lavaan::inspect(fit, "std")$lambda
  cronbach_alpha <- psych::alpha(data_input)$total$raw_alpha
  print(paste("Cronbach's Alpha for", group_name, ":", cronbach_alpha))
}



handy_fit <- function(model_input, data_input) {
  model <- paste(model_input)
  fit <- lavaan::cfa(model, data = data_input, std.lv = TRUE)
  print(lavaan::fitMeasures(fit, c("chisq", "df", "pvalue","cfi", "tli","rmsea", "srmr")))
}

compare_fit <- function(model_1, model_2, data) {
  fit_1 <- lavaan::cfa(model_1, data, std.lv = TRUE)
  fit_2 <- lavaan::cfa(model_2, data, std.lv = TRUE)
  print(lavaan::anova(fit_1, fit_2))
}


pick_check <- function(test, data) {
  if (test %in% c("KMO", "Kaiser, Meyer, Olkin", "Kaiser", "Meyer", "Olkin"))
    return(performance::check_factorstructure(data)$KMO)
  if (test %in% c("Bartlett", "Bartlett's"))
    return(performance::check_factorstructure(data)$sphericity)
  else
    return("Check typo")
}



