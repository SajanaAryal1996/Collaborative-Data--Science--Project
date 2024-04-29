#’@FACTOR
#’@does exploratory factor analysis and confirmatory factor analysis for two group and calculate cronbach alpha value
#’@param - Factor analysis is used for dimension reduction
#’@keywords - Aplha, cronbach, factor
#’ @export - makes the function available for others to use when your package is loaded
#’@examples - sample code

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



