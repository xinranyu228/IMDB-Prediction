required_packages = c("ggplot2", "e1071", "car", "sandwich", "dplyr", "tidyr", "readr", "corrplot", "lmtest", "modelr", "splines", "glue", "boot", 'methods', 'caTools')

for (package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    if (require(package, character.only = TRUE)) {
      cat(paste(package, "has been installed and loaded.\n"))
    } else {
      cat(paste("Failed to install and load", package, "\n"))
    }
  } else {
    cat(paste(package, "is already loaded.\n"))
  }
}



IMDB_data = read.csv("/Users/lauray/Documents/学习/MMA/Fall 2023/MGSC661/mid/IMDB_data_Fall_2023.csv")
test_df = read.csv("/Users/lauray/Documents/学习/MMA/Fall 2023/MGSC661/mid/test_data_IMDB_Fall_2023.csv")
attach(IMDB_data)

# Change month to number
month_mapping = c("Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6, 
                "Jul" = 7, "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12)

IMDB_data$release_month =month_mapping[IMDB_data$release_month]
attach(test_df)
test_df$release_month =month_mapping[test_df$release_month)]
attach(IMDB_data)
table(IMDB_data$release_month)

#DUMMIFY MATURING RATING WITH X AS THE REFERENCE 

IMDB_data$maturity_rating = as.factor(IMDB_data$maturity_rating)

IMDB_data$maturity_rating = relevel(IMDB_data$maturity_rating, ref="X")

maturity_dummies = model.matrix(~maturity_rating-1, data=IMDB_data)

colnames(maturity_dummies) = gsub("maturity_rating", "maturity", colnames(maturity_dummies))

IMDB_data = cbind(IMDB_data, maturity_dummies[, -1])

IMDB_data$maturity_rating = NULL


## Add variables
# AVG previous director rate
director_rate_df = data.frame(director = unique(director), director_rate = NA)
for (i in 1:length(director_rate_df$director)) {
  score_list = numeric() 
  
  for (j in 1:length(IMDB_data$director)) {
    if (director_rate_df$director[i] == IMDB_data$director[j]) {
      score_list = c(score_list, IMDB_data$imdb_score[j])
    }
  }
  
  avg = mean(score_list, na.rm = TRUE) 
  director_rate_df$director_rate[i] = avg
}
IMDB_data = merge(IMDB_data, director_rate_df, by = "director", all.x = TRUE)
table(is.na(IMDB_data$director_rate))
test_df = merge(test_df, director_rate_df, by = "director", all.x = TRUE)
table(is.na(test_df$director_rate))
# too many NA, won't use

# AVG previous distributor rate
distributor_rate_df = data.frame(distributor = unique(distributor), distributor_rate = NA)
for (i in 1:length(distributor_rate_df$distributor)) {
  score_list = numeric() 
  
  for (j in 1:length(IMDB_data$distributor)) {
    if (distributor_rate_df$distributor[i] == IMDB_data$distributor[j]) {
      score_list = c(score_list, IMDB_data$imdb_score[j])
    }
  }
  
  avg = mean(score_list, na.rm = TRUE) 
  distributor_rate_df$distributor_rate[i] = avg
}
IMDB_data = merge(IMDB_data, distributor_rate_df, by = "distributor", all.x = TRUE)
table(is.na(IMDB_data$distributor_rate))
test_df = merge(test_df, distributor_rate_df, by = "distributor", all.x = TRUE)
table(is.na(test_df$distributor_rate))


# AVG previous cinematographer rate
cinematographer_rate_df = data.frame(cinematographer = unique(cinematographer), cinematographer_rate = NA)
for (i in 1:length(cinematographer_rate_df$cinematographer)) {
  score_list = numeric() 
  
  for (j in 1:length(IMDB_data$cinematographer)) {
    if (cinematographer_rate_df$cinematographer[i] == IMDB_data$cinematographer[j]) {
      score_list = c(score_list, IMDB_data$imdb_score[j])
    }
  }
  
  avg = mean(score_list, na.rm = TRUE) 
  cinematographer_rate_df$cinematographer_rate[i] = avg
}
IMDB_data = merge(IMDB_data, cinematographer_rate_df, by = "cinematographer", all.x = TRUE)
table(is.na(IMDB_data$cinematographer_rate))
test_df = merge(test_df, cinematographer_rate_df, by = "cinematographer", all.x = TRUE)
table(is.na(test_df$cinematographer_rate))

# AVG previous production_company rate
production_company_rate_df = data.frame(production_company = unique(production_company), production_company_rate = NA)
for (i in 1:length(production_company_rate_df$production_company)) {
  score_list = numeric() 
  
  for (j in 1:length(IMDB_data$production_company)) {
    if (production_company_rate_df$production_company[i] == IMDB_data$production_company[j]) {
      score_list = c(score_list, IMDB_data$imdb_score[j])
    }
  }
  
  avg = mean(score_list, na.rm = TRUE) 
  production_company_rate_df$production_company_rate[i] = avg
}
IMDB_data = merge(IMDB_data, production_company_rate_df, by = "production_company", all.x = TRUE)
table(is.na(IMDB_data$production_company_rate))
test_df = merge(test_df, production_company_rate_df, by = "production_company", all.x = TRUE)
table(is.na(test_df$production_company_rate))
# too many NA, won't use

attach(IMDB_data)

new_predictors = c("imdb_score", "director_rate", "distributor_rate", "cinematographer_rate","production_company_rate")
# Calculate correlation matrix
cor_matrix = cor(IMDB_data[new_predictors], use="pairwise.complete.obs")

print(cor_matrix)

options(repr.plot.width=20, repr.plot.height=20)

corrplot(cor_matrix, method="color", type="upper", order="hclust", 
         addCoef.col = "black", # Add correlation coefficients on the plot
         tl.col="black", tl.srt=45, # Text label color and rotation
         tl.cex = 0.6, # Text label size
         cl.cex = 0.9) # Color legend size

attach(IMDB_data)

variables = c('imdb_score', 'movie_budget', 'release_year','release_month', 'duration', 'nb_news_articles', 'actor1_star_meter', 'actor2_star_meter', 'actor3_star_meter', 'nb_faces', 'movie_meter_IMDBpro','action','adventure','scifi','thriller','musical','romance','western','sport','horror','drama','war','animation','crime',  'distributor_rate')

# Loop through each variable and create histogram and boxplot
for (variable in variables){
  # Calculate binwidth using the Freedman-Diaconis rule
  n = length(IMDB_data[[variable]])
  IQR = IQR(IMDB_data[[variable]], na.rm = TRUE)
  binwidth = 2 * IQR * (n^(-1/3))
  
  if (binwidth == 0) {
    binwidth = 30  # Or some other sensible default
  }
  
  # Histogram
  histogram_plot = ggplot(IMDB_data, aes_string(x=variable)) + 
    geom_histogram(binwidth=binwidth, fill='skyblue', color='black') +
    theme_minimal() +
    labs(title=paste('Histogram of', variable), x=variable, y='Frequency')
  
  # Boxplot
  boxplot_plot = ggplot(IMDB_data, aes_string(y=variable)) + 
    geom_boxplot(fill='skyblue', color='black') +
    theme_minimal() +
    labs(title=paste('Boxplot of', variable), y=variable)
  
  print(histogram_plot)
  print(boxplot_plot)
}

# Loop through each variable and calculate skewness
for(variable in variables) {
  print(paste(variable, "Skewness: ", skewness(IMDB_data[[variable]])))
}

IMDB_data_sub = IMDB_data[variables]

cor_matrix = cor(IMDB_data_sub, use="pairwise.complete.obs")

print(cor_matrix)

options(repr.plot.width=20, repr.plot.height=20)

corrplot(cor_matrix, method="color", type="upper", order="hclust", 
         addCoef.col = "black", # Add correlation coefficients on the plot
         tl.col="black", tl.srt=45, # Text label color and rotation
         tl.cex = 0.6, # Text label size
         cl.cex = 0.9) # Color legend size



# Outliers ----------------------------------------------------------------
numeric_sub = c('imdb_score', 'movie_budget', 'release_year','release_month', 'duration', 
                'nb_news_articles',  'actor1_star_meter', 'actor2_star_meter', 
                'actor3_star_meter', 'nb_faces', 'movie_meter_IMDBpro', 'distributor_rate')
IMDB_data_num_sub = IMDB_data[numeric_sub]

# Loop through each variable and print outliers
for(variable in numeric_sub) {
  bp = boxplot.stats(IMDB_data[[variable]])$out
  print(paste(variable, "Outliers: ", bp))
}

#The results for this don't make sense 

#OR

# Fit a multiple linear regression model
# 'imdb_score' is the response variable and all other variables are predictors (Dummies not included)

attach(IMDB_data)
model = lm(imdb_score ~ movie_budget + release_year + release_month + duration + nb_news_articles + actor1_star_meter + actor2_star_meter + actor3_star_meter + nb_faces + distributor_rate, data = IMDB_data)
qqPlot(model, envelope=list(style="none")) #[1] 316 492

outlier_test_result = outlierTest(model)

print(outlier_test_result)

outlier_row_indices = c(905,1619,1854,890,1354,39,1149)

outliers = IMDB_data[outlier_row_indices, ]

print("Outliers:")
print(outliers)


# Collinearity ------------------------------------------------------------

model = lm(imdb_score ~ ., data = IMDB_data_sub)

vif_values = vif(model)

print(vif_values)
# no collinearity

vif_df = data.frame(Variable = names(vif_values),
                     VIF = unname(vif_values))

library(knitr)
kable(vif_df, 
      caption = "Variance Inflation Factors",
      col.names = c("Variable", "VIF"))


#################PART 3 - EXPLORE VARIABLE RELATIONSHIPS#####################

#Examine the correlation coefficient between Y and each predictor, xi
Y = IMDB_data$imdb_score

variables = c('imdb_score', 'movie_budget', 'release_year','release_month', 'duration', 'nb_news_articles', 
               'actor1_star_meter', 'actor2_star_meter', 'actor3_star_meter',
               'nb_faces', 'movie_meter_IMDBpro','action','adventure','scifi','thriller','musical','romance','western','sport','horror','drama','war','animation','crime','maturityApproved','maturityG','maturityGP','maturityM','maturityPassed','maturityPG','maturityPG-13','maturityR', 'distributor_rate')
# Loop over each numeric predictor xi
for(xi in variables[-which(variables=="imdb_score")]){
  # Print correlation coefficient between Y and xi
  corr_coeff = cor(IMDB_data[[xi]], Y, use = "complete.obs")
  print(paste("Correlation coefficient between Y and", xi, ":", corr_coeff))
  # Determine if correlation is positive or negative
  if(corr_coeff > 0){print(paste("Correlation between Y and", xi, "is positive."))} else {print(paste("Correlation between Y and", xi, "is negative."))}
  # Determine if correlation is strong or weak
  if(abs(corr_coeff) > 0.5){print(paste("Correlation between Y and", xi, "is strong."))} else {print(paste("Correlation between Y and", xi, "is weak."))}

}

# strong correlation with Y: distributor_rate

for(xi in variables[-which(variables=="imdb_score")]){
  xi_sym = rlang::sym(xi)
  
  scatter_plot = ggplot(IMDB_data, aes(x = !!xi_sym, y = imdb_score)) + 
    geom_point() +
    labs(title = paste("Scatter plot of Y against", xi),
         x = xi, 
         y = "imdb_score") +
    theme_minimal()
    print(scatter_plot)
}

# Heteroskedasticity ------------------------------------------------------

#NCV TEST 
heteroskedastic_predictors = c()
non_heteroskedastic_predictors = c()

for (xi in variables) {
  
  if (xi %in% colnames(IMDB_data)) {
    
    model = lm(as.formula(paste("imdb_score ~", xi)), data = IMDB_data)
    
    print(paste("Non-Constant Variance test for", xi))
    
    plot(model$fitted.values, residuals(model), main = paste("Residuals vs Fitted values for", xi))
    # Perform the Non-Constant Variance Test
    ncv_result = ncvTest(model)
    print(ncv_result)
    # If the p-value of the ncvTest is below 0.05, flag the predictor as potentially heteroskedastic
    if (ncv_result$p < 0.05) {
      heteroskedastic_predictors = c(heteroskedastic_predictors, xi)
    } else {
      non_heteroskedastic_predictors = c(non_heteroskedastic_predictors, xi)
    }
    print(coeftest(model, vcov = vcovHC(model, type = "HC1")))
  } else {
    print(paste("Variable", xi, "not found in the data."))
  }
}


print(paste("The following predictors were flagged as potentially heteroskedastic:", paste(heteroskedastic_predictors, collapse = ", ")))

print(paste("The following predictors were NOT flagged as potentially heteroskedastic:", paste(non_heteroskedastic_predictors, collapse = ", ")))

'''
> print(paste("The following predictors were flagged as potentially heteroskedastic:", paste(heteroskedastic_predictors, collapse = ", ")))
[1] "The following predictors were flagged as potentially heteroskedastic: movie_budget, duration, nb_news_articles, actor3_star_meter, movie_meter_IMDBpro, adventure, scifi, thriller, romance, drama, war, crime, distributor_rate"
> print(paste("The following predictors were NOT flagged as potentially heteroskedastic:", paste(non_heteroskedastic_predictors, collapse = ", ")))
[1] "The following predictors were NOT flagged as potentially heteroskedastic: release_year, release_month, actor1_star_meter, actor2_star_meter, nb_faces, action, musical, western, sport, horror, animation"
'''

#fix the HETEROSKEDASTIC PREDICTORS 

all_predictors = list()

for (xi in non_heteroskedastic_predictors) {
  all_predictors[[xi]] = "Non-Heteroskedastic"
}

for (xi in heteroskedastic_predictors) {
  
  model = lm(as.formula(paste("imdb_score ~", xi)), data = IMDB_data)
  
  robust_se = sqrt(diag(vcovHC(model)))
  
  coefs = coeftest(model, vcov = vcovHC(model, type = "HC1"))
  
  result = list(
    predictor = xi,
    robust_standard_errors = robust_se,
    coefficient_estimates = coefs
  )
  
  # Add the result to the list of all predictors
  all_predictors[[xi]] = result
}

print(all_predictors)

for (xi in heteroskedastic_predictors) {
  
  model = lm(as.formula(paste("imdb_score ~", xi)), data = IMDB_data)
  
  robust_se = sqrt(diag(vcovHC(model)))
  
  print(paste("Robust standard errors for", xi))
  print(robust_se)
  
  print(paste("Coefficient estimates with robust standard errors for", xi))
  coefs = coeftest(model, vcov = vcovHC(model, type = "HC1"))
  print(coefs)
}


#Run simple linear regressions between Y and each predictor xi.

# Create empty data frames to store rsq and p-values
importance_dataset = data.frame(var=character(), rsq=numeric(), pvalue=numeric(), degree=integer())

for (var in variables) {
  model = lm(paste('imdb_score ~', var), data = IMDB_data)
  rsquared = summary(model)$r.squared
  p_value = summary(model)$coefficients[2, 4]
  print(paste('Regression results for', var, ':'))
  print(summary(model))
  importance_dataset = rbind(importance_dataset, data.frame(var = var, rsq = rsquared, pvalue = p_value))
  plot(IMDB_data[[var]], IMDB_data$imdb_score)
  abline(model, col="red")
  title(var)
}
# sort importance of each variable by r-squared in a descending order
importance_dataset = importance_dataset[order(-importance_dataset$rsq), ]

print("Importance:")
print(importance_dataset)

#Examine correlations between all predictors

# Subset the dataframe to keep only the variables of interest
df = IMDB_data[, variables]

# Calculate correlation matrix
cor_matrix = cor(df)

# Print the correlation matrix
print(cor_matrix)

# Check for multicollinearity (VIF: Variance Inflation Factors)
# Create a linear model (replace 'imdb_score ~ .' with your actual model formula if different)
model = lm(imdb_score ~ ., data = df)

vif_results = vif(model)
print(vif_results)
## no collinearity

# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle")

## no correlation

###################STEP 4 - TEST NON-LINEARITY AND FIT##################

predictors = c('imdb_score', 'movie_budget', 'release_year','release_month', 'duration', 'nb_news_articles', 
                'actor1_star_meter', 'actor2_star_meter', 'actor3_star_meter',
                'nb_faces', 'movie_meter_IMDBpro','action','adventure','scifi','thriller','musical','romance','western','sport','horror','drama','war','animation','crime','maturityApproved','maturityG','maturityGP','maturityM','maturityPassed','maturityPG','maturityPG-13','maturityR', 'distributor_rate')


# Minimum unique values 
min_unique = 5

attach(IMDB_data)
# Initialize results dataframe 
results_degree = data.frame(var=character(),
                      best_deg=integer(),
                      rsq_lin=numeric(),
                      rsq_quad=numeric(),  
                      rsq_cubic=numeric(),
                      rsq_4=numeric(),
                      rsq_5=numeric())

# Loop through predictors
for(var in predictors){
  
  # Get number of unique values
  n = length(unique(IMDB_data[[var]]))
  
  # Check minimum threshold
  if(n < min_unique){
    print(paste0(var, " has only ", n, " unique values. Skipping."))
    next
  }
  
  # Fit models
  lm_lin = lm(imdb_score ~ IMDB_data[[var]], data=IMDB_data)
  lm_quad = lm(imdb_score ~ poly(IMDB_data[[var]], 2), data=IMDB_data)
  lm_cubic = lm(imdb_score ~ poly(IMDB_data[[var]], 3), data=IMDB_data) 
  lm_4 = lm(imdb_score ~ poly(IMDB_data[[var]], 4), data=IMDB_data)
  lm_5 = lm(imdb_score ~ poly(IMDB_data[[var]], 5), data=IMDB_data)
  
  # ANOVA test 
  anova_result = anova(lm_lin, lm_quad, lm_cubic, lm_4, lm_5)
  
  significant_degrees = c(2, 3, 4, 5)
  best_degree = 1
  
  for (i in 1:length(significant_degrees)) {
    if (!is.na(anova_result$`Pr(>F)`[significant_degrees[i]]) && 
        anova_result$`Pr(>F)`[significant_degrees[i]] < 0.05) {
      best_degree = max(significant_degrees[i], best_degree)
    } else {
      break
    }
  }
  
  
  # Extract R-squared values
  rsq_lin = summary(lm_lin)$r.squared
  rsq_quad = summary(lm_quad)$r.squared
  rsq_cubic = summary(lm_cubic)$r.squared
  rsq_4 = summary(lm_4)$r.squared
  rsq_5 = summary(lm_5)$r.squared
  
  new_row = data.frame(var = var, 
                        best_deg = best_degree, 
                        rsq_lin = rsq_lin, 
                        rsq_quad = rsq_quad,  
                        rsq_cubic = rsq_cubic,
                        rsq_4 = rsq_4,
                        rsq_5 = rsq_5)

  results_degree = rbind(results_degree, new_row)

}

print(results_degree)

# List of predictors  
predictors = c('movie_budget', 'release_year','release_month', 'duration', 'nb_news_articles', 
               'actor1_star_meter', 'actor2_star_meter', 'actor3_star_meter', 
               'nb_faces', 'movie_meter_IMDBpro','action','adventure','scifi'
               ,'thriller','musical','romance','western','sport','horror','drama','war',
               'animation','crime','distributor_rate')
#Which one gives you a higher r-squared and out-of-sample performance
library(glue)

# Make sure predictors vector includes 'imdb_score'
predictors = c(predictors, 'imdb_score')

IMDB_data_subset = IMDB_data[predictors]

set.seed(123)
index = sample(1:nrow(IMDB_data_subset), size = 0.8*nrow(IMDB_data_subset))

train = IMDB_data_subset[index,]
test = IMDB_data_subset[-index,]

# Degrees to try 
degrees = 1:5

# Initialize dataframes
train_rsq = data.frame(predictor=character(), degree=integer(), rsq=numeric())
test_rsq = data.frame(predictor=character(), degree=integer(), rsq=numeric())

for(predictor in predictors){
  
  if(predictor != 'imdb_score'){
    for(deg in 1:min(length(unique(train[[predictor]]))-1, 5)){
      
      formula = glue("imdb_score ~ poly({predictor}, {deg})")
      
      formula = as.formula(formula)
      
      model = lm(formula, data = train)
      
      rsq_train = summary(model)$r.squared
      
      # Predict on test set and calculate R-squared
      pred = predict(model, newdata = test)
      ssr = sum((pred - mean(test$imdb_score))^2)
      sst = sum((test$imdb_score - mean(test$imdb_score))^2)
      rsq_test = ssr / sst
      
      # Store results
      train_rsq = rbind(train_rsq, data.frame(predictor, deg, rsq_train))
      test_rsq = rbind(test_rsq, data.frame(predictor, deg, rsq_test))
    }
  }
}

# Print results
print("Training R-squared:")
print(train_rsq)

print("Test R-squared:")
print(test_rsq)



#SPLINES 
# Install and load necessary packages
#install.packages("splines")
library(splines)
#install.packages("dplyr")
library(dplyr)
#install.packages("glue")
library(glue)

# List of predictors  
predictors2 = c('movie_budget', 'release_year', 'duration', 'nb_news_articles', 'actor1_star_meter', 'actor2_star_meter', 'actor3_star_meter','nb_faces', 'movie_meter_IMDBpro','release_month', 'distributor_rate')



# Initialize an empty data frame to store results
results_spline = data.frame()

# Loop through predictors
for(pred in predictors2) {
  # Build formula string
  formula = paste("imdb_score ~", pred)
  
  # Convert to formula object
  formula = as.formula(formula)
  
  # Linear model
  lm_linear = lm(formula, data = IMDB_data)
  
  # Spline models
  lm_spline3 = update(lm_linear, . ~ . + bs(IMDB_data[[pred]], df = 3))
  lm_spline5 = update(lm_linear, . ~ . + bs(IMDB_data[[pred]], df = 5))
  lm_spline7 = update(lm_linear, . ~ . + bs(IMDB_data[[pred]], df = 7))
  
  # Print the number of knots for each spline model
  print(paste("Number of knots for", pred, "with df = 3:", length(attr(bs(IMDB_data[[pred]], df = 3), "knots"))))
  print(paste("Number of knots for", pred, "with df = 5:", length(attr(bs(IMDB_data[[pred]], df = 5), "knots"))))
  print(paste("Number of knots for", pred, "with df = 7:", length(attr(bs(IMDB_data[[pred]], df = 7), "knots"))))
  
  # Compare models
  anova_result = anova(lm_linear, lm_spline3, lm_spline5, lm_spline7)
  
  
  # Check spline improvement
  if(all(!is.na(anova_result$`Pr(>F)`[2:4])) && any(anova_result$`Pr(>F)`[2:4] < 0.05)) {
    spline_improved = TRUE
    best_df = which.min(anova_result$`Pr(>F)`[2:4]) + 2
  } else {
    spline_improved = FALSE
    best_df = NA
  }
  # Extract R-squared
  rsq_linear = summary(lm_linear)$r.squared
  rsq_spline3 = summary(lm_spline3)$r.squared
  rsq_spline5 = summary(lm_spline5)$r.squared
  rsq_spline7 = summary(lm_spline7)$r.squared
  
  # Store results
  results_spline = rbind(results_spline, 
                   data.frame(pred, spline_improved, best_df, rsq_linear, rsq_spline3, rsq_spline5, rsq_spline7))
  
}

print(results_spline)



################# 5 - Feature Selection #####################
# Using r-squared and p-value to conduct feature selection
# Match the degree and importance
for (i in 1:nrow(importance_dataset)) {
  match_found =FALSE
  for (j in 1:nrow(results_degree)) {
    if (results_degree$var[j] == importance_dataset$var[i]) {
      importance_dataset$degree[i] =results_degree$best_deg[j]
      match_found =TRUE
    }
  }
  if (!match_found) {
    importance_dataset$degree[i] = 1
  }
}

# Print the updated importance_dataset with assigned degree values
print("Updated Importance Dataset:")
print(importance_dataset)

# filter variables based on p-value <= 0.05
importance_dataset_p = importance_dataset[importance_dataset$pvalue<=0.05,]
print(importance_dataset_p)

## Interaction

variables = c('movie_budget', 'release_year','release_month', 'duration', 'nb_news_articles', 'actor1_star_meter', 'actor2_star_meter', 'actor3_star_meter', 'nb_faces', 'movie_meter_IMDBpro','action','adventure','scifi','thriller','musical','romance','western','sport','horror','drama','war','animation','crime','distributor_rate')

importance_interaction =data.frame(var1=character(), var2=character(), 
                                     best_degree1=integer(), best_degree2=integer(), 
                                     best_rsq=numeric(), 
                                     rsq_1_1=numeric(), rsq_1_2=numeric(), 
                                     rsq_2_1=numeric(), rsq_2_2=numeric())

for(var1 in variables){
  for(var2 in variables){
    n1 = length(unique(IMDB_data[[var1]]))
    n2 = length(unique(IMDB_data[[var2]]))
    # Check minimum threshold
    if(n1 < min_unique){
      print(paste0(var1, " has only ", n1, " unique values. Skipping."))
      next
    }
    if(n2 < min_unique){
      print(paste0(var2, " has only ", n2, " unique values. Skipping."))
      next
    }
    if (var1 != var2) {
      lm_1_1 = lm(imdb_score ~ IMDB_data[[var1]]*IMDB_data[[var2]], data=IMDB_data)
      lm_1_2 = lm(imdb_score ~ IMDB_data[[var1]]*poly(IMDB_data[[var2]], 2), data=IMDB_data)
      lm_2_1 = lm(imdb_score ~ poly(IMDB_data[[var1]], 2)*IMDB_data[[var2]], data=IMDB_data) 
      lm_2_2 = lm(imdb_score ~ poly(IMDB_data[[var1]], 2)*poly(IMDB_data[[var2]], 2), data=IMDB_data)
      
      # ANOVA test 
      anova_result = anova(lm_1_1, lm_1_2, lm_2_1, lm_2_2)
      
      # Check significance 
      if (!is.na(anova_result$'Pr(>F)'[2]) && anova_result$'Pr(>F)'[2] < 0.05) {
        best_degree1 = 1
        best_degree2 = 2
        best_model = 2
      } else if (!is.na(anova_result$'Pr(>F)'[3]) && anova_result$'Pr(>F)'[3] < 0.05) {
        best_degree1 = 2
        best_degree2 = 1
        best_model = 3
      } else if (!is.na(anova_result$'Pr(>F)'[4]) && anova_result$'Pr(>F)'[4] < 0.05) {
        best_degree1 = 2
        best_degree2 = 2
        best_model = 4
      } else {
        best_degree1 = 1
        best_degree2 = 1
        best_model = 1
      }
      
      # Extract R-squared values
      rsq = c()
      rsq[1] = summary(lm_1_1)$r.squared
      rsq[2] = summary(lm_1_2)$r.squared
      rsq[3] = summary(lm_2_1)$r.squared
      rsq[4] = summary(lm_2_2)$r.squared
      best_rsq = rsq[best_model]
      
      new_row = data.frame(var1 = var1, 
                           var2 = var2,
                           best_degree1 = best_degree1, 
                           best_degree2 = best_degree2, 
                           best_rsq = best_rsq, 
                           rsq_1_1 = rsq[1],
                           rsq_1_2 = rsq[2],  
                           rsq_2_1 = rsq[3],
                           rsq_2_2 = rsq[4])
      
      importance_interaction = rbind(importance_interaction, new_row)
    }
  }
}
importance_interaction = importance_interaction[order(-importance_interaction$best_rsq), ]
importance_interaction

############ Training model ##############
set.seed(42)
print(importance_dataset_p)
results_df = data.frame(formula = integer(),
                        rsq = numeric(),
                        MSE = numeric())

formula = "imdb_score ~ "

formula_list = list()

# calculate mse and rsq for each formula
for (i in 1:nrow(importance_dataset_p)) {
  var = importance_dataset_p$var[i]
  degree = importance_dataset_p$degree[i]
  
  if (degree==1){
    formula = paste(formula, var, sep = "")
  }
  else {
    formula = paste(formula, "poly(", var, ",", degree, ")", sep = "")
  }
  formula_list[[i]] = as.formula(formula)
  
  model = lm(formula, data = IMDB_data)

  rsq = summary(model)$r.squared
  mse_list = list()
  for (j in 1:10) {
    fit = glm(formula, data = IMDB_data)
    mse_list[j] = cv.glm(data = IMDB_data, glmfit = fit, K = 10)$delta[1]
  }
  mse_values = sapply(mse_list, function(x) x[[1]])
  avg_mse = mean(mse_values, na.rm = TRUE)
    
  results_df = rbind(results_df, data.frame(formula = i,
                                               rsq = rsq,
                                               avg_MSE = avg_mse))
  if (i < nrow(importance_dataset_p)) {
    formula = paste(formula, "+", sep = "")
  }
}
# Print the results dataframe
print(results_df)

# Create a line plot for rsq
ggplot(results_df, aes(x = formula, y = rsq, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "R-squared Values for Different Formulas",
       x = "Formula Index",
       y = "R-squared") +
  theme_minimal()

# Create a line plot for MSE
ggplot(results_df, aes(x = formula, y = avg_MSE, group = 1)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Average Mean Squared Error (MSE) for Different Formulas",
       x = "Formula Index",
       y = "MSE") +
  theme_minimal()

formula_list[[3]]
best_formula ="imdb_score ~ distributor_rate + poly(duration, 2) + drama"
model = lm(best_formula, data = IMDB_data)

best_formula ="imdb_score ~ distributor_rate + poly(duration, 2) + drama"
best_model = lm(best_formula, data = IMDB_data)
best_rsq = summary(best_model)$r.squared
fit = glm(best_formula, data = IMDB_data)
best_mse = cv.glm(data = IMDB_data, glmfit = fit, K = 10)$delta[1]

sample_index = sample(1:nrow(IMDB_data), 0.8 * nrow(IMDB_data))

train_data = IMDB_data[sample_index, ]
test_data = IMDB_data[-sample_index, ]

# Fit the model on the training data
best_model = lm(best_formula, data = train_data)

# Predict imdb_score values on the test data
predicted_scores = predict(best_model, newdata = as.data.frame(test_data))

# Create a scatter plot
plot(test_data$imdb_score, predicted_scores, 
     xlab = "Actual imdb_score", 
     ylab = "Predicted imdb_score", 
     main = "Actual vs Predicted imdb_score",
     pch = 16,  # Use solid circles as points
     col = rgb(0, 0, 1, alpha = 0.5))

# Add a diagonal line to visualize perfect predictions
abline(0, 1, col = "red", lwd = 2)




############ model with highest predictive power but cannot be used for the test df#######

formula ="imdb_score ~ director_rate + cinematographer_rate + production_company_rate + distributor_rate + poly(duration, 2) + drama "
model = lm(formula, data = IMDB_data)
rsq = summary(model)$r.squared
fit = glm(formula, data = IMDB_data)
mse = cv.glm(data = IMDB_data, glmfit = fit, K = 10)$delta[1]
sample_index = sample(1:nrow(IMDB_data), 0.8 * nrow(IMDB_data))

train_data = IMDB_data[sample_index, ]
test_data = IMDB_data[-sample_index, ]

predicted_scores = predict(model, newdata = as.data.frame(test_data))

# Create a scatter plot
plot(test_data$imdb_score, predicted_scores, 
     xlab = "Actual imdb_score", 
     ylab = "Predicted imdb_score", 
     main = "Actual vs Predicted imdb_score",
     pch = 16,  # Use solid circles as points
     col = rgb(0, 0, 1, alpha = 0.5))

# Add a diagonal line to visualize perfect predictions
abline(0, 1, col = "red", lwd = 2)


############# spline ###############
model = lm(imdb_score ~ distributor_rate + bs(duration, df=3) + drama, data=IMDB_data)
           #+ poly(nb_news_articles, 5) + poly(release_year, 3)
rsq = summary(model)$r.squared
rsq-best_rsq
fit = glm(imdb_score ~ distributor_rate + bs(duration,df=3) + drama + poly(nb_news_articles, 5) + poly(release_year, 3), data = IMDB_data)
mse = cv.glm(data = IMDB_data, glmfit = fit, K = 10)$delta[1]
mse - best_mse



############# interaction ##############
importance_interaction
importance_interaction_new = head(importance_interaction, 20)
formula_list_interaction = list()
# Initialize results dataframe
results_interaction_df = data.frame(formula = character(),
                                     rsq = numeric(),
                                     MSE = numeric())

# Base formula
base_formula = "imdb_score ~ distributor_rate + poly(duration, 2) + drama +"

# Iterate through importance_interaction_new
for (i in 1:nrow(importance_interaction_new)) {
  var1 = importance_interaction_new$var1[i]
  degree1 = importance_interaction_new$best_degree1[i]
  var2 = importance_interaction_new$var2[i]
  degree2 = importance_interaction_new$best_degree2[i]
  
  # Construct the formula
  if (degree1 == 1){
    formula1 = var1
  } else {
    formula1 = paste0("poly(", var1, ",", degree1, ")")
  }
  
  if (degree2 == 1){
    formula2 =  paste0("*" , var2)
  } else {
    formula2 = paste0("*poly(" , var2 , "," , degree2 , ")")
  }
  
  formula = paste0(base_formula, formula1, formula2)
  

  # Store the formula in the list
  formula_list_interaction[[i]] = as.formula(formula)
  
  # Fit the model
  model = lm(formula, data = IMDB_data)
  
  # Calculate R-squared
  rsq = summary(model)$r.squared
  
  # Calculate MSE using 10-fold cross-validation
  fit = glm(formula, data = IMDB_data)
  mse = cv.glm(data = IMDB_data, glmfit = fit, K = 10)$delta[1]
  
  # Add the results to the dataframe
  results_interaction_df = rbind(results_interaction_df, 
                                  data.frame(formula = formula, 
                                             rsq = rsq, 
                                             MSE = mse))
}

# Print or use results_interaction_df as needed
print(results_interaction_df)

best_formula = "imdb_score ~ distributor_rate + poly(duration, 2) + drama +distributor_rate*poly(release_year,2)"
best_model = lm(best_formula, data = IMDB_data)
best_rsq = summary(best_model)$r.squared
fit = glm(best_formula, data = IMDB_data)
best_mse = cv.glm(data = IMDB_data, glmfit = fit, K = 10)$delta[1]

####### dummy #######
# maturity_rating - high vif
IMDB_data$maturity_rating = as.factor(IMDB_data$maturity_rating)
attach(IMDB_data)
formula = "imdb_score ~ distributor_rate + poly(duration, 2) + drama +distributor_rate*poly(release_year,2)+maturity_rating"
model = lm(formula, data = IMDB_data)
rsq = summary(model)$r.squared
rsq-best_rsq
fit = glm(formula, data = IMDB_data)
mse = cv.glm(data = IMDB_data, glmfit = fit, K = 10)$delta[1]
mse-best_mse

# aspect_ratio - no need because the data is focus on 1.85 and 2.35
IMDB_data$aspect_ratio = as.factor(IMDB_data$aspect_ratio)
attach(IMDB_data)
formula = "imdb_score ~ distributor_rate + poly(duration, 2) + drama +distributor_rate*poly(release_year,2)+maturity_rating++aspect_ratio"
model = lm(formula, data = IMDB_data)
rsq = summary(model)$r.squared
rsq-best_rsq
fit = glm(imdb_score ~ director_rate + production_company_rate + cinematographer_rate + poly(duration, 2)+aspect_ratio, data = IMDB_data)
mse = cv.glm(data = IMDB_data, glmfit = fit, K = 10)$delta[1]
mse-best_mse

# Outliers
outlierTest(best_model)
IMDB_data = IMDB_data[-c(1619,1354,1854,890,39,1331)]
dim(IMDB_data)
attach(IMDB_data)
best_rsq = summary(best_model)$r.squared
fit = glm(best_formula, data = IMDB_data)
best_mse = cv.glm(data = IMDB_data, glmfit = fit, K = 10)$delta[1]

sample=sample.split(IMDB_data$imdb_score, SplitRatio = 0.5) 
train_data = subset(IMDB_data, sample==TRUE)
test_data = subset(IMDB_data, sample==FALSE)

fit = lm(best_formula, data = train_data)

actual = test_data$imdb_score
prediction = predict(fit, test_data)
squared_error = (actual-prediction)^2
mse = mean(squared_error)
mse

in_sample_predictions = predict(fit, train_data)

in_sample_squared_error = (train_data$imdb_score - in_sample_predictions)^2

in_sample_mse = mean(in_sample_squared_error)
in_sample_mse

# Fit the model on the training data
best_model = lm(best_formula, data = train_data)

# Predict imdb_score values on the test data
predicted_scores = predict(best_model, newdata = as.data.frame(test_data))

# Create a scatter plot
plot(test_data$imdb_score, predicted_scores, 
     xlab = "Actual imdb_score", 
     ylab = "Predicted imdb_score", 
     main = "Actual vs Predicted imdb_score",
     pch = 16,  # Use solid circles as points
     col = rgb(0, 0, 1, alpha = 0.5))

# Add a diagonal line to visualize perfect predictions
abline(0, 1, col = "red", lwd = 2)


# Test --------------------------------------------------------------------
IMDB_data = read.csv("/Users/lauray/Documents/学习/MMA/Fall 2023/MGSC661/mid/IMDB_data_Fall_2023.csv")
test_df = read.csv("/Users/lauray/Documents/学习/MMA/Fall 2023/MGSC661/mid/test_data_IMDB_Fall_2023.csv")
# pre-processing
IMDB_data$release_month =month_mapping[IMDB_data$release_month]
test_df$release_month =month_mapping[test_df$release_month]

attach(IMDB_data)
## Add variables
# AVG previous distributor rate
distributor_rate_df = data.frame(distributor = unique(distributor), distributor_rate = NA)
for (i in 1:length(distributor_rate_df$distributor)) {
  score_list = numeric() 
  
  for (j in 1:length(IMDB_data$distributor)) {
    if (distributor_rate_df$distributor[i] == IMDB_data$distributor[j]) {
      score_list = c(score_list, IMDB_data$imdb_score[j])
    }
  }
  
  avg = mean(score_list, na.rm = TRUE) 
  distributor_rate_df$distributor_rate[i] = avg
}
IMDB_data = merge(IMDB_data, distributor_rate_df, by = "distributor", all.x = TRUE)
table(is.na(IMDB_data$distributor_rate))
test_df = merge(test_df, distributor_rate_df, by = "distributor", all.x = TRUE)
table(is.na(test_df$distributor_rate))
test_df$distributor_rate[is.na(test_df$distributor_rate)] = mean(IMDB_data$distributor_rate)

IMDB_data = IMDB_data[-c(1619,1354,1854,890,39,1331)]
attach(IMDB_data)
attach(test_df)

model = lm(best_formula, data = IMDB_data)
predicted_scores = predict(model, newdata = test_df)

test_df$predicted_imdb_score = predicted_scores
result = test_df[, c(2, ncol(test_df))]
print(result)

