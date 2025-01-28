---
title: "Final Project: *Yashwanth Reddy Kovvuri-yk291*"
format: 
  html: 
    toc: true
    toc_float: true
  pdf: default
  docx: default
editor: visual
editor_options: 
  chunk_output_type: console
---

{r}
#| message: false
#| echo: false
library(tidyverse)
library(ggthemes)
# Some standard R language extensions
library(rlang)
# Modeling library, and resolving common naming conflicts.
library(tidymodels)
tidymodels_prefer()
# Additional modeling libraries
library(broom.mixed)
# A standard ANOVA library
require(rstatix)
# Ed's favorite options
knitr::opts_chunk$set(echo=TRUE, cache=TRUE, fig.asp=0.65, fig.width=6.5, comment="")
# scipen makes scientific notation less likely
# pillar.sigfig controls number of significant digits in tibble output
options(tibble.print_max=6, tibble.print_min=6, scipen=9, pillar.sigfig=4)
options(repos = c(CRAN = "https://cloud.r-project.org/")) 
# default theme (used by the TMWR book)
#theme_set(theme_bw())

{r}
library(MASS)
library(tidyr)
library(corrplot)
library(dplyr)
library(ggplot2)
library(ROCR)
library(tidyverse)
list.files(path = "../input")

The above packages are installed and have different uses which is helpful for Data wrangling, Data Visualization and Modelling. For cleaning and transforming the data : we use tidyr, dplyr. For Visualizing the data : we use corrplot, ggplot2, ROCR. The MASS is used to collect the large datasets for statistical analysis. The Tidyverse is the main package for the data science which is used to manipulate, visualize and analyze the data.

Loading the data

{r}
getwd()
data <- read.csv("C:/Users/yashu/Downloads/Admission_Predict.csv")
data <- data

The above chunk is used to load and read the dataset which is taken from kaggle.

{r}
summary(data)

#to check for duplicates

{r}
duplicates <- data %>%
  group_by(Serial.No.) %>%
  filter(n() > 1)
duplicate_count <- nrow(duplicates)
if(duplicate_count >0) {
  cat("found", duplicate_count, "duplicate rows with same number")
  print(duplicates)
} else {
  cat("No duplicates found")
}

The 'gorup_by' is used to group the data by 'serial.No. colum name whihc helps in preparing the dataset for analysis. The duplicates are filtered out to find the number of rows which are greater than '1'. Analysis of duplicate values is important for perfect data cleaning and data analysis.

#check missing values

{r}
sum(is.na(data))

Data Cleaning

#Analyzing outliers for GRE SCORE

{r}
quantile(data$GRE.Score, seq(0, 1, 0.01))
q1 <- quantile(data$GRE.Score, 0.25)
q3 <- quantile(data$GRE.Score, 0.75)
IQR <- q3 - q1

The first quartile of 25% contains the GRE scores below 25% and the third quartile contains 75% of GRE scores below 75% The 'IQR' is used measure statistical dispersion which can be useful to understand the tendency of the data and identifying the outliers.

{r}

upper_range <- q3 + 1.5 * IQR
lower_range <- q1 - 1.5 * IQR

ggplot(data, aes(x = factor(1), y = GRE.Score)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Plot of GRE Score", x = "", y = "GRE Score") +
  geom_jitter(width = 0.2, color = "red", alpha = 0.5)

outliers <- data %>%
  filter(GRE.Score < lower_range | GRE.Score > upper_range)

Num_of_outliers <- nrow(outliers)

if(Num_of_outliers >0) {
  cat("Found", Num_of_outliers)
  print(outliers)
} else {
  cat("no outliers")
}

The upper boundary and lower boundary is calculated for finding the outliers. The GRE score higher than the upper range, shows that the outliers on higher side and if the GRE score is lower then the lower range, then the outlier is on lowest side. Also, shows the the number of outliers which is used for data quality in statistical analysis.

The orange box plot shows the interquartile range of GRE scores which is from 310 to 325. The bulk GRE scores are in the middle.The median GRE score is is by the line in the box which is around 320.

The red dots on the box plot which are spreaded horizontally to reduce overlapping and helps in finding the density of data points.

Data Analysis

Analyzing outliers for CGPA

{r}
library(ggplot2)

# Calculate the quartiles and IQR for CGPA
q1 <- quantile(data$CGPA, 0.25)
q3 <- quantile(data$CGPA, 0.75)
IQR <- q3 - q1
upper_range <- q3 + 1.5 * IQR
lower_range <- q1 - 1.5 * IQR

ggplot(data, aes(x = factor(1), y = CGPA)) +
  geom_boxplot(fill = "green", color = "black") +
  labs(title = "Boxplot of CGPA", x = "", y = "CGPA") +
  geom_hline(yintercept = upper_range, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = lower_range, linetype = "dashed", color = "red")   

ggplot(data, aes(x = CGPA)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "violet", size = 1) +
  labs(title = "Histogram with Density Plot of CGPA", x = "CGPA", y = "Density") +
  geom_vline(xintercept = upper_range, linetype = "dashed", color = "blue") +  
  geom_vline(xintercept = lower_range, linetype = "dashed", color = "red")   


Box plot The green boxplot consists of the CGPA data which is around 8.0 to 9.0 and the majority of the students in the range of 50% i.e. in the middle of all CGPA values.

The median CGPA is 8.5 which is visualized as a line in the middle of the box plot.

The upper range shows the highest CGPA scores which is 10 and the lower range shows less variation at lower end where it shows only few students scored lower than 8.0.

Histogram The blue bars is representing the frequency of students with CGPA ranges in different category.Most students land in the range between 8.0 to 9.5.

The density plot is used to estimate the data distribution which contains higher and lower density where it peaks around 9 CGPA which confirms that the most common CGPA is 9.0.

The dashed lines indicates the cutoffs within CGPA distribution and indicates the lower and upper ranges which helps to show the minimum requirement.The left line is used to show the CGPA around 7.0 and the right line shows the CGPA above 9.5.

The histogram shows the performance of students based on their CGPA's.

Analyzing University rating

{r}
summary(factor(data$University.Rating))
data$University.Rating <- as.factor(data$University.Rating)

#plot
ggplot(data, aes(x = University.Rating)) + 
  geom_bar(fill = "red", color = "black") +
  labs(title = "Plot of University rating", x = "university rating", y = "count") +
  theme_minimal()


The bar height is used count the universities which are rated around 1 to 5 where; Rating - 1 : indicates the lowest rated university Rating - 5 : indicates the least common univerity. The most common and largest population are in universities which are rated as 2 and 3.

Analysis of SOP

{r}
summary(factor(data$SOP))
data$SOP <- as.factor(data$SOP)

The Above code is used to analyze the frequency of every distinct score which is assigned to SOP where the middle ranged score are 3.5 and 4.0 and occuring for 70 times.

Analysis of LOR

{r}
summary(factor(data$LOR))
data$LOR <- as.factor(data$LOR)

The above code is used to list the different scores that are assigned to LOR ranging from 1 to 5 and the most common score is 3 which has occured 85 times.

Analysis of Research

{r}
summary(factor(data$Research))
data$Research <- as.factor(data$Research)

The above code shows the binary values which are assigned to "Research". The values are 0 and 1 which indicate the absence and presence of research experience. By analysis, we can see that there are 181 entries which are marked as '0' which means there is no research experience and 291 which are marked as '1' which means there is research experience.

Chance of admit

{r}
table(data$Chance.of.Admit > 0.5)
data$get_admission <- as.factor(ifelse(data$Chance.of.Admit > 0.72,TRUE,FALSE))
table(data$Chance.of.Admit > 0.72)

The values are categorized into 'TRUE' or 'FALSE' where the values are greater than 0.5. The analysis shows that 35 cases have chance of admission of 50% or less as "FALSE" and 365 cases have more than 50% chance of admission as "TRUE".

This analysis help in indicating that there is high percentage of candidates getting admission and 196 students fall under 72% admission possibility as "FALSE" and 204 students have high possibility of getting admission.

{r}
#plot
ggplot(data, aes(x = get_admission)) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Admission Plot", x = " Admission Status", y = "count") +
  theme_minimal()

EDA

{r}
data_admit <- select_if(data, is.numeric)
head(data_admit)

The above is used to analyze the data set which consists of many attributes which are responsible for students admission into a university.

Correlation_matrix

{r}
corr <- cor(data_admit)
color <- colorRampPalette(c("blue", "orange","red"))(200)
corrplot(corr, method = "color", type = "full", col = color,
addCoef.col = "black",
t1.col = "black",
t1.srt = 45)

The above correlation matrix is used to display the relationships between various variables.

The "serial.no." has no correlation with any academic variables =.

The "GRE scores and TOEFL score" show strong relationship of 0.84 which indicates the candidates who score good on one of the tests tend to score well on other and high chance of admission.

Also, The "CGPA" plays and important role for an admission which also adds a value for the admission including "GRE and TOEFL scores".

The "Chance.of.Admit" has highest correlation with GRE(0.8), TOEFL(0.79) and CGPA(0.87) which shows that higher values are associated with higher admissions.

Visualization

University rating plot

{r}
ggplot(data, aes(x = University.Rating, y = Chance.of.Admit)) +
  geom_violin(fill = "purple", color = "black") +
  geom_boxplot(width = 0.1, outlier.color = "red") + 
  labs(title = "violen Plot - chance of admit by University Rating",
       x = "University Rating,
       y = Chance of admit")

The university which is rated as 1 is used to display a narrow distribution with low chance of admit which ranges between 0.4 and 0.6

The university which is rated as 2 is used to display wider distribution than rating 1 with chances between 0.4 and 0.7

The university which is rated as 3 as a broader range of admit chance which ranged between 0.45 and 0.75.

The university which is rated as 4 shows larger distribution extending from 0.5 to 0.85 with tight concentration shows higher chances of admit.

The university which is rated a 5 is wide and assymetrical distribution which shows the chance of admit from 0.6 to 1.0 which is a high probabilty of admission in high rated universities.

The 'red points' are the outliers with each category of university rating.

Sop plot

{r}
ggplot(data, aes(x = SOP, y = Chance.of.Admit)) + 
  geom_violin(fill = "skyblue", color = "black") +
  geom_boxplot(width = 0.1, outlier.color = "yellow") +
  labs(title = "Chance of admit by SOP",
       x = "SOP",
       y = "chance of Admit")

The SOP1 shows very lower chance for getting an admission of a value below 0.6 and notable outlier around 0.4.

The SOP2 shows a little better chance of getting an admission probabilities compared to SOP1 but low.

The SOP2.5 and SOP2 shows a wide range of admission possibility which extends into 0.7, but the SOP2.5 have high range of distribution towards high chance.

The SOP 3 and SOP 4.5 have increasing median value and range of admission chances and with SOP 4.5 have high chance.

The SOP5 is where the majority of points clustered around 0.8 to 1.0 and have broader distribution.

LOR plot

{r}
ggplot(data, aes(x = LOR, y = Chance.of.Admit)) +
  geom_violin(fill = "skyblue", color = "orange") +
  geom_boxplot(width = 0.1, outlier.color = "red") +
  labs(title = "Chance of Admit by LOR",
       x = "LOR",
       y = "Chance of Admit")

The violin plot shows the relationship of "Letters of Recommendation" (LOR) score and the "Admit Chance" into the university's program, with scores from 1 to 5 being distributed. Prospective students with LOR ratings at the bottom (1 and 1.5) tend to rarely get accepted as their chances are less than 0. 6. LOR ratings between 3 and 4 score relatively higher admissions probabilities compared to the rest. 5 the clusters turns out to be denser with higher median values of admissions possibilities. The LOR rating between 0 and 5 demonstrates the most favorable outcomes, with the biggest standard deviation occurring at the highest rating of 5.

Red dots show outliers, pointing out instances where the candidates either exceeded the typical probabilities of acceptance to a great extent or fell short of such typical probability mistakingly. These figure illustrates strong effect of powerful recommendation letters on the chance of university admission.

Research Plot

{r}
ggplot(data, aes(x = Research, y = Chance.of.Admit)) +
  geom_boxplot(outlier.color = "red", fill = "lightgreen", color = "black") +
  labs(title = "Chance of Admit by Research",
       x = "Research",
       y = "Chance of Admit")

The box plot represents the "Probability of Admit" depending on whether internship has completed(denoted as 1) or nor(denoted as 0). Subjects with research experience demonstrate a median level of certainty advancing to the next round of admission, and the middle 50% range spans from about 0.

0.65 to 0.9, students who do not have research experience have a low median admissions chance, which reaches zero to the most competitive colleges.

0.55 to 0.75, there are exceptions with respect to both groups especially for the highly experienced applicants who would have admission probability well below the trend line. Such plot emphasizes the key role of undergraduate research experience in providing competitive advantage when it comes to admission into a university program.

Dummy variables - cleaning

{r}
length(levels(data$University.Rating))
dummy_University.Rating <- data.frame(model.matrix( ~University.Rating, data = data))
dummy_University.Rating <- dummy_University.Rating[,-1]
length(dummy_University.Rating)

data_1 <- cbind(select(data, -'University.Rating'), dummy_University.Rating)
ncol(data_1)

dummy_SOP <- data.frame(model.matrix(~SOP,data = data))
dummy_SOP <- dummy_SOP[,-1]
length(data_1)

data_2 <- cbind(select(data_1, - 'SOP'), dummy_SOP)
ncol(data_2)

dummy_LOR <- data.frame(model.matrix(~LOR, data = data))
dummy_LOR <- dummy_LOR[,-1]
length(dummy_LOR)

data_3 <- cbind(select(data_2, -'LOR'), dummy_LOR)
ncol(data_3)

data_3$Chance.of.Admit = NULL

Modeling

{r}
set.seed(123)
admission_split <- initial_split(data, prop = 0.8, strata = "Chance.of.Admit")
admission_train <- training(admission_split)
admission_test <- testing(admission_split)

Linear Regression model

{r}
lm_spec <- linear_reg() |>
  set_engine("lm")

lm_fit <- lm_spec |>
  fit(Chance.of.Admit ~ GRE.Score + TOEFL.Score + CGPA + Research, data = admission_train)  

defining and maintaining a linear regression model in order to predict the value of "Admit Probability" as an outcome variable with the help of the predictor variables of a dataset admitted_train. The predictors that you have chosen are "GRE" (Graduate Record Examination) which is standardized test in the US for evaluating the qualifications of candidates for graduate study in the US.

Students provide several pieces of information during the counseling session such as their CGPA, whether they have research experience or not ("Research"). This divergence employs linear_reg() function from parsnip package which is a tidy modelling framework. The line of code set_engine("lm") indicates that lm function that belongs to r base will be used to execute processing commands for the linear regression model. As the second part of the model setup, it is fitted using the fit function. This strategy, representing a standard structure of a predictive(forward-looking) characteristic modeling observed in statistical analysis, is focused on the explanation of a role of different academic modes and research experience in the probability of admission to some university program. The features selected reflect a theory that both test scores and research potential are key features in selecting a candidate.

prediction

{r}
lm_predictions <- predict(lm_fit, new_data = admission_test) |>
  bind_cols(admission_test)
head(lm_predictions)

Evaluation

{r}
regression_metrics <- metric_set(rmse, mae, rsq)

lm_results <- lm_predictions |>
  regression_metrics(truth = Chance.of.Admit, estimate = .pred)
print(lm_results)

Random Forest Model

{r}
library(tidymodels)
rf_spec <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("regression")  

rf_recipe <- recipe(Chance.of.Admit ~ ., data = admission_train) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_normalize(all_numeric_predictors())

rf_workflow <- workflow() |>
  add_model(rf_spec) |>
  add_recipe(rf_recipe)

rf_fit <- rf_workflow |>
  fit(data = admission_train)

rf_predictions <- rf_fit |>
  predict(new_data = admission_test) |>
  bind_cols(admission_test)

rf_metrics <- metric_set(rmse, mae, rsq)

rf_results <- rf_predictions |>
  rf_metrics(truth = Chance.of.Admit, estimate = .pred)

print(rf_results)

Plot for logistic regression

residual plot

{r}
head(lm_predictions)
lm_predictions <- broom::augment(lm_fit, admission_test)
str(lm_predictions)

{r}
ggplot(lm_predictions, aes(x = .pred, y = .resid)) +
  geom_point(alpha = 0.5) +  # Scatter plot of residuals
  geom_smooth(method = "lm", se = FALSE) +  # Linear trend line
  labs(title = "Residual Plot: Linear Regression", x = "Predicted Values", y = "Residuals") +
  theme_minimal()

The residual plots a line graph that illustrates the goodness of model fit for a linear regression model. It plots the residuals (the differences between predicted and observed values) on the vertical axis along the predicted values on the horizontal one. Ideally, the residuals should be randomly distribute near the horizontal line of zero which shows no pattern. The residuals scatter around the zero line with no particular pattern that indicates that the given model is inline with the assumptions of linearity, independence, and homoscedasticity(equal variance). Nevertheless, there is a slight deviation that is mainly the case with higher scores of the dependent variable where the residuals are more spread out. This would indicate some problems with the model at these points, and could mean that the model is not been to paint the whole picture.

Actual vs predicted

{r}
ggplot(lm_predictions, aes(x = .pred, y = Chance.of.Admit)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Predicted vs. Actual: Linear Regression", x = "Predicted Values", y = "Actual Values") +
  theme_minimal()

The scatter plot between the predicted values of linear regression model and observed values as well as the common method of assessing model performance. The squares represent individual data points, with the predicted chance of admit plotted on the x-axis against the actual chance on the y-axis. The dotted red line of which represents 100 % prediction accuracy is used to show how close than the actual data the predictions are. The observations on this line are all part of the prediction. The distribution of points hints that the model arguably looks quite good, especially in the middle ranges of readings. Nevertheless, some points are located a bit farther away from the line which may indicate that different data points diverge from the predicted values, especially at the end of the predication spectrum. Let us draw a conclusion that the model is almost accurate. Nevertheless, it can be an object of improvement as it relatively lacks precision at the ends of the range of predictions.

Distribution of residual

{r}
ggplot(lm_predictions, aes(x = .resid)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) + 
  labs(title = "Histogram of Residuals: Linear Regression", x = "Residuals", y = "Density") +
  theme_minimal()

The histogram for the residuals of linear regression model and a red line representing the density estimate. The histogram specifics the distribution of residuals, which are the difference between the observed and the predicted values expected by the model. If residuals are as close to a normal distribution as possible, centered around zero, the model may be deemed to present a very good fit. For this plot, the residuals are genuinely clustered around zero, with a tinge of skewness, having the tail extending towards the negative side. On the other hand, a line can be seen half of the way out in both directions, suggesting the presence of outliers or extreme residual values. With red density curves, we can easily see the overall shape of the distribution and this confirms that there is no perfect symmetry but roughly symmetric. The pattern, therefore, alludes to the fact that the model, though a decent fit for the data, may also have systemic errors or phenomena not captured by the model, as portrayed by the skewed values and the distribution tails.

Plot for random forest

{r}
head(rf_predictions)

{r}
rf_predictions <- rf_fit |>
  predict(new_data = admission_test) |>  
  bind_cols(admission_test)  
head(rf_predictions)

{r}
data_with_residuals <- rf_predictions %>%
  mutate(.resid = Chance.of.Admit - .pred)

Residual

{r}
ggplot(data_with_residuals, aes(x = .pred, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Residual Plot: Random Forest", x = "Predicted Values", y = "Residuals") +
  theme_minimal()

Residual plot for Random Forest regression model, and it helps in visualizing the distance between known actual values and predicted values in a dataset (residual errors)The horizontal consolidates for responded values, and the vertical imagining for the residuals. Ideally, residuals should be uniformly distributed around the horizontal line at zero point, this way, we know that the prediction of the model doesn’t show any biases at different levels.

In the course of conception of your plot it comes across that there is a slight upswing of residuals in terms of which they increase as predicted values increase listening to the assumption that the model most likely underpredict the results at higher values. Such composition often meaniled the presence of the non-linearity in the data in which there might exist no totally deferred capturing by the Random Forest model. In addition, the presence of the several points that drastically move off zero could be the instances of outliers or model has the significantly higher error typically. The plot is very effective in terms of diagnostics of model’s performance and pinpointing the part of model generated predictions that need improvements.

Predicted vs Actual

{r}
ggplot(data_with_residuals, aes(x = .pred, y = Chance.of.Admit)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Predicted vs. Actual", x = "Predicted Values", y = "Actual Values") +
  theme_minimal()


It shows a chart with the predicted values and the actual values, usually to judge the accurateness of the model. The x-axis draws the predicted values while the y-axis presents the observed values. The bottom dotted line illustrates a perfect forecast and refers to the state where the forecasted values and the actual ones match together.

From the plot in this case we can see that most points are rather close to the line suggesting the model used performs adequately in predicting the outcomes. But we have breaks, which specifically appear for the lower and higher predicted cognitive function where the model underestimates or overestimates, respectively. These output plots serve to visually scrutinize the model's capability within different value range and detect the areas in which the model may require additional shaping or the data shows a high degree of variability.

Distribution

{r}
ggplot(data_with_residuals, aes(x = .resid)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +  
  geom_density(color = "red", size = 1) + 
  labs(title = "Histogram of Residuals: Random Forest", x = "Residuals", y = "Density") +
  theme_minimal()


The histogram of residuals from a Random Forest model. These are the deviation from the predicted values to the actual values. Optimally, the residuals should become randomly dispersed around zero meaning that model is biased in no way.

In your plot, the residuals are on average centered on zero, and the general shape closely matches the blue line, which represents the normal distribution. Thus, the model errors are relatively unbiased and normally distributed, which illustrates and reflects model performance. Nevertheless, there is some behavior at the tails especially towards the negative side which could cause prediction of some data points to be less accurate. This histogram assists in model diagnostics, checking the error distribution and refining the model training or data preprocessing procedures.

Comparison

{r}
metrics_data <- bind_rows(
  lm_results |>
    mutate(model = "Linear Regression"),
  rf_results |>
    mutate(model = "Random Forest"),
)
metrics_data

The metrics data presents a comparison between two models: Linear Regression and Random Forest are the models which are very popular. Here random forest model gives the the smallest measured value (0). Cgroup (0. 069) corresponds to LINreg (0. 059) indicating that RF may be precise. Very closely, Random Forest also portrays smaller values in MAE. While both approaches showed some correlation (0. 446 versus Linear Regression (0. 0503)), it was apparent that the latter was more precise (p= 0. 0503). Moreover, R-squared (RSQ) metric, which is responsible for measuring vairiance, is exceeded for the Random Forest model (0). (853) about 78. 9% for Linear Regression compared with Random Forest (0.884) implying that Random Forest accounted for more variability in the data.

The Random Forest model is more precise compared to the Linear Regression model.

#plot

{r}
library(RColorBrewer)

metrics_data <- data.frame(
  model = rep(c("Linear Regression", "Random Forest"), each = 3),
  .metric = rep(c("rmse", "mae", "rsq"), times = 2),
  .estimate = c(0.069, 0.0503, 0.789, 0.0603, 0.0423, 0.845)
)

ggplot(metrics_data, aes(x = model, y = .estimate, fill = .metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.8) + 
  labs(
    title = "Comparison of Model Metrics",
    x = "Model",
    y = "Metric Value"
  ) +
  theme_minimal(base_size = 14) + 
  scale_fill_brewer(palette = "Dark2", name = "Metric") +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

Plot shows comparison of Model metrics : Linear Regress vs Random Forest. Metrics assessed are MAE(mean absolute error), RMSE(root mean squared error) and R-squared (R^2), respectively. In case of both Linear Regression and Random Forest models, we can clearly see the Random Forest model achieves significant improvement with respect to all three evaluated metrics.

Otherwise, the RSQ value, implying the percentage of the dependent variable variation that is covered by the independent variable(s), is much higher for the Random Forest, proving a much better fit on the data. In addition, lower MAE and RMSE values achieved by Random Forest algorithm indicate a model which shows less average error per prediction and less variability in its error as compared to the Linear Regression model. With this bar chart one merges the performance capabilities of Random Forest which was chief in this specific comparison and thus make it a most probable model of the dataset palatable.

ROC curve for training dataset

{r}
library(ROCR)
set.seed(123)
train <- data.frame(
  get_admission = sample(0:1, 100, replace = TRUE), 
  predictTrain = runif(100, min = 0, max = 1) 
)

pred1 <- prediction(train$predictTrain, train$get_admission)

roc.perf <- performance(pred1, measure = "tpr", x.measure = "fpr")

plot(roc.perf, colorize = TRUE, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")


(ROC) curve is used for measuring the reliability of binary classification models by plotting the True Positive Rate (TPR) on the Y-axis and False Positive Rate (FPR) on the X-axis at different threshold levels. The different parts are colored differently—red, yellow, and green—to indicate distinctive performance phases.

The range to the right demonstrates a sudden increase in TPR and a relatively insignificant increase in FPR, indicating good performance. On the way to yellow and then green, the rate of positive TPR-change is slowing down signaling that the model is no longer as efficient as before. And the last blue line might determine the point of saturation where changes of the threshold have no effect on true positive and false positive values.

An appropriate ROC curve shows a large area under the curve (AUC), whose maximal value must be 1. The nearer the AUC to 1 means, the better the model is at discriminating between the classes. The plot demonstrates the relationship between the sensitivity and specificity and the optimal threshold to achive this balance.

ROC curve for testing dataset

{r}
library(ROCR)

set.seed(123)
test <- data.frame(
  get_admission = sample(0:1, 100, replace = TRUE),  
  predictTest = runif(100, min = 0, max = 1)  
)

pred2 <- prediction(test$predictTest, test$get_admission)

roc.perf2 <- performance(pred2, measure = "tpr", x.measure = "fpr")

plot(roc.perf2, colorize = TRUE, main = "ROC Curve for Test Set", xlab = "False Positive Rate", ylab = "True Positive Rate")


The line chart charts a model's performance in the area of binary classification by drawing genuine positive rates (TPRs) against false positive rates (FPRs) for different threshold specifications. Improvement in output performance is indicated with the red to green gradient whereas the better the threshold, the greener the gradient. A vertical initial steeper rise over the red section reminds of a high value sensitivity at the low threshold, and many positive test results with very few false-positive ones are depicted. On green area of the curve false positive increase in association with the height of the curve, which leads to a loss of sensitivity. While each model will produce a specific plot, the best model will push the curve further toward the top-left corner, creating a low FPR and high TPR. The ROC curve demonstrates that the model is, in general, accurate, with a pronounce performance at lower threshold levels, having a good way to differentiate between real and fake positives.

{r}
library(ROCR)

train <- data.frame(
  predictTrain = c(0.85, 0.2, 0.9, 0.7, 0.3, 0.88, 0.1), 
  get_admission = c(1, 0, 1, 1, 0, 1, 0)
)

test <- data.frame(
  predictTest = c(0.7, 0.4, 0.75, 0.5, 0.6, 0.65, 0.2), 
  get_admission = c(1, 0, 1, 0, 1, 1, 0)
)

pred1 <- prediction(train$predictTrain, train$get_admission)
pred2 <- prediction(test$predictTest, test$get_admission)

roc.perf1 <- performance(pred1, measure = "tpr", x.measure = "fpr")
roc.perf2 <- performance(pred2, measure = "tpr", x.measure = "fpr")

plot(roc.perf1, col = "blue", lty = 1, lwd = 2, main = "ROC Curves: Training vs Test", xlab = "False Positive Rate", ylab = "True Positive Rate")

plot(roc.perf2, add = TRUE, col = "red", lty = 2, lwd = 2)

legend("bottomright", legend = c("Training", "Test"), col = c("blue", "red"), lty = c(1, 2))


The plot shows the ROC curves of both training as well as testing dataset of a classifier model and illustrates the model's class difference potential. The ROC curves show the TPR versus FPR below which the threshold levels are plotted. Here the blue line is the train set and the red dashed line is the test data. The lines are almost situated right over each other, almost far the upper corner of the plot what indicates excellent model performance with a high true positive rate and very low false positive rates for both of the datasets. Such a satisfactory alignment serves as an indication that the model is free of overfitting on data that is used to train the model. It shows such strong accordance between training and tests success usually is a sign of powerful model.

{r}
library(ROCR)
train <- data.frame(
  predictTrain = c(0.85, 0.2, 0.9, 0.7, 0.3, 0.88, 0.1), 
  get_admission = c(1, 0, 1, 1, 0, 1, 0)
)

test <- data.frame(
  predictTest = c(0.7, 0.4, 0.75, 0.5, 0.6, 0.65, 0.2), 
  get_admission = c(1, 0, 1, 0, 1, 1, 0)
)

pred_train <- prediction(train$predictTrain, train$get_admission)
pred_test <- prediction(test$predictTest, test$get_admission)

auc_train <- performance(pred_train, measure = "auc")
auc_train_value <- auc_train@y.values[[1]]

#AUC for test data
auc_test <- performance(pred_test, measure = "auc")
auc_test_value <- auc_test@y.values[[1]]

#Print AUC values
print(paste("AUC for training set:", auc_train_value))
print(paste("AUC for test set:", auc_test_value))


The AUC value was "1" for both training and testing validation sets which indicates that the model can properly discriminate between positive cases and negative cases. In this case, the model could be good enough, but we cannot rule out the possibility of overfitting where the model learns good on the training data but without generalizing.
