## LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment 5 scenario
## Turtle Games’s sales department has historically preferred to use R when performing 
## sales analyses due to existing workflow systems. As you’re able to perform data analysis 
## in R, you will perform exploratory data analysis and present your findings by utilising 
## basic statistics and plots. You'll explore and prepare the data set to analyse sales per 
## product. The sales department is hoping to use the findings of this exploratory analysis 
## to inform changes and improvements in the team. (Note that you will use basic summary 
## statistics in Module 5 and will continue to go into more detail with descriptive 
## statistics in Module 6.)

################################################################################

## Assignment 5 objective
## Load and wrangle the data. Use summary statistics and groupings if required to sense-check
## and gain insights into the data. Make sure to use different visualisations such as scatterplots, 
## histograms, and boxplots to learn more about the data set. Explore the data and comment on the 
## insights gained from your exploratory data analysis. For example, outliers, missing values, 
## and distribution of data. Also make sure to comment on initial patterns and distributions or 
## behaviour that may be of interest to the business.

################################################################################

# Module 5 assignment: Load, clean and wrangle data using R

## It is strongly advised that you use the cleaned version of the data set that you created and 
##  saved in the Python section of the course. Should you choose to redo the data cleaning in R, 
##  make sure to apply the same transformations as you will have to potentially compare the results.
##  (Note: Manual steps included dropping and renaming the columns as per the instructions in module 1.
##  Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 

## 1. Open your RStudio and start setting up your R environment. 
## 2. Open a new R script and import the turtle_review.csv data file, which you can download from 
##      Assignment: Predicting future outcomes. (Note: You can use the clean version of the data 
##      you saved as csv in module 1, or, can manually drop and rename the columns as per the instructions 
##      in module 1. Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 
## 3. Import all the required libraries for the analysis and view the data. 
## 4. Load and explore the data.
##    - View the head the data.
##    - Create a summary of the new data frame.
## 5. Perform exploratory data analysis by creating tables and visualisations to better understand 
##      groupings and different perspectives into customer behaviour and specifically how loyalty 
##      points are accumulated. Example questions could include:
##    - Can you comment on distributions, patterns or outliers based on the visual exploration of the data?
##    - Are there any insights based on the basic observations that may require further investigation?
##    - Are there any groupings that may be useful in gaining deeper insights into customer behaviour?
##    - Are there any specific patterns that you want to investigate
## 6. Create visualisations.
##    - Create scatterplots, histograms, and boxplots to visually explore the loyalty_points data.
##    - Select appropriate visualisations to communicate relevant findings and insights to the business.
## 7. Note your observations and recommendations to the technical and business users.

###############################################################################

## Prepare the workstation, import necessary libraries, read the csv file and 
## sense check the data.

# Import the necessary libraries.
library(tidyverse)
library(dplyr)
library(ggplot2)
library(skimr)
library(DataExplorer)

# Read the clean turtle_reviews.csv file.
reviews <- read.csv("reviews_clean.csv", header = TRUE)

# View and explore the data frame.
head(reviews)
dim(reviews)
glimpse(reviews)

# Determine the descriptive statistics.
summary(reviews)

# Create a data profile report.
DataExplorer::create_report(reviews)

###############################################################################

## Explore data distribution.

# Loyalty points distribution.
ggplot(reviews,
       aes(x = loyalty_points)) +
  geom_histogram(fill='lightblue', color='black') +
  scale_x_continuous(breaks=seq(0, 7000, 500), "Loyalty points") +
  scale_y_continuous(breaks=seq(0, 300, 50), "Number of customers") +
  labs(title = "Loyalty Points Distribution") +
  theme_minimal()

# Spending score distribution.
ggplot(reviews,
       aes(x = spending_score)) +
  geom_histogram(fill='palevioletred', color='black', bins = 20) +
  scale_x_continuous(breaks=seq(0, 100, 10), "Spending score") +
  scale_y_continuous(breaks=seq(0, 200, 50), "Number of customers") +
  labs(title = "Spending Score Distribution") +
  theme_minimal()

# Remuneration distribution.
ggplot(reviews,
       aes(x = remuneration)) +
  geom_histogram(fill='mediumpurple1', color='black', bins = 20) +
  scale_x_continuous(breaks=seq(0, 120, 10), "Remuneration") +
  scale_y_continuous(breaks=seq(0, 200, 50), "Number of customers") +
  labs(title = "Remuneration Distribution") +
  theme_minimal()

# Age distribution.
ggplot(reviews,
       aes(x = age)) +
  geom_histogram(fill='darkolivegreen4', color='black', bins = 20) +
  scale_x_continuous(breaks=seq(0, 80, 10), "Age") +
  scale_y_continuous(breaks=seq(0, 400, 50), "Number of customers") +
  labs(title = "Age Distribution") +
  theme_minimal()

# Gender distribution.
ggplot(reviews,
       aes(x = gender)) +
  geom_bar(fill='firebrick', color='black') +
  scale_y_continuous(breaks=seq(0, 1200, 100), "Number of customers") +
  labs(title = "Gender Distribution") +
  theme_minimal()

# Education distribution.
ggplot(reviews,
       aes(x = education)) +
  geom_bar(fill='gold', color='black') +
  scale_y_continuous(breaks=seq(0, 1000, 100), "Number of customers") +
  labs(title = "Education Distribution") +
  theme_minimal()

## - Majority of customers have accumulated roughly between 800 and 1750
##   loyalty points. Accumulation of loyalty points decrease significantly after 
##   a customer hits 1750, Turtle Games could target customers who sit in the 
##   majority range to ensure they continue to accumulate loyalty points.
## - Majority of customers have a spending score around 50.
## - Only a few customers earn more than £85,000.
## - Majority of customers are between 30 and 40 years old.
## - Majority of customers are female.
## - Majority of customers have a graduate degree.

###############################################################################

## Explore relationships between variables.

# Age vs loyalty points.
ggplot(reviews, 
       aes(x = age, 
           y = loyalty_points)) +
  geom_point(color = 'mediumpurple1',
             alpha = 0.5,  
             size = 1.5) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              size = 1,
              color = 'black') +
  scale_x_continuous(breaks = seq(0, 75, 5), "Age") +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  labs(title = "Age vs loyalty points") +
  theme_minimal()

# Remuneration vs loyalty points.
ggplot(reviews, 
       aes(x=remuneration, y=loyalty_points)) +
  geom_point(color = 'hotpink',
             alpha = 0.5,  
             size = 1.5) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              size = 1,
              color = 'black') +
  scale_x_continuous(breaks = seq(0, 120, 10), "Remuneration") +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  labs(title = "Remuneration vs loyalty points") +
  theme_minimal()

# Spending score vs loyalty points.
ggplot(reviews, 
       aes(x = spending_score, 
           y = loyalty_points)) +
  geom_point(color = 'mediumorchid1',
             alpha = 0.5,  
             size = 1.5) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              size = 1,
              color = 'black') +
  scale_x_continuous(breaks = seq(0, 100, 10), "Spending score") +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  labs(title = "Spending score vs loyalty points") +
  theme_minimal()

# Relation between age, loyalty_points, and education. 
ggplot(reviews,
       aes(x = age, 
           y = loyalty_points, 
           color = education)) +
  geom_point(alpha = 0.5, 
             size = 1.5) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              size = 1) +
  scale_x_continuous(breaks = seq(0, 75, 5), "Age") +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  scale_color_manual(values = c('lightseagreen', 'yellow', 'maroon2', 
                                'orange', 'royalblue1')) +
  labs(title = "Relationship between age, loyalty points, and education")

# Relation between remuneration, loyalty_points, and education. 
ggplot(reviews,
       aes(x = remuneration, 
           y = loyalty_points, 
           color = education)) +
  geom_point(alpha = 0.5, 
             size = 1.5) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              size = 1) +
  scale_x_continuous(breaks = seq(0, 120, 10), "Remuneration") +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  scale_color_manual(values = c('lightseagreen', 'yellow', 'maroon2', 
                                'orange', 'royalblue1')) +
  labs(title = "Relationship between remuneration, loyalty points, 
       and education")

# Relation between spending score, loyalty_points, and education. 
ggplot(reviews,
       aes(x = spending_score, 
           y = loyalty_points, 
           color = education)) +
  geom_point(alpha = 0.5, 
             size = 1.5) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              size = 1) +
  scale_x_continuous(breaks = seq(0, 100, 10), "Spending score") +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  scale_color_manual(values = c('lightseagreen', 'yellow', 'maroon2', 
                                'orange', 'royalblue1')) +
  labs(title = "Relationship between spending score, loyalty points, 
       and education")

# Distribution of loyalty points by gender.
ggplot(reviews, 
       aes(x = gender, 
           y = loyalty_points)) +
  geom_boxplot(fill = 'lightpink1',
               notch = TRUE,
               outlier.color = 'hotpink3') +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  labs(title = "Loyalty Points by Gender",
       x = "Gender") +
  theme_minimal()

# Distribution of loyalty points by education.
ggplot(reviews, 
       aes(x = education, 
           y = loyalty_points)) +
  geom_boxplot(fill = 'mediumpurple',
               notch = TRUE,
               outlier.color = 'purple4') +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  labs(title = "Loyalty Points by Education",
       x = "Education") +
  theme_minimal()

# Average loyalty points by education level.
ggplot(reviews, 
       aes(x = education, 
           y = loyalty_points)) +
  geom_bar(stat = "summary", 
           fun = "mean", 
           fill = "skyblue") +
  labs(x = "Education", 
       y = "Average Loyalty Points", 
       title = "Average Loyalty Points by Education") +
  theme_minimal()

# Average loyalty points by gender.
ggplot(reviews, 
       aes(x = gender, 
           y = loyalty_points)) +
  geom_bar(stat = "summary", 
           fun = "mean", 
           fill = "orange2") +
  labs(x = "Education", 
       y = "Average Loyalty Points", 
       title = "Average Loyalty Points by Gender") +
  theme_minimal()

# Compare spending score and loyalty points based on education.
ggplot(reviews,
       aes(x=spending_score, 
           y=loyalty_points)) +
  geom_point(color="royalblue1",
             alpha=0.75,
             size=1.5) +
  scale_x_continuous(breaks = seq(0, 100, 10), "Spending score") +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  labs(title = "Relationship Between Spending Score and Loyalty Points 
       Based on Education") +
  facet_wrap(~education)

# Compare spending score and loyalty points based on gender.
ggplot(reviews,
       aes(x=spending_score, 
           y=loyalty_points)) +
  geom_point(color="maroon1",
             alpha=0.75,
             size=1.5) +
  scale_x_continuous(breaks = seq(0, 100, 10), "Spending score") +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  labs(title = "Relationship Between Spending Score and Loyalty Points 
       Based on Gender") +
  facet_wrap(~gender)

# Compare remuneration and loyalty points based on education.
ggplot(reviews,
       aes(x=remuneration, 
           y=loyalty_points)) +
  geom_point(color="darkolivegreen",
             alpha=0.75,
             size=1.5) +
  scale_x_continuous(breaks = seq(0, 120, 10), "Remuneration") +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  labs(title = "Relationship Between Remuneration and Loyalty Points 
       Based on Education") +
  facet_wrap(~education)

# Compare remuneration and loyalty points based on gender.
ggplot(reviews,
       aes(x=remuneration, 
           y=loyalty_points)) +
  geom_point(color="darkorange",
             alpha=0.75,
             size=1.5) +
  scale_x_continuous(breaks = seq(0, 120, 10), "Remuneration") +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  labs(title = "Relationship Between Remuneration and Loyalty Points 
       Based on Gender") +
  facet_wrap(~gender)

# Compare age and loyalty points based on education.
ggplot(reviews,
       aes(x=age, 
           y=loyalty_points)) +
  geom_point(color="firebrick",
             alpha=0.75,
             size=1.5) +
  scale_x_continuous(breaks = seq(0, 80, 10), "Age") +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  labs(title = "Relationship Between Age and Loyalty Points 
       Based on Education") +
  facet_wrap(~education)

# Compare age and loyalty points based on gender
ggplot(reviews,
       aes(x=age, 
           y=loyalty_points)) +
  geom_point(color="gold",
             alpha=0.75,
             size=1.5) +
  scale_x_continuous(breaks = seq(0, 80, 10), "Age") +
  scale_y_continuous(breaks = seq(0, 7000, 1000), "Loyalty points") +
  labs(title = "Relationship Between Age and Loyalty Points 
       Based on Gender") +
  facet_wrap(~gender)

## - Based on the analysis outliers are can be detected in the data set, 
##   particularly for loyalty points.
## - Older customers accumulate less loyalty points. 
## - Customers who have higher spending scores accumulate more loyalty points.
## - Customers who have higher remuneration accumulate more loyalty points.
## - Customers who have a graduate degree continue to accumulate more 
##   loyalty points as they age, whereas customers who have a basic education
##   accumulate significantly less loyalty points as they age.
## - As remuneration increases, the accumulation of loyalty points increases
##   across all education levels.
## - As spending score increases, the accumulation of loyalty points increases
##   across all education levels.
## - Female customers have accumulated the highest number of loyalty points.
## - An average accumulation of loyalty points is around 1500.
## - A noticeable number of customers who earn high salaries accumulate 
##   surprisingly few loyalty points.

###############################################################################

## Clustering with k-means using R.

# Create a new data frame containing the remuneration and 
# spending score columns.
reviews1 <- reviews[, c("remuneration", "spending_score")]
head(reviews1)

# Create a scatterplot using remuneration and spending score.
ggplot(reviews1, 
       aes(x = remuneration,
           y = spending_score)) +
  geom_point() +
  scale_x_continuous(breaks=seq(0, 120, 10), "Remuneration") +
  scale_y_continuous(breaks=seq(0, 100, 10), "Spending score") +
  labs(title = "Remuneration vs spending score")

# Scale the data.
reviews1_scale <- scale(reviews1)

# View the output.
head(reviews1_scale)

# Calculate distance metrics between observations.
reviews2 <- dist(reviews1_scale)

# View the output.
head(reviews2)

# Select the optimal number of clusters (k).
fviz_nbclust(reviews1_scale, 
             kmeans, 
             method = "wss") +
  labs(subtitle = "Elbow method")

# Create the model.
model <- kmeans(reviews1_scale, 
                centers = 5, 
                nstart = 100)

# View the output.
print(model)

# Visualise the clusters.
fviz_cluster(model, 
             data = reviews1_scale) +
  labs(x = "Remuneration", 
       y = "Spending score",
       subtitle = "5 clusters") +
  theme_minimal()

## - 5 clusters can be identified in the data, and these clusters can be 
##   identified as different customer profiles.

###############################################################################

## Explore the most popular products. 

# A quick overview of the product data.
table(reviews$product)
names(product_count[product_count == 10])

## Note that there are almost 200 products that have been purchased 10 times. 

# Create a new variable to store product count.
product_count <- table(reviews$product)

# Sort the products by popularity.
top_products <- sort(product_count, decreasing = TRUE)

# Limit top 10 products.
top_10_products <- head(top_products, 10)

# View the output.
print(top_10_products)

# Convert the top 10 products into a data frame.
top_10_df <- data.frame(product = names(top_10_products), 
                        count = as.numeric(top_10_products))

# Reorder the levels of the products based on frequency counts.
top_10_df$product <- factor(top_10_df$product, 
                         levels = top_10_df$product[order(-top_10_df$count)])

# Create a barplot using ggplot2
ggplot(top_10_df, 
       aes(x = product, 
           y = count)) +
  geom_bar(stat = "identity", 
           fill = "indianred2") +
  scale_y_continuous(breaks=seq(0, 14, 2)) +
  labs(title = "Top 10 Most Popular Products",
       x = "Product",
       y = "Count") +
  theme_minimal()

# Sort the products by least popular.
bottom_products <- sort(product_count)

# Limit bottom 10 products.
bottom_10_products <- head(bottom_products, 10)

# View the output.
print(bottom_10_products)

# Convert the bottom 10 products into a data frame.
bottom_10_df <- data.frame(product = names(bottom_10_products), 
                        count = as.numeric(bottom_10_products))

# Reorder the levels of the products based on frequency counts.
bottom_10_df$product <- factor(bottom_10_df$product, 
                            levels = bottom_10_df$product
                            [order(-bottom_10_df$count)])

# Create a barplot using ggplot2
ggplot(bottom_10_df, 
       aes(x = product, 
           y = count)) +
  geom_bar(stat = "identity", 
           fill = "dodgerblue3") +
  scale_y_continuous(breaks=seq(0, 14, 2)) +
  labs(title = "Bottom 10 Least Popular Products",
       x = "Product",
       y = "Count") +
  theme_minimal()

## Explore the most popular product by gender.

# Group by gender and product.
product_count_by_gender <- reviews %>%
  group_by(gender, product) %>%
  summarize(count = n()) %>%
  ungroup()

# Determine the most popular product by gender.
most_popular_product_by_gender <- product_count_by_gender %>%
  group_by(gender) %>%
  summarize(most_popular_product_by_gender = product[which.max(count)])

# View the output.
print(most_popular_product_by_gender)

## Exploring the most popular product by education.

# Group by education and product.
product_count_by_education <- reviews %>%
  group_by(education, product) %>%
  summarize(count = n()) %>%
  ungroup()

# Determine the most popular product by gender.
most_popular_product_by_education <- product_count_by_education %>%
  group_by(education) %>%
  summarize(most_popular_product_by_education = product[which.max(count)])

# View the output.
print(most_popular_product_by_education)

## - Product 1012, 1031, 979 and 977 are the most popular and have been purchased 
##   13, 13, 12, and 11 times respectively. It is important to take into 
##   consideration that there are almost 200 products following with 10 purchases 
##   respectively, and further information would be necessary to determine 
##   popularity order. 
## - Product 466, 453, 254, 1463, 1459 and 263 are the least popular and have 
##   been purchased 8, 8, 8, 9, 9, and 9 times respectively. It is important to 
##   take into consideration that there are almost 200 products following with 
##   10 purchases respectively, and further information would be necessary to 
##   determine popularity order. 
## - The most popular product for men is 7141 and for women 10995.
## - The most popular product for basic education is 6504, PhD 1970, 
##   diploma 504, graduate 291, and postgraduate 326.

###############################################################################
###############################################################################
###############################################################################

# Assignment 6 scenario

## In Module 5, you were requested to redo components of the analysis using Turtle Games’s preferred 
## language, R, in order to make it easier for them to implement your analysis internally. As a final 
## task the team asked you to perform a statistical analysis and create a multiple linear regression 
## model using R to predict loyalty points using the available features in a multiple linear model. 
## They did not prescribe which features to use and you can therefore use insights from previous modules 
## as well as your statistical analysis to make recommendations regarding suitability of this model type,
## the specifics of the model you created and alternative solutions. As a final task they also requested 
## your observations and recommendations regarding the current loyalty programme and how this could be 
## improved. 

################################################################################

## Assignment 6 objective
## You need to investigate customer behaviour and the effectiveness of the current loyalty program based 
## on the work completed in modules 1-5 as well as the statistical analysis and modelling efforts of module 6.
##  - Can we predict loyalty points given the existing features using a relatively simple MLR model?
##  - Do you have confidence in the model results (Goodness of fit evaluation)
##  - Where should the business focus their marketing efforts?
##  - How could the loyalty program be improved?
##  - How could the analysis be improved?

################################################################################

## Assignment 6 assignment: Making recommendations to the business.

## 1. Continue with your R script in RStudio from Assignment Activity 5: Cleaning, manipulating, and 
##     visualising the data.
## 2. Load and explore the data, and continue to use the data frame you prepared in Module 5.
## 3. Perform a statistical analysis and comment on the descriptive statistics in the context of the 
##     review of how customers accumulate loyalty points.
##  - Comment on distributions and patterns observed in the data.
##  - Determine and justify the features to be used in a multiple linear regression model and potential
##.    concerns and corrective actions.
## 4. Create a Multiple linear regression model using your selected (numeric) features.
##  - Evaluate the goodness of fit and interpret the model summary statistics.
##  - Create a visual demonstration of the model
##  - Comment on the usefulness of the model, potential improvements and alternate suggestions that could 
##     be considered.
##  - Demonstrate how the model could be used to predict given specific scenarios. (You can create your own 
##     scenarios).
## 5. Perform exploratory data analysis by using statistical analysis methods and comment on the descriptive 
##     statistics in the context of the review of how customers accumulate loyalty points.
## 6. Document your observations, interpretations, and suggestions based on each of the models created in 
##     your notebook. (This will serve as input to your summary and final submission at the end of the course.)

################################################################################

## Prepare the workstation, import necessary libraries, and sense check the data.

# Import the necessary libraries.
library(moments)
library(psych)

# View and explore the data frame.
head(reviews)
dim(reviews)
glimpse(reviews)

# Determine the descriptive statistics.
summary(reviews)

###############################################################################

## Perform statistical analysis.

# Age.
summary(reviews$age)
mean(reviews$age)
median(reviews$age)
min(reviews$age)
max(reviews$age)
quantile(reviews$age, 0.25)
quantile(reviews$age, 0.75)
var(reviews$age)
sd(reviews$age)

boxplot(reviews$age)
hist(reviews$age)

qqnorm(reviews$age)
qqline(reviews$age)

shapiro.test(reviews$age)
skewness(reviews$age)
kurtosis(reviews$age)

## - Age is not normally distributed.
## - Age is right skewed.
## - Age has a light tailed distribution, which means that there is a 
##   lower probability of extreme outlier values.

# Remuneration.
summary(reviews$remuneration)
mean(reviews$remuneration)
median(reviews$remuneration)
min(reviews$remuneration)
max(reviews$remuneration)
quantile(reviews$remuneration, 0.25)
quantile(reviews$remuneration, 0.75)
var(reviews$remuneration)
sd(reviews$remuneration)

boxplot(reviews$remuneration)
hist(reviews$remuneration)

qqnorm(reviews$remuneration)
qqline(reviews$remuneration)

shapiro.test(reviews$remuneration)
skewness(reviews$remuneration)
kurtosis(reviews$remuneration)

## - Remuneration is not normally distributed.
## - Remuneration is right skewed.
## - Remuneration has a light tailed distribution, which means that there is a 
##   lower probability of extreme outlier values.

# Spending score.
summary(reviews$spending_score)
mean(reviews$spending_score)
median(reviews$spending_score)
min(reviews$spending_score)
max(reviews$spending_score)
quantile(reviews$spending_score, 0.25)
quantile(reviews$spending_score, 0.75)
var(reviews$spending_score)
sd(reviews$spending_score)

boxplot(reviews$spending_score)
hist(reviews$spending_score)

qqnorm(reviews$spending_score)
qqline(reviews$spending_score)

shapiro.test(reviews$spending_score)
skewness(reviews$spending_score)
kurtosis(reviews$spending_score)

## - Spending score is not normally distributed.
## - Spending score is slightly left skewed.
## - Spending score has a light tailed distribution, which means that there is a 
##   lower probability of extreme outlier values.

# Loyalty points.
summary(reviews$loyalty_points)
mean(reviews$loyalty_points)
median(reviews$loyalty_points)
min(reviews$loyalty_points)
max(reviews$loyalty_points)
quantile(reviews$loyalty_points, 0.25)
quantile(reviews$loyalty_points, 0.75)
var(reviews$loyalty_points)
sd(reviews$loyalty_points)

boxplot(reviews$loyalty_points)
hist(reviews$loyalty_points)

qqnorm(reviews$loyalty_points)
qqline(reviews$loyalty_points)

shapiro.test(reviews$loyalty_points)
skewness(reviews$loyalty_points)
kurtosis(reviews$loyalty_points)

## - Loyalty points is not normally distributed.
## - Loyalty points is right skewed.
## - Loyalty points has a heavy tailed distribution, which means that there is a 
##   higher probability of extreme outlier values.

###############################################################################

## Perform multiple linear regression.

# Create a new data frame containing the age, remuneration, spending_score and
# loyalty_points columns.
reviews3 <- reviews[, c("age", "remuneration", "spending_score", 
                        "loyalty_points")]

# View the output.
head(reviews3)

# Create a plot to examine the data.
plot(reviews3)

# Determine correlation between variables.
cor(reviews3)

# Visualise the correlation.
corPlot(reviews3, cex=2)

## - Spending score and remuneration have relatively strong correlation
##   (0.67 and 0.62 respectively with loyalty points. 
## - The is a slight negative correlation between age and loyalty points.

# Create a model and specify the lm function and the variables. In this model
# the explanatory variables are remuneration and spending_score.
model1 = lm(loyalty_points ~ remuneration + spending_score, 
            data = reviews3)

# Views the output.
summary(model1)

# Create a model new and specify the lm function and the variables. In this 
# model the explanatory variables are age, remuneration, and spending_score.
model2 = lm(loyalty_points ~ age + remuneration + spending_score, 
            data = reviews3)

# View the output.
summary(model2)

## - Model2 has a higher Adjusted R-squared, indicating better performance. 
##   Since there are no separate training and test data sets, new data can 
##   be generated for the independent variables to make predictions 
##   about loyalty points.

# Generate new data for independent variables to make predictions. 
test_data <- data.frame(
  age = abs(runif(100, 
                  min = min(reviews3$age), 
                  max = max(reviews3$age))),
  remuneration = abs(runif(100, 
                           min = min(reviews3$remuneration), 
                           max = max(reviews3$remuneration))),
  spending_score = abs(runif(100, 
                             min = min(reviews3$spending_score), 
                             max = max(reviews3$spending_score))))

# View output.
head(test_data)

# Predict loyalty points for the new data.
predictions <- predict(model2, 
                       newdata = test_data)

# View output.
print(predictions)

## - In a linear regression model the assumption is that residuals are normally 
##   distributed with constant variance. If the dependent variable is highly 
##   skewed, as in this case loyalty points (1.46) could be considered to be, 
##   it might violate the assumption of normality leading to incorrect 
##   inference and biased parameter estimates. A highly skewed dependent 
##   variable can affect model performance and accuracy, and transforming  
##   the dependent variable could help mitigate the effects of skewness.


###############################################################################
###############################################################################
