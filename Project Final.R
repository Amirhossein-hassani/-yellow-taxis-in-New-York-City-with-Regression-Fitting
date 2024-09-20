# Load the libraries
library(dplyr)
library(tidyr)
library(readxl)
library(plotly)
library(data.table)
library(tidyverse)
library(lubridate)
library(nortest)
library(glmnet)
library(forecast)
library(tseries)
library(randomForest)
library(gridExtra)
library(farver)
library(pheatmap)
library(ggplot2)
library(leaps)
library(sandwich)
library(lmtest)




data <- read.csv("C:/Users/ASUS/Desktop/Uni/Regression Analysis/Project/Taxi Datset.csv")

#select data

set.seed(123) # Setting a seed for reproducibility
random_indices <- sample(1:nrow(data), 100000)

# Step 3: Subset your data frame using the random indices
data <- data[random_indices, ]

 



#CLEAN


#deleting columns which do not help us
#data <- data %>% select(-improvement_surcharge)
#data <- data %>% select(-congestion_surcharge)
#data <- data %>% select(-tolls_amount)
#data <- data %>% select(-mta_tax)
#data <- data %>% select(-store_and_fwd_flag)
#data <- data %>% select(-RatecodeID)


data = data[,-c(6,7,13,15,16,18)]


# Remove duplicate rows
data <- data %>% distinct()

# Remove rows with any missing values
data <- na.omit(data)

#Filtering Data
#remove negative data
data <- data %>%
  filter(trip_distance >= 0, tip_amount>= 0, total_amount>= 0, extra>= 0)



# View the structure of the data
str(data)
# Summary statistics
summary(data)

# Load necessary libraries
install.packages("fitdistrplus")
library(fitdistrplus)

# Visual Inspection
hist(data$total_amount, main="Histogram of Var1", xlab="Var1", col="blue")
plot(density(data$total_amount), main="Density Plot of Var1", xlab="Var1")

# Summary Statistics
summary(data$total_amount)

# Normality Test
ks.test(data$total_amount, "pnorm", mean(data$total_amount), sd(data$total_amount))

# Fit distributions
fit_normal <- fitdist(data$total_amount, "norm")
fit_exp <- fitdist(data$total_amount, "exp")

# Compare goodness-of-fit
plot(fit_normal)
plot(fit_exp)
gofstat(list(fit_normal, fit_exp))

# Q-Q Plot
qqnorm(data$total_amount)
qqline(data$total_amount, col = "red")

# Visual Inspection
hist(data$fare_amount, main="Histogram of Var1", xlab="Var1", col="blue")
plot(density(data$fare_amount), main="Density Plot of Var1", xlab="Var1")

# Summary Statistics
summary(data$fare_amount)

# Normality Test
ks.test(data$fare_amount, "pnorm", mean(data$fare_amount), sd(data$fare_amount))

# Fit distributions
fit_normal <- fitdist(data$fare_amount, "norm")
fit_exp <- fitdist(data$fare_amount, "exp")

# Compare goodness-of-fit
plot(fit_normal)
plot(fit_exp)
gofstat(list(fit_normal, fit_exp))

# Q-Q Plot
qqnorm(data$fare_amount)
qqline(data$fare_amount, col = "red")

# Visual Inspection
hist(data$tip_amount, main="Histogram of Var1", xlab="Var1", col="blue")
plot(density(data$tip_amount), main="Density Plot of Var1", xlab="Var1")

# Summary Statistics
summary(data$tip_amount)

# Normality Test
ks.test(data$tip_amount, "pnorm", mean(data$tip_amount), sd(data$tip_amount))

# Fit distributions
fit_normal <- fitdist(data$tip_amount, "norm")
fit_exp <- fitdist(data$tip_amount, "exp")

# Compare goodness-of-fit
plot(fit_normal)
plot(fit_exp)
gofstat(list(fit_normal, fit_exp))

# Q-Q Plot
qqnorm(data$tip_amount)
qqline(data$tip_amount, col = "red")



######################### Identify outliers using the IQR method and removing them ##########################
#The code worked but identifying outliers with this method was not a good idea

#trip_distance
#Q1 <- quantile(data$trip_distance, 0.25)
#Q3 <- quantile(data$trip_distance, 0.75)
#IQR <- Q3 - Q1

#outliers_trip_distance <- data %>%
# filter(trip_distance < (Q1 - 1.5 * IQR) | trip_distance > (Q3 + 1.5 * IQR))

# Remove outliers
#data <- data[data$trip_distance >= (Q1 - 1.5 * IQR) & data$trip_distance <= (Q3 + 1.5 * IQR), ]


#tip_amount
#Q1 <- quantile(data$tip_amount, 0.25)
#Q3 <- quantile(data$tip_amount, 0.75)
#IQR <- Q3 - Q1

#outliers_tip_amount <- data %>%
#  filter(tip_amount < (Q1 - 1.5 * IQR) | tip_amount > (Q3 + 1.5 * IQR))
# Remove outliers
#data <- data[data$tip_amount >= (Q1 - 1.5 * IQR) & data$tip_amount <= (Q3 + 1.5 * IQR), ]


#tolls_amount
#Q1 <- quantile(data$tolls_amount, 0.25)
#Q3 <- quantile(data$tolls_amount, 0.75)
#IQR <- Q3 - Q1

#outliers_tolls_amount <- data %>%
#  filter(tolls_amount < (Q1 - 1.5 * IQR) | tolls_amount > (Q3 + 1.5 * IQR))
# Remove outliers
#data <- data[data$tolls_amount >= (Q1 - 1.5 * IQR) & data$tolls_amount <= (Q3 + 1.5 * IQR), ]


#total_amount
#Q1 <- quantile(data$total_amount, 0.25)
#Q3 <- quantile(data$total_amount, 0.75)
#IQR <- Q3 - Q1

#outliers_total_amount <- data %>%
#  filter(total_amount < (Q1 - 1.5 * IQR) | total_amount > (Q3 + 1.5 * IQR))
# Remove outliers
#data <- data[data$total_amount >= (Q1 - 1.5 * IQR) & data$total_amount <= (Q3 + 1.5 * IQR), ]
#------------------------------------------------------------

#congestion_surcharge
#Q1 <- quantile(data$congestion_surcharge, 0.25)
#Q3 <- quantile(data$congestion_surcharge, 0.75)
#IQR <- Q3 - Q1

#outliers_congestion_surcharge <- data %>%
#  filter(congestion_surcharge < (Q1 - 1.5 * IQR) | congestion_surcharge > (Q3 + 1.5 * IQR))
# Remove outliers
#data <- data[data$congestion_surcharge >= (Q1 - 1.5 * IQR) & data$congestion_surcharge <= (Q3 + 1.5 * IQR), ]

# improvement_surcharge
#Q1 <- quantile(data$ improvement_surcharge, 0.25)
#Q3 <- quantile(data$ improvement_surcharge, 0.75)
#IQR <- Q3 - Q1

#outliers_improvement_surcharge <- data %>%
#  filter( improvement_surcharge < (Q1 - 1.5 * IQR) |  improvement_surcharge > (Q3 + 1.5 * IQR))
# Remove outliers
#data <- data[data$improvement_surcharge >= (Q1 - 1.5 * IQR) & data$improvement_surcharge <= (Q3 + 1.5 * IQR), ]

#extra
#Q1 <- quantile(data$extra, 0.25)
#Q3 <- quantile(data$extra, 0.75)
#IQR <- Q3 - Q1

#outliers_extra <- data %>%
#  filter(extra < (Q1 - 1.5 * IQR) | extra > (Q3 + 1.5 * IQR))
# Remove outliers
#data <- data[data$extra >= (Q1 - 1.5 * IQR) & data$extra <= (Q3 + 1.5 * IQR), ]



#############################################################################################################

#load the lubridate package
library(lubridate)


# Convert the tpep_pickup_datetime column to a datetime object
data <- data %>%
  mutate(tpep_pickup_datetime = mdy_hms(tpep_pickup_datetime))

# Extract day, month, year, and rounded hour
data <- data %>%
  mutate(
    day_p = day(tpep_pickup_datetime),
    month_p = month(tpep_pickup_datetime),
    year_d = year(tpep_pickup_datetime),
    hour_p = hour(tpep_pickup_datetime) 
  )


data$hour_p <- as.character(data$hour_p)
data$month_p <- as.character(data$month_p)
data$day_p <- as.character(data$day_p)



data <- data %>%
  mutate(tpep_dropoff_datetime = mdy_hms(tpep_dropoff_datetime))

# Extract day, month, year, and rounded hour
data <- data %>%
  mutate(
    day_d = day(tpep_dropoff_datetime),
    month_d = month(tpep_dropoff_datetime),
    year_d = year(tpep_dropoff_datetime),
    hour_d = hour(tpep_dropoff_datetime) 
  )


data$hour_d <- as.character(data$hour_d)
data$month_d <- as.character(data$month_d)
data$day_d <- as.character(data$day_d)




############################################# graph Residual #########################

############################################# Graphs #################################

# Visual Inspection
hist(data$passenger_count, main="Histogram of passenger_count", xlab="passenger_count", col="blue")

##############

vendor_totals <- data %>%
  group_by(VendorID) %>%
  summarize(total_amount = sum(total_amount, na.rm = TRUE))
# Create the bar plot using ggplot2
ggplot(vendor_totals, aes(x = factor(VendorID), y = total_amount)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Amount by Vendor ID",
       x = "Vendor ID",
       y = "Total Amount") +
  theme_minimal()

##############

# Aggregate the total amount by PULocationID
pulocation_totals <- data %>%
  group_by(PULocationID) %>%
  summarize(total_amount = sum(total_amount, na.rm = TRUE))

s_pul = pulocation_totals[order(pulocation_totals$total_amount, decreasing = TRUE),]

# Create the bar plot using ggplot2
ggplot(s_pul[1:30,], aes(x = factor(PULocationID), y = total_amount)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Amount by Pickup Location ID",
       x = "Pickup Location ID",
       y = "Total Amount") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#############

# Calculate total sum across all payment types
total_sum <- data %>% summarise(total_all = sum(total_amount, na.rm = TRUE))

# Add percentages to each bar
payment_totals <- data %>%
  group_by(payment_type) %>%
  summarize(total_amount = sum(total_amount, na.rm = TRUE),
            percentage = (sum(total_amount, na.rm = TRUE) / total_sum$total_all) * 100)

# Create the circular plot using ggplot2
ggplot(payment_totals, aes(x = "", y = total_amount, fill = payment_type)) +
  geom_bar(stat = "identity", width = 1) + # Set width to 1 for proper circular representation
  coord_polar("y") + # Convert to circular plot
  labs(title = "Total Amount by Payment Type",
       fill = "Payment Type") +
  theme_void() + # Use minimal theme for a clean look
  theme(legend.position = "bottom") + # Position the legend at the bottom
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white") # Add percentages above bars


###############


monthly_totals <- data %>%
  group_by(month_p) %>%
  summarize(total_amount = sum(total_amount, na.rm = TRUE))

monthly_totals$percent = monthly_totals$total_amount/sum(monthly_totals$total_amount) *100


# Create the pie chart using ggplot2
ggplot(monthly_totals, aes(x = "", y = percent, fill = factor(month_p))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") + # Using a pastel color palette for a softer look
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5), size = 4, color = "black") + # Adding labels with percentages
  labs(title = "Monthly Totals as Percentage of Total", x = NULL, y = NULL) +
  theme(legend.position = "right") # Positioning the legend outside the plot for better visibility


###############


daily_totals <- data %>%
  group_by(day_p) %>%
  summarize(total_amount = sum(total_amount, na.rm = TRUE))

# Create the bar plot using ggplot2
ggplot(daily_totals, aes(x = as.numeric(day_p), y = total_amount)) +
  geom_point(stat = "identity", fill = "steelblue") + geom_line(color='red') +
  labs(title = "Total Amount by day",
       x = "day",
       y = "Total Amount") +
  theme_bw()


###################

hourly_totals <- data %>%
  group_by(hour_p) %>%
  summarize(total_amount = sum(total_amount, na.rm = TRUE))

# Create the bar plot using ggplot2
ggplot(hourly_totals, aes(x = as.numeric(hour_p), y = total_amount)) +
  geom_point(stat = "identity", fill = "steelblue") + geom_line(color='red') +
  labs(title = "Total Amount by hour",
       x = "hour",
       y = "Total Amount") +
  theme_minimal()



#################


############################################ convert categorical to dummy #########################################


dummy_data <- model.matrix(~ VendorID - 1, data = data)
data <- cbind(data, dummy_data)
data = data[,-1]
data = data[,-20]


data$payment_type <- as.character(data$payment_type)
dummy_data <- model.matrix(~ payment_type - 1, data = data)
data <- cbind(data, dummy_data)
data <- data[,-7]
data <- data[,-22]


dummy_data <- model.matrix(~ hour_d - 1, data = data)
data <- cbind(data, dummy_data)
data <- data[,-17]
data <- data[,-21]


#dummy_data <- model.matrix(~ month_d - 1, data = data)
#data <- cbind(data, dummy_data)
data <- data[,-16]
#data <- data %>% select(-month_d01)


#dummy_data <- model.matrix(~ day_d - 1, data = data)
#data <- cbind(data, dummy_data)
data <- data[,-15]
#data <- data %>% select(-day_d01)


dummy_data <- model.matrix(~ hour_p - 1, data = data)
data <- cbind(data, dummy_data)
data <- data[,-14]
data <- data [,-42]



#dummy_data <- model.matrix(~ month_p - 1, data = data)
#data <- cbind(data, dummy_data)
data <- data[,-12]
#data <- data %>% select(-month_p01)


#dummy_data <- model.matrix(~ day_p - 1, data = data)
#data <- cbind(data, dummy_data)
data <- data [,-11]
#data <- data %>% select(-day_p01)


data <- data[,-2]
data <- data[,-1]

#
data <- data[,-3]
data <- data[,-4]
#

####################################### Build the regression model ################################

# Correlation Matrix
correlation_matrix <- cor(data)


pheatmap(correlation_matrix,
         color = colorRampPalette(c("blue", "white", "red"))(100), # smoother gradient
         clustering_method = "complete",
         display_numbers = TRUE, # show correlation values in the cells
         number_format = "%.2f", # format numbers to two decimal places
         fontsize_number = 3, # font size of the numbers
         fontsize =6, # font size of the labels
         fontsize_row = 6, # font size of the row labels
         fontsize_col = 6, # font size of the column labels
         cellwidth = 10, # width of each cell
         cellheight = 10, # height of each cell
         border_color = "grey60", # color of the cell borders
         main = "Correlation Matrix Heatmap" # title of the heat map
)


#As we can see in correlation matrix every hour in pickup is highly correlated to hours in dropoff and this is totally rational
data = data[,-c(12:34)]

model <- lm(total_amount ~ ., data = data)
summary(model)

# Compute VIF
library(car)

vif_values = car::vif(model)

# Print VIF values
print(vif_values)


# because of the big amount of VIF we omit payment_type1
data <- data [,-9]


model <- lm(total_amount ~ ., data = data)
summary(model)

data <- data[,-7]

vif_values = car::vif(model)
print(vif_values)


########################################################################################

########################################### BOX - COX ################################################


# Step 1: Install and load the MASS package
install.packages("MASS")
library(MASS)

# Assuming your data frame is named data and it contains the 'total_amount' column
# Step 2: Perform the Box-Cox transformation
# First, we need to ensure 'total_amount' is positive, as Box-Cox transformation requires positive values.
# If 'total_amount' contains non-positive values, shift the data by adding a constant to make all values positive.

if (any(data$total_amount <= 0)) {
  shift <- abs(min(data$total_amount)) + 1
  data$total_amount <- data$total_amount + shift
} else {
  shift <- 0
}

# Apply the Box-Cox transformation and find the best lambda
boxcox_result <- boxcox(total_amount ~ 1, data = data, lambda = seq(-2, 2, by = 0.1))
best_lambda <- boxcox_result$x[which.max(boxcox_result$y)]

# Step 3: Apply the transformation to the 'total_amount' column
data$total_amount_boxcox <- (data$total_amount^best_lambda - 1) / best_lambda

# If you added a shift, you might want to keep track of it for inverse transformation
print(paste("Best lambda:", best_lambda))
if (shift != 0) {
  print(paste("Shift applied to total_amount:", shift))
}


###########################################################################################
boxcox_model <- lm(total_amount_boxcox ~ ., data = data)
summary(model)

# Calculate residuals
residuals = residuals(boxcox_model)

# Plot residuals vs. predicted values
plot(fitted(boxcox_model), residuals, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs. Fitted")
abline(h=0)                # Add horizontal line


# Normal probability plot of residuals
qqnorm(residuals, main = "Normal Probability Plot of Residuals")

###########################################################################################

# 4. If still heteroscedastic, try log transformation
data$log_total_amount <- log(data$total_amount + 1)
log_model <- lm(log_total_amount ~ ., data = data)

plot(log_model, which = 1)  # Residual vs Fitted plot

residuals = residuals(log_model)
qqnorm(residuals, main = "Normal Probability Plot of Residuals")
outlier_indices = which(residuals > 0.5 * max(residuals) )
print(outlier_indices)

leverage = hatvalues(log_model)
plot(1:length(leverage), leverage, xlab="Observation Index", ylab="Leverage", main="Leverage VS. Observation Index")

#There is only one outlier in "Leverage VS. Observation Index" plot
outlier_indices1 = which(leverage > 0.5 * max(leverage) )
print(outlier_indices1)
#######################################################################

#Using 1/y transformation:
data$reversed_total_amount <- 1/(data$total_amount + 1)
reversed_model <- lm(reversed_total_amount ~ ., data = data)

plot(reversed_model, which = 1)  # Residual vs Fitted plot

residuals = residuals(reversed_model)
qqnorm(residuals, main = "Normal Probability Plot of Residuals")

leverage = hatvalues(reversed_model)
plot(1:length(leverage), leverage, xlab="Observation Index", ylab="Leverage", main="Leverage VS. Observation Index")
#######################################################################

#Using log(1/y) transformation
data$log_reversed_total_amount <- log(1/(data$total_amount + 1))
log_reversed_model <- lm(log_reversed_total_amount ~ ., data = data)

plot(log_reversed_model, which = 1)  # Residual vs Fitted plot
residuals = residuals(log_reversed_model)
qqnorm(residuals, main = "Normal Probability Plot of Residuals")

leverage = hatvalues(log_reversed_model)
plot(1:length(leverage), leverage, xlab="Observation Index", ylab="Leverage", main="Leverage VS. Observation Index")
########################################################################

#Yeo-Johnson Transformation
install.packages("VGAM") 
library(VGAM)
data$YJ_total_amount <-yeo.johnson(data$total_amount + 1, lambda = 0.5)
YJ_model <- lm(YJ_total_amount ~ ., data = data)

plot(YJ_model, which = 1)  # Residual vs Fitted plot
residuals = residuals(YJ_model)
qqnorm(residuals, main = "Normal Probability Plot of Residuals")

leverage = hatvalues(YJ_model)
plot(1:length(leverage), leverage, xlab="Observation Index", ylab="Leverage", main="Leverage VS. Observation Index")
########################################################################

data$exp_total_amount <-exp(data$total_amount + 1)
exp_model <- lm(exp_total_amount ~ ., data = data)

plot(exp_model, which = 1)  # Residual vs Fitted plot

residuals = residuals(exp_model)
qqnorm(residuals, main = "Normal Probability Plot of Residuals")

leverage = hatvalues(exp_model)
plot(1:length(leverage), leverage, xlab="Observation Index", ylab="Leverage", main="Leverage VS. Observation Index")
######################################################################

#Best model was log_normal_model in which few outliers exist. Indices for outliers are 
#either shown in their plot or printed.
#index 36 is outlier in both "Residual vs Fitted plot" and QQ-Plot so we remove it

data <- data[-c(36), ] #????

data$log_total_amount <- log(data$total_amount + 1)
log_model <- lm(log_total_amount ~ ., data = data)

plot(log_model, which = 1)  # Residual vs Fitted plot

#QQ-plot does not look quite normal
residuals = residuals(log_model)
qqnorm(residuals, main = "Normal Probability Plot of Residuals")

leverage = hatvalues(log_model)
plot(1:length(leverage), leverage, xlab="Observation Index", ylab="Leverage", main="Leverage VS. Observation Index")









###################################################### Hypothesis and F-test about hours ################################################
#Using log(1/y) transformation
data$log_reversed_total_amount <- log(1/(data$total_amount + 1))
log_reversed_model <- lm(log_reversed_total_amount ~ ., data = data)



# Define the hypothesis: All equal to each other

# Define the hypothesis
hypothesis <- c("hour_p0 = hour_p2", "hour_p2 = hour_p3", "hour_p3 = hour_p4", 
                "hour_p4 = hour_p5" ,"hour_p5 = hour_p6","hour_p6 = hour_p7",
                "hour_p7 = hour_p8","hour_p8 = hour_p9","hour_p9 = hour_p10",
                "hour_p10 = hour_p11","hour_p11 = hour_p12","hour_p12 = hour_p13",
                "hour_p13 = hour_p14","hour_p14 = hour_p15","hour_p15 = hour_p16",
                "hour_p16 = hour_p17","hour_p17 = hour_p18","hour_p18 = hour_p19",
                "hour_p19 = hour_p20","hour_p20 = hour_p21","hour_p21 = hour_p22",
                "hour_p22 = hour_p23")

# Perform the F-test
f_test_result <- linearHypothesis(log_reversed_model, hypothesis)

# Print the results
print(f_test_result)



############################################ doing all of the steps fpr grooup of hours not all of them ###############################################
#Using log(1/y) transformation
data$log_reversed_total_amount <- log(1/(data$total_amount + 1))
log_reversed_model <- lm(log_reversed_total_amount ~ ., data = data)



# Define the hypothesis
hypothesis <- c("hour_p0 = hour_p2", "hour_p2 = hour_p3", "hour_p3 = hour_p4", "hour_p4 = hour_p5")

# Perform the F-test
f_test_result <- linearHypothesis(log_reversed_model, hypothesis)

# Print the results
print(f_test_result)

#6-8
# Define the hypothesis
hypothesis <- c("hour_p6 = hour_p7", "hour_p7 = hour_p8")

# Perform the F-test
f_test_result <- linearHypothesis(log_reversed_model, hypothesis)

# Print the results
print(f_test_result)

#9-11
# Define the hypothesis
hypothesis <- c("hour_p9 = hour_p10", "hour_p10 = hour_p11")

# Perform the F-test
f_test_result <- linearHypothesis(log_reversed_model, hypothesis)

# Print the results
print(f_test_result)

#12-14***
# Define the hypothesis
hypothesis <- c("hour_p12 = hour_p13", "hour_p13 = hour_p14")

# Perform the F-test
f_test_result <- linearHypothesis(log_reversed_model, hypothesis)

# Print the results
print(f_test_result)

#15-17
# Define the hypothesis
hypothesis <- c("hour_p15 = hour_p16", "hour_p16 = hour_p17")

# Perform the F-test
f_test_result <- linearHypothesis(log_reversed_model, hypothesis)

# Print the results
print(f_test_result)

#18-20***
# Define the hypothesis
hypothesis <- c("hour_p18 = hour_p19", "hour_p19 = hour_p20")

# Perform the F-test
f_test_result <- linearHypothesis(log_reversed_model, hypothesis)

# Print the results
print(f_test_result)

#21-23
# Define the hypothesis
hypothesis <- c("hour_p21 = hour_p22", "hour_p22 = hour_p23")

# Perform the F-test
f_test_result <- linearHypothesis(log_reversed_model, hypothesis)

# Print the results
print(f_test_result)


################################################# Robust #######################################

#Using log(1/y) transformation
data$log_reversed_total_amount <- log(1/(data$total_amount + 1))
log_reversed_model <- lm(log_reversed_total_amount ~ ., data = data)  #non-robust

#robust
coeftest(model , vcov=vcovHC(log_reversed_model, "HC0"))
