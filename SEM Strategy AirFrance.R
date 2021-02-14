####################################################################
####                 Understanding the Data                     ####
####################################################################

# This code will import the data frame 
library(readxl)
air_france <- read_excel("~/Desktop/Air France Case Spreadsheet Supplement.xls")
View(air_france)

####################################################################
####                    Problem Statement                       ####
####################################################################

# How can AirFrance optimize their use of keywords and platforms in 
# order to increase either conversion or awareness in the US market 

####################################################################
####                   Massaging the Data                       ####
####################################################################

# Making sure our data_frames are uniform 
new_af <- as.data.frame(air_france)

# Subsetting USA
us_airf <- new_af[ which(new_af$`Publisher Name` == "Google - US" | 
                           new_af$`Publisher Name` == "MSN - US" |
                           new_af$`Publisher Name` == "Yahoo - US" |
                           new_af$`Publisher Name` == "Overture - US") , ]

# Subsetting branded keywords from unbranded and store in brand_unbrand
brand_unbrand <- grepl(pattern = "air france|airfrance", x = us_airf$Keyword)

# Creating new binary column for brand_unbrand -> TRUE = Branded, FALSE = Unbranded 
us_airf$`Branded Unbranded KW` <- factor(brand_unbrand, levels = c("FALSE", "TRUE"), 
                                         labels = c("Unbranded", "Branded"))

# Creating Publisher Name binary:
us_airf$`Publisher Name` <- as.factor(us_airf$`Publisher Name`)

# In order to evaluate all Publishers equally, we have devided 3 KPIs by the total costs
# See graphs in Visualize for conclusions 

# Creating Cost per Booking: Total Volume of Bookings / Total Costs
us_airf$`Cost per Booking` <- us_airf$`Total Volume of Bookings` / us_airf$`Total Cost`

# Changing the NaN and Inf in 0 -> 0 Tot. Vol / 0 Costs is still 0 Amount per booking
us_airf$`Cost per Booking` <- as.numeric(gsub("NaN", 0, us_airf$`Cost per Booking`))
us_airf$`Cost per Booking` <- as.numeric(gsub("Inf", 0, us_airf$`Cost per Booking`))

# Creating ROA: Amount / Total Cost
us_airf$`Return on Advertisement` <- us_airf$`Amount` / us_airf$`Total Cost` 

# Changing the NaN and Inf in 0 -> 0 Amount / 0 Costs is still 0 ROA
us_airf$`Return on Advertisement` <- as.numeric(gsub("NaN", 0, us_airf$`Return on Advertisement`))
us_airf$`Return on Advertisement` <- as.numeric(gsub("Inf", 0, us_airf$`Return on Advertisement`))

# Creating Cost per Impression: Impressions / Total Cost
us_airf$`Cost per Impression` <- us_airf$`Impressions` / us_airf$`Total Cost`

# Changing the NaN in 0 -> 0 Impressions / 0 Costs is still 0 Cost per Impressions
us_airf$`Cost per Impression` <- as.numeric(gsub("NaN", 0, us_airf$`Cost per Impression`))

# Cleaning up us_airf with columns not used - store in clean_df
clean_df <- us_airf[-c(1,3,5:11)]

# Moving Brand column next to Keyword column for overview - store in my_afus
my_afus <- clean_df[ , c(1,15,2,3:14,16:18)]

# Checking if there are NAs in my_afus 
colSums(is.na(my_afus))

summary(my_afus$`Publisher Name`)
summary(my_afus$`Branded Unbranded KW`)

####################################################################
####                Descriptive Statistics                      ####
####################################################################

##################  NORMALIZE DATA FOR FORMULAS  ###################

# creating a UDF for min-max normalization
afus_normalize <- function(x){
  afus_min <- min(x, na.rm = T)
  afus_max <- max(x, na.rm = T)
  min_max <- (x - afus_min)/(afus_max - afus_min)
  return(min_max)
} # closing afus_normalize

# Normalize all KPIs fom our formulas
my_afus$av_cpc_norm <- afus_normalize(x=my_afus$`Avg. Cost per Click`)
my_afus$engine_ctr_norm <- afus_normalize(x=my_afus$`Engine Click Thru %`)
my_afus$avg_pos_norm <- afus_normalize(x=my_afus$`Avg. Pos.`)
my_afus$trans_conv_norm <- afus_normalize(x=my_afus$`Trans. Conv. %`)
my_afus$tot_cost_trans_norm <- afus_normalize(x=my_afus$`Total Cost/ Trans.`)
my_afus$search_engine_bid_norm <- afus_normalize(x=my_afus$`Search Engine Bid`)
my_afus$cost_booking_norm <- afus_normalize(x=my_afus$`Cost per Booking`)
my_afus$roa_norm <- afus_normalize(x=my_afus$`Return on Advertisement`)
my_afus$cost_impression_norm <- afus_normalize(x=my_afus$`Cost per Impression`)


############# CREATING FORMULAS FOR 2 CAMPAIGN GOALS ###############

# adding new variable to my_afus DF brand vs unbrand
my_afus$`Conversion Score` <- c()
my_afus$`Awareness Score` <- c()

# Create scaler to see how many rows in my_afus
rowsinafus<- nrow(my_afus)

###################### CONVERSION CAMPAIGN #########################
# Formula for conversion campaigns: 

# 0.10*av. cpc + 0.10*Engine CTR + 0.15*trans conv % + 0.15*total costs/trans + 
# 0.20*cost per booking + 0.30*ROA
for (i in 1:rowsinafus) {
  my_afus$`Conversion Score`[i] <- 
    0.10*my_afus$av_cpc_norm [i] +
    0.10*my_afus$engine_ctr_norm[i] +
    0.15*my_afus$trans_conv_norm[i] +
    0.15*my_afus$tot_cost_trans_norm[i] +
    0.20*my_afus$cost_booking_norm[i] +
    0.30*my_afus$roa_norm[i]
} #closing the i loop

# Check the mean for Conversion Score 
summary(my_afus$`Conversion Score`)

# Create new column for Conversion Success (success = 1, failure = 0)
# for loop to decide Conversion Success Based on mean (above = success)
for (i in 1:rowsinafus) {
  if (my_afus$`Conversion Score`[i] >= mean(my_afus$`Conversion Score`)) {
    my_afus$`Conversion Success`[i] <- 1
  } else {
    my_afus$`Conversion Success`[i] <- 0
  } 
} # closing i-loop 

# Turning Conversion Success into a binary factor
my_afus$`Conversion Success` <- as.factor(my_afus$`Conversion Success`)

###################### AWARENESS CAMPAIGN #########################

# Formula for awareness campaigns: 

# 0.10*SEB + 0.20*av cpc + 0.15*Engine CTR + 0.30*cost/impressions + 0.25*av. pos.
for (i in 1:rowsinafus) {
  my_afus$`Awareness Score`[i] <- 
    0.10*my_afus$search_engine_bid_norm[i] +
    0.20*my_afus$av_cpc_norm[i] +
    0.15*my_afus$engine_ctr_norm[i] +
    0.30*my_afus$cost_impression_norm[i] +
    0.25*my_afus$avg_pos_norm[i]
} #closing the i loop

# Check the mean for Awereness Score
summary(my_afus$`Awareness Score`)

# Create new column for Awareness Success (success = 1, failure = 0)
# for loop to decide Awereness Success based on mean (above = success)
for (i in 1:rowsinafus) {
  if (my_afus$`Awareness Score`[i] >= mean(my_afus$`Awareness Score`)) {
    my_afus$`Awareness Success`[i] <- 1
  } else {
    my_afus$`Awareness Success`[i] <- 0
  } 
} # closing i-loop 

# Turning Awareness Success into a binary factor
my_afus$`Awareness Success` <- as.factor(my_afus$`Awareness Success`)

####################################################################
####                 Predictive Statistics                      ####
####################################################################

###################### TRAINING/TESTING ############################

# Open library for Stratified 
library(splitstackshape)

# Because of sharing the code (gives us similar results)
set.seed(1)

# Stratified for Publishers and Branded/Unbranded KWs 
training_testing <- stratified(as.data.frame(my_afus),
                               group = 1:2, size = 0.8,
                               bothSets = T)

# Calling training_afus and testing_afus
training_afus <- training_testing$SAMP1
testing_afus <- training_testing$SAMP2

# Viewing publisher and brand/unbrand in table 
table(training_afus$`Publisher Name`)
table(testing_afus$`Publisher Name`)
table(training_afus$`Branded Unbranded KW`)
table(testing_afus$`Branded Unbranded KW`)

# Calling Library for Log 
library(class)

################### CREATING LOGIT FOR CONV ########################

# MODEL 1c: Publ, Brand/Unbrand, Publ*Brand/Unbrand  -> BASELINE/Intercept = GOOGLE and UNBRANDED KWs
conv_logit_all <- glm(`Conversion Success` ~ `Publisher Name`+ `Branded Unbranded KW` + `Publisher Name`*`Branded Unbranded KW`, 
                      data = training_afus, family = "binomial")

# MODEL 2c: Publ  -> BASELINE/Intercept = GOOGLE
conv_logit_pub <- glm(`Conversion Success` ~ `Publisher Name`, 
                      data = training_afus, family = "binomial")

# MODEL 3c: Brand/Unbrand  -> BASELINE/Intercept = UNBRANDED KWs
conv_logit_brand <- glm(`Conversion Success` ~ `Branded Unbranded KW`, 
                        data = training_afus, family = "binomial")

# MODEL 4c: Publ*Brand/Unbrand  -> BASELINE/Intercept = GOOGLE and UNBRANDED KWs
conv_logit_p_b <- glm(`Conversion Success` ~ `Publisher Name` + `Branded Unbranded KW`, 
                      data = training_afus, family = "binomial")

# Check summaries of 4 models
summary(conv_logit_all)
summary(conv_logit_pub)
summary(conv_logit_brand)
summary(conv_logit_p_b)

### Conclusions conversion 
# Model 1c - all: branded Overture followed by Google          - AIC: 3233.5 
# Model 2c - publ: Google performs best                        - AIC: 3242.7
# Model 3c - brand: unbranded perform best when alone          - AIC: 3772.6
# Model 4c - p_b: unbranded Google performs best               - AIC: 3238.9

# Model 1c has the lowest AIC, so we will be using that model. 

################## CREATING LOGIT FOR AWARE ########################

# MODEL 1a: Publ, Brand/Unbrand, Publ*Brand/Unbrand  -> BASELINE/Intercept = GOOGLE and UNBRANDED KWs
aware_logit_all <- glm(`Awareness Success` ~ `Publisher Name`+ `Branded Unbranded KW` + `Publisher Name`*`Branded Unbranded KW`, 
                       data = training_afus, family = "binomial")

# MODEL 2a: Publ  -> BASELINE/Intercept = GOOGLE and UNBRANDED KWs
aware_logit_pub <- glm(`Awareness Success` ~ `Publisher Name`, 
                       data = training_afus, family = "binomial")

# MODEL 3a: Brand/Unbrand  -> BASELINE/Intercept = GOOGLE and UNBRANDED KWs
aware_logit_brand <- glm(`Awareness Success` ~ `Branded Unbranded KW`, 
                         data = training_afus, family = "binomial")

# MODEL 4a: Publ*Brand/Unbrand  -> BASELINE/Intercept = GOOGLE and UNBRANDED KWs
aware_logit_p_b <- glm(`Awareness Success` ~ `Publisher Name` + `Branded Unbranded KW`, 
                       data = training_afus, family = "binomial")

# Check summaries of 4 models
summary(aware_logit_all)
summary(aware_logit_pub)
summary(aware_logit_brand)
summary(aware_logit_p_b)

### Conclusions Awareness
# Model 1a - all: Unbranded Google works best                     - AIC: 3167.9
# Model 2a - publ: Google works best                              - AIC: 3180.8
# Model 3a - brand: unbranded works best                          - AIC: 3808.0
# Model 4a - p_b: Unbranded Google works best                     - AIC: 3170.5

# Model 1a has the lowest AIC, so we will be using that model. 


############### CLASSIFICATION MODELS CONV #########################

# Add library for models
library(caret)

# Train confusion matrix CONVERSION 
conv_predict_logit_train <- predict(conv_logit_all, training_afus, type= "response")
confusionMatrix(data= as.factor(as.numeric(conv_predict_logit_train > 0.5)),
                reference= training_afus$`Conversion Success`)

# Test confusion matrix
conv_predict_logit_test <- predict(conv_logit_all, testing_afus, type= "response")
confusionMatrix(data= as.factor(as.numeric(conv_predict_logit_test > 0.5)),
                reference= testing_afus$`Conversion Success`)

### TEST ACCURACY = 0.6479

# Add library for ROC AUC & tree 
library(ROCR)
library(rpart)
library(rpart.plot)

# Train ROC AUC
conv_predict_logit_train_ROC <- predict(conv_logit_all, training_afus, type = "response")
conv_prediction_logit_train_ROC <- prediction(conv_predict_logit_train_ROC, training_afus$`Conversion Success`)
conv_perf_logit_train_ROC <- performance(conv_prediction_logit_train_ROC, "tpr", "fpr")

# Plot ROC AUC Train 
plot(conv_perf_logit_train_ROC, col= "red")

# Trest ROC AUC
conv_predict_logit_test_ROC <- predict(conv_logit_all, testing_afus, type = "response")
conv_prediction_logit_test_ROC <- prediction(conv_predict_logit_test_ROC,testing_afus$`Conversion Success`)
conv_perf_logit_test_ROC <- performance(conv_prediction_logit_test_ROC, "tpr", "fpr")

# Plot ROC AUC Test 
plot(conv_perf_logit_test_ROC, col= "red")

# Train GINI tree
conv_tree <- rpart(`Conversion Success` ~ `Avg. Cost per Click` + `Engine Click Thru %` + `Trans. Conv. %` +
                     `Total Cost/ Trans.` + `Cost per Booking` + `Return on Advertisement`,
                   data = training_afus, 
                   method = "class", cp = 0.008)

# Check optimal cp Train
plotcp(conv_tree)

# Plot GINI tree Train
rpart.plot(conv_tree, type = 1, extra = 1, 
           box.palette = c("dark blue", "dark red"), border.col = 0, 
           col = "white", branch.lty = 1, yesno=2)

# Test GINI tree 
conv_tree_predict <- predict(conv_logit_all, testing_afus, type = "response")
conv_tree_prediction <- prediction(conv_tree_predict, 
                                   testing_afus$`Conversion Success`)

conv_tree_performance <- performance(conv_tree_prediction, "tpr", "fpr")
conv_tree_test <- rpart(`Conversion Success` ~ `Avg. Cost per Click` + `Engine Click Thru %` + `Trans. Conv. %` +
                          `Total Cost/ Trans.` + `Cost per Booking` + `Return on Advertisement`,  
                        data = testing_afus,  
                        method = "class", cp = 0.019) 

# Check optimal cp Test
plotcp(conv_tree_test)

# Plot GINI tree Test
rpart.plot(conv_tree_test, type = 1, extra = 1, 
           box.palette = c("dark red", "dark blue"), border.col = 0, 
           col = "white", branch.lty = 1, yesno=2, compress = T)

# Compare 2 classification models which to use
plot(conv_perf_logit_test_ROC, col = "black", lty = 2, lwd = 2)
plot(conv_tree_performance, col = "red", lty = 3, lwd = 3, add = TRUE)

############### CLASSIFICATION MODELS AWARE ########################

# Train confusion matrix AWARENESS 
aware_predict_logit_train <- predict(aware_logit_all, training_afus, type= "response")
confusionMatrix(data= as.factor(as.numeric(aware_predict_logit_train > 0.5)),
                reference= training_afus$`Awareness Success`)

# Test confusion matrix
aware_predict_logit_test <- predict(aware_logit_all, testing_afus, type= "response")
confusionMatrix(data= as.factor(as.numeric(aware_predict_logit_test > 0.5)),
                reference= testing_afus$`Awareness Success`)

### TESTING ACCURACY = 0.6609

# Train ROC AUC AWARENESS
aware_predict_logit_train_ROC <- predict(aware_logit_all, training_afus, type = "response")
aware_prediction_logit_train_ROC <- prediction(aware_predict_logit_train_ROC, training_afus$`Awareness Success`)
aware_perf_logit_train_ROC <- performance(aware_prediction_logit_train_ROC, "tpr", "fpr")

# Plot ROC AUC Train 
plot(aware_perf_logit_train_ROC, col= "red")

# Test ROC AUC
aware_predict_logit_test_ROC <- predict(aware_logit_all, testing_afus, type = "response")
aware_prediction_logit_test_ROC <- prediction(aware_predict_logit_test_ROC,testing_afus$`Awareness Success`)
aware_perf_logit_test_ROC <- performance(aware_prediction_logit_test_ROC, "tpr", "fpr")

# Plot ROC AUC Test 
plot(aware_perf_logit_test_ROC, col= "red")

# Train GINI tree AWARENESS
aware_tree <- rpart(`Awareness Success` ~ `Search Engine Bid`+ `Avg. Cost per Click` +
                      `Engine Click Thru %`+ `Cost per Impression` + `Avg. Pos.`, 
                    data = training_afus, 
                    method = "class", cp = 0.022)

# Check optimal cp Train
plotcp(aware_tree)

# Plot GINI tree Train
rpart.plot(aware_tree, type = 1, extra = 1, 
           box.palette = c("dark blue", "dark red"), border.col = 0, 
           col = "white", branch.lty = 1, yesno=2)

# Test GINI tree 
aware_tree_predict <- predict(aware_logit_all, testing_afus, type = "response")
aware_tree_prediction <- prediction(aware_tree_predict, 
                                    testing_afus$`Awareness Success`)

aware_tree_performance <- performance(aware_tree_prediction, "tpr", "fpr")
aware_tree_test <- rpart(`Awareness Success` ~ `Search Engine Bid`+ `Avg. Cost per Click` +
                           `Engine Click Thru %`+ `Cost per Impression` + `Avg. Pos.`,  
                         data = testing_afus,  
                         method = "class", cp = 0.015) 

# Check optimal cp Test
plotcp(aware_tree_test)

# Plot GINI tree Test
rpart.plot(aware_tree_test, type = 1, extra = 1, 
           box.palette = c("dark red", "dark blue"), border.col = 0, 
           col = "white", branch.lty = 1, yesno=2, compress = T)

# Compare 2 classification models which to use
plot(aware_perf_logit_test_ROC, col = "black", lty = 2, lwd = 2)
plot(aware_tree_performance, col = "red", lty = 3, lwd = 3, add = TRUE)


####################################################################
####                        Visualize                           ####
####################################################################

# Open library for ggplot and plotly 
library(ggplot2)
library(plotly)

# 1a) Histogram to show cost per publisher to show base of assumption (mean)
publ_hist_mean <- ggplot(my_afus, aes(x=`Publisher Name`, y=`Total Cost`)) + 
  stat_summary(geom="bar", fill="dark blue")

# Make plot interactive 
ggplotly(publ_hist_mean)

# 1b) Histogram to show cost per publisher to show base of assumption (sum)
publ_hist_sum <- ggplot(my_afus, aes(x=`Publisher Name`, y=`Total Cost`)) + 
  stat_sum(geom="bar", fill="dark blue")

# Make plot interactive 
ggplotly(publ_hist_sum)

# 2) Histogram to show cost per booking vs publisher to confirm assumption
publ_hist_cob <- ggplot(my_afus, aes(x=`Publisher Name`, y=`Cost per Booking`)) + 
  stat_sum(geom="bar", fill="dark red")

# Make plot interactive 
ggplotly(publ_hist_cob)

# 3) Histogram to show cost per amount (ROA) vs publisher to confirm assumption
publ_hist_roa <- ggplot(my_afus, aes(x=`Publisher Name`, y=`Return on Advertisement`)) + 
  stat_sum(geom="bar", fill="dark red")

# Make plot interactive 
ggplotly(publ_hist_roa)

# 4) Histogram to show cost per Impression vs publisher to confirm assumption
publ_hist_cpi <- ggplot(my_afus, aes(x=`Publisher Name`, y=`Cost per Impression`)) + 
  stat_sum(geom="bar", fill="dark red")

# Make plot interactive 
ggplotly(publ_hist_cpi)


# Conclusion of graphs:
# 1) Overture and Google have the highest total costs spend
# 2 & 3) Yahoo has the highest ROA, but also the highest Cost per Booking, so its a trade-off
# 4) Overture has the highest Cost per Impression 
# Overall -> See how formulas influence these single KPIs 
#         -> Keep an eye on Overture, Google and Yahoo 
#         -> Most likely MSN will not perform well 
