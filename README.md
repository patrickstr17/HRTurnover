# HRTurnover


###################################################
# Packages
###################################################
library('readxl')
library('lattice')
library(ggplot2)
library(corrplot)
library(dplyr)

###################################################
# Data import, exploration and selection
###################################################


# Loading the data (data was prepared in the clustering file!)

loaded_data <- read.csv("Chapter_6_Turnover_team_DATA.csv")

# A summary of the dataset
summary(loaded_data)
str(loaded_data)

#converting data types
loaded_data$Country <- factor(loaded_data$Country, 
                              levels=c(1,2,3,4), 
                              labels = c('UK','United States','Canada','Spain'))
loaded_data$UKdummy <- factor(loaded_data$UKdummy, 
                              levels=c(0,1), 
                              labels = c('Not UK', 'UK'))
loaded_data$USAdummy <- factor(loaded_data$USAdummy, 
                               levels=c(0,1), 
                               labels = c('Not USA', 'USA'))
loaded_data$SpainDummy <- factor(loaded_data$SpainDummy, 
                                 levels=c(0,1), 
                                 labels = c('Not Spain', 'Spain'))
loaded_data$CanadaDummy <- factor(loaded_data$CanadaDummy, 
                                  levels=c(0,1), 
                                  labels = c('Not Canada', 'Canada'))

#Missing Values Analysis
sum(is.na(loaded_data))

# Outlier Analysis for all variables

## boxplot
par(mfrow=c(2,4), mar=c(3,3,2,1))  

boxplot(loaded_data$TeamSize, main="Team Size", 
        col="lightblue", pch=19)
boxplot(loaded_data$TeamSeparation, main="Team Separation", 
        col="lightgreen", pch=19)
boxplot(loaded_data$Engagement, main="Engagement", 
        col="lightcoral", pch=19)
boxplot(loaded_data$TeamLeader, main="Team Leader", 
        col="lightyellow", pch=19)
boxplot(loaded_data$SociallyResponsible, main="Socially Responsible", 
        col="lightpink", pch=19)
boxplot(loaded_data$DriveForPerformance, main="Drive For Performance", 
        col="lightgray", pch=19)
boxplot(loaded_data$PerfDevReward, main="Perf Dev Reward", 
        col="lightcyan", pch=19)
boxplot(loaded_data$WLB, main="Work–Life Balance", 
        col="lightgoldenrodyellow", pch=19)

## histogram
par(mfrow=c(2, 4), mar=c(3, 3, 2, 1))
hist(loaded_data$TeamSize, main = "Team Size", col = "lightblue")
hist(loaded_data$TeamSeparation, main = "Team Separation", col = "lightgreen")
hist(loaded_data$Engagement, main = "Engagement", col = "lightcoral")
hist(loaded_data$TeamLeader, main = "Team Leader", col = "lightyellow")
hist(loaded_data$SociallyResponsible, main = "Socially Responsible", 
     col = "lightpink")
hist(loaded_data$DriveForPerformance, main = "Drive For Performance", 
     col = "lightgray")
hist(loaded_data$PerfDevReward, main = "Perf Dev Reward", col = "lightcyan")
hist(loaded_data$WLB, main = "Work–Life Balance", col = "lightgoldenrodyellow")

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

# there are only small outliers that have no big impact on the overall result
# at least for drive for performance we should try log-standardization
loaded_data1 = loaded_data
loaded_data1$DriveForPerformance = log(loaded_data1$DriveForPerformance)
boxplot(loaded_data1$DriveForPerformance, main="Drive For Performance", 
        col="lightgray", pch=19)

#Turnover differences between countries
country_means <- loaded_data %>%
  group_by(Country) %>%
  summarise(mean_turnover = mean(TeamSeparation, na.rm = TRUE))
print(country_means)

ggplot(country_means, aes(x = Country, y = mean_turnover)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Mean Turnover per Country", x = "Country", y = "Mean Turnover")

# data selection
# the following rows are irrelevant: TeamNumber, Country
regr_data = subset(loaded_data, select = -c(TeamNumber, Country))
summary(regr_data)

print(colnames(regr_data))


###################################################
# Exploration of relationships between variables
###################################################


# Pair-wise relationships of the variables: the scatterplot matrix
exploration_data = subset(regr_data, 
                          select = -c(UKdummy, USAdummy, CanadaDummy, SpainDummy))
splom(~exploration_data, groups=NULL,data=exploration_data)

corr_matrix <- cor(regr_data[, c("TeamSize", "TeamSeparation", "Engagement", 
                                 "TeamLeader","SociallyResponsible", 
                                 "DriveForPerformance","PerfDevReward", "WLB", 
                                 "UKdummy", "USAdummy","CanadaDummy", "UKdummy", 
                                 "USAdummy", "CanadaDummy", "SpainDummy")])
corrplot(corr_matrix, method = "color")

###################################################
# Model 0: Country differences (log reg)
###################################################

# UK
modelUK = glm (UKdummy ~ TeamSize + TeamSeparation + Engagement 
               + TeamLeader + SociallyResponsible + DriveForPerformance 
               + PerfDevReward + WLB,
               data = regr_data,
               binomial(link="logit")
)
summary(modelUK)
## significant with WLB
## possible significance with Engagement and TeamLeader


# USA
modelUSA = glm (USAdummy ~ TeamSize + TeamSeparation + Engagement 
                + TeamLeader + SociallyResponsible + DriveForPerformance 
                + PerfDevReward + WLB,
                data = regr_data,
                binomial(link="logit")
)
summary(modelUSA)
## significant with Engagement and DriveForPerformance
## possible significance with TeamLeader


# Canada
modelCanada = glm (CanadaDummy ~ TeamSize + TeamSeparation + Engagement 
                   + TeamLeader + SociallyResponsible + DriveForPerformance 
                   + PerfDevReward + WLB,
                   data = regr_data,
                   binomial(link="logit")
)
summary(modelCanada)
## significant with TeamSize, Engagement, TeamLeader and DriveForPerformance


# Spain
modelSpain = glm (SpainDummy ~ TeamSize + TeamSeparation + Engagement 
                  + TeamLeader + SociallyResponsible + DriveForPerformance 
                  + PerfDevReward + WLB,
                  data = regr_data,
                  binomial(link="logit")
)
summary(modelSpain)
## significant with TeamSize, Engagement and TeamLeader
## possibly significant for SociallyResponsible and DriveForPerformance

# Plots 
uk_data <- subset(regr_data, UKdummy == 'UK')
usa_data <- subset(regr_data, USAdummy == 'USA')
spain_data <- subset(regr_data, SpainDummy == 'Spain')
canada_data <- subset(regr_data, CanadaDummy == 'Canada')

# Team Size
par(mfrow=c(1, 4), mar=c(3, 3, 2, 1))
boxplot(usa_data$TeamSize, main="Team Size", col="lightblue", pch=19)
boxplot(uk_data$TeamSize, main="Team Size", col="lightcoral", pch=19)
boxplot(spain_data$TeamSize, main="Team Size", col="lightgreen", pch=19)
boxplot(canada_data$TeamSize, main="Team Size", col="yellow", pch=19)

# Engagement
par(mfrow=c(1, 4), mar=c(3, 3, 2, 1))
boxplot(usa_data$Engagement, main="Engagement", col="lightblue", pch=19)
boxplot(uk_data$Engagement, main="Engagement", col="lightcoral", pch=19)
boxplot(spain_data$Engagement, main="Engagement", col="lightgreen", pch=19)
boxplot(canada_data$Engagement, main="Engagement", col="yellow", pch=19)

# Team Leader
par(mfrow=c(1, 4), mar=c(3, 3, 2, 1))
boxplot(usa_data$TeamLeader, main="Team Leader", col="lightblue", pch=19)
boxplot(uk_data$TeamLeader, main="Team Leader", col="lightcoral", pch=19)
boxplot(spain_data$TeamLeader, main="Team Leader", col="lightgreen", pch=19)
boxplot(canada_data$TeamLeader, main="Team Leader", col="yellow", pch=19)

# Drive for Performance
par(mfrow=c(1, 4), mar=c(3, 3, 2, 1))
boxplot(usa_data$DriveForPerformance, main="Drive For Performance", 
        col="lightblue", pch=19)
boxplot(uk_data$DriveForPerformance, main="Drive For Performance", 
        col="lightcoral", pch=19)
boxplot(spain_data$DriveForPerformance, main="Drive For Performance", 
        col="lightgreen", pch=19)
boxplot(canada_data$DriveForPerformance, main="Drive For Performance", 
        col="yellow", pch=19)


###################################################
# Model 1: Reasons for turnover (lin reg)
###################################################


# Estimation of the linear model to predict turnover with all variables
model1 <- lm(TeamSeparation ~ TeamSize + Engagement + TeamLeader 
             + SociallyResponsible + DriveForPerformance + PerfDevReward + WLB 
             + UKdummy + USAdummy + CanadaDummy + SpainDummy, data = regr_data)
summary(model1)

##Model 1.5 without Spain Dummy
model1.5 <- lm(TeamSeparation ~ TeamSize + Engagement + TeamLeader 
               + SociallyResponsible + DriveForPerformance + PerfDevReward 
               + WLB + UKdummy + USAdummy + CanadaDummy, data = regr_data)
summary(model1.5)

## p-Values:engagement and driver for performance have significant impact
##the model can not be used for further predictions


###############################################################
# Model 2: Prediction of team drive for performance (lin reg)
###############################################################

model2 <- lm(DriveForPerformance ~ TeamSize + TeamSeparation + Engagement 
             + TeamLeader + SociallyResponsible + PerfDevReward + WLB + UKdummy 
             + USAdummy + CanadaDummy + UKdummy + USAdummy + CanadaDummy 
             + SpainDummy, data = regr_data)
summary(model2)

# rerun it without UK this time
model2.5 <- lm(DriveForPerformance ~ TeamSize + TeamSeparation + Engagement 
               + TeamLeader + SociallyResponsible + PerfDevReward + WLB 
               +UKdummy + USAdummy + CanadaDummy + SpainDummy, data = regr_data)
summary(model2.5)

##the model can not be used for further predictions


###############################################################
# Model 3: Engagement 
###############################################################

model3 <- lm(Engagement ~ TeamSize + TeamSeparation + TeamLeader 
             + SociallyResponsible + DriveForPerformance + PerfDevReward 
             + WLB + USAdummy + CanadaDummy + SpainDummy 
             + UKdummy, data = regr_data)

model3.5 <- lm(Engagement ~ TeamSize + TeamSeparation + TeamLeader 
               + SociallyResponsible + DriveForPerformance + PerfDevReward + WLB 
               + USAdummy + CanadaDummy + SpainDummy, data = regr_data)
summary(model3.5)

## significant impact (p-Values): hypothesis is right!
## highest R^2 value so far. 51% is explained -> but still only ok not good 
## Model estimation will be acceptable, therefore we continue


# Confidence Intervals on the Parameters
confint(model3.5, level = .95)

# examplatory data
beispiel_data <- data.frame(
  TeamSize = 15,
  TeamSeparation = 0.5,
  TeamLeader = 0.2,
  SociallyResponsible = 0.1,
  DriveForPerformance = 0.5,
  PerfDevReward = 0.002,
  WLB = 0.1,
  USAdummy = 'USA',
  CanadaDummy = 'Canada',
  SpainDummy = 'Spain'
)

conf_int <- predict(model3.5, newdata = beispiel_data, level = 0.95, 
                    interval = "confidence")

conf_int

# Evaluating the Residuals: centered on zero with a constant variance
with(model3.5, {
  plot(fitted.values, residuals, ylim = c(-25, 25))
  points(c(min(fitted.values), max(fitted.values)), c(0, 0), type = "l")
})


# Evaluating the Normality Assumption of residuals
hist(model3.5$residuals, main="Histogram of residuals")

qqnorm(model3.5$residuals, ylab="Residuals")
qqline(model3.5$residuals)
## In general it fits, but you see that there are still uncertainties
