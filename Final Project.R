library(plyr)
library(stringr)
library(RVAideMemoire)
library(gmodels)
library(tidyverse)

# data_path = "C:\\Workspace\\wordwide_ski_resort.csv"
data = read.csv(file.choose(), header=T)
str(data)
# Preprocessing Steps
# Remove rows with empty values
data <- na.omit(data) # 519 obs. -> 515 obs.

# Remove obvious useless column
df = subset(data, select = -c(Total.lifts.open, Resort.name))

# Revalue
# Continent
unique(df$Continent) # "Europe","Rest of the world","America"
df$Continent <- as.numeric(revalue(df$Continent, c(
  "Europe" = 1,
  "Rest of the world" = 2,
  "America" = 3
)))

# Country
x <- unique(df$Country)
for (i in 1:length(df[,1])) {
  for (j in 1:length(x)) {
    if(df$Country[i] == x[j]) {
      df$Country[i] <- as.numeric(j)
      break
    }
  }
}
df$Country <- strtoi(df$Country)
unique(df$Country)

# Child.friendly
unique(df$Child.friendly)
df$Child.friendly <- as.numeric(revalue(df$Child.friendly, c(
  "Yes" = 1,
  "No" = 2
)))

# Season
x <- unique(df$Season)
for (i in 1:length(df[,1])) {
  for (j in 1:length(x)) {
    if(df$Season[i] == x[j]) {
      df$Season[i] <- as.numeric(j)
      break
    }
  }
}
df$Season <- strtoi(df$Season)
unique(df$Season)

# Snowparks
unique(df$Snowparks)
df$Snowparks <- as.numeric(revalue(df$Snowparks, c(
  "Yes" = 1,
  "No" = 2,
  "no report" = 0
)))

# Nightskiing
unique(df$Nightskiing)
df$Nightskiing <- as.numeric(revalue(df$Nightskiing, c(
  "Yes" = 1,
  "No" = 2
)))

# Summer.skiing
unique(df$Summer.skiing)
df$Summer.skiing <- as.numeric(revalue(df$Summer.skiing, c(
  "Yes" = 1,
  "No" = 2,
  "no report" = 0
)))

# Avg..snow.depth.last.5.seasons
unique(df$Avg..snow.depth.last.5.seasons)
df$Avg..snow.depth.last.5.seasons <- revalue(df$Avg..snow.depth.last.5.seasons, c(
  "no report" = 0
))
df$Avg..snow.depth.last.5.seasons <- str_replace_all(df$Avg..snow.depth.last.5.seasons, ",", "")
df$Avg..snow.depth.last.5.seasons <- strtoi(df$Avg..snow.depth.last.5.seasons)

# Check correlation
COR <- cor(df)
View(COR)
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = COR, col = col, symm = TRUE)


# MLR


# Remove variables with aliased coefficients
rdf <- subset(df, select = -c(Total.lifts, Total.slopes,Country,Season))

# visualize a table
library(DT)
datatable(rdf)

# Check VIF
# No variables with VIF > 10
library(DescTools)
model1 <- lm(Ski.pass.price.adult ~ ., data=rdf)
VIF(model1)

# PCA
library(factoextra)
p<-prcomp(rdf,center=T, scale=T)
fviz_eig(p)
# 3 components
plot(p)
abline(1,0)
# 6 components
library(psych)
p2<-psych::principal(rdf,rotate='varimax',nfactors=3,score=TRUE)
p3<-print(p2$loadings, cutoff=.4, sort=T)


# Logistic Regression
# LR model
# Only suitable for binary, revalue the dependent variable. 
#rdf1<-subset(rdf,select = -c(Ski.pass.price.adult))
rdf1<-subset(rdf,select = c(Ski.pass.price.adult,Beginner.slopes,Intermediate.slopes,Surface.lift.etc.,Chairlifts.etc.,Gondola.etc.,Lift.capacity,Difficult.slopes,Continent,Highest.point,Lowest.point,Difficult.slopes))
names(rdf1)

rdf1$Ski.pass.price.adult<-as.numeric(rdf1$Ski.pass.price.adult)
rdf1$Ski.pass.price.adult_1<-cut(rdf1$Ski.pass.price.adult,c(-1,70,150))
table(rdf1$Ski.pass.price.adult_1)
rdf1$Ski.pass.price.adult_1<-0*(rdf1$Ski.pass.price.adult_1=='no')+0*(rdf1$Ski.pass.price.adult_1=='(-1,70]')+1*(rdf1$Ski.pass.price.adult_1=='(70,150]')
names(rdf1)

rdf2<-subset(rdf1,select = -c(Ski.pass.price.adult))

rdf2$Ski.pass.price.adult_1<-as.factor(rdf2$Ski.pass.price.adult_1)
log_reg <- glm(Ski.pass.price.adult_1 ~ ., family = "binomial", data = rdf2)

library(gtsummary)
log_reg %>% 
  gtsummary::tbl_regression(exp = TRUE) 

# cut ski ticket prices to three levels: cheap, moderate, expensive
# cheap-->0, moderate-->1, expensive-->2


# confusion matrix
library(rsample) 
# Create training (70%) and test (30%) sets
set.seed(123)
split <- initial_split(rdf2, prop = .7, strata = "Ski.pass.price.adult_1")
train <- training(split)
test  <- testing(split)

library(caret)
train$Ski.pass.price.adult_1<- as.factor(train$Ski.pass.price.adult_1)
log_reg_1 = train(
  form = Ski.pass.price.adult_1 ~ .,
  data = train,
  method = "glm",
  family = "binomial"
)
summary(log_reg_1)

pred <- predict(log_reg_1, test)
confusionMatrix(pred, as.factor(test$Ski.pass.price.adult_1), mode = 'everything')

# ROC Curve, C-statistics
log_reg_train <- glm(Ski.pass.price.adult_1 ~ ., data=train, family=binomial)
library(ROCR)
log_reg_test_prob <- log_reg_train %>% predict(test, type = "response")
preds <- prediction(as.numeric(log_reg_test_prob), test$Ski.pass.price.adult_1)
perf <- performance(preds,"tpr","fpr")
plot(perf,colorize=TRUE)

## Get AUCs
library(precrec)
precrec_obj <- evalmod(scores = log_reg_test_prob, labels = test$Ski.pass.price.adult_1)
sm_aucs <- auc(precrec_obj)
sm_aucs



