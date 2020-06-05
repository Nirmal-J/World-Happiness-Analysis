## import libraries

library(tidyverse)
library(caret)
library(DescTools)
library(ggcorrplot)


# ----- Import dataset -----


data <- read.csv("2019.csv")


# ----- Explore and visualize data -----

# This shows an organized section of data frame



glimpse(data)

# structure of the data frame


str(data)

summary(data)


# shows highest 10 scores


head(data, n=10)


# shows lowest 10 scores


tail(data, n=10)


# histogram of scores

## Rich countries are definitely happier than poor countries,â??? he said. â???oItâ???Ts no joy to be poor.â??? But unlike the United States, where a loss of faith in institutions has dovetailed with the drop in happiness reported by Americans, people in Scandinavian countries believe in one another and their governments, Professor Sachs said.In Finland, 91 percent of survey respondents reported being satisfied with their president and 86 percent said they trust the police.


hist(data$Score, freq=TRUE, col="black", border="white", 
     main="2019 Happiness Scores", xlab="Score", ylab="Count")


# plot gdp per cap vs score


ggplot(data = data, aes(x = Score, y = GDP.per.capita)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)


# plot generosity vs score


ggplot(data = data, aes(x = Score, Generosity)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

# plot life expectancy vs score


ggplot(data = data, aes(x = Score, Healthy.life.expectancy)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)


# Thus, moving forward, let us explore the correlation between the other predictors (economy, family, trust, etc.) and happiness score



temp <- data[, c(3,4,5,6,7,8,9)]
cormat <- signif(cor(temp), 2)
ggcorrplot(cormat)


# ----- Summation model -----
# find  predicted score by sum method and calculate the corresponding RMSE


sum_model <- data %>% mutate(pred_score = GDP.per.capita +
                               Social.support +
                               Healthy.life.expectancy +
                               Freedom.to.make.life.choices + 
                               Generosity + 
                               Perceptions.of.corruption + 
                               1.85, 
                             RMSE = RMSE(Score, pred_score))



# show top results of the summation model


sum_model %>%
  filter(Overall.rank <= 5) %>%
  select(Overall.rank, Country.or.region, Score, pred_score, RMSE)



# ----- RESULT -----

# ----- Develop generalized linear model -----


# --- test for an appropriate probability in data partitioning



ps <- seq(from=.30, to=.90, by=.01)

rmses <- sapply(ps, function(p){
  train_index <- createDataPartition(data$Score,
                                     times=1,
                                     p=p,
                                     list=FALSE)
  train <- data[train_index,]
  test <- data[-train_index,]
  fit <- glm(Score ~ GDP.per.capita +
               Social.support +
               Healthy.life.expectancy + 
               Freedom.to.make.life.choices + 
               Generosity + 
               Perceptions.of.corruption, 
             data = train)
  test <- test %>% 
    mutate(pred_score = predict.glm(fit, newdata=test))
  RMSE(test$Score, test$pred_score)
})





# no real clear winner in terms of best accuracy in probabilities



plot(ps, rmses)
ps[which.min(rmses)]
min(rmses)

rm(ps, rmses)

