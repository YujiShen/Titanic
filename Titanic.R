library(GGally)
setwd('/Users/Yuji/Workspace/R/Titanic')
train <- read.csv("train.csv")
test<- read.csv("test.csv")

summary(train)
str(train)
train$Survived <- factor(train$Survived, level = c(0, 1), label = c("Dead", "Survived"))
train$Pclass <- factor(train$Pclass, label = c("1st", "2nd", "3rd"))

ggplot(aes(x = Age), data = train) +
  geom_density() +
  facet_wrap(~Survived)

ggplot(aes(x = Sex), data = train) +
  geom_bar(aes(fill = Sex)) +
  facet_wrap(~Survived)

ggplot(aes(x = Pclass), data = train) +
  geom_bar(aes(fill = Pclass)) +
  facet_wrap(~Survived)

ggplot(aes(x = Age, y = Fare), data = train) +
  geom_point(aes(color = Sex)) +
  geom_smooth(aes(x = Age, y = Fare, fill = "orange"), 
              data = subset(train, Sex == "female")) + 
  geom_smooth(aes(x = Age, y = Fare, fill = "steel"), 
              data = subset(train, Sex == "male")) +
  coord_cartesian(ylim = c(quantile(train$Fare, 0.05), 
                         quantile(train$Fare, 0.95)))

ggplot(aes(x = Age, y = Fare), data = train) +
  geom_point(aes(color = Survived)) +
  geom_smooth(aes(x = Age, y = Fare, fill = "steel"), 
              data = subset(train, Survived == "Survived")) + 
  geom_smooth(aes(x = Age, y = Fare, fill = "orange"), 
              data = subset(train, Survived == "Dead")) +
  coord_cartesian(ylim = c(0, quantile(train$Fare, 0.95)))

summary(train$Pclass)
ggplot(aes(x = Fare), data = train) +
  geom_histogram(binwidth = 5) + 
  facet_wrap(~Pclass) +
  coord_cartesian(xlim = c(0, quantile(train$Fare, 0.95)))

fare_by_class <- train %>%
  group_by(Pclass) %>%
  summarise(mean_fare = mean(Fare),
          median_fare = median(Fare),
          n = n())
ggplot(aes())