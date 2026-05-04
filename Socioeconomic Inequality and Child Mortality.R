install.packages("tidyverse")
library(tidyverse)
data <- read.csv(file.choose())
names(data)
library(dplyr)
data_clean <- data %>%
  select(Entity, Year, Child.mortality.rate, GDP.per.capita..int...)
data_clean <- data_clean %>%
  filter(Year == 2019) %>%
  na.omit()
data_clean <- data_clean %>%
  filter(!grepl("World|income|Asia|Europe|Africa|America", Entity))
nrow(data_clean)
head(data_clean)
head(data_clean, 20)
head(data_clean, 160)
view(data_clean)
library(ggplot2)
ggplot(data_clean, aes(x = GDP.per.capita..int..., y = Child.mortality.rate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "GDP vs Child Mortality",
    x = "GDP per capita",
    y = "Child Mortality Rate"
  )
ggplot(data_clean, aes(x = log(GDP.per.capita..int...), y = Child.mortality.rate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "GDP vs Child Mortality (Log Scale)",
    x = "Log GDP per capita",
    y = "Child Mortality Rate"
  )
model <- lm(Child.mortality.rate ~ log(GDP.per.capita..int...), data = data_clean)
summary(model)
view(data_clean)
ggplot(data_clean, aes(x = Child.mortality.rate)) +
  geom_density() +
  labs(
    title = "Distribution of Child Mortality",
    x = "Child Mortality Rate"
  )
