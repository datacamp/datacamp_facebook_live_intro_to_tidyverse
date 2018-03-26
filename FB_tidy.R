library(tidyverse)

# Load data ---------------------------

# Import data
titanic <- readr::read_csv("data/train.csv")

# Check out the head of your dataframe
titanic

# Summarize titanic
summary(titanic)

# Summarize titanic using a pipe
titanic %>%
  summary()

# Summarize titanic after dropping na
titanic %>%
  drop_na() %>%
  summary()

# Wrangling your data ---------------------------

# Subset to get all 'Male' rows
titanic %>% 
  filter(Sex == "male")

# Subset to get all 'Female' rows
titanic %>%
  filter(Sex == "female")

# Arrange by increasing Fare
titanic %>%
  arrange(Fare)

# Arrange by decreasing Fare
titanic %>%
  arrange(desc(Fare))

# Create new column fam_size (size of family)
titanic %>%
  mutate(fam_size = Parch + SibSp)

# Create new column fam_size (size of family)
# Arrange by decreasing FamSize
titanic %>%
  mutate(fam_size = Parch + SibSp) %>%
  arrange(desc(fam_size))

# Turn numerical values of Survived column to 'No' & 'Yes' (new dataframe)
titanic1 <- titanic %>%
  mutate(Survived = ifelse(Survived == 1, "Yes", "No"))

# Plot data ---------------------------

# Plot barplot of passenger Sex
ggplot(titanic1, aes(Sex)) +
  geom_bar()

# Plot barplot of passenger Sex & stack according to Survival
ggplot(titanic1, aes(Sex)) +
  geom_bar(aes(fill = Survived))

# Scatter plot of Age vs Fare
ggplot(titanic, aes(x = Age, y = Fare)) +
  geom_point()

# Scatter plot of Age vs Fare colored by Sex
ggplot(titanic, aes(x = Age, y = Fare, color = Sex)) +
  geom_point()

# Scatter plot of Age vs Fare colored by Sex faceted by Survived
ggplot(titanic1, aes(x = Age, y = Fare, color = Sex)) +
  geom_point() +
  facet_wrap(~ Survived)


# Summarize data ---------------------------

# Check out mean Fare
titanic %>% 
  summarize(meanFare = mean(Fare))

# Check out mean Fare
titanic %>% 
  summarize(medianFare = median(Fare))

# Check out mean Fare for men
titanic %>%
  filter(Sex == 'male') %>% 
  summarize(meanFare = mean(Fare))

# Check out mean Fare for women
titanic %>%
  filter(Sex == 'female') %>% 
  summarize(meanFare = mean(Fare))

# Check out mean Fare & number of survivors for women
titanic %>%
  filter(Sex == 'female') %>% 
  summarize(meanFare = mean(Fare),
            Survived = sum(Survived))

# Check out mean Fare & number of survivors grouped by Sex
titanic %>% 
  group_by(Sex) %>% 
  summarise(meanFare = mean(Fare), 
            Survived = sum(Survived))

# Check out mean Fare & proportion of survivors grouped by Sex
titanic %>% 
  group_by(Sex) %>% 
  summarise(meanFare = mean(Fare), 
            Survived = sum(Survived)/n())
