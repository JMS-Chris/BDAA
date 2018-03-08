


#---- 0) Download medals data and install R packages ----

# medals data: https://www.kaggle.com/the-guardian/olympic-games/data
# install.packages(c("tidyverse", "gapminder", "caret", "rpart", "rpart.plot", "randomForest", "xgboost", "DiagrammeR"))


#---- 1) Load libraries ----

library(tidyverse)
library(gapminder)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
library(DiagrammeR)


#---- 2) Medals data ----

#base path to the downloaded files
filepath <- "C:/"

#load each file into memory
winter <- read_csv(paste0(filepath, "winter.csv"))
dictionary <- read_csv(paste0(filepath, "dictionary.csv"))

#check out the contents of the files
View(winter)
View(dictionary)

#combine the files into 1 large file
winter_w_dictionary <- left_join(winter %>% rename(Code = Country),
                                 dictionary %>% select(Country, Code))

#view combined file
View(winter_w_dictionary)

#plot the number of medals won by country by year
winter_w_dictionary %>%
  count(Year, Country) %>%
  ggplot(aes(x = Year, y = n, color = Country)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = as.numeric(unlist(distinct(
    winter_w_dictionary, Year
  ))),
  minor_breaks = NULL)

#which codes have NA for country
winter_w_dictionary %>%
  filter(is.na(Country)) %>%
  count(Code) %>%
  mutate(Code = reorder(Code, n)) %>%
  ggplot() +
  geom_col(aes(x = Code, y = n, fill = Code)) +
  coord_flip() +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Athlete Medals for NA Countries",
    subtitle = "Total for all games competed",
    caption = "Data source: https://www.kaggle.com/the-guardian/olympic-games/data",
    x = "Country code",
    y = "Total athlete medals"
  ) +
  theme(legend.position = "none")
#URS = Soviet Union, GDR = East Germany, TCH = Czechoslovakia, FRG = West Germany
#EUN = Unified Team (former Soviet Union), EUA = United Team of Germany,
#YUG = Yugoslavia, ROU = Romania (listed as ROM in dictionary) (2 medals in 1968)

#are there any NAs after 1994?
winter_w_dictionary %>%
  filter(is.na(Country), Year >= 1994) %>%
  count(Code, sort = TRUE)

#medals table lists each individual medalist inflating total counts
winter_w_dictionary %>%
  filter(Year == 2014,
         Sport == "Ice Hockey",
         Gender == "Women",
         Medal == "Gold")

#calculate each country's final medal count for the 1994 games onward
winter_1994_onward_country_medals <- winter_w_dictionary %>%
  filter(Year >= 1994) %>%
  mutate(Specific_Event = if_else((Discipline == "Biathlon" &
                                     Event == "Relay Mix") |
                                    (Discipline == "Luge" &
                                       Event == "Mixed Relay") |
                                    (Discipline == "Figure skating" &
                                       Event != "Individual"),
                                  str_c(Discipline, Event, sep = " "),
                                  str_c(Discipline, Gender, Event, sep = " ")
  )) %>%
  distinct(Year, Country, Specific_Event, Medal) %>%
  count(Year, Country) %>%
  rename(Medals = n) %>%
  arrange(Year, desc(Medals), Country)

#view data transformation result
View(winter_1994_onward_country_medals)

#actual number of medals won by country by year
winter_1994_onward_country_medals %>%
  ggplot(aes(x = Year, y = Medals, color = Country)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = as.numeric(unlist(
    distinct(winter_1994_onward_country_medals, Year)
  )),
  minor_breaks = NULL)


#---- 3) Host countries data ----

#create table of host countries
host_countries <- data.frame(
  Year = seq(1994, 2006, 4),
  Host_Country = c("Norway", "Japan", "United States", "Italy")
)

#view this table
View(host_countries)


#---- 4) Gapminder data ----

#check out the gapminder data
View(gapminder_unfiltered)

#which years contain data
gapminder_unfiltered %>%
  filter(year >= 1992) %>%
  count(year) %>%
  ggplot() +
  geom_col(aes(year, n)) +
  scale_x_continuous(breaks = seq(1992, 2007))

#which country names don't match between our data sources
anti_join(
  winter_1994_onward_country_medals %>% distinct(Country),
  gapminder_unfiltered %>% distinct(country),
  by = c("Country" = "country")
)

#update the gapminder dataset to align to the medal data's years and country names
gapminder_unfiltered_updated <- gapminder_unfiltered %>%
  filter(year %in% c(1992, 1997, 2002, 2007)) %>%
  mutate(
    year = case_when(
      year == 1992 ~ 1994,
      year == 1997 ~ 1998,
      year == 2002 ~ 2002,
      year == 2007 ~ 2006
    ),
    country = case_when(
      country == "Korea, Rep." ~ "Korea, South",
      country == "Slovak Republic" ~ "Slovakia",
      TRUE ~ as.character(country)
    )
  ) %>%
  rename_all(str_to_title)

#join the medals and gapminder datasets together putting a 0 for countries which did not medal
winter_1994_to_2006_country_medals_w_gapminder <- right_join(winter_1994_onward_country_medals,
                                                             gapminder_unfiltered_updated) %>%
  mutate(Medals = if_else(is.na(Medals), 0, as.numeric(Medals)))

#join in continent information for the host countries
host_countries_continents <- left_join(
  host_countries,
  gapminder_unfiltered_updated %>% distinct(Country, Continent),
  by = c("Host_Country" = "Country")
) %>%
  rename(Host_Continent = Continent)

#join in the host information and create hosting flags
winter_1994_to_2006_country_medals_w_gapminder_and_host <-
  left_join(winter_1994_to_2006_country_medals_w_gapminder,
            host_countries_continents) %>%
  mutate(
    Is_Host = (Country == Host_Country),
    Is_In_Host_Continent = (Continent == Host_Continent)
  )


#---- 5) Creating the modeling data ----

#create the base modeling data set removing any potential unique identifiers
base_model_data <-
  winter_1994_to_2006_country_medals_w_gapminder_and_host %>%
  select(-Year,-Country,-Host_Country,-Host_Continent)

#set the random number generator seed for repeatable results
set.seed(1)

#create a 60/20/20 training/validation/test partition of the model data
train_flag <- createDataPartition(y = base_model_data$Medals, p = 0.6, list = FALSE)
training_data <- base_model_data[train_flag, ]
validation_test_temp <- base_model_data[-train_flag, ]

#split the remaining 40% not in the training set into the validation and test sets
set.seed(1)
validation_flag <- createDataPartition(y = validation_test_temp$Medals, p = 0.5, list = FALSE)
validation_data <- validation_test_temp[validation_flag, ]
test_data <- validation_test_temp[-validation_flag, ]


#---- 6) rpart ----

#fit a cart on the training data
set.seed(1)
rpart_fit <- rpart(Medals ~ ., data = training_data)

#view the tree
rpart.plot(rpart_fit, type = 3, extra = 101)

#predict the training set
predict(rpart_fit)

#calculate the root-mean-square error for the training data
sqrt(mean((training_data$Medals - predict(rpart_fit)) ^ 2))


#predict the validation set
predict(rpart_fit, validation_data)

#predict the validation set and create an actual vs predicted data frame
rpart_actual_vs_predicted <-
  bind_cols(data.frame(Actual = validation_data$Medals),
            data.frame(Predicted = predict(rpart_fit, validation_data)))

#view the actual vs predicted for the validation set
rpart_actual_vs_predicted %>%
  ggplot() +
  geom_abline(slope = 1) +
  geom_point(aes(x = Predicted, y = Actual))

#calculate the RMSE and print it to the screen
(rpart_rmse <- sqrt(mean(( rpart_actual_vs_predicted$Actual - rpart_actual_vs_predicted$Predicted) ^ 2)))


#---- 7) randomForest ----

#fit a random forest on the training data
set.seed(1)
randomForest_fit <- randomForest(Medals ~ ., data = training_data)

#view a single tree
getTree(randomForest_fit, 1, labelVar = TRUE)

#calculate the root-mean-square error for the training data
sqrt(mean((training_data$Medals - predict(randomForest_fit)) ^ 2))


#predict the validation set and create an actual vs predicted data frame
randomForest_actual_vs_predicted <-
  bind_cols(data.frame(Actual = validation_data$Medals),
            data.frame(Predicted = predict(randomForest_fit, validation_data)))

#view the actual vs predicted for the validation set
randomForest_actual_vs_predicted %>%
  ggplot() +
  geom_abline(slope = 1) +
  geom_point(aes(x = Predicted, y = Actual))

#calculate the RMSE and print it to the screen
(randomForest_rmse <- sqrt(mean((randomForest_actual_vs_predicted$Actual - randomForest_actual_vs_predicted$Predicted) ^ 2)))


#---- 8) xgboost ----

#stage the x's and y's for the training set for xgboost
xgboost_training_data <- model.matrix(Medals ~ . - 1, data = training_data)
xgboost_training_label <- as.matrix(training_data[, "Medals"])

#fit xgboost on the staged training data
set.seed(1)
xgboost_fit <- xgboost(data = xgboost_training_data,
                       label = xgboost_training_label,
                       nrounds = 10)

#view the tree
xgb.plot.tree(feature_names = colnames(xgboost_training_data), xgboost_fit)


#stage the x's for the validation set for xgboost
xgboost_validation_data <- model.matrix(Medals ~ . - 1, data = validation_data)

#predict the validation set and create an actual vs predicted data frame
xgboost_actual_vs_predicted <-
  bind_cols(data.frame(Actual = validation_data$Medals),
            data.frame(Predicted = predict(xgboost_fit, xgboost_validation_data)))

#view the actual vs predicted for the validation set
xgboost_actual_vs_predicted %>%
  ggplot() +
  geom_abline(slope = 1) +
  geom_point(aes(x = Predicted, y = Actual))

#calculate the RMSE and print it to the screen
(xgboost_rmse <- sqrt(mean((xgboost_actual_vs_predicted$Actual - xgboost_actual_vs_predicted$Predicted) ^ 2)))


#---- 9) Compare models and predict test data ----

#let's review the results graphically
bind_rows(rpart_actual_vs_predicted %>% mutate(Model = "rpart"),
          randomForest_actual_vs_predicted %>% mutate(Model = "randomForest"),
          xgboost_actual_vs_predicted %>% mutate(Model = "xgboost")) %>%
  mutate(Model = factor(Model, levels = c("rpart", "randomForest", "xgboost"))) %>%
  ggplot() +
  geom_abline(slope = 1) +
  geom_point(aes(x = Predicted, y = Actual, color = Model, shape = Model))

#and let's compare each model's valdiation set RMSE
data.frame(Model = c("rpart", "randomForest", "xgboost"),
           RMSE = c(rpart_rmse, randomForest_rmse, xgboost_rmse))


#finally let's predict our test set with the best model
xgboost_test_data <- model.matrix(Medals ~ . - 1, data = test_data)

#and calculate the RMSE for the test set
sqrt(mean((test_data$Medals - predict(xgboost_fit, xgboost_test_data)) ^ 2))
