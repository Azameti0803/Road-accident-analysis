# Install and load necessary libraries
install.packages("readxl")  # Install the package
install.packages("openxlsx")
install.packages("lubridate")
install.packages("MASS")  # Install if not already installed
install.packages("VIM")
install.packages("corrplot")
install.packages("pscl")
install.packages("randomForest")
install.packages("Metrics")   # Install Metrics package (if not installed)
install.packages("caret") 
install.packages("corrplot")  # Install the package (only needed once)
install.packages("gt")
install.packages("tidyverse")  # For data manipulation and visualization
install.packages("ggplot2")    # For advanced plotting
install.packages("dplyr")      # For data manipulation
install.packages("reshape2")
install.packages("AER")
###########################load packages
# Load necessary libraries
library(broom)
library(dplyr)
library(tidyselect)
# Load the AER package
# Load caret package
library(caret)
library(AER)    # For overdispersion test
library(reshape2)
library(car)   # For VIF
library(vcd)   # For Cramér's 
library(dplyr)
library(xtable)
library(tidyr)
library(gt)  # For creating a nicely formatted table
library(knitr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)  # Load the package
library(openxlsx)
library(readxl)  # Load the package
library(caret)
library(Metrics)              # Load the package
library(randomForest)

library(MASS)  # Load the package
library(pscl)
library(ggplot2)
library(dplyr)
library(stats)
library(VIM)
library(lubridate)
library(splines)  # Needed for natural splines if required
save.image("my_workspace.RData")  # Saves everything
 load("my_workspace.RData")

####################################
####################
##########################

#selecting the file containing the dataset
file_path <- file.choose()
print(file_path)  # Display the selected file path

# Extract directory path
dir_path <- dirname(file_path)

# Set working directory
setwd(dir_path)

writeLines(file_path, "file_path.txt")

# Read the saved file path
file_path <- readLines("file_path.txt")

# Load the data
myprojectdata <- read.csv(file_path)
View(myprojectdata)

myprojectdata <- read.csv("C:\\Users\\pazamet\\OneDrive\\SPRING 2025\\
                          MATH 453 REG ANA\\
                          Baccidents_2017_to_2023_english.csv")

View(myprojectdata)

# Inspect the first few rows
head(myprojectdata)


# Check the structure of the dataset
str(myprojectdata)

# Summary statistics
summary(myprojectdata)

# Check for missing values
colSums(is.na(myprojectdata))

# Check for duplicates
sum(duplicated(myprojectdata))
class(myprojectdata$Date)
# Convert the Date column from character to Date
myprojectdata$Date <- as.Date(myprojectdata$Date, format = "%m/%d/%Y")

# Extract the year
myprojectdata$year <- format(myprojectdata$Date, "%Y")

# Extract the month
myprojectdata$month <- format(myprojectdata$Date, "%m")

# Extract the day
myprojectdata$day <- format(myprojectdata$Date, "%d")

class(myprojectdata$km)

# Remove commas and convert the km column to numeric
myprojectdata$km <- as.numeric(gsub(",", ".", myprojectdata$km))
myprojectdata$km[is.na(myprojectdata$km)] <- 
  median(myprojectdata$km, na.rm = TRUE)

sum(is.na(myprojectdata$km))


# Extract the hour as numeric value
myprojectdata$hour_of_day <- as.numeric(format(myprojectdata$hour, "%H"))



class(myprojectdata$hour_of_day)




# Convert categorical columns to factors
str(myprojectdata)
class(myprojectdata)
myprojectdata <- myprojectdata %>%
  mutate(
    week_day = as.factor(week_day),
    weather_timestamp = as.factor(weather_timestamp),
    road_type = as.factor(road_type),
    road_delineation = as.factor(road_delineation),
    state = as.factor(state),
    city = as.factor(city),
    road_direction = as.factor(road_direction),
    wheather_condition = as.factor(wheather_condition),
    cause_of_accident = as.factor(cause_of_accident),
    type_of_accident = as.factor(type_of_accident),
    regional = as.factor(regional)
  )
myprojectdata$year <- as.numeric(myprojectdata$year)
myprojectdata$month <- as.numeric(myprojectdata$month)
myprojectdata$day <- as.numeric(myprojectdata$day)




detach("package:MASS", unload = TRUE)

# Select only the variables of concern
variables_of_interest <- c("year", "month", "day", "week_day", 
                           "hour_of_day", "weather_timestamp", 
                           "wheather_condition", 
                           "road_type", "road_direction", "road_delineation",
                           "km","vehicles_involved","people", "deaths")
names(myprojectdata)
print(variables_of_interest)
class(variables_of_interest)


# Create a new dataframe with the selected variables
filtered_data <- myprojectdata %>% select(all_of(variables_of_interest))
filtered_data <- myprojectdata %>% select(year, month, day, week_day, 
                                          hour_of_day, weather_timestamp, 
                                          wheather_condition, 
                                          road_type,road_direction, road_delineation,
                                          km,vehicles_involved,people,deaths)
filtered_data <- myprojectdata %>%
  select(
    year, month, day, week_day, 
    hour_of_day, weather_timestamp, 
    wheather_condition, road_type, 
    road_direction, road_delineation, 
    km, vehicles_involved, people, deaths
  )


# View the structure of the new dataframe
str(filtered_data)

View(filtered_data)

summary(myprojectdata)
summary(filtered_data)
# Inspect the structure of the dataset
str(filtered_data)

# Summary statistics
summary(filtered_data)

# Save the subsetted dataframe
write.csv("filtered_data.csv", row.names = FALSE)



###################data analysis and visualization

###################data analysis and visualization

###################data analysis and visualization

###################data analysis and visualization

###################data analysis and visualization


###################data analysis and visualization

########################Perform exploratory data analysis (EDA) to 
#identify trends and patterns in the data.############
########################Perform exploratory data analysis (EDA) to 
#identify trends and patterns in the data.############
########################Perform exploratory data analysis (EDA) to 
#identify trends and patterns in the data.############
########################Perform exploratory data analysis (EDA) to 
#identify trends and patterns in the data.############
# Histogram for numeric variables

# Create the bar plot
ggplot(filtered_data, aes(x = factor(year), y = deaths, fill = week_day)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Deaths by Year and Weekday",
       x = "Year",
       y = "Number of Deaths",
       fill = "Weekday") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(filtered_data, aes(x = deaths)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Deaths", x = "Deaths", y = "Frequency")

ggplot(filtered_data, aes(x = vehicles_involved)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Distribution of Vehicles Involved", x = "Vehicles Involved", 
       y = "Frequency")

ggplot(filtered_data, aes(x = km)) +
  geom_histogram(binwidth = 50, fill = "orange", color = "black") +
  labs(title = "Distribution of Kilometer Markers", x = "Kilometer",
       y = "Frequency")

####################
# Bar plots for categorical variables
ggplot(filtered_data, aes(x = week_day)) +
  geom_bar(fill = "navy") +
  labs(title = "Accidents by Day of the Week", x = "Day of the Week", 
       y = "Count")

ggplot(filtered_data, aes(x = weather_timestamp)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Accidents by Time of Day", x = "Time of Day", y = "Count")

ggplot(filtered_data, aes(x = road_type)) +
  geom_bar(fill = "brown") +
  labs(title = "Accidents by Road Type", x = "Road Type", y = "Count")



####################

ggplot(filtered_data, aes(x = km, y = deaths)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              color = "blue") +  # Quadratic effect
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), 
              color = "red") +   # Cubic effect
  labs(title = "Effect of Distance on Fatalities",
       x = "Distance (km)", y = "Deaths") +
  theme_minimal()

ggplot(filtered_data, 
       aes(x = km, y = deaths, color = road_type)) +
  geom_smooth(method = "glm", formula = y ~ x) +
  labs(title = "")

ggplot(filtered_data, 
       aes(x = people, y = deaths, color = road_type)) +
  geom_smooth(method = "glm", formula = y ~ x) +
  labs(title = "")

ggplot(filtered_data, 
       aes(x = people, y = deaths, color = road_delineation)) +
  geom_smooth(method = "glm", formula = y ~ x) +
  labs(title = "")






ggplot(filtered_data$hour_of_day, aes(x = Var1, y = Freq)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Accidents by Hour of the Day", 
       x="Hour of Day", y="Number of Accidents") +
  theme_minimal()




# Create a bar plot of accidents by year
ggplot(data = as.data.frame(year),
       aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue", 
           color = "black") + 
  labs(x = "Year", y = "Number of Accidents",
       title = "Number of Accidents by Year") +
  theme_minimal()


# Scatter plot of distance traveled vs total injured
ggplot(myprojectdata, aes(x = km, y = total_injured)) +
  geom_point() +
  labs(title = "Distance Traveled vs Total Injured",
       x = "Distance (km)", y = "Total Injured") +
  theme_minimal()

# Scatter plot of distance traveled vs  people
ggplot(myprojectdata, aes(x = km, y =  people)) +
  geom_point() +
  labs(title = "Distance Traveled vs  people",
       x = "Distance (km)", y = " people") +
  theme_minimal()

# Scatter plot of distance traveled vs deaths
ggplot(myprojectdata, aes(x = km, y = deaths)) +
  geom_point() +
  labs(title = "Distance Traveled vs Deaths",
       x = "Distance (km)", y = "Deaths") +
  theme_minimal()


ggplot(myprojectdata, aes(x = km, y = deaths)) +
  geom_point(alpha = 0.3) +
  labs(title = "Distance Traveled vs Deaths", 
       x = "Distance (km)", y = "Deaths") +
  theme_minimal()



ggplot(filtered_data, aes(x = vehicles_involved,
                                   y = deaths,
                                   color = road_delineation)) +
  geom_smooth(method = "glm", formula = y ~ x,
              family = "poisson") +
labs(title = "Interaction Effect of road_delineation and Veh Involved on deaths",
       x = "Number of Vehicles Involved", 
       y = "deaths") +
  theme_minimal()

ggplot(train_data, 
       aes(x = vehicles_involved, y = deaths, color = road_type)) +
  geom_smooth(method = "glm", formula = y ~ x, family = "poisson") +
  labs(title = "Effect of Vehicles Involved on Fatalities by Road Type")


#######
# Scatter plot
ggplot(filtered_data, aes(x = vehicles_involved, y = deaths)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Deaths vs. Vehicles Involved", x = "Vehicles Involved",
       y = "Deaths")
############################

ggplot(myprojectdata_filtered, 
       aes(x = vehicles_involved, y = deaths, color = road_type)) +
  geom_smooth(method = "glm", formula = y ~ x, family = "poisson") +
  labs(title = "Effect of Vehicles Involved on Fatalities by Road Type")


ggplot(filtered_data, aes(x = vehicles_involved,
                          y = deaths,
                          color = road_type)) +
  geom_smooth(method = "glm", formula = y ~ x,
              family = "poisson") +
  labs(title = "Int Effect of Road Type and Veh Involved on deaths",
       x = "Number of Vehicles Involved", 
       y = "Predicted Fatalities") +
  theme_minimal()

ggplot(train_data, aes(x = vehicles_involved,
                       y = deaths,
                       color = road_type)) +
  geom_smooth(method = "glm", formula = y ~ x,
              family = "poisson") +
  labs(title = "Int Effect of Road Type and Veh Involved on deaths",
       x = "Number of Vehicles Involved", 
       y = "Predicted Fatalities") +
  theme_minimal()

ggplot(train_data, aes(x = vehicles_involved,
                       y = deaths,
                       color = road_delineation)) +
  geom_smooth(method = "glm", formula = y ~ x,
              family = "poisson") +
labs(title = "Interaction Effect of road_delineation and Veh Involved on deaths",
       x = "Number of Vehicles Involved", 
       y = "deaths") +
  theme_minimal()











# Summarize total deaths by weekday across all years
total_deaths_by_weekday <- death_summary %>%
  summarise(
    Friday = sum(friday),
    Monday = sum(monday),
    Saturday = sum(saturday),
    Sunday = sum(sunday),
    Thursday = sum(thursday),
    Tuesday = sum(tuesday),
    Wednesday = sum(wednesday)
  )

# Compute total deaths across all weekdays
total_deaths_by_weekday$Total <- rowSums(total_deaths_by_weekday)

# Compute percentage for each weekday
weekday_percentages <- total_deaths_by_weekday %>%
  mutate(across(everything(), ~ round(. / Total * 100, 2))) 

# Combine total deaths and percentages into one table
final_table <- rbind(total_deaths_by_weekday, weekday_percentages)
rownames(final_table) <- c("Total Deaths", "Percentage")

# Convert to LaTeX table with professional formatting
latex_table <- xtable(final_table, 
            caption = "Total Number of Deaths by Weekday (with Percentages)")

# Print LaTeX code
print(latex_table, include.rownames = TRUE, booktabs = TRUE)



ggplot(filtered_data, aes(x = road_type)) +
  geom_bar(fill = "brown") +
  labs(title = "Accidents by Road Type", x = "Road Type", y = "Count")



# Box plot
ggplot(filtered_data, aes(x = road_type, y = deaths)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Deaths by Road Type", x = "Road Type", y = "Deaths")


# Box plot
ggplot(filtered_data, aes(x = wheather_condition, y = deaths)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Deaths by Weather Condition", x = "Weather Condition",
       y = "Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Box plot
ggplot(filtered_data, aes(x = wheather_condition, y = deaths)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Deaths by Weather Condition", x = "Weather Condition", 
       y = "Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Line plot
filtered_data %>%
  group_by(year) %>%
  summarise(accidents = n()) %>%
  ggplot(aes(x = year, y = accidents)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "", x = "Year", y = "Number of Accidents")



# Bar plot
filtered_data %>%
  group_by(month) %>%
  summarise(accidents = n()) %>%
  ggplot(aes(x = month, y = accidents)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Accidents by Month", x = "Month", y = "Number of Accidents")

# Line plot
filtered_data %>%
  group_by(hour_of_day) %>%
  summarise(accidents = n()) %>%
  ggplot(aes(x = hour_of_day, y = accidents)) +
  geom_line(color = "green") +
  geom_point(color = "purple") +
  labs(title = "", x = "Hour of Day", y = "Number of Accidents")


# Select numeric variables
numeric_data <- filtered_data %>%
  select(deaths, vehicles_involved, people, km)

# Compute correlation matrix
cor_matrix <- cor(numeric_data)

# Plot correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", 
         tl.srt = 45)



# Function to compute Cramér’s V
cramers_v <- function(var1, var2, data) {
  tbl <- table(data[[var1]], data[[var2]])
  assocstats(tbl)$cramer
}

# Check associations between categorical variables
cat_vars <- c("road_type", "road_delineation", "week_day", "weather_timestamp")

# Generate Cramér's V matrix for categorical variables
cramers_v_matrix <- matrix(NA, nrow = length(cat_vars), ncol = length(cat_vars),
                           dimnames = list(cat_vars, cat_vars))

for (i in 1:length(cat_vars)) {
  for (j in 1:length(cat_vars)) {
    if (i != j) {
      cramers_v_matrix[i, j] <- cramers_v(cat_vars[i], cat_vars[j],
                                          myprojectdata)
    }
  }
}

# Print Cramér’s V matrix
print("Cramér’s V Matrix:")
print(cramers_v_matrix)
# Create the Cramér's V matrix as a dataframe
cramers_v_matrix <- matrix(c(
  NA, 0.0993, 0.0246, 0.0173,
  0.0993, NA, 0.0140, 0.0329,
  0.0246, 0.0140, NA, 0.0680,
  0.0173, 0.0329, 0.0680, NA
), nrow = 4, byrow = TRUE)

# Assign row and column names
rownames(cramers_v_matrix) <- colnames(cramers_v_matrix) <- c("road_type", 
                        "road_delineation", "week_day", "weather_timestamp")

# Convert matrix to long format
cramers_v_df <- melt(cramers_v_matrix, na.rm = TRUE)

# Create heatmap
ggplot(cramers_v_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 3)), color = "white", size = 5) +  # Add values to tiles
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Cramér's V Heatmap", fill = "Cramér's V", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Save a plot
ggsave("deaths_vs_vehicles.png", plot = last_plot(), width = 8, height = 6)
####################################
####################


# Count accidents by day of the important features
table(myprojectdata$week_day)
table(myprojectdata$hour_of_day)
table(myprojectdata$month)
table(myprojectdata$year)
table(myprojectdata$state)
table(myprojectdata$weather_timestamp)
table(myprojectdata$type_of_accident)
table(myprojectdata$wheather_condition)
table(myprojectdata$cause_of_accident)
class(myprojectdata$hour_of_day)
class(myprojectdata$week_day)
class(myprojectdata$city)
class(myprojectdata$state)
plot(myprojectdata$deaths~myprojectdata$km)
plot(myprojectdata$km~myprojectdata$deaths)



##############

#######################poisson models 


library(MASS)  # Load the package

# Check the structure of the dataset
str(filtered_data)

# Fit Poisson regression model
poisson_model <- glm(deaths ~ road_type + wheather_condition + hour_of_day + 
                       road_delineation + vehicles_involved + people, 
                     data = filtered_data, 
                     family = poisson(link = "log"))

# Summarize the model
summary(poisson_model)

# Check for overdispersion
dispersiontest(poisson_model)

# Exponentiate coefficients to get Incidence Rate Ratios (IRR)
irr <- exp(coef(poisson_model))
print(irr)



nb_model <- glm.nb(deaths ~ road_type + wheather_condition + hour_of_day + 
                     road_delineation + vehicles_involved + people, 
                   data = filtered_data)
summary(nb_model)


#############introducing interaction term
poisson_model_interaction <- glm(deaths ~ road_type * wheather_condition +
                                   hour_of_day + road_delineation + 
                                   vehicles_involved + people, 
                                 data = filtered_data, 
                                 family = poisson(link = "log"))

# Summarize the model
summary(poisson_model_interaction)

# Check for overdispersion
dispersiontest(poisson_model)

# Exponentiate coefficients to get Incidence Rate Ratios (IRR)
irr <- exp(coef(poisson_model))
print(irr)

plot(poisson_model)




# Fit a full Poisson regression model
poisson_full_model <- glm(deaths ~ year + month + day + week_day + hour_of_day +
                            weather_timestamp + wheather_condition + road_type +
                            road_direction + road_delineation + km +
                            vehicles_involved + people, 
                          data = filtered_data, 
                          family = poisson(link = "log"))

# Summarize the model
summary(poisson_full_model)

# Check for overdispersion
dispersiontest(poisson_full_model)

# Exponentiate coefficients to get Incidence Rate Ratios (IRR)
irr_full <- exp(coef(poisson_full_model))
print(irr_full)


# Fit Poisson regression model with 'people' as an offset
poisson_model_offset <- glm(deaths ~ road_type + week_day + wheather_condition + 
                              hour_of_day + road_delineation +
                              vehicles_involved, 
                            offset = log(people),  # Include 'people' as an offset
                            data = filtered_data, 
                            family = poisson(link = "log"))

# Summarize the model
summary(poisson_model_offset)

# Check for overdispersion
dispersiontest(poisson_model_offset)

# Exponentiate coefficients to get Incidence Rate Ratios (IRR)
irr_offset <- exp(coef(poisson_model_offset))
print(irr_offset)


#################
# Fit negative binomial regression model with 'people' as an offset
nb_model_offset <- glm.nb(deaths ~ road_type + week_day + wheather_condition +
                            hour_of_day + road_delineation + vehicles_involved, 
                          offset(log(people)),  # Include 'people' as an offset
                          data = filtered_data)

# Summarize the model
summary(nb_model_offset)

# Exponentiate coefficients to get Incidence Rate Ratios (IRR)
irr_nb_offset <- exp(coef(nb_model_offset))
print(irr_nb_offset)

# Summarize the model

# Check for overdispersion
dispersiontest(poisson_model)

# Exponentiate coefficients to get Incidence Rate Ratios (IRR)
irr <- exp(coef(poisson_model))
print(irr)

plot(poisson_model)




# Fit a full Poisson regression model
poisson_full_model <- glm(deaths ~ year + month + day + week_day + hour_of_day +
                            weather_timestamp + wheather_condition + road_type +
                            road_direction + road_delineation + km +
                            vehicles_involved + people, 
                          data = filtered_data, 
                          family = poisson(link = "log"))

# Summarize the model
summary(poisson_full_model)

# Check for overdispersion
dispersiontest(poisson_full_model)

# Exponentiate coefficients to get Incidence Rate Ratios (IRR)
irr_full <- exp(coef(poisson_full_model))
print(irr_full)


# Fit Poisson regression model with 'people' as an offset
poisson_model_offset <- glm(deaths ~ road_type + wheather_condition +
                              hour_of_day + road_delineation + vehicles_involved, 
                            offset = log(people),  # Include 'people' as an offset
                            data = filtered_data, 
                            family = poisson(link = "log"))

# Summarize the model
summary(poisson_model_offset)

# Check for overdispersion
dispersiontest(poisson_model_offset)

# Exponentiate coefficients to get Incidence Rate Ratios (IRR)
irr_full <- exp(coef(poisson_model_offset))
print(irr_full)



poisson_model_prevbest <- glm(deaths ~ km + vehicles_involved + road_delineation + 
                           week_day + weather_timestamp + road_type * vehicles_involved, 
                         offset = log(people), family = "poisson", 
                         data = filtered_data)
# Summarize the model

# Summarize the model
summary(poisson_model_prevbest)

# Check for overdispersion
dispersiontest(poisson_model_prevbest)

# Exponentiate coefficients to get Incidence Rate Ratios (IRR)
irr_full <- exp(coef(poisson_model_prevbes))
print(irr_full)


poisson_model_crevbest <- glm(deaths ~ km + vehicles_involved + road_delineation + 
                                week_day + weather_timestamp +hour_of_day+ 
                                road_type * vehicles_involved, 
                              offset = log(people), family = "poisson", 
                              data = filtered_data)


# Summarize the model
summary(poisson_model_crevbest)

# Check for overdispersion
dispersiontest(poisson_model_crevbest)

# Exponentiate coefficients to get Incidence Rate Ratios (IRR)
irr_full <- exp(coef(poisson_model_crevbes))
print(irr_full)



poisson_model_try <- glm(deaths ~ km + vehicles_involved + road_delineation + 
                                week_day + weather_timestamp +hour_of_day+ 
                                road_type * vehicles_involved + road_direction, 
                              offset = log(people), family = "poisson", 
                              data = filtered_data)


# Summarize the model
summary(poisson_model_try)

# Check for overdispersion
dispersiontest(poisson_model_try)

# Exponentiate coefficients to get Incidence Rate Ratios (IRR)
irr_full <- exp(coef(poisson_model_try))
print(irr_full)


#######################################################



# Fit a full model with all predictors
full_model <- glm(deaths ~ ., offset = log(people),
                  family = "poisson", data = filtered_data)
min_model <- glm(deaths ~1 , offset = log(people),
                 family = "poisson", data = filtered_data)

summary(min_model)
# Check for overdispersion
dispersiontest(min_model)
# View the final selected model


# Stepwise model selection (both forward and backward)
forward_model <- stepAIC(full_model,offset = log(people), direction = "forward")



backward_model <- stepAIC(full_model, offset = log(people),direction = "backward")

both_model <- stepAIC(full_model,offset = log(people), direction = "both")
summary(both_model)
# Check for overdispersion
dispersiontest(both_model)
# View the final selected model

beststepmodel <- glm(formula = deaths ~ year + month +
                      week_day + hour_of_day + 
                      weather_timestamp + wheather_condition + 
                      road_type + road_direction + 
                      road_delineation + km + vehicles_involved , 
                    family = "poisson", 
                    data = filtered_data, offset = log(people))
summary(beststepmodel)
# Check for overdispersion
dispersiontest(beststepmodel)
# View the final selected model


basepoison <- glm(deaths ~ week_day + hour_of_day + weather_timestamp + 
                    wheather_condition + road_type  + road_delineation + 
                    km + vehicles_involved,offset = log(people),family = "poisson",
                  data = filtered_data)
summary(basepoison)
# Check for overdispersion
dispersiontest(basepoison)


basepoison2 <- glm(deaths ~ week_day + hour_of_day + weather_timestamp + 
                   road_type  + road_delineation + 
                    km + vehicles_involved,offset = log(people),family = "poisson",
                  data = filtered_data)
summary(basepoison2)
# Check for overdispersion
dispersiontest(basepoison2)


poisson_model_int <- glm(deaths ~ km + vehicles_involved + road_delineation + 
                           week_day + weather_timestamp +hour_of_day+ 
                           road_type * vehicles_involved + road_direction, 
                         offset = log(people), family = "poisson", 
                         data = filtered_data)


# Summarize the model
summary(poisson_model_int)

# Check for overdispersion
dispersiontest(poisson_model_int)



poisson_model_int2 <- glm(deaths ~  vehicles_involved + road_delineation + 
                            km *road_type  +week_day +weather_timestamp +
                            hour_of_day+ 
                            vehicles_involved + road_direction, 
                          offset = log(people), family = "poisson", 
                          data = filtered_data)


# Summarize the model
summary(poisson_model_int2)

# Check for overdispersion
dispersiontest(poisson_model_int2)

poisson_model_int3 <- glm(deaths ~  vehicles_involved + hour_of_day+
                            km + road_type  +week_day +weather_timestamp +
                            road_delineation * vehicles_involved +
                            road_direction,offset = log(people), 
                          family = "poisson",data = filtered_data)
                          


# Summarize the model
summary(poisson_model_int3)

# Check for overdispersion
dispersiontest(poisson_model_int3)


# Model with quadratic effect of km
poisson_model_poly <- glm(deaths ~ poly(km, 2) + road_type + week_day 
                   + weather_timestamp + 
                     road_delineation + vehicles_involved, 
                   offset = log(people), family = "poisson",
                   data = filtered_data)
# Summarize the model
summary(poisson_model_poly)

# Check for overdispersion
dispersiontest(poisson_model_poly)

# Model with quadratic effect of vehicles_involved
poisson_model_poly2 <- glm(deaths ~ poly(vehicles_involved, 2) + road_type + week_day 
                          + weather_timestamp + 
                            road_delineation + vehicles_involved, 
                          offset = log(people), family = "poisson",
                          data =filtered_data)
# Summarize the model
summary(poisson_model_poly2)

# Check for overdispersion
dispersiontest(poisson_model_poly2)

# Model with quadratic effect of vehicles_involved
poisson_model_poly3 <- glm(deaths ~ poly(vehicles_involved, 2)+ week_day 
                           + weather_timestamp + 
                             road_delineation + vehicles_involved * road_type , 
                           offset = log(people), family = "poisson",
                           data = filtered_data)
# Summarize the model
summary(poisson_model_poly3)

# Check for overdispersion
dispersiontest(poisson_model_poly3)






AIC(basepoison,poisson_model_int, poisson_model_int2, poisson_model_int3)
dispersion_check <- function(model) {
  dev_ratio <- deviance(model) / df.residual(model)
  return(dev_ratio)
}

dispersion_check(basepoison)
dispersion_check(poisson_model_int)
dispersion_check(poisson_model_int2)
dispersion_check(poisson_model_int3)
dispersion_check(poisson_model_int4)
anova(basepoison)
anova(poisson_model_int2)
anova(poisson_model_int3)
anova(poisson_model_int4)
anova(poisson_model_int)
####################
#########comparing the models
AIC(beststepmodel, basepoison, basepoison2,poisson_model_int,poisson_model_int2,poisson_model_int3,poisson_model_int4)  # Compare multiple models

summary(beststepmodel)$deviance  # Residual deviance
summary(beststepmodel)$df.residual  # Residual degrees of freedom
summary(basepoison)$deviance  # Residual deviance
summary(basepoison)$df.residual  # Residual degrees of freedom
summary(basepoison2)$deviance  # Residual deviance
summary(basepoison2)$df.residual  # Residual degrees of freedom
summary(poisson_model_int)$deviance  # Residual deviance
summary(poisson_model_int)$df.residual  # Residual degrees of freedom
summary(poisson_model_int2)$deviance  # Residual deviance
summary(poisson_model_int2)$df.residual  # Residual degrees of freedom
summary(poisson_model_int3)$deviance  # Residual deviance
summary(poisson_model_int3)$df.residual  # Residual degrees of freedom
summary(poisson_model_int4)$deviance  # Residual deviance
summary(poisson_model_int4)$df.residual  # Residual degrees of freedom

# Likelihood ratio test
# Likelihood ratio test
# Likelihood ratio test
# Likelihood ratio test
anova(beststepmodel,basepoison,basepoison2,poisson_model_int,
      poisson_model_int2,poisson_model_int3, 
      poisson_model_int4,poisson_model_poly,
      poisson_model_poly2,poisson_model_poly3,test = "Chisq")  # Likelihood ratio test

#####################


# List of models
models <- list(
  basepoisson = basepoison,
  basepoisson2 = basepoison2,
  beststepmodel = beststepmodel,
  poisson_model_int = poisson_model_int,
  poisson_model_int2 = poisson_model_int2,
  poisson_model_int3 = poisson_model_int3,
  poisson_model_poly = poisson_model_poly,
  poisson_model_poly2 = poisson_model_poly2,
  poisson_model_poly3 = poisson_model_poly3
)

# Extract model statistics
model_summary <- do.call(rbind, lapply(names(models), function(model_name) {
  model <- models[[model_name]]
  data.frame(
    Model = model_name,
    AIC = AIC(model),
    LogLikelihood = logLik(model),
    Deviance = deviance(model),
    df = df.residual(model)
  )
}))

# Print table
print(model_summary)


###############
# Define models
models <- list(
  basepoisson = basepoison,
  basepoisson2 = basepoison2,
  beststepmodel = beststepmodel,
  poisson_model_int = poisson_model_int,
  poisson_model_int2 = poisson_model_int2,
  poisson_model_int3 = poisson_model_int3,
  poisson_model_poly = poisson_model_poly,
  poisson_model_poly2 = poisson_model_poly2,
  poisson_model_poly3 = poisson_model_poly3
)

# Choose a base model for LRT comparison (e.g., basepoisson)
base_model <- basepoison

# Function to extract model statistics
extract_model_info <- function(model, base_model) {
  dispersion <- sum(residuals(model, type = "pearson")^2) / df.residual(model)
  lrt_pvalue <- anova(base_model, model, test = "Chisq")[2, "Pr(>Chi)"]
  
  data.frame(
    AIC = AIC(model),
    LogLikelihood = logLik(model)[1],
    Deviance = deviance(model),
    Dispersion = dispersion,
    LRT_P_Value = ifelse(is.na(lrt_pvalue), "-", lrt_pvalue)  # Handle NA cases
  )
}

# Extract information from all models
model_summary <- do.call(rbind, lapply(models, extract_model_info, base_model = base_model))

# Add model names
model_summary <- cbind(Model = names(models), model_summary)

# Print table
print(model_summary)

# Convert to LaTeX format using xtable (Optional)
library(xtable)
xtable(model_summary, digits = 4)


##########


#####################################



###########Now spliting the real data into training and testing




# Now try creating the partition again
train_index <- createDataPartition(filtered_data$deaths, p = 0.8, list = FALSE)


# Split data into training (80%) and testing (20%)
set.seed(199) # For reproducibility
train_index <- createDataPartition(filtered_data$deaths, p = 0.8, list = FALSE)
train_data <- filtered_data[train_index, ]
test_data <- filtered_data[-train_index, ]

View(train_data)
View(test_data)

str(train_data)


################fitting the models on just the train data

tbeststepmodel <- glm(formula = deaths ~ year + month +
                       week_day + hour_of_day + 
                       weather_timestamp + wheather_condition + 
                       road_type + road_direction + 
                       road_delineation + km + vehicles_involved , 
                     family = "poisson", 
                     data = train_data, offset = log(people))
summary(tbeststepmodel)
# Check for overdispersion
dispersiontest(tbeststepmodel)
# View the final selected model


tbasepoison <- glm(deaths ~ week_day + hour_of_day + weather_timestamp + 
                    wheather_condition + road_type  + road_delineation + 
                    km + vehicles_involved,offset = log(people),family = "poisson",
                  data = train_data)
summary(tbasepoison)
# Check for overdispersion
dispersiontest(tbasepoison)


tbasepoison2 <- glm(deaths ~ week_day + hour_of_day + weather_timestamp + 
                     road_type  + road_delineation + 
                     km + vehicles_involved,offset = log(people),family = "poisson",
                   data = train_data)
summary(tbasepoison2)
# Check for overdispersion
dispersiontest(tbasepoison2)


tpoisson_model_int <- glm(deaths ~ km + vehicles_involved + road_delineation + 
                           week_day + weather_timestamp +hour_of_day+ 
                           road_type * vehicles_involved + road_direction, 
                         offset = log(people), family = "poisson", 
                         data = train_data)


# Summarize the model
summary(tpoisson_model_int)

# Check for overdispersion
dispersiontest(tpoisson_model_int)




tpoisson_model_int2 <- glm(deaths ~  vehicles_involved + road_delineation + 
                            km *road_type  +week_day +weather_timestamp +
                            hour_of_day+ 
                            vehicles_involved + road_direction, 
                          offset = log(people), family = "poisson", 
                          data = train_data)


# Summarize the model
summary(tpoisson_model_int2)

# Check for overdispersion
dispersiontest(tpoisson_model_int2)

tpoisson_model_int3 <- glm(deaths ~  vehicles_involved + hour_of_day+
                            km + road_type  +week_day +weather_timestamp +
                            road_delineation * vehicles_involved +
                            road_direction,offset = log(people), 
                          family = "poisson",data = train_data)



# Summarize the model
summary(tpoisson_model_int3)

# Check for overdispersion
dispersiontest(tpoisson_model_int3)


# Model with quadratic effect of km
tpoisson_model_poly <- glm(deaths ~ poly(km, 2) + road_type + week_day 
                          + weather_timestamp + 
                            road_delineation + vehicles_involved, 
                          offset = log(people), family = "poisson",
                          data = train_data)
# Summarize the model
summary(tpoisson_model_poly)

# Check for overdispersion
dispersiontest(tpoisson_model_poly)

# Model with quadratic effect of vehicles_involved
tpoisson_model_poly2 <- glm(deaths ~ poly(vehicles_involved, 2) + road_type + week_day 
                           + weather_timestamp + 
                             road_delineation + vehicles_involved, 
                           offset = log(people), family = "poisson",
                           data =train_data)
# Summarize the model
summary(tpoisson_model_poly2)

# Check for overdispersion
dispersiontest(tpoisson_model_poly2)

# Model with quadratic effect of vehicles_involved
tpoisson_model_poly3 <- glm(deaths ~ poly(vehicles_involved, 2)+ week_day 
                           + weather_timestamp + 
                             road_delineation + vehicles_involved * road_type , 
                           offset = log(people), family = "poisson",
                           data = train_data)
# Summarize the model
summary(tpoisson_model_poly3)

# Check for overdispersion
dispersiontest(tpoisson_model_poly3)






AIC(tbasepoison,tpoisson_model_int, tpoisson_model_int2, tpoisson_model_int3)
dispersion_check <- function(model) {
  dev_ratio <- deviance(model) / df.residual(model)
  return(dev_ratio)
}

dispersion_check(basepoison)
dispersion_check(poisson_model_int)
dispersion_check(poisson_model_int2)
dispersion_check(poisson_model_int3)
dispersion_check(poisson_model_int4)
anova(basepoison)
anova(poisson_model_int2)
anova(poisson_model_int3)
anova(poisson_model_int4)
anova(poisson_model_int)
####################
#########comparing the models
AIC(tbeststepmodel, tbasepoison, tbasepoison2,tpoisson_model_int,
    tpoisson_model_int2,tpoisson_model_int3)  # Compare multiple models

summary(beststepmodel)$deviance  # Residual deviance
summary(beststepmodel)$df.residual  # Residual degrees of freedom
summary(basepoison)$deviance  # Residual deviance
summary(basepoison)$df.residual  # Residual degrees of freedom
summary(basepoison2)$deviance  # Residual deviance
summary(basepoison2)$df.residual  # Residual degrees of freedom
summary(poisson_model_int)$deviance  # Residual deviance
summary(poisson_model_int)$df.residual  # Residual degrees of freedom
summary(poisson_model_int2)$deviance  # Residual deviance
summary(poisson_model_int2)$df.residual  # Residual degrees of freedom
summary(poisson_model_int3)$deviance  # Residual deviance
summary(poisson_model_int3)$df.residual  # Residual degrees of freedom
summary(poisson_model_int4)$deviance  # Residual deviance
summary(poisson_model_int4)$df.residual  # Residual degrees of freedom

# Likelihood ratio test
# Likelihood ratio test
# Likelihood ratio test
# Likelihood ratio test
anova(tbeststepmodel,tbasepoison,tbasepoison2,tpoisson_model_int,
      tpoisson_model_int2,tpoisson_model_int3, 
     tpoisson_model_poly,
      tpoisson_model_poly2,tpoisson_model_poly3,test = "Chisq")  # Likelihood ratio test

#####################


# List of models
tmodels <- list(
  tbasepoisson = tbasepoison,
  tbasepoisson2 = tbasepoison2,
  tbeststepmodel = tbeststepmodel,
  tpoisson_model_int = tpoisson_model_int,
 tpoisson_model_int2 = tpoisson_model_int2,
  tpoisson_model_int3 = tpoisson_model_int3,
  tpoisson_model_int4 = tpoisson_model_int4,
  tpoisson_model_poly = tpoisson_model_poly,
  tpoisson_model_poly2 = tpoisson_model_poly2,
  tpoisson_model_poly3 = tpoisson_model_poly3
)

# Extract model statistics
tmodel_summary <- do.call(rbind, lapply(names(tmodels), function(tmodel_name) {
  tmodel <- tmodels[[tmodel_name]]
  data.frame(
    tModel = tmodel_name,
    AIC = AIC(tmodel),
    LogLikelihood = logLik(tmodel),
    Deviance = deviance(tmodel),
    df = df.residual(tmodel)
  )
}))

# Print table
print(tmodel_summary)


###############
# Define models
tmodels <- list(
  tbasepoisson = tbasepoison,
  tbasepoisson2 = tbasepoison2,
  tbeststepmodel = tbeststepmodel,
  tpoisson_model_int = tpoisson_model_int,
  tpoisson_model_int2 = tpoisson_model_int2,
  tpoisson_model_int3 = tpoisson_model_int3,
  tpoisson_model_poly = tpoisson_model_poly,
  tpoisson_model_poly2 = tpoisson_model_poly2,
  tpoisson_model_poly3 = tpoisson_model_poly3
)

# Choose a base model for LRT comparison (e.g., basepoisson)
tbase_model <- tbasepoison

# Function to extract model statistics
textract_model_info <- function(model, tbase_model) {
  dispersion <- sum(residuals(model, type = "pearson")^2) / df.residual(model)
  lrt_pvalue <- anova(tbase_model, model, test = "Chisq")[2, "Pr(>Chi)"]
  
  data.frame(
    AIC = AIC(model),
    LogLikelihood = logLik(model)[1],
    Deviance = deviance(model),
    Dispersion = dispersion,
    LRT_P_Value = ifelse(is.na(lrt_pvalue), "-", lrt_pvalue)  # Handle NA cases
  )
}

# Extract information from all models
tmodel_summary <- do.call(rbind, lapply(tmodels, textract_model_info, tbase_model = tbase_model))

# Add model names
tmodel_summary <- cbind(Model = names(tmodels), tmodel_summary)

# Print table
print(tmodel_summary)

# Convert to LaTeX format using xtable (Optional)
library(xtable)
xtable(tmodel_summary, digits = 4)



###########################
##########################


# Define a function to compute error metrics
evaluate_model <- function(model, test_data) {
  predictions <- predict(model, newdata = test_data, type = "response")
  actual <- test_data$deaths  # Replace with actual response variable
  
  rmse <- rmse(actual, predictions)
  mae <- mae(actual, predictions)
  mean_dev <- mean((actual - predictions)^2 / predictions)  # Poisson deviance
  
  return(c(RMSE = rmse, MAE = mae, Mean_Deviance = mean_dev))
}

# List of models
models <- list(
  tbeststepmodel = tbeststepmodel, 
  tbasepoison = tbasepoison, 
  tbasepoison2 = tbasepoison2, 
  tpoisson_model_int = tpoisson_model_int, 
  tpoisson_model_int2 = tpoisson_model_int2, 
  tpoisson_model_int3 = tpoisson_model_int3, 
  tpoisson_model_poly = tpoisson_model_poly, 
  tpoisson_model_poly2 = tpoisson_model_poly2, 
  tpoisson_model_poly3 = tpoisson_model_poly3
)

# Apply function to all models
results <- sapply(models, evaluate_model, test_data = test_data)

# Convert to a data frame
results_df <- as.data.frame(t(results))
results_df <- round(results_df, 4)  # Round for readability

# Print results
print(results_df)



# Generate predictions for each model on test_data
predictions_tbeststepmodel <- predict(tbeststepmodel, test_data, type = "response")
predictions_tbasepoisson <- predict(tbasepoison, test_data, type = "response")
predictions_tbasepoisson2 <- predict(tbasepoison2, test_data, type = "response")
predictions_tpoisson_int <- predict(tpoisson_model_int, test_data, type = "response")
predictions_tpoisson_int2 <- predict(tpoisson_model_int2, test_data, type = "response")
predictions_tpoisson_int3 <- predict(tpoisson_model_int3, test_data, type = "response")
predictions_tpoisson_poly <- predict(tpoisson_model_poly, test_data, type = "response")
predictions_tpoisson_poly2 <- predict(tpoisson_model_poly2, test_data, type = "response")
predictions_tpoisson_poly3 <- predict(tpoisson_model_poly3, test_data, type = "response")

# Extract the actual deaths from the test data
actual_deaths <- test_data$deaths

# Calculate evaluation metrics for each model
mse_tbeststepmodel <- mse(actual_deaths, predictions_tbeststepmodel)
mse_tbasepoisson <- mse(actual_deaths, predictions_tbasepoisson)
mse_tbasepoisson2 <- mse(actual_deaths, predictions_tbasepoisson2)
mse_tpoisson_int <- mse(actual_deaths, predictions_tpoisson_int)
mse_tpoisson_int2 <- mse(actual_deaths, predictions_tpoisson_int2)
mse_tpoisson_int3 <- mse(actual_deaths, predictions_tpoisson_int3)
mse_tpoisson_poly <- mse(actual_deaths, predictions_tpoisson_poly)
mse_tpoisson_poly2 <- mse(actual_deaths, predictions_tpoisson_poly2)
mse_tpoisson_poly3 <- mse(actual_deaths, predictions_tpoisson_poly3)

mae_tbeststepmodel <- mae(actual_deaths, predictions_tbeststepmodel)
mae_tbasepoisson <- mae(actual_deaths, predictions_tbasepoisson)
mae_tbasepoisson2 <- mae(actual_deaths, predictions_tbasepoisson2)
mae_tpoisson_int <- mae(actual_deaths, predictions_tpoisson_int)
mae_tpoisson_int2 <- mae(actual_deaths, predictions_tpoisson_int2)
mae_tpoisson_int3 <- mae(actual_deaths, predictions_tpoisson_int3)
mae_tpoisson_poly <- mae(actual_deaths, predictions_tpoisson_poly)
mae_tpoisson_poly2 <- mae(actual_deaths, predictions_tpoisson_poly2)
mae_tpoisson_poly3 <- mae(actual_deaths, predictions_tpoisson_poly3)

rmse_tbeststepmodel <- rmse(actual_deaths, predictions_tbeststepmodel)
rmse_tbasepoisson <- rmse(actual_deaths, predictions_tbasepoisson)
rmse_tbasepoisson2 <- rmse(actual_deaths, predictions_tbasepoisson2)
rmse_tpoisson_int <- rmse(actual_deaths, predictions_tpoisson_int)
rmse_tpoisson_int2 <- rmse(actual_deaths, predictions_tpoisson_int2)
rmse_tpoisson_int3 <- rmse(actual_deaths, predictions_tpoisson_int3)
rmse_tpoisson_poly <- rmse(actual_deaths, predictions_tpoisson_poly)
rmse_tpoisson_poly2 <- rmse(actual_deaths, predictions_tpoisson_poly2)
rmse_tpoisson_poly3 <- rmse(actual_deaths, predictions_tpoisson_poly3)

# Create a comparison table
performance_table <- data.frame(
  Model = c("tbeststepmodel", "tbasepoisson", "tbasepoisson2", 
            "tpoisson_model_int", "tpoisson_model_int2", "tpoisson_model_int3", 
            "tpoisson_model_poly", "tpoisson_model_poly2",
            "tpoisson_model_poly3"),
  MSE = c(mse_tbeststepmodel, mse_tbasepoisson, mse_tbasepoisson2, 
          mse_tpoisson_int, mse_tpoisson_int2, mse_tpoisson_int3, 
          mse_tpoisson_poly, mse_tpoisson_poly2, mse_tpoisson_poly3),
  MAE = c(mae_tbeststepmodel, mae_tbasepoisson, mae_tbasepoisson2, 
          mae_tpoisson_int, mae_tpoisson_int2, mae_tpoisson_int3, 
          mae_tpoisson_poly, mae_tpoisson_poly2, mae_tpoisson_poly3),
  RMSE = c(rmse_tbeststepmodel, rmse_tbasepoisson, rmse_tbasepoisson2, 
           rmse_tpoisson_int, rmse_tpoisson_int2, rmse_tpoisson_int3, 
           rmse_tpoisson_poly, rmse_tpoisson_poly2, rmse_tpoisson_poly3)
)

# Print the performance table
print(performance_table)



actual_deaths <- test_data$deaths
head(actual_deaths)
actual_deaths[actual_deaths == 0] <- 0.001
predictions_tbeststepmodel[predictions_tbeststepmodel <= 0] <- 0.001
predictions_tbasepoisson[predictions_tbasepoisson <= 0] <- 0.001
predictions_tbasepoisson2[predictions_tbasepoisson2 <= 0] <- 0.001
predictions_tpoisson_int[predictions_tpoisson_int <= 0] <- 0.001
predictions_tpoisson_int2[predictions_tpoisson_int2 <= 0] <- 0.001
predictions_tpoisson_int3[predictions_tpoisson_int3 <= 0] <- 0.001
predictions_tpoisson_poly[predictions_tpoisson_poly <= 0] <- 0.001
predictions_tpoisson_poly2[predictions_tpoisson_poly2 <= 0] <- 0.001
predictions_tpoisson_poly3[predictions_tpoisson_poly3 <= 0] <- 0.001


# Function to calculate deviance for Poisson models
calculate_deviance <- function(actual, predicted) {
  2 * sum(actual * log(actual / predicted) - (actual - predicted))
}

# Function to calculate pseudo R-squared for Poisson models
calculate_r2 <- function(deviance_model, deviance_null) {
  1 - (deviance_model / deviance_null)
}
null_model <- glm(deaths ~ 1, offset=log(people),data = test_data, 
                  family = poisson)
null_predictions <- predict(null_model, type = "response")
null_predictions[null_predictions <= 0] <- 0.001
null_deviance <- calculate_deviance(actual_deaths, null_predictions)
# Recalculate deviance and R²
deviance_tbeststepmodel <- calculate_deviance(actual_deaths, predictions_tbeststepmodel)
deviance_tbasepoisson <- calculate_deviance(actual_deaths, predictions_tbasepoisson)
deviance_tbasepoisson2 <- calculate_deviance(actual_deaths, predictions_tbasepoisson2)
deviance_tpoisson_int <- calculate_deviance(actual_deaths, predictions_tpoisson_int)
deviance_tpoisson_int2 <- calculate_deviance(actual_deaths, predictions_tpoisson_int2)
deviance_tpoisson_int3 <- calculate_deviance(actual_deaths, predictions_tpoisson_int3)
deviance_tpoisson_poly <- calculate_deviance(actual_deaths, predictions_tpoisson_poly)
deviance_tpoisson_poly2 <- calculate_deviance(actual_deaths, predictions_tpoisson_poly2)
deviance_tpoisson_poly3 <- calculate_deviance(actual_deaths, predictions_tpoisson_poly3)

r2_tbeststepmodel <- calculate_r2(deviance_tbeststepmodel, null_deviance)
r2_tbasepoisson <- calculate_r2(deviance_tbasepoisson, null_deviance)
r2_tbasepoisson2 <- calculate_r2(deviance_tbasepoisson2, null_deviance)
r2_tpoisson_int <- calculate_r2(deviance_tpoisson_int, null_deviance)
r2_tpoisson_int2 <- calculate_r2(deviance_tpoisson_int2, null_deviance)
r2_tpoisson_int3 <- calculate_r2(deviance_tpoisson_int3, null_deviance)
r2_tpoisson_poly <- calculate_r2(deviance_tpoisson_poly, null_deviance)
r2_tpoisson_poly2 <- calculate_r2(deviance_tpoisson_poly2, null_deviance)
r2_tpoisson_poly3 <- calculate_r2(deviance_tpoisson_poly3, null_deviance)


# Add deviance and R² to the performance table
performance_table$Deviance <- c(deviance_tbeststepmodel, 
                                deviance_tbasepoisson, deviance_tbasepoisson2, 
                                deviance_tpoisson_int, 
                                deviance_tpoisson_int2, 
                                deviance_tpoisson_int3, 
                                deviance_tpoisson_poly, 
                                deviance_tpoisson_poly2, deviance_tpoisson_poly3)
performance_table$R2 <- c(r2_tbeststepmodel, r2_tbasepoisson, r2_tbasepoisson2, 
                          r2_tpoisson_int, r2_tpoisson_int2, r2_tpoisson_int3, 
                          r2_tpoisson_poly, r2_tpoisson_poly2, r2_tpoisson_poly3)

# Print the updated performance table
print(performance_table)



#Due to the performance of my laptop the random forest model is unable to 
####run properly on the whole train dataset
# Train Random Forest on real-world train data
set.seed(111)
train_sample <- train_data[sample(nrow(train_data), size = 10000), ]  # Take a random sample

View(train_sample)


# Train a Random Forest model
set.seed(123) # For reproducibility
rf_model <- randomForest(deaths ~ ., data = train_sample, ntree = 500, importance = TRUE)

# Generate predictions on test data
rf_predictions <- predict(rf_model, test_data)


actual_deaths <- test_data$deaths
actual_deaths[actual_deaths == 0] <- 0.001
rf_predictions <- predict(rf_model, test_data)
rf_predictions[rf_predictions <= 0] <- 0.001

null_model <- glm(deaths ~ 1, offset=log(people),data = test_data, 
                  family = poisson)
null_predictions <- predict(null_model, type = "response")
null_predictions[null_predictions <= 0] <- 0.001
null_deviance <- calculate_deviance(actual_deaths, null_predictions)


# Function to calculate deviance for Poisson models
calculate_deviance <- function(actual, predicted) {
  # Add a small constant to avoid log(0)
  actual <- actual + 0.001
  predicted <- predicted + 0.001
  2 * sum(actual * log(actual / predicted) - (actual - predicted))
}

# Function to calculate pseudo R-squared for Poisson models
calculate_r2 <- function(deviance_model, deviance_null) {
  1 - (deviance_model / deviance_null)
}


# Evaluate performance
rf_mse <- mse(test_data$deaths, rf_predictions)
rf_mae <- mae(test_data$deaths, rf_predictions)
rf_rmse <- rmse(test_data$deaths, rf_predictions)
rf_deviance <- calculate_deviance(actual_deaths, rf_predictions)
rf_r2 <- calculate_r2(rf_deviance, null_deviance) # Use null_deviance from earlier

# Compare with the best Poisson model
poisson_mse <- 0.1081
poisson_mae <- 0.1404
poisson_rmse <- 0.3288
poisson_deviance <- 34639.13
poisson_r2 <- 0.0671

# Create a comparison table
comparison_table <- data.frame(
  Model = c("Poisson Model (tpoisson_model_int)", "Random Forest"),
  MSE = c(poisson_mse, rf_mse),
  MAE = c(poisson_mae, rf_mae),
  RMSE = c(poisson_rmse, rf_rmse),
  Deviance = c(poisson_deviance, rf_deviance),
  R2 = c(poisson_r2, rf_r2)
)

# Print the comparison table
print(comparison_table)




# Define the tuning grid
tune_grid <- expand.grid(mtry = c(2, 5, 10)) # Example values for mtry

# Set up cross-validation
train_control <- trainControl(method = "cv", number = 5) # 5-fold cross-validation

# Train the Random Forest model with tuning
rf_tuned <- train(
  deaths ~ ., 
  data = train_sample, 
  method = "rf", 
  trControl = train_control, 
  tuneGrid = tune_grid, 
  ntree = 500
)

# Print the best tuning parameters
print(rf_tuned$bestTune)

# Generate predictions on test data
rf_tuned_predictions <- predict(rf_tuned, test_data)

# Evaluate performance
rf_tuned_mse <- mse(test_data$deaths, rf_tuned_predictions)
rf_tuned_mae <- mae(test_data$deaths, rf_tuned_predictions)
rf_tuned_rmse <- rmse(test_data$deaths, rf_tuned_predictions)
rf_tuned_deviance <- calculate_deviance(test_data$deaths, rf_tuned_predictions)
rf_tuned_r2 <- calculate_r2(rf_tuned_deviance, null_deviance)

# Add to the comparison table
comparison_table <- rbind(
  comparison_table,
  data.frame(
    Model = "Tuned Random Forest",
    MSE = rf_tuned_mse,
    MAE = rf_tuned_mae,
    RMSE = rf_tuned_rmse,
    Deviance = rf_tuned_deviance,
    R2 = rf_tuned_r2
  )
)

# Print the updated comparison table
print(comparison_table)




# Generate predictions using the Poisson model
poisson_predictions <- predict(tpoisson_model_int, test_data, type = "response")

# Generate predictions using the Random Forest model
rf_predictions <- predict(rf_model, test_data)

# Create a comparison dataframe
comparison_results <- data.frame(
  Actual = test_data$deaths,
  Poisson_Predictions = poisson_predictions,
  RF_Predictions = rf_predictions
)

# View the first few rows of the comparison dataframe
head(comparison_results, n=20)




# Load ggplot2 for visualization
library(ggplot2)

# Scatterplot for Poisson model
ggplot(comparison_results, aes(x = Actual, y = Poisson_Predictions)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "",
       x = "Actual Deaths",
       y = "Predicted Deaths") +
  theme_minimal()

# Scatterplot for Random Forest model
ggplot(comparison_results, aes(x = Actual, y = RF_Predictions)) +
  geom_point(alpha = 0.5, color = "green") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "",
       x = "Actual Deaths",
       y = "Predicted Deaths") +
  theme_minimal()

# Calculate residuals
comparison_results$Poisson_Residuals <- comparison_results$Actual - comparison_results$Poisson_Predictions
comparison_results$RF_Residuals <- comparison_results$Actual - comparison_results$RF_Predictions

# Residual plot for Poisson model
ggplot(comparison_results, aes(x = Poisson_Predictions, y = Poisson_Residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "",
       x = "Predicted Deaths",
       y = "Residuals") +
  theme_minimal()

# Residual plot for Random Forest model
ggplot(comparison_results, aes(x = RF_Predictions, y = RF_Residuals)) +
  geom_point(alpha = 0.5, color = "green") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "",
       x = "Predicted Deaths",
       y = "Residuals") +
  theme_minimal()


# Distribution plot
library(tidyr)

# Reshape the data for plotting
comparison_long <- comparison_results %>%
  pivot_longer(cols = c(Poisson_Predictions, RF_Predictions),
               names_to = "Model",
               values_to = "Predictions")

# Plot distributions
ggplot(comparison_long, aes(x = Predictions, fill = Model)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Actual)), color = "red", linetype = "dashed") +
  labs(title = "",
       x = "Deaths",
       y = "Density") +
  theme_minimal()


###################











View(test_data)
View(train_data)


# Select a random sample of test data
set.seed(123)  # Ensures reproducibility
sample_indices <- sample(nrow(test_data), 10)  # Select 10 random rows
test_sample <- test_data[sample_indices, ]
view(test_sample)

# Get actual deaths
test_sample$actual_deaths <- test_sample$deaths

# Get Poisson model predictions
test_sample$poisson_pred <- predict(tpoisson_model_int, newdata = test_sample, type = "response")

# Get Random Forest predictions
test_sample$rf_pred <- predict(rf_model, newdata = test_sample)

# Create a comparison table
comparison_sample <- test_sample %>%
  select(actual_deaths, poisson_pred, rf_pred)

# Print the comparison
print(comparison_sample)
########################
#####################
###################
###################simulations
##################
########################
########################
########################
########################
#####################
###################
###################simulations
##################
########################
########################
#######################

tpoisson_model_int <- glm(deaths ~ km + vehicles_involved + road_delineation + 
                            week_day + weather_timestamp +hour_of_day+ 
                            road_type * vehicles_involved + road_direction, 
                          offset = log(people), family = "poisson", 
                          data = train_data)


summary(tpoisson_model_int)


dispersiontest(tpoisson_model_int)

# Extract coefficients from the best Poisson model
beta <- coef(tpoisson_model_int)
print(beta)  # Check estimated coefficients

set.seed(713)  # Ensure reproducibility
n<-10000
sim_data <- data.frame(
  km = runif(n, min = min(train_data$km, na.rm = TRUE), 
             max = max(train_data$km, na.rm = TRUE)),  # Random distances
  road_delineation = sample(levels(train_data$road_delineation), n, replace = TRUE),
  week_day = sample(levels(train_data$week_day), n, replace = TRUE),
  weather_timestamp = sample(levels(train_data$weather_timestamp), n, replace = TRUE),
  hour_of_day = sample(min(train_data$hour_of_day, 
                           na.rm = TRUE):max(train_data$hour_of_day, na.rm = TRUE),
                       n, replace = TRUE),
  road_type = sample(levels(train_data$road_type), n, replace = TRUE),
  road_direction = sample(levels(train_data$road_direction), n, replace = TRUE),
  vehicles_involved = sample(min(train_data$vehicles_involved,
                                 na.rm = TRUE):max(train_data$vehicles_involved, na.rm = TRUE),
                             n, replace = TRUE),
  people = sample(min(train_data$people, na.rm = TRUE):max(train_data$people, na.rm = TRUE),
                  n, replace = TRUE)
)

View(sim_data)

# Ensure categorical variables are factors
sim_data$road_delineation <- factor(sim_data$road_delineation, 
                      levels = levels(train_data$road_delineation))
sim_data$week_day <- factor(sim_data$week_day, 
                            levels = levels(train_data$week_day))
sim_data$weather_timestamp <- factor(sim_data$weather_timestamp, 
                           levels = levels(train_data$weather_timestamp))
sim_data$road_type <- factor(sim_data$road_type, 
                             levels = levels(train_data$road_type))
sim_data$road_direction <- factor(sim_data$road_direction, 
                                  levels = levels(train_data$road_direction))

# Compute expected fatalities (lambda) using the model
sim_data$lambda <- exp(predict(tpoisson_model_int, newdata = sim_data,
                               type = "link"))

# Simulate fatalities using Poisson distribution
sim_data$deaths <- rpois(n = 10000, lambda = sim_data$lambda)

sim_data[, remove(fatalities)]

# View first few rows of simulated data
head(sim_data)

# Remove the 'fatalities' column
sim_data <- sim_data[, !(names(sim_data) %in% c("fatalities"))]

# Check structure to confirm removal
str(sim_data)

View(sim_data)

########################
set.seed(321)  # For reproducibility
sample_size <- floor(0.8 * nrow(sim_data))
train_indices <- sample(seq_len(nrow(sim_data)), size = sample_size)

sim_train_data <- sim_data[train_indices, ]
sim_test_data <- sim_data[-train_indices, ]
View(sim_train_data)
View(sim_test_data)


best_poisson_model <- glm(
  deaths ~ km + vehicles_involved + road_delineation + week_day + 
    weather_timestamp + hour_of_day + road_type * vehicles_involved + 
    road_direction,
  offset = log(people),  # Include the offset term
  family = poisson,
  data = sim_train_data
)


sim_test_data$predicted_deaths <- predict(best_poisson_model, 
          newdata = sim_test_data, type = "response")


comparison <- data.frame(
  Actual = sim_test_data$deaths,
  Predicted = sim_test_data$predicted_deaths
)

head(comparison, n=20)  # View the first few rows of the comparison



# Extract estimated coefficients
estimated_coefficients <- coef(best_poisson_model)

# True parameters used in simulation (replace with your actual values)
true_coefficients <- c(
  Intercept = -4.0,
  km = 0.0001,
  vehicles_involved = -0.3,
  road_delineation_Bridge = 0.9,
  road_delineation_Curve = 0.8,
  road_delineation_Intersection = -0.3,
  road_delineation_Not_Reported = 0.8,
  road_delineation_Overpass = 0.15,
  road_delineation_Roundabout = -0.5,
  road_delineation_Straight = 0.7,
  road_delineation_Temporary_Detour = 0.85,
  road_delineation_Tunnel = -0.2,
  week_day_Monday = 0.01,
  week_day_Saturday = 0.09,
  week_day_Sunday = 0.15,
  week_day_Thursday = 0.02,
  week_day_Tuesday = 0.0,
  week_day_Wednesday = -0.04,
  weather_timestamp_Night = 0.67,
  weather_timestamp_Sunrise = 0.64,
  weather_timestamp_Sunset = 0.23,
  hour_of_day = -0.009,
  road_type_Multiple = 0.36,
  road_type_Simple = 0.1,
  road_direction_Increasing = 0.06,
  road_direction_Not_Informed = -1.35,
  `vehicles_involved:road_type_Multiple` = -0.29,
  `vehicles_involved:road_type_Simple` = 0.34
)

# Create a comparison table
parameter_comparison <- data.frame(
  Parameter = names(estimated_coefficients),
  Estimated = estimated_coefficients,
  True = true_coefficients
)

print(parameter_comparison)

# Calculate evaluation metrics
mae <- mean(abs(sim_test_data$deaths - sim_test_data$predicted_deaths))
rmse <- sqrt(mean((sim_test_data$deaths - sim_test_data$predicted_deaths)^2))
mape <- mean(abs((sim_test_data$deaths - sim_test_data$predicted_deaths) / sim_test_data$deaths)) * 100

cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")
cat("MAPE:", mape, "\n")
###########################################


# Train the Random Forest model
rf_model <- randomForest(
  deaths ~ .,
  data = sim_train_data,
  ntree = 500,
  importance = TRUE
)

# Predict on the test data
sim_test_data$rf_predicted_deaths <- predict(rf_model, newdata = sim_test_data)

# Evaluate Random Forest performance
rf_mae <- mean(abs(sim_test_data$deaths - sim_test_data$rf_predicted_deaths))
rf_rmse <- sqrt(mean((sim_test_data$deaths - sim_test_data$rf_predicted_deaths)^2))
rf_mape <- mean(abs((sim_test_data$deaths - sim_test_data$rf_predicted_deaths) / sim_test_data$deaths)) * 100

cat("Random Forest MAE:", rf_mae, "\n")
cat("Random Forest RMSE:", rf_rmse, "\n")
cat("Random Forest MAPE:", rf_mape, "\n")

sim_performance_comparison <- data.frame(
  Model = c("Poisson", "Random Forest"),
  MAE = c(mae, Forest_mae),
  RMSE = c(rmse, Forest_rmse),
  MAPE = c(mape, Forest_mape)
)

print(sim_performance_comparison)




# For Poisson model
null_deviance <- best_poisson_model$null.deviance
residual_deviance <- best_poisson_model$deviance
mcfadden_r2 <- 1 - (residual_deviance / null_deviance)

# For Random Forest
rf_r2 <- rf_model$rsq[length(rf_model$rsq)]  # R-squared for the last tree

mse <- mean((sim_test_data$deaths - sim_test_data$predicted_deaths)^2)
rf_mse <- mean((sim_test_data$deaths - sim_test_data$rf_predicted_deaths)^2)


naive_forecast <- mean(sim_train_data$deaths)
naive_mae <- mean(abs(sim_test_data$deaths - naive_forecast))
mase <- mae / naive_mae
rf_mase <- rf_mae / naive_mae


poisson_aic <- AIC(best_poisson_model)
rf_aic <- NA  # AIC is not directly applicable to Random Forest

poisson_loglik <- logLik(best_poisson_model)
rf_loglik <- NA  # Log-likelihood is not directly applicable to Random Forest

poisson_deviance <- deviance(best_poisson_model)
rf_deviance <- NA  # Deviance is not directly applicable to Random Forest


residuals <- sim_test_data$deaths - sim_test_data$predicted_deaths
plot(sim_test_data$predicted_deaths, residuals, main = "", xlab = "Predicted", ylab = "Residuals")
abline(h = 0, col = "red")


performance_comparison <- data.frame(
  Model = c("Poisson", "Random Forest"),
  MAE = c(mae, rf_mae),
  RMSE = c(rmse, rf_rmse),
  R2 = c(mcfadden_r2, rf_r2),
  MSE = c(mse, rf_mse),
  MASE = c(mase, rf_mase)
)

print(performance_comparison)

# Create a comparison table
comparison_table <- data.frame(
  Actual = sim_test_data$deaths,  # Actual deaths
  Poisson_Predicted = sim_test_data$predicted_deaths,  # Predicted deaths from Poisson model
  RF_Predicted = sim_test_data$rf_predicted_deaths  # Predicted deaths from Random Forest model
)

# Display the first 20 rows of the comparison table
head(comparison_table, n = 20)






comparison_long <- pivot_longer(comparison_table, cols = -Actual, names_to = "Model", values_to = "Predicted")

# Create a scatterplot
ggplot(comparison_long, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "",
    x = "Actual Deaths",
    y = "Predicted Deaths",
    color = "Model"
  ) +
  theme_minimal()
######################################
save.image("my_workspace.RData")  # Saves everything




###########################################
######################










