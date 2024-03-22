library(psych)
library('KMsurv')
library("survival")
library(tidyverse).
library(lubridate)
library(ggplot2)
library("survminer")
library("lifecontingencies")
library("wordcloud")
library("openxlsx")
library(gridExtra)
library(dplyr)
library("pracma")
library("reshape2")



# Read the data from a CSV file
data <- read_csv("../Data/Processed Data/CLEANED_2024-srcsc-superlife-inforce-dataset.csv")

# Create a new column 'Lifespan' using the provided formula
data$Lifespan <- data$Issue.age + (2024 - data$Issue.year)


# Create a function to categorize age groups into 10-year intervals
get_age_group <- function(age) {
  if (age >= 25  && age <= 44) {
    return("25-44")
  } else if (age >= 45 && age <= 59) {
    return("45-59")
  } else if (age >= 60) {
    return("60+")
  } else {
    return("Other")
  }
}


# Apply the function to create a new column 'Age_Group' based on 'Lifespan'
data$Age_Group <- sapply(data$Lifespan, get_age_group)

# Separate deceased and censored policyholders
deceased_data <- data[!is.na(data$Death.indicator), ]
censored_data <- data[is.na(data$Death.indicator), ]
policyholder_data <- censored_data[is.na(censored_data$Lapse.Indicator), ]

# Print the distribution of ages in each age group
age_group_distribution <- prop.table(table(data$Age_Group)) 
print(age_group_distribution)

#load incentives data
# Create a dataset in R based on the provided table. Cranked the table in chatgpt to hardcode it into a table 
intervention_data <- data.frame(
  Intervention_Name = c("Wellness Programs", "Fitness Tracking Incentives", "Smoking Cessation Programs",
                        "Annual Health Check-ups", "Telemedicine Services", "Healthy Eating Campaigns",
                        "Weight Management Programs", "Mental Health Support","Financial Planning Assistance", "Educational Workshops", "Incentives for Vaccinations",
                        "Regular Dental Check-ups", "Vision Care Programs", "Safety Campaigns",
                        "Driving Safety Courses", "Heart Health Screenings", "Chronic Disease Management",
                        "Sleep Hygiene Programs", "Community Fitness Challenges", "Discounted Gym Memberships",
                        "Online Health Resources", "Personalized Health Plans", "Well-being Apps",
                        "Hydration Campaigns", "Sun Safety Awareness", "Emergency Preparedness Training",
                        "Social Connection Initiatives", "Holistic Stress Reduction",
                        "Financial Incentives for Healthy Behavior", "Genetic Testing",
                        "Alcohol Moderation Programs", "Environmental Wellness",
                        "Employee Assistance Programs", "Holistic Nutrition Education",
                        "Incentives for Preventive Screenings", "Holistic Health Assessments",
                        "Cancer Prevention Initiatives", "Community Gardens",
                        "Active Aging Programs", "Home Safety Inspections",
                        "Mindfulness Programs", "Parenting Support Services",
                        "Travel Safety Tips", "Financial Literacy Workshops","Hiking and Outdoor Activities Groups",
                        "Cognitive Health Programs",
                        "Art and Creativity Classes",
                        "Mind-Body Wellness Retreats",
                        "Incentives for Regular Medication Adherence",
                        "Ergonomic Workstation Assessments"), 
  Impact_on_Mortality = c("2-5% reduction", "3-6% reduction", "0-50% reduction",
                          "5-10% reduction", "3-5% reduction", "2-4% reduction",
                          "5-10% reduction", "3-8% reduction","2-4% reduction", "2-4% reduction", "2-8% reduction",
                          "2-4% reduction", "2-3% reduction", "3-5% reduction",
                          "2-4% reduction", "5-10% reduction", "5-10% reduction",
                          "3-5% reduction", "2-5% reduction", "3-6% reduction",
                          "2-4% reduction", "3-6% reduction", "2-4% reduction",
                          "2-3% reduction", "2-4% reduction", "2-4% reduction",
                          "3-5% reduction", "3-8% reduction", "2-5% reduction",
                          "2-4% reduction", "3-6% reduction", "2-4% reduction",
                          "2-4% reduction", "3-5% reduction", "5-10% reduction",
                          "3-6% reduction", "5-10% reduction", "2-4% reduction",
                          "3-6% reduction", "3-5% reduction", "3-8% reduction",
                          "2-4% reduction", "2-4% reduction", "2-4% reduction",
                          "3-6% reduction", "3-6% reduction", "2-4% reduction",
                          "3-6% reduction", "2-5% reduction", "2-4% reduction"
  ),
  Per_Capita_Cost = c("C90-C345 per year", "C35-C175 per tracker", "C870-C3485 per participant",
                      "C175-C870 per check-up", "C50-C175 per consultation", "C10-C35 per participant",
                      "C175-C870 per program", "C90-C345 per counseling session", "C90-C345 per session",
                      "C20-C85 per workshop", "C20-C85 per incentive", "C90-C345 per check-up",
                      "C90-C345 per participant", "C10-C35 per participant", "C85-C175 per course",
                      "C90-C345 per screening", "C175-C870 per program", "C20-C85 per program",
                      "C10-C35 per participant", "C175-C870 per membership", "C10-C35 per participant",
                      "C90-C345 per plan", "C10-C35 per app", "C10-C35 per campaign",
                      "C10-C35 per campaign", "C20-C85 per training session", "C10-C35 per social event",
                      "C20-C85 per session", "C20-C85 per incentive", "C90-C345 per test",
                      "C90-C345 per program", "C10-C35 per campaign", "C90-C345 per counseling session",
                      "C20-C85 per session", "C20-C85 per incentive", "C90-C345 per assessment",
                      "C20-C85 per initiative", "C10-C35 per garden plot", "C20-C85 per program",
                      "C20-C85 per inspection", "C20-C85 per session", "C10-C35 per session",
                      "C10-C35 per campaign", "C20-C85 per workshop", "C20-C85 per group",
                      "C20-C85 per program", "C10-C35 per class", "C90-C345 per retreat",
                      "C20-C85 per incentive", "C20-C85 per assessment"))



# Extracting lower_bound and upper_bound
mort_lower_bound <- as.numeric(gsub("%.*", "", sapply(strsplit(intervention_data$Impact_on_Mortality, "-"), `[`, 1)))
mort_upper_bound <- as.numeric(gsub("%.*", "", sapply(strsplit(intervention_data$Impact_on_Mortality, "-"), `[`, 2)))

# Extracting lower_bound and upper_bound
c_lower_bound <- as.numeric(gsub("C| per.*", "", sapply(strsplit(intervention_data$Per_Capita_Cost, "-"), `[`, 1)))
c_upper_bound <- as.numeric(gsub("C| per.*", "", sapply(strsplit(intervention_data$Per_Capita_Cost, "-"), `[`, 2)))

# Apply the function to extract lower and upper bounds for Per_Capita_Cost
# Impact per cost is determined as the average impact / average cost
intervention_data <- intervention_data %>%
  mutate(Mort_Lower_Bound = mort_lower_bound,
         Mort_Upper_Bound = mort_upper_bound,
         Cost_Lower_Bound = c_lower_bound,
         Cost_Upper_Bound = c_upper_bound,
         impact_per_cost = ((Mort_Lower_Bound + Mort_Upper_Bound) / 2) / ((Cost_Lower_Bound + Cost_Upper_Bound) / 2))%>%
  arrange(desc(impact_per_cost))




##########################################
# Age group percentages
age_group_percentages <- c("25-34" = 0.20, "35-54" = 0.5, "55-64" = 0.75, "65+" = 0.9)


# Create a function to calculate impact per cost for each age group
calculate_impact_per_cost_age <- function(intervention_data, age_group_percentage) {
  impact_per_cost_age <- (intervention_data$Mort_Upper_Bound - age_group_percentage * (intervention_data$Mort_Upper_Bound - intervention_data$Mort_Lower_Bound)) /
    (intervention_data$Cost_Lower_Bound + (1-age_group_percentage) * (intervention_data$Cost_Upper_Bound - intervention_data$Cost_Lower_Bound))
  return(impact_per_cost_age)
}


# Create a list to store results for each age group
result_data_list <- list()

# Calculate impact per cost for each age group
for (age_group in names(age_group_percentages)) {
  age_group_percentage <- age_group_percentages[age_group]
  
  intervention_data_age <- intervention_data %>%
    mutate(impact_per_cost_age = calculate_impact_per_cost_age(., age_group_percentage),
           Age_Group = age_group) %>%
    arrange(desc(impact_per_cost_age))
  
  # Store the result for the current age group in the list
  result_data_list[[age_group]] <- intervention_data_age %>% select(Intervention_Name, Age_Group, impact_per_cost_age)
}


################## mortality modelling ########################
mortality_table <- read_excel("../Data/Case Study Data/srcsc-2024-lumaria-mortality-table.xlsx", skip = 13)
mortality_table <- mortality_table[, 1:2]

# Determine the chosen interventions
chosen_interventions <- intervention_data %>%
  filter(Intervention_Name %in% c("Safety Campaigns", "Community Fitness Challenges", "screening Aging Programs", "Smoking Cessation Programs"))

# Merge the data frames based on Age_Group
policyholder_mortality <- merge(policyholder_data, mortality_table, by.x = "Lifespan", by.y = "Age", all.x = TRUE)

# Load neoplasms mortality loading data
neoplasms_loading <- read.csv("Neoplasm_Mortality_Loading.csv")

# Create a sequence of ages from 27 to 88
age_range <- data.frame(Age.at.Death = 27:88)

# Merge with original data, replacing missing values with zeros
neoplasms_loading <- merge(age_range, neoplasms_loading, by = "Age.at.Death", all.x = TRUE)
neoplasms_loading[is.na(neoplasms_loading)] <- 0

policyholder_mortality <- merge(policyholder_mortality, neoplasms_loading, by.x = "Lifespan", by.y = "Age.at.Death", all.x = TRUE)


# Rename the new column
colnames(policyholder_mortality)[ncol(policyholder_mortality)] <- "Screening.Weight"

# Program design
# All policyholders participate in the safety campaigns
policyholder_mortality$Safety_flag <- 1

# All policyholders participate in fitness challenges
policyholder_mortality$Fitness_flag <- 1

# All policyholders over the age of 40 participate in the screening program
policyholder_mortality$screening_flag <- ifelse(policyholder_mortality$Lifespan >= 40, 1, 0)

# All smokers participate in the smoking cessation program
policyholder_mortality$Smoking_flag <- ifelse(policyholder_mortality$Smoker.Status == "S", 1, 0)


# Set an enggagement rate of 25%
engagement_rate <- 1

# Function to adjust flags based on engagement rate
adjust_flag <- function(flag) {
  num_engaged <- sum(flag == 1)
  num_to_engage <- round(num_engaged * engagement_rate)
  indices <- sample(which(flag == 1), num_to_engage)
  new_flag <- rep(0, length(flag))
  new_flag[indices] <- 1
  return(new_flag)
}

# Adjust flags based on engagement rate
policyholder_mortality$Safety_flag <- adjust_flag(policyholder_mortality$Safety_flag)
policyholder_mortality$Fitness_flag <- adjust_flag(policyholder_mortality$Fitness_flag)
policyholder_mortality$screening_flag <- adjust_flag(policyholder_mortality$screening_flag)
policyholder_mortality$Smoking_flag <- adjust_flag(policyholder_mortality$Smoking_flag)

# Calculate the sum of all flags for each policyholder
policyholder_mortality$Total_flags <- rowSums(policyholder_mortality[, c("Safety_flag", "Fitness_flag", "screening_flag", "Smoking_flag")])

# Count the number of policyholders with zero total flags
num_not_engaged <- sum(policyholder_mortality$Total_flags == 0)

# Calculate the proportion of policyholders that do not engage in any flag
proportion_not_engaged <- num_not_engaged / nrow(policyholder_mortality)



# For reproducibility
set.seed(42)  

# Generate random values for the dummy flags
policyholder_mortality$safety_dummy <- ifelse(policyholder_mortality$Safety_flag == 1, runif(nrow(policyholder_mortality), 0.03, 0.05), 0)

# Setting mortality rates for different age groups
policyholder_mortality$fitness_dummy <- ifelse(policyholder_mortality$Fitness_flag == 1,
                                               #4% to 5% for 25-44 year olds
                                               ifelse(policyholder_mortality$Age_Group == "25-44", 
                                                      runif(nrow(policyholder_mortality), 0.04, 0.05),
                                                      #3% to 4% for 45-59 year olds
                                                      ifelse(policyholder_mortality$Age_Group == "45-59", 
                                                             runif(nrow(policyholder_mortality), 0.03, 0.04),
                                                             #2% to 3% for 60+ year olds
                                                             ifelse(policyholder_mortality$Age_Group == "60+", 
                                                                    runif(nrow(policyholder_mortality), 0.02, 0.03), 
                                                                    0))), 
                                               0)

# Setting mortality rates for screenings of different age groups
policyholder_mortality$screening_dummy <- ifelse(policyholder_mortality$screening_flag == 1,
                                                 # 8% to 10% for 25-44 year olds
                                                 ifelse(policyholder_mortality$Age_Group == "25-44", 
                                                        runif(nrow(policyholder_mortality), 0.08, 0.1) * policyholder_mortality$Screening.Weight,
                                                        # 6% to 8% for 45-59 year olds
                                                        ifelse(policyholder_mortality$Age_Group == "45-59", 
                                                               runif(nrow(policyholder_mortality), 0.06, 0.08) * policyholder_mortality$Screening.Weight,
                                                               # 5% to 7% for 60+ year olds
                                                               ifelse(policyholder_mortality$Age_Group == "60+", 
                                                                      runif(nrow(policyholder_mortality), 0.05, 0.07) * policyholder_mortality$Screening.Weight, 
                                                                      0))), 
                                                 0)



# Setting mortality rates for smokers of different age groups
policyholder_mortality$smoking_dummy <- ifelse(policyholder_mortality$Smoking_flag == 1,
                                               #12.5% to 50% for 25-44 year olds
                                               ifelse(policyholder_mortality$Age_Group == "25-44", 
                                                      runif(nrow(policyholder_mortality), 0.125, 0.5),
                                                      #11.5% to 25% for 45-59 year olds
                                                      ifelse(policyholder_mortality$Age_Group == "45-59", 
                                                             runif(nrow(policyholder_mortality), 0.115, 0.25),
                                                             #7.5% to 11.5% for 60+ year olds
                                                             ifelse(policyholder_mortality$Age_Group == "60+", 
                                                                    runif(nrow(policyholder_mortality), 0.075, 0.115), 
                                                                    0))), 
                                               0)



# Calculate the product of (1 - dummy) values for mortality_impact
policyholder_mortality$mortality_impact <- (1 - policyholder_mortality$safety_dummy) *
  (1 - policyholder_mortality$fitness_dummy) *
  (1 - policyholder_mortality$screening_dummy) *
  (1 - policyholder_mortality$smoking_dummy)




policyholder_mortality$improved_mortality <- policyholder_mortality$mortality_impact * policyholder_mortality$Mortality.Rate
policyholder_mortality$mortality_reduction <- policyholder_mortality$Mortality.Rate - policyholder_mortality$improved_mortality



# Group by Lifespan and calculate the averages
average_mortality_table <- policyholder_mortality %>%
  group_by(Lifespan) %>%
  summarise(new_mortality = mean(improved_mortality, na.rm = TRUE),
            average_mortality_reduction = mean(mortality_reduction, na.rm = TRUE),
            old_mortality= mean(Mortality.Rate) )

# Calculate new mortality 20 years into the future
average_mortality_table$new_mortality_20_years_future <- average_mortality_table$new_mortality * 
  (1 - average_mortality_table$average_mortality_reduction) ^ 20


# Generate a sequence of ages from 1 to 120
all_ages <- data.frame(Age = 1:120)

# Expand the mortality table to include ages 1 to 120
average_mortality_table_expanded <- merge(all_ages, average_mortality_table, by.x = "Age", by.y = "Lifespan", all.x = TRUE)

# Replace new_mortality with mortality.rate from mortality_table
average_mortality_table_expanded$old_mortality <- mortality_table$Mortality.Rate[match(average_mortality_table_expanded$Age, mortality_table$Age)]

# Replace new_mortality with old_mortality for ages 1 to 26 and 89 to 120
average_mortality_table_expanded$new_mortality <- ifelse(average_mortality_table_expanded$Age <= 26 | average_mortality_table_expanded$Age >= 89,
                                                         mortality_table$Mortality.Rate[match(average_mortality_table_expanded$Age, mortality_table$Age)],
                                                         average_mortality_table_expanded$new_mortality)

write.xlsx(average_mortality_table_expanded, file = "average_mortality_table.xlsx", rowNames = FALSE)

# Calculate lives saved before running the program
average_mortality_table$old_deaths <- average_mortality_table$old_mortality * nrow(policyholder_mortality)
average_mortality_table$new_deaths <- average_mortality_table$new_mortality * nrow(policyholder_mortality)
average_mortality_table$lives_saved_now <- average_mortality_table$old_deaths - average_mortality_table$new_deaths

# Calculate lives saved 20 years into the future
average_mortality_table$new_mortality_future <- average_mortality_table$new_mortality * 
  (1 - average_mortality_table$average_mortality_reduction) ^ 20
average_mortality_table$new_deaths_future <- average_mortality_table$new_mortality_future * nrow(policyholder_mortality)
average_mortality_table$lives_saved_future <- average_mortality_table$old_deaths - average_mortality_table$new_deaths_future



# Create a new dataframe for plotting
lives_saved_plot <- data.frame(
  Lifespan = average_mortality_table$Lifespan,
  Lives_Saved_Now = average_mortality_table$lives_saved_now,
  Lives_Saved_Future = average_mortality_table$lives_saved_future
)

# Melt the dataframe for easier plotting
lives_saved_plot <- melt(lives_saved_plot, id.vars = "Lifespan", variable.name = "Scenario", value.name = "Lives_Saved")

# Plot both curves on one plot
ggplot(lives_saved_plot, aes(x = Lifespan, y = Lives_Saved, color = Scenario)) +
  geom_line() +
  geom_point() +
  labs(title = "Comparison of Lives Saved Now implementation and 20 Years Prior",
       x = "Lifespan",
       y = "Number of Lives Saved",
       color = "Scenario") +
  scale_color_manual(values = c("red", "blue"))  # Adjust colors as needed

# Group by Lifespan and calculate the averages
cost_table_T20 <- policyholder_mortality %>%
  filter(Policy.type == "T20")%>%
  group_by(Lifespan) %>%
  summarise(average_cost = mean(Face.amount, na.rm = TRUE) )

# Group by Lifespan and calculate the averages
cost_table_SPWL <- policyholder_mortality %>%
  filter(Policy.type == "SPWL")%>%
  group_by(Lifespan) %>%
  summarise(average_cost = mean(Face.amount, na.rm = TRUE) )

# Merge average_mortality_table with cost_table_T20 based on Lifespan
merged_data_T20 <- merge(average_mortality_table, cost_table_T20, by = "Lifespan")

# Merge average_mortality_table with cost_table_SPWL based on Lifespan
merged_data_SPWL <- merge(average_mortality_table, cost_table_SPWL, by = "Lifespan")

# Calculate the claims cost reduction for T20 policies
cost_reduction_now_T20 <- sum(merged_data_T20$lives_saved_now * merged_data_T20$average_cost)
cost_reduction_future_T20 <- sum(merged_data_T20$lives_saved_future * merged_data_T20$average_cost)

# Calculate the claims cost reduction for SPWL policies
cost_reduction_now_SPWL <- sum(merged_data_SPWL$lives_saved_now * merged_data_SPWL$average_cost)
cost_reduction_future_SPWL <- sum(merged_data_SPWL$lives_saved_future * merged_data_SPWL$average_cost)

# Print the claims cost reduction for T20 policies
print(paste("Claims Cost Reduction Now (T20):", cost_reduction_now_T20))
print(paste("Claims Cost Reduction Future (T20):", cost_reduction_future_T20))

# Print the claims cost reduction for SPWL policies
print(paste("Claims Cost Reduction Now (SPWL):", cost_reduction_now_SPWL))
print(paste("Claims Cost Reduction Future (SPWL):", cost_reduction_future_SPWL))


# Create a data frame for plotting
cost_reduction_data <- data.frame(
  Policy_Type = c("T20", "T20", "SPWL", "SPWL"),
  Scenario = c("Now", "20 years prior", "Now", "20 years prior"),
  Claims_Cost_Reduction = c(cost_reduction_now_T20, cost_reduction_future_T20, cost_reduction_now_SPWL, cost_reduction_future_SPWL)
)

# Create a ggplot
ggplot(cost_reduction_data, aes(x = Policy_Type, y = Claims_Cost_Reduction, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Claims Cost Reduction for T20 and SPWL Policies",
       x = "Policy Type",
       y = "Claims Cost Reduction") +
  scale_fill_manual(values = c("Now" = "blue", "20 years prior" = "red")) +
  theme_minimal()

