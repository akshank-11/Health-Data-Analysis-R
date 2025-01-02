 install.packages("tidyverse")
library(tidyverse)
 
#Import the Clinical Data.  Study the data first.  This will save you significant time.
 
 
#this is just to encourage you to look at variables like age, and LoHS
min(clinical_data$age)
max(clinical_data$age)
mean(clinical_data$age)
mean(clinical_data$LoHS)
 
clinical_data <- clinical_data %>%
  mutate(age_group = case_when((age >=1 & age < 19) ~ "8-18",
                           	(age >=19 & age < 25) ~ "19-24",
                           	(age >=25 & age < 41) ~ "25-40",
                           	(age >=41 & age < 55) ~ "41-54",
                           	(age >=55 & age < 65) ~ "55-64",
                           	age >=65 ~ "65-older"))
 
 
#Please paste your R code after this with appropriate comments
 
#####Summary of preop variables######
 summary(clinical_data[c("preop_hb", "preop_plt", "preop_aptt", "preop_gluc", "preop_alb", 
                        "preop_ast", "preop_alt", "preop_bun", "preop_cr")])

#### Data Visualization - Are there any preop variables affecting mortality? #####
 
normal_ranges <- data.frame(
  Parameter = c("preop_hb", "preop_plt", "preop_aptt", "preop_gluc",
                "preop_alb", "preop_ast", "preop_alt", "preop_bun", "preop_cr"),
  Normal_Lower = c(14, 150, 24, 70, 3.4, 0, 0, 7, 0.7),
  Normal_Upper = c(18, 400, 36, 170, 5.4, 40, 60, 20, 1.2)
)
 
# Reshape the data for easier plotting
norm_abnorm <- clinical_data %>%
  gather(key = "Parameter", value = "Value",
         matches("^preop_(hb|plt|aptt|gluc|alb|ast|alt|bun|cr)$"))
 
# Merge with normal ranges data
norm_abnorm <- merge(norm_abnorm, normal_ranges, by = "Parameter", all.x = TRUE)
 
# Create a new column indicating normal or abnormal
norm_abnorm <- norm_abnorm %>%
  mutate(Range = case_when(
	Value >= Normal_Lower & Value <= Normal_Upper ~ "Normal",
	TRUE ~ "Abnormal"
  ))
 
# Calculate the probability of death for patients with normal and abnormal lab values
death_prob <- norm_abnorm %>%
  group_by(Parameter, Range) %>%
  summarise(prob_of_death = mean(death_inhosp, na.rm = TRUE))
 
# Plotting the probability of death based on lab parameters and their ranges
ggplot(death_prob, aes(x = Parameter, y = prob_of_death, fill = Range)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Effect of Pre-operation Lab Parameters Range on Probability of Death",
   	x = "Pre-Op Lab Parameters", y = "Probability of Death") +
  scale_fill_manual(values = c("Normal" = "green", "Abnormal" = "red")) 
 
 
#--------------- Data Visualization -Which category of BMI affects the preop variables value? ####
 
clinical_data <- clinical_data %>%
  mutate(BMI_Category = case_when(
	bmi < 18.5 ~ "Underweight",
	bmi >= 18.5 & bmi < 25 ~ "Normal",
	bmi >= 25 & bmi < 30 ~ "Overweight",
	bmi >= 30 ~ "Obese",
	TRUE ~ NA_character_
  ))
 
# Calculate the probability of abnormal lab values for each BMI category
lab_prob_by_bmi <- clinical_data %>%
  group_by(BMI_Category) %>%
  summarise(prob_abnormal_alb = mean(preop_alb < 3.4 | preop_alb > 5.4, na.rm = TRUE),
            prob_abnormal_ast = mean(preop_ast > 40, na.rm = TRUE),
            prob_abnormal_alt = mean(preop_alt > 60, na.rm = TRUE),
            prob_abnormal_plt = mean(preop_plt < 150 | preop_plt > 400, na.rm = TRUE))
 
# Reshape the data for plotting
lab_prob_long <- lab_prob_by_bmi %>%
  pivot_longer(cols = starts_with("prob_abnormal"),
               names_to = "Lab_Parameter", values_to = "Probability")
 
# Plotting the relation visually
ggplot(lab_prob_long, aes(x = BMI_Category, y = Probability, fill = Lab_Parameter)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Lab_Parameter) +
  labs(title = "Probability of Abnormal Pre-operation Lab Values by BMI Category",
   	x = "BMI Category", y = "Probability",
   	fill = "Lab Parameter") +
  scale_fill_manual(values = c("prob_abnormal_alb" = "salmon4",
                               "prob_abnormal_ast" = "magenta4",
                               "prob_abnormal_alt" = "olivedrab",
                               "prob_abnormal_plt" = "darkslateblue"))
 
 
#--------------- Data Visualization - Which preop variable in abnormal range has the highest average LoHS? ######
 
abnormal_data <- norm_abnorm %>%
  filter(Parameter %in% c("preop_alb", "preop_plt", "preop_ast", "preop_alt"),
     	Range == "Abnormal")
 
# Calculate average LoHS for patients with abnormal preop_alb, preop_plt, preop_ast, preop_alt
avg_lohs_abnormal <- abnormal_data %>%
  group_by(Parameter) %>%
  summarise(avg_lohs = mean(LoHS, na.rm = TRUE))
 
ggplot(avg_lohs_abnormal, aes(x = Parameter, y = avg_lohs, fill = Parameter)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Length of Hospital Stay for Patients with Abnormal Pre-op Parameters",
   	x = "Pre-op Parameter", y = "Average LoHS")+
scale_fill_manual(values = c("preop_alb" = "salmon4",
                             "preop_ast" = "magenta4",
                             "preop_alt" = "olivedrab",
                             "preop_plt" = "darkslateblue"))
 
#--------------- Data Visualization -Association between Preop blood results and death in Hospital
 
 ggplot(clinical_data, aes(x = preop_alb, color = factor(death_inhosp))) +​
  geom_density() +​
  labs(title = "Distribution of Preop Albumin by Death in Hospital",​
   	x = "Preop Albumin (g/dL)",​
   	y = "Density",​
   	color = "Death in Hospital")​
 ​
ggplot(clinical_data, aes(x = preop_hb, color = factor(death_inhosp))) +​
  geom_density() +​
  labs(title = "Distribution of Preop Haemoglobin by Death in Hospital",​
   	x = "Preop Hb ",​
   	y = "Density",​
   	color = "Death in Hospital")​
 ​
 ​
ggplot(clinical_data, aes(x = preop_gluc, color = factor(death_inhosp))) +​
  geom_density() +​
  labs(title = "Distribution of Preop Glucose by Death in Hospital",​
   	x = "Preop Glucose",​
   	y = "Density",​
   	color = "Death in Hospital")​

 #--------------- Data Visualization - BMI v/s preOp glucose 


ggplot(df, aes(x = bmi, y = preop_gluc)) +
  geom_point() +
  labs(title = "Pre-operative Glucose vs. BMI",
       x = "BMI",
       y = "Glucose (mg/dL)") +
  theme_bw()
 
 
#--------------- Data Visualization - Common types of surgeries - What are the most common types of surgeries performed in healthcare?
 
surgery_counts <- clinical_data %>%
  group_by(optype) %>%
  summarise(count = n()) %>%
  arrange(desc(count))  # Arrange by descending count
 
# Showing the top 10 most common types of surgeries
top_surgeries <- head(surgery_counts, 10)
 
# Plotting the bar plot
ggplot(top_surgeries, aes(x = reorder(optype, -count), y = count)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(title = "Top 10 Most Common Types of Surgeries",
   	x = "Surgery Type",
   	y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------- Data Visualization - Mortality rate v/s Surgery Type (R)
mortality_data <- clinical_data %>%
    group_by(optype) %>%
    summarise(mortality_rate = mean(death_inhosp) * 100)
 
  ggplot(mortality_data, aes(x = optype, y = mortality_rate)) +
	geom_bar(stat = "identity", fill = "red") +
	labs(title = "Mortality Rate by Operation Type", x = "Operation Type", y = "Mortality Rate (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 
#--------------- Data Visualization 2 - Most common type of surgery by age group and sex - What is the most common type of surgery across every age group and sex?
 
age_group_levels <- c("8-18", "19-24", "25-40", "41-54", "55-64", "65-older")
 
# Convert age_group to a factor with custom level order
clinical_data$age_group <- factor(clinical_data$age_group, levels = age_group_levels)
 
# Group by age group, sex, and surgery type, and calculate the count of surgeries
surgery_counts <- clinical_data %>%
  group_by(age_group, sex, optype) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(age_group, sex) %>%
  slice(which.max(count))  # Get the surgery type with the maximum count for each age group and sex
 
# Visualize the most common type of surgery for each age group and sex
ggplot(surgery_counts, aes(x = age_group, y = count, fill = optype)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ sex, scales = "free", ncol = 2) +  # Facet by sex
  labs(title = "Most Common Type of Surgery by Age Group and Sex",
   	x = "Age Group",
   	y = "Count",
   	fill = "Surgery Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 15), axis.text.y = element_text(size = 15))
 
#--------------- Data Visualization 3 - Top 3 types of surgeries for each age group among females - Can we analyze female healthcare analytics by age group?
 
# Filter data for females
female_data <- clinical_data %>% filter(sex == "F")
 
# Group by age group and surgery type, and calculate the count of surgeries
surgery_counts <- female_data %>%
  group_by(age_group, optype) %>%
  summarise(count = n()) %>%
  ungroup()
 
# For each age group, select the top 3 surgeries
top_surgeries <- surgery_counts %>%
  group_by(age_group) %>%
  top_n(3, count) %>%
  ungroup()
 
# Visualize the top 3 common surgeries in each age group for females
ggplot(top_surgeries, aes(x = age_group, y = count, fill = optype)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ age_group, scales = "free") +  # Facet by age group
  labs(title = "Top 3 Common Surgeries in Each Age Group for Females",
   	x = "Age Group",
   	y = "Frequency",
   	fill = "Surgery Type") +
  theme_minimal()
 
 
#--------------- Data Visualization 4 - Length of Hospital Stay (LoHS) for different surgery types - What is the association between different surgery types and LoHS?
 
# Computing average length of hospital stay for each surgery type
avg_lohs_by_surgery <- clinical_data %>%
  group_by(optype) %>%
  summarise(avg_lohs = mean(LoHS)) %>%
  arrange(desc(avg_lohs))
 
# Reorder surgery types based on average LoHS
avg_lohs_by_surgery <- avg_lohs_by_surgery %>%
  arrange(desc(avg_lohs)) %>%
  mutate(optype = factor(optype, levels = optype))
 
# Visualize the results
ggplot(avg_lohs_by_surgery, aes(x = avg_lohs, y = optype)) +
  geom_bar(stat = "identity", fill = "darkgreen", width = 0.5) +
  labs(title = "Average Length of Hospital Stay by Surgery Type",
   	x = "Average Length of Hospital Stay (Days)",
   	y = "Surgery Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

 
#--------------- Data Visualization - ICU days stayed and type of Operation 
 
  avg_icu_days <- clinical_data %>%
    group_by(optype) %>%
    summarise(avg_icu_days = mean(icu_days, na.rm = TRUE))
 
  ggplot(avg_icu_days, aes(x = optype, y = avg_icu_days)) +
	geom_bar(stat = "identity", fill = "skyblue") +
	labs(title = "Average ICU Days by Operation Type", x = "Operation Type", y = "Average ICU Days") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------- Data Visualization - BMI v/s Age and Sex
 
problem3 <- clinical_data %>%
  group_by(age, sex) %>%
  summarise(mean_bmi = mean(bmi, na.rm = TRUE))
 
ggplot(problem3, aes(x = age, y = mean_bmi, color = sex)) +
  geom_line() +
  labs(title = "BMI versus Age, Faceted by Sex", x = "Age", y = "Mean BMI") +
  theme_minimal()
 
 #--------------- Data Visualization - Age group v/s Length of Hospital Days Stayed
 
  age_lohs <- clinical_data %>%
    group_by(age_group, LoHS) %>%
    summarise(avg_lohs = mean(age_lohs$LoHS, na.rm = TRUE))
 
  ggplot(age_lohs, aes(x = age_group, y = LoHS)) +
    geom_boxplot(fill = "deeppink", color = "black") +
	labs(title = "Length of Hospital Stay by Age Group", x = "Age Group", y = "Length of Hospital Stay (Days)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
#--------------- Data Visualization - BMI v/s Length of Hospital Days stayed
	
bmi_length <- clinical_data %>%
    mutate(hospital_stay_range = cut(LoHS, breaks = time_ranges, labels = c("0-3 days", "4-7 days", "8-14 days", "15-30 days", "31+ days")))
 
 
ggplot(bmi_length, aes(x = hospital_stay_range, y = bmi)) +
    geom_boxplot() +
	labs(title = "BMI of Patients within Length of Hospital Stays Time Ranges", x = "Length of Hospital Stays (Days)", y = "BMI") +
    theme_minimal()
 







 
 
 
 
 
 
 


