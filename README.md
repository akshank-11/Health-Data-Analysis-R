# Exploring Preoperative Lab Results and Surgical Outcomes

### Project Overview
This project investigates the association between preoperative laboratory parameters and key surgical outcomes, including 
Length of Hospital Stay (LoHS), in-hospital mortality (death_in_hosp), and ICU stay duration. Using a high-resolution dataset of 5,800+ surgical cases, 
the analysis provides insights into how preoperative metrics such as hemoglobin, albumin, and glucose levels impact patient outcomes. 
The findings aim to facilitate better clinical decision-making and improve patient care.

### Dataset Description
The dataset comprises anonymized patient data, including demographics, preoperative lab results, and postoperative outcomes. Key variables include:

Patient Demographics: Age, Sex, BMI, Height, Weight, and Surgical Department.
Preoperative Lab Parameters:
Hemoglobin (preop_hb)
Platelet Count (preop_plt)
Activated Partial Thromboplastin Time (preop_aptt)
Glucose (preop_gluc)
Albumin (preop_alb)
Liver Enzymes (preop_ast, preop_alt)
Blood Urea Nitrogen (preop_bun)
Creatinine (preop_cr)
Clinical Outcomes:
Length of Hospital Stay (LoHS)
Postoperative ICU stay duration (icu_days)
In-hospital mortality (death_in_hosp)


### -------------- Data Exploration and Preparation --------------
- Basic statistics for age and Length of Hospital Stay (LoHS)
- Creating derived variables like age_group and BMI_Category
- Use of summary() to understand the distribution of lab parameters

### -------------- Data Exploration and Preparation --------------
- Surgery-Specific Analyses:
  - Common surgery types and their frequencies
  - Mortality rates associated with different types of surgeries
  - Most frequent surgeries by age group and sex
- Relationship between BMI and pre-op glucose levels
- Density plots for parameters like albumin, hemoglobin, and glucose stratified by mortality outcomes
  
### -------------- Data Visualization ----------------------------
- Probability of Death: Bar charts depicting the relationship between lab parameters and in-hospital mortality
- BMI Categories: Analysis of BMI categories against abnormal lab parameters using bar plots
- Length of Stay (LoHS): Relationship between abnormal lab parameters and LoHS
   
