#EPI 514: Individual Assignment - Example R Code


#load appropriate packages
#install.packages("survey")
#install.packages("tidyverse")
library(survey)
library(tidyverse)
library(epiR)

#load data
data <- read.csv("/Users/nicolelorona/Documents/UW SPH/Year 4/Spring '21/EPI 514 TA/Data/homework1.csv")

##### PART 1 #####

### Question 1
#original variable with crosstab
with(data, table(diabete3, useNA = "ifany"))

#code new outcome variable with no diabetes as baseline
data$diab_di <- cut(data$diabete3,
                    breaks = c(1,2,7),
                    right = F,
                    labels = c("Diabetes",
                               "No Diabetes")) %>%
  factor(levels = c("No Diabetes",
                    "Diabetes"))

#check coding with crosstab
with(data, table(diab_di, diabete3, useNA = "ifany"))


### Questions 2 & 3
#create a function to describe missingness in data frame
#NOTE: We do NOT expect you to know how to create a function like this!
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      nmiss=sum(is.na(x)),
      n=length(x),
      propmiss=sum(is.na(x))/length(x)
    ) })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d) 
  d$variable <- row.names(d) 
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$propmiss), ])
}
#use propmiss to describe missingness
propmiss(data)

#create factor variable for bmi setting 'normal' as the baseline
data$bmi_cat <- factor(data$X_bmi5cat,
                       levels = c(2,1,3,4),
                       labels = c("Normal",
                                  "Underweight",
                                  "Overweight",
                                  "Obese"))

#check with crosstab
with(data, table(bmi_cat, X_bmi5cat, useNA = "ifany"))


#create factor variable for race 
data$race_cat <- factor(data$X_imprace,
                        labels = c("White_nh",
                                   "Black_nh",
                                   "Asian_nh",
                                   "AIAN_nh",
                                   "Hispanic",
                                   "Other"))

#check with crosstab
with(data, table(race_cat, X_imprace, useNA = "ifany"))

#create factor variable for sex with missing
data$sex_cat <- factor(data$sex,
                       levels = c(1,2, NA),
                       labels = c("Male",
                                  "Female"))

#check with crosstab
with(data, table(sex, sex_cat, useNA = "ifany"))


#create factor variable for emotional support with missing
data$emot_cat <- factor(data$esupport,
                        levels = c(0, 1, NA),
                        labels = c("Never_Rarely",
                                   "Usually_Often"))

#check with crosstab
with(data, table(emot_cat, esupport, useNA = "ifany"))



#create factor for age group
data$age_cat <- factor(data$X_age_g,
                       labels = c("18-24",
                                  "25-24",
                                  "34-44",
                                  "45-54",
                                  "55-64",
                                  "65+"))

#check with crosstab
with(data, table(age_cat, X_age_g))


#create income group factor with missing
data$inc_cat <- factor(data$X_incomg,
                       levels = c(1:5, NA),
                       labels = c("<15k",
                                  "15-24k",
                                  "25-34k",
                                  "35-50k",
                                  "50k+"))

#check with crosstab
with(data, table(inc_cat, X_incomg, useNA = "ifany"))


#create marital status factor with missing
data$mar_cat <- factor(data$marital,
                       levels = c(1:6, NA),
                       labels = c("Married",
                                  "Divorced",
                                  "Widowed",
                                  "Separated",
                                  "Never",
                                  "Unmar_Couple"))

#check with crosstab
with(data, table(mar_cat, marital, useNA = "ifany"))


### Question 5
#set how the survey package should handle rows with only 1 psu
options(survey.lonely.psu = "adjust")

#assign weights
design <- svydesign(data = data, 
                    id = ~1, 
                    strata = ~x_ststr, 
                    weights = ~X_llcpwt)

#Prevalence of diabetes
svytable(~diab_di, design) %>% 
  prop.table()

#within each category of BMI
svytable(~diab_di + bmi_cat, design) %>% 
  prop.table(margin = 2)

### Question 6
#Create indicator exposure variables for bivariate comparisons
data <- data %>% mutate(
  underwt_bin = case_when(
    bmi_cat=="Underweight"~1,
    bmi_cat=="Normal" ~ 2),
  underwt_bin = factor(underwt_bin, labels=c("Underweight", "Normal")), 
  overwt_bin = case_when(
    bmi_cat=="Overweight"~1,
    bmi_cat=="Normal" ~ 2),
  overwt_bin = factor(overwt_bin, labels=c("Overweight", "Normal")),
  obese_bin = case_when(
    bmi_cat=="Obese" ~ 1, 
    bmi_cat=="Normal" ~ 2),
  obese_bin = factor(obese_bin, labels=c("Obese", "Normal")), 
  diab_di = factor(diab_di, levels=c("Diabetes", "No Diabetes"))
)
strat_1 <- with(data,
                table(underwt_bin, diab_di))
strat_2 <- with(data,
                table(overwt_bin, diab_di))
strat_3 <- with(data, 
                table(obese_bin, diab_di))
#run analysis
epi.2by2(strat_1)
epi.2by2(strat_2)
epi.2by2(strat_3)



##### PART 2 #####

### Question 7
#assess confounding

#based on DAG, suspect the following confounders:
#sex, age, income, emotional support

#sex
strat_1 <- with(data,
                table(underwt_bin, diab_di, sex_cat))
strat_2 <- with(data,
                table(overwt_bin, diab_di, sex_cat))
strat_3 <- with(data, 
                table(obese_bin, diab_di, sex_cat))
#run analysis
epi.2by2(strat_1)
epi.2by2(strat_2)
epi.2by2(strat_3)

#emotional support
strat_1 <- with(data,
                table(underwt_bin, diab_di, emot_cat))
strat_2 <- with(data,
                table(overwt_bin, diab_di, emot_cat))
strat_3 <- with(data, 
                table(obese_bin, diab_di, emot_cat))
#run analysis
epi.2by2(strat_1)
epi.2by2(strat_2)
epi.2by2(strat_3)

#age
strat_1 <- with(data,
                table(underwt_bin, diab_di, age_cat))
strat_2 <- with(data,
                table(overwt_bin, diab_di, age_cat))
strat_3 <- with(data, 
                table(obese_bin, diab_di, age_cat))
#run analysis
epi.2by2(strat_1)
epi.2by2(strat_2)
epi.2by2(strat_3)

#income
strat_1 <- with(data,
                 table(underwt_bin, diab_di, inc_cat))
strat_2 <- with(data,
                table(overwt_bin, diab_di, inc_cat))
strat_3 <- with(data, 
                table(obese_bin, diab_di, inc_cat))
#run analysis
epi.2by2(strat_1)
epi.2by2(strat_2)
epi.2by2(strat_3)



### Question 9
##Effect modification
#Evaluate: age, sex, race, and income

#age
strat_1_under <- with(subset(data, age_cat == "18-24"),
                        table(underwt_bin, diab_di))
strat_2_under <- with(subset(data, age_cat == "25-24"),
                        table(underwt_bin, diab_di))
strat_3_under <- with(subset(data, age_cat == "34-44"),
                        table(underwt_bin, diab_di))
strat_4_under <- with(subset(data, age_cat == "45-54"),
                        table(underwt_bin, diab_di))
strat_5_under <- with(subset(data, age_cat == "55-64"),
                        table(underwt_bin, diab_di))
strat_6_under <- with(subset(data, age_cat == "65+"),
                        table(underwt_bin, diab_di))

strat_1_over <- with(subset(data, age_cat == "18-24"),
                       table(overwt_bin, diab_di))
strat_2_over <- with(subset(data, age_cat == "25-24"),
                       table(overwt_bin, diab_di))
strat_3_over <- with(subset(data, age_cat == "34-44"),
                       table(overwt_bin, diab_di))
strat_4_over <- with(subset(data, age_cat == "45-54"),
                       table(overwt_bin, diab_di))
strat_5_over <- with(subset(data, age_cat == "55-64"),
                       table(overwt_bin, diab_di))
strat_6_over <- with(subset(data, age_cat == "65+"),
                       table(overwt_bin, diab_di))

strat_1_obese <- with(subset(data, age_cat == "18-24"),
                        table(obese_bin, diab_di))
strat_2_obese <- with(subset(data, age_cat == "25-24"),
                        table(obese_bin, diab_di))
strat_3_obese <- with(subset(data, age_cat == "34-44"),
                        table(obese_bin, diab_di))
strat_4_obese <- with(subset(data, age_cat == "45-54"),
                        table(obese_bin, diab_di))
strat_5_obese <- with(subset(data, age_cat == "55-64"),
                        table(obese_bin, diab_di))
strat_6_obese <- with(subset(data, age_cat == "65+"),
                        table(obese_bin, diab_di))


#Run Analysis
epi.2by2(strat_1_under)
epi.2by2(strat_2_under)
epi.2by2(strat_3_under)
epi.2by2(strat_4_under)
epi.2by2(strat_5_under)
epi.2by2(strat_6_under)

epi.2by2(strat_1_over)
epi.2by2(strat_2_over)
epi.2by2(strat_3_over)
epi.2by2(strat_4_over)
epi.2by2(strat_5_over)
epi.2by2(strat_6_over)

epi.2by2(strat_1_obese)
epi.2by2(strat_2_obese)
epi.2by2(strat_3_obese)
epi.2by2(strat_4_obese)
epi.2by2(strat_5_obese)
epi.2by2(strat_6_obese)

#sex
strat_F_under <- with(subset(data, sex_cat == "Female"),
                        table(underwt_bin, diab_di))

strat_M_under <- with(subset(data, sex_cat == "Male"),
                        table(underwt_bin, diab_di))

strat_F_over <- with(subset(data, sex_cat == "Female"),
                       table(overwt_bin, diab_di))

strat_M_over <- with(subset(data, sex_cat == "Male"),
                       table(overwt_bin, diab_di))

strat_F_obese <- with(subset(data, sex_cat == "Female"),
                        table(obese_bin, diab_di))

strat_M_obese <- with(subset(data, sex_cat == "Male"),
                        table(obese_bin, diab_di))


#Run Analysis
epi.2by2(strat_F_under)
epi.2by2(strat_M_under)
epi.2by2(strat_F_over)
epi.2by2(strat_M_over)
epi.2by2(strat_F_obese)
epi.2by2(strat_M_obese)


#race
strat_W_under <- with(subset(data, race_cat == "White_nh"),
                       table(underwt_bin, diab_di))
strat_B_under <- with(subset(data, race_cat == "Black_nh"),
                        table(underwt_bin, diab_di))
strat_A_under <- with(subset(data, race_cat == "Asian_nh"),
                        table(underwt_bin, diab_di))
strat_AI_under <- with(subset(data, race_cat == "AIAN_nh"),
                        table(underwt_bin, diab_di))
strat_H_under <- with(subset(data, race_cat == "Hispanic"),
                        table(underwt_bin, diab_di))
strat_O_under <- with(subset(data, race_cat == "Other"),
                        table(underwt_bin, diab_di))

strat_W_over <- with(subset(data, race_cat == "White_nh"),
                        table(overwt_bin, diab_di))
strat_B_over <- with(subset(data, race_cat == "Black_nh"),
                        table(overwt_bin, diab_di))
strat_A_over <- with(subset(data, race_cat == "Asian_nh"),
                        table(overwt_bin, diab_di))
strat_AI_over <- with(subset(data, race_cat == "AIAN_nh"),
                         table(overwt_bin, diab_di))
strat_H_over <- with(subset(data, race_cat == "Hispanic"),
                        table(overwt_bin, diab_di))
strat_O_over <- with(subset(data, race_cat == "Other"),
                        table(overwt_bin, diab_di))

strat_W_obese <- with(subset(data, race_cat == "White_nh"),
                        table(obese_bin, diab_di))
strat_B_obese <- with(subset(data, race_cat == "Black_nh"),
                        table(obese_bin, diab_di))
strat_A_obese <- with(subset(data, race_cat == "Asian_nh"),
                        table(obese_bin, diab_di))
strat_AI_obese <- with(subset(data, race_cat == "AIAN_nh"),
                         table(obese_bin, diab_di))
strat_H_obese <- with(subset(data, race_cat == "Hispanic"),
                        table(obese_bin, diab_di))
strat_O_obese <- with(subset(data, race_cat == "Other"),
                        table(obese_bin, diab_di))


#Run Analysis
epi.2by2(strat_W_under)
epi.2by2(strat_B_under)
epi.2by2(strat_A_under)
epi.2by2(strat_AI_under)
epi.2by2(strat_H_under)
epi.2by2(strat_O_under)

epi.2by2(strat_W_over)
epi.2by2(strat_B_over)
epi.2by2(strat_A_over)
epi.2by2(strat_AI_over)
epi.2by2(strat_H_over)
epi.2by2(strat_O_over)

epi.2by2(strat_W_obese)
epi.2by2(strat_B_obese)
epi.2by2(strat_A_obese)
epi.2by2(strat_AI_obese)
epi.2by2(strat_H_obese)
epi.2by2(strat_O_obese)

#Income
strat_1_under <- with(subset(data, inc_cat == "<15k"),
                        table(underwt_bin, diab_di))
strat_2_under <- with(subset(data, inc_cat == "15-24k"),
                        table(underwt_bin, diab_di))
strat_3_under <- with(subset(data, inc_cat == "25-34k"),
                        table(underwt_bin, diab_di))
strat_4_under <- with(subset(data, inc_cat == "35-50k"),
                        table(underwt_bin, diab_di))
strat_5_under <- with(subset(data, inc_cat == "50k+"),
                        table(underwt_bin, diab_di))

strat_1_over <- with(subset(data, inc_cat == "<15k"),
                        table(overwt_bin, diab_di))
strat_2_over <- with(subset(data, inc_cat == "15-24k"),
                       table(overwt_bin, diab_di))
strat_3_over <- with(subset(data, inc_cat == "25-34k"),
                       table(overwt_bin, diab_di))
strat_4_over <- with(subset(data, inc_cat == "35-50k"),
                       table(overwt_bin, diab_di))
strat_5_over <- with(subset(data, inc_cat == "50k+"),
                       table(overwt_bin, diab_di))

strat_1_obese <- with(subset(data, inc_cat == "<15k"),
                        table(obese_bin, diab_di))
strat_2_obese <- with(subset(data, inc_cat == "15-24k"),
                        table(obese_bin, diab_di))
strat_3_obese <- with(subset(data, inc_cat == "25-34k"),
                        table(obese_bin, diab_di))
strat_4_obese <- with(subset(data, inc_cat == "35-50k"),
                        table(obese_bin, diab_di))
strat_5_obese <- with(subset(data, inc_cat == "50k+"),
                        table(obese_bin, diab_di))




#Run Analysis
epi.2by2(strat_1_under)
epi.2by2(strat_2_under)
epi.2by2(strat_3_under)
epi.2by2(strat_4_under)
epi.2by2(strat_5_under)

epi.2by2(strat_1_over)
epi.2by2(strat_2_over)
epi.2by2(strat_3_over)
epi.2by2(strat_4_over)
epi.2by2(strat_5_over)

epi.2by2(strat_1_obese)
epi.2by2(strat_2_obese)
epi.2by2(strat_3_obese)
epi.2by2(strat_4_obese)
epi.2by2(strat_5_obese)




### Question 10

strat_F_under <- with(subset(data, sex_cat == "Female"),
                        table(diab_di, underwt_bin, inc_cat))

strat_M_under <- with(subset(data, sex_cat == "Male"),
                        table(underwt_bin, diab_di, inc_cat))

strat_F_over <- with(subset(data, sex_cat == "Female"),
                       table(overwt_bin, diab_di, inc_cat))

strat_M_over <- with(subset(data, sex_cat == "Male"),
                       table(overwt_bin, diab_di, inc_cat))

strat_F_obese <- with(subset(data, sex_cat == "Female"),
                        table(obese_bin, diab_di, inc_cat))

strat_M_obese <- with(subset(data, sex_cat == "Male"),
                        table(obese_bin, diab_di, inc_cat))


#Run Analysis
epi.2by2(strat_F_under)
epi.2by2(strat_M_under)
epi.2by2(strat_F_over)
epi.2by2(strat_M_over)
epi.2by2(strat_F_obese)
epi.2by2(strat_M_obese)
