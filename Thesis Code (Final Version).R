#load in packages
library(foreign)
library(tidyverse) 
library(car)
library(arsenal)
library(plyr)
library(plm)
library(lme4)
library(lmerTest)
library(texreg)
library(grid)
library(ggplot2)
library(ggeffects)
setwd("~/Documents/EBSIPE/thesis/datasets/cleaned data/code")

#read in data
ESS_2014<-read.dta("ESS7E02_2.dta") #data on individual health outcome variables, measures of precarity
labour_market<- read_csv("labour market characteristics draft 3.csv") #data on measures of labour market dualization (ALMP spending equal treatment of temporary and permanent workers)
#merge datasets
full_data<-left_join(labour_market, ESS_2014)

#Outcome variables
#Mental health score
#Felt depressed, how often past week
summary(full_data$fltdpr)
full_data$fltdpr_num<- as.numeric(full_data$fltdpr)
full_data  <- full_data %>%   #collapse categories   
  mutate(felt_depressed = factor (fltdpr_num,
                                  levels = c(1,2,3,4),
                                  labels = c("None or almost none of the time ", "Some of the time", "Most of the time", "All or almost all of the time")))
summary(full_data$felt_depressed)

#recode from 0-3
full_data$fltdpr_num1[full_data$fltdpr_num== 1] <- 0
full_data$fltdpr_num1[full_data$fltdpr_num== 2] <- 1
full_data$fltdpr_num1[full_data$fltdpr_num== 3] <- 2
full_data$fltdpr_num1[full_data$fltdpr_num== 4] <- 3

summary(full_data$fltdpr_num1)

#Felt everything did as effort, how often past week
summary(full_data$flteeff)
full_data$flteeff_num<- as.numeric(full_data$flteeff)
full_data  <- full_data %>%   #collapse categories   
  mutate(felt_effort = factor (flteeff_num,
                               levels = c(1,2,3,4),
                               labels = c("None or almost none of the time ", "Some of the time", "Most of the time", "All or almost all of the time")))
summary(full_data$felt_effort)

#recode from 0-3
full_data$flteeff_num1[full_data$flteeff_num== 1] <- 0
full_data$flteeff_num1[full_data$flteeff_num== 2] <- 1
full_data$flteeff_num1[full_data$flteeff_num== 3] <- 2
full_data$flteeff_num1[full_data$flteeff_num== 4] <- 3

summary(full_data$flteeff_num1)

#Sleep was restless, how often past week
summary(full_data$slprl)
full_data$slprl_num<- as.numeric(full_data$slprl)
full_data  <- full_data %>%   #collapse categories   
  mutate(sleep_restless = factor (slprl_num,
                                  levels = c(1,2,3,4),
                                  labels = c("None or almost none of the time ", "Some of the time", "Most of the time", "All or almost all of the time")))
summary(full_data$sleep_restless)

#recode from 0-3
full_data$slprl_num1[full_data$slprl_num== 1] <- 0
full_data$slprl_num1[full_data$slprl_num== 2] <- 1
full_data$slprl_num1[full_data$slprl_num== 3] <- 2
full_data$slprl_num1[full_data$slprl_num== 4] <- 3

summary(full_data$slprl_num1)

#Were happy, how often past week
summary(full_data$wrhpp)
full_data$wrhpp_num<- as.numeric(full_data$wrhpp)
full_data <- full_data %>%   #collapse categories   
  mutate(were_happy = factor (wrhpp_num,
                              levels = c(1,2,3,4),
                              labels = c("None or almost none of the time ", "Some of the time", "Most of the time", "All or almost all of the time")))
summary(full_data$sleep_restless)

#recode from 0-3
full_data$wrhpp_num1[full_data$wrhpp_num== 1] <- 3
full_data$wrhpp_num1[full_data$wrhpp_num== 2] <- 2
full_data$wrhpp_num1[full_data$wrhpp_num== 3] <- 1
full_data$wrhpp_num1[full_data$wrhpp_num== 4] <- 0

summary(full_data$wrhpp_num1)

#Felt lonely, how often past week
summary(full_data$fltlnl)
full_data$fltlnl_num<- as.numeric(full_data$fltlnl)
full_data  <- full_data %>%   #collapse categories   
  mutate(felt_lonely = factor (fltlnl_num,
                               levels = c(1,2,3,4),
                               labels = c("None or almost none of the time ", "Some of the time", "Most of the time", "All or almost all of the time")))
summary(full_data$felt_lonely)

#recode from 0-3
full_data$fltlnl_num1[full_data$fltlnl_num== 1] <- 0
full_data$fltlnl_num1[full_data$fltlnl_num== 2] <- 1
full_data$fltlnl_num1[full_data$fltlnl_num== 3] <- 2
full_data$fltlnl_num1[full_data$fltlnl_num== 4] <- 3

summary(full_data$fltlnl_num1)

#Enjoyed life, how often past week
summary(full_data$enjlf)
full_data$enjlf_num<- as.numeric(full_data$enjlf)
full_data <- full_data%>%   #collapse categories   
  mutate(enjoyed_life = factor (enjlf_num,
                                levels = c(1,2,3,4),
                                labels = c("None or almost none of the time ", "Some of the time", "Most of the time", "All or almost all of the time")))
summary(full_data$enjoyed_life)

#recode from 0-3
full_data$enjlf_num1[full_data$enjlf_num== 1] <- 3
full_data$enjlf_num1[full_data$enjlf_num== 2] <- 2
full_data$enjlf_num1[full_data$enjlf_num== 3] <- 1
full_data$enjlf_num1[full_data$enjlf_num== 4] <- 0

summary(full_data$fltlnl_num1)

#Felt sad, how often past week
summary(full_data$fltsd)
full_data$fltsd_num<- as.numeric(full_data$fltsd)
full_data  <- full_data %>%   #collapse categories   
  mutate(felt_sad = factor (fltsd_num,
                            levels = c(1,2,3,4),
                            labels = c("None or almost none of the time ", "Some of the time", "Most of the time", "All or almost all of the time")))
summary(full_data$felt_sad)

#recode from 0-3
full_data$fltsd_num1[full_data$fltsd_num== 1] <- 0
full_data$fltsd_num1[full_data$fltsd_num== 2] <- 1
full_data$fltsd_num1[full_data$fltsd_num== 3] <- 2
full_data$fltsd_num1[full_data$fltsd_num== 4] <- 3

summary(full_data$fltsd_num1)

#Could not get going, how often past week
summary(full_data$cldgng)
full_data$cldgng_num<- as.numeric(full_data$cldgng)
full_data  <- full_data %>%   #collapse categories   
  mutate(get_going = factor (cldgng_num,
                             levels = c(1,2,3,4),
                             labels = c("None or almost none of the time ", "Some of the time", "Most of the time", "All or almost all of the time")))
summary(full_data$get_going)

#recode from 0-3
full_data$cldgng_num1[full_data$cldgng_num== 1] <- 0
full_data$cldgng_num1[full_data$cldgng_num== 2] <- 1
full_data$cldgng_num1[full_data$cldgng_num== 3] <- 2
full_data$cldgng_num1[full_data$cldgng_num== 4] <- 3

summary(full_data$fltsd_num1)

#new variable for overall mental health score:

full_data<- full_data%>%
  mutate(mental_health_score= (fltdpr_num1 + flteeff_num1 + slprl_num1 + wrhpp_num1 + fltlnl_num1 + enjlf_num1 + fltsd_num1 + cldgng_num1))

summary(full_data$mental_health_score)

#categorical variable for depressed
full_data <- full_data %>% 
  mutate(depressed = ifelse(mental_health_score > 8, yes = 1, no = 0) )

full_data <- full_data %>% 
  mutate(mental_health_binary = factor(depressed,
                                       levels = c(0,1),
                                       labels = c("not depressed", "depressed")))

summary(full_data$mental_health_binary)

#self rated health 
summary(full_data$health)
class(full_data$health)

full_data$self_health_num<- as.numeric(full_data$health) #recode to numeric
summary(full_data$self_health_num)

full_data$self_health[full_data$self_health_num == 1] <- 0
full_data$self_health[full_data$self_health_num == 2] <- 0
full_data$self_health[full_data$self_health_num == 3] <- 1
full_data$self_health[full_data$self_health_num == 4] <- 1
full_data$self_health[full_data$self_health_num == 5] <- 1

summary(full_data$self_health)

full_data <- full_data %>% 
  mutate(self_health_binary = factor(self_health,
                                     levels = c(0,1),
                                     labels = c("good self-rated health", "less than good self-rated health")))

summary(full_data$self_health_binary)

#physical health 
# Health problems, last 12 months: heart or circulation problem
summary(full_data$hltprhc)
class(full_data$hltprhc)

full_data$heart_problem_num<- as.numeric(full_data$hltprbn) #recode to numeric
full_data$heart_problem_num1[full_data$heart_problem_num== 1] <- 0
full_data$heart_problem_num1[full_data$heart_problem_num== 2] <- 1

summary(full_data$heart_problem_num)
summary(full_data$heart_problem_num1)

#Health problems, last 12 months: high blood pressure
summary(full_data$hltprhb)
class(full_data$hltprhb)

full_data$hbp_num<- as.numeric(full_data$hltprhb) #recode to numeric

full_data$hbp_num1[full_data$hbp_num== 1] <- 0
full_data$hbp_num1[full_data$hbp_num== 2] <- 1

summary(full_data$hbp_num)
summary(full_data$hbp_num1)

#Health problems, last 12 months: breathing problems
summary(full_data$hltprbp)
class(full_data$hltprbp)

full_data$breathingproblems_num<- as.numeric(full_data$hltprbp) #recode to numeric

full_data$breathingproblems_num1[full_data$breathingproblems_num== 1] <- 0
full_data$breathingproblems_num1[full_data$breathingproblems_num== 2] <- 1

summary(full_data$breathingproblems_num)
summary(full_data$breathingproblems_num1)

#Health problems, last 12 months: allergies
summary(full_data$hltpral)
class(full_data$hltpral)

full_data$allergies_num<- as.numeric(full_data$hltpral) #recode to numeric
full_data$allergies_num1[full_data$allergies_num== 1] <- 0
full_data$allergies_num1[full_data$allergies_num== 2] <- 1

summary(full_data$allergies_num)
summary(full_data$allergies_num1)

#Health problems, last 12 months: back or neck pain
summary(full_data$hltprbn)
class(full_data$hltprbn)
full_data$backneck_pain_num<- as.numeric(full_data$hltprbn) #recode to numeric
full_data$backneck_pain_num1[full_data$backneck_pain_num== 1] <- 0
full_data$backneck_pain_num1[full_data$backneck_pain_num== 2] <- 1

summary(full_data$backneck_pain_num)
summary(full_data$backneck_pain_num1)

#Health problems, last 12 months: muscular or joint pain in hand or arm
summary(full_data$hltprpa)
class(full_data$hltprpa)

full_data$hand_pain_num<- as.numeric(full_data$hltprpa) #recode to numeric

full_data$hand_pain_num1[full_data$hand_pain_num== 1] <- 0
full_data$hand_pain_num1[full_data$hand_pain_num== 2] <- 1

summary(full_data$hand_pain_num)
summary(full_data$hand_pain_num1)

#Health problems, last 12 months: muscular or joint pain in foot or leg
summary(full_data$hltprpf)
class(full_data$hltprpf)

full_data$foot_pain_num<- as.numeric(full_data$hltprpf) #recode to numeric

full_data$foot_pain_num1[full_data$foot_pain_num== 1] <- 0
full_data$foot_pain_num1[full_data$foot_pain_num== 2] <- 1

summary(full_data$foot_pain_num)
summary(full_data$foot_pain_num1)

#Health problems, last 12 months: stomach or digestion related
summary(full_data$hltprsd)
class(full_data$hltprsd)

full_data$stomach_num<- as.numeric(full_data$hltprsd) #recode to numeric

full_data$stomach_num1[full_data$stomach_num== 1] <- 0
full_data$stomach_num1[full_data$stomach_num== 2] <- 1

summary(full_data$stomach_num)
summary(full_data$stomach_num1)

#Health problems, last 12 months: skin condition related
summary(full_data$hltprsc)
class(full_data$hltprsc)

full_data$skin_num<- as.numeric(full_data$hltprsc) #recode to numeric

full_data$skin_num1[full_data$skin_num== 1] <- 0
full_data$skin_num1[full_data$skin_num== 2] <- 1

summary(full_data$skin_num)
summary(full_data$skin_num1)

#Health problems, last 12 months: severe headaches
summary(full_data$hltprsh)
class(full_data$hltprsh)

full_data$headache_num<- as.numeric(full_data$hltprsh) #recode to numeric

full_data$headache_num1[full_data$headache_num== 1] <- 0
full_data$headache_num1[full_data$headache_num== 2] <- 1

summary(full_data$headache_num)
summary(full_data$headache_num1)

#Health problems, last 12 months: diabetes
summary(full_data$hltprdi)
full_data$diabetes_num<- as.numeric(full_data$hltprdi) #recode to numeric

full_data$diabetes_num1[full_data$diabetes_num== 1] <- 0
full_data$diabetes_num1[full_data$diabetes_num== 2] <- 1

summary(full_data$diabetes_num)
summary(full_data$diabetes_num1)

#no health problems
summary(full_data$hltprnt)

#physical health score:
full_data<- full_data%>%
  mutate(physical_health_score= (heart_problem_num1 + hbp_num1 + breathingproblems_num1 + allergies_num1 + backneck_pain_num1 + hand_pain_num1 + foot_pain_num1 + stomach_num1 + skin_num1 + headache_num1 + diabetes_num1))

summary(full_data$physical_health_score)

full_data <- full_data  %>% 
  mutate(health_issues = ifelse(physical_health_score> 0, yes = 1, no = 0) )

summary(full_data $health_issues)
class(full_data $health_issues)

full_data  <- full_data  %>% 
  mutate(physical_health_binary = factor(health_issues,
                                         levels = c(0,1),
                                         labels = c("has health issue", "no health issues")))

summary(full_data$physical_health_binary)

#precarious work 
##recoding variables
# employment contracts
#wrkctra = Employment contract unlimited or limited duration
summary(full_data $wrkctra)
class(full_data $wrkctra)

full_data $wrkctra_num<- as.numeric(full_data $wrkctra) #recode to numeric
summary(full_data $wrkctra_num)

#coerce into factor
full_data   <- full_data  %>%      
  mutate(contract = factor (wrkctra_num,
                            levels = c(1,2),
                            labels = c("Unlimited", "Limited")))

summary(full_data$contract)
summary(full_data$wrkctra)

#individual controls
#income 
summary(full_data$hinctnta)
class(full_data$hinctnta)

#Feeling about household's income nowadays
summary(full_data$hincfel)
class(full_data$hincfel)
full_data$hincfel_num<- as.numeric(full_data$hincfel)
full_data  <- full_data %>%    
  mutate(feeling_income= factor (hincfel_num,
                                 levels = c(1,2,3,4),
                                 labels = c("Living comfortably on present income","Coping on present income", "Difficult on present income", "Very difficult on present income")))
summary(full_data$feeling_income)

#gender
summary(full_data$gndr)
class(full_data$gndr)

#age
summary(full_data$agea)
class(full_data$agea)
full_data$age<- as.numeric(full_data$agea)
summary(full_data$age)

#Belong to minority ethnic group in country
summary(full_data$blgetmg)
class(full_data$blgetmg)
full_data$blgetmg_num<- as.numeric(full_data$blgetmg) #recode to numeric
full_data<-full_data%>%
  mutate(ethnic_minority = factor(blgetmg_num,
                                  levels = c(1,2),
                                  labels = c("Yes", "No")))

summary(full_data$ethnic_minority)


#Born in country
summary(full_data$brncntr)
full_data$brncntr_num<- as.numeric(full_data$brncntr)
full_data  <- full_data %>%    
  mutate(native = factor (brncntr_num,
                          levels = c(1,2),
                          labels = c("Yes","No")))
summary(full_data$native)

#education level 
summary(full_data$eisced)
full_data$eisced_num<- as.numeric(full_data$eisced)

full_data$eisced_num1[full_data$eisced_num== 1] <- 0
full_data$eisced_num1[full_data$eisced_num== 2] <- 1
full_data$eisced_num1[full_data$eisced_num== 3] <- 1
full_data$eisced_num1[full_data$eisced_num== 4] <- 2
full_data$eisced_num1[full_data$eisced_num== 5] <- 2
full_data$eisced_num1[full_data$eisced_num== 6] <- 3
full_data$eisced_num1[full_data$eisced_num== 7] <- 4
full_data$eisced_num1[full_data$eisced_num== 8] <- 4

full_data  <- full_data %>%    
  mutate(education = factor (eisced_num1,
                             levels = c(1,2,3,4),
                             labels = c("less than or lower secondary", "upper secondary", "advanced vocational, sub-degree", "tertiary education")))
summary(full_data$education)

#labour market indicators & country controls
#country level controls

#min wage binary
summary(full_data$min_wage_binary)
class(full_data$min_wage_binary)

#union density 
summary(full_data$union_density)
class(full_data$union_density)

#level of unemployment 
summary(full_data$UE_rate)
class(full_data$UE_rate)

#GDP logged
summary(full_data$GDP_logged)

#ratio of temporary to regular contracts
summary(full_data$contract_ratio)

#r/s between EPL and active measures
full_data$ALMP_adjusted_num<- as.numeric(full_data$ALMP_adjusted)
summary(full_data$ALMP_adjusted_num)
summary(full_data$EPL_score)

cor(x = full_data$EPL_score, 
    y = full_data$ALMP_adjusted_num, 
    use = "complete.obs") #move in the same direction?

cor(x = full_data$regulation_temp, 
    y = full_data$ALMP_adjusted_num, 
    use = "complete.obs") #move in the opposite

cor(x = full_data$regulation_temp, 
    y = full_data$Active_measures, 
    use = "complete.obs") #move in the opposite

table_a <- tableby(
  formula =  contract ~ cntry,
  data = full_data)

# view the table using summary()
summary(object = table_a,
        text = T, 
        labelTranslations = c(
          cntry= "Country"), 
        digits = 3, 
        test = F, 
        total = F, 
        title = "Contract type by country")

summary(full_data$ALMP_adjusted_num)
summary(full_data$EPL_score)

ggplot(data = full_data) + 
  aes(x = ALMP_adjusted, y = EPL_score) +
  geom_point()

ggplot(data = full_data) + 
  aes(x = ALMP_adjusted, y = regulation_temp) +
  geom_point()

#creating a sample with complete cases
complete_cases<-full_data %>% select(quadrant,gndr, age, native,feeling_income,education,cntry,contract,depressed,health_issues,self_health,UE_rate, GDP_logged, contract_ratio,EPL_score,ALMP_adjusted_num, mental_health_binary, physical_health_binary, self_health_binary)

summary(complete_cases)
complete_cases[complete.cases(complete_cases), ] # Keep only the complete rows
complete_cases1 <- complete_cases[complete.cases(complete_cases), ]

#bivariate descriptive results 
#descriptive stats
table1 <- tableby(
  formula =  contract ~ mental_health_binary + physical_health_binary + self_health_binary,
  data = complete_cases1)

# view the table using summary()
summary(object = table1,
        text = T, 
        labelTranslations = c(
          mental_health_binary= "Mental health ",
          physical_health_binary = "Physical health",
          self_health_binary = "Self-rated health"
        ), 
        digits = 3, 
        test = F, 
        total = F, 
        title = "")

#descriptive stats
table2 <- tableby(
  formula =  contract ~ gndr + age + native + feeling_income + education,
  data = complete_cases1)

# view the table using summary()
summary(object = table2,
        text = T, 
        labelTranslations = c(
          gndr= "Gender",
          agea= "Age",
          native= "Native-born",
          feeling_income= "Feelings toward present income",
          education="Educational level"
        ), 
        digits = 3, 
        test = F, 
        total = F, 
        title = "Characteristics of Sample")

#analysis

#multi-level modelling w random effects 
physical<-lmer(health_issues ~  contract + any other controls or interactions + (1 |  countries), data = full_data)   

#part 1: what is the relationship between precarious work and health?
#mental health
mental_random<- lmer(depressed ~  contract + (1 |  cntry), data = complete_cases1)
mental_controls_random <- lmer(depressed~  contract + native + agea + gndr + feeling_income + education + (1 |  cntry), data = complete_cases1)

#physical health
physical_random <- lmer(health_issues ~  contract + (1 |  cntry), data = complete_cases1)
physical_controls_random <- lmer(health_issues ~  contract + native + agea + gndr + feeling_income + education + (1 |  cntry), data = complete_cases1)

#self rated health 
self_random <- lmer(self_health~  contract + (1 |  cntry), data = complete_cases1)
self_controls_random <- lmer(self_health ~  contract + native + agea + gndr + feeling_income + education + (1 |  cntry), data = complete_cases1)


knitreg(list(mental_random, mental_controls_random, physical_random, physical_controls_random,self_random, self_controls_random),
        custom.model.names = c ("Model 1", "Model 2", "Model 3", "Model 4",
                                "Model 5", "Model 6"),
        custom.coef.names = c ("(Intercept)",
                               "Limited contract (ref. category: Unlimited contract)",
                               "Migrant (ref. category: Native)",
                               "Age",
                               "Female (ref. category: Male)",
                               "Coping on present income (ref. category: Coping well on present income)",
                               "Diffiult on present income",
                               "Very difficult on present income",
                               "Educational level: upper secondary (ref. category: less than and lower secondary",
                               "Educational level: vocational, subdegree",
                               "Educational level: tertiary education" ))

#part 2: how does dualization modify this relationship?
#understanding dualization: quadrant analysis
summary(complete_cases1$quadrant)
complete_cases1 <- complete_cases1 %>% 
  mutate(quadrant= factor(quadrant, 
                          levels = c(1,2,3,4), 
                          labels = c("Low ALMP, Low equal protection legislation",
                                     "Low ALMP, High equal protection legislation", 
                                     "High ALMP, High equal protection legislation",
                                     "High ALMP, Low equal protection legislation")))

mental_health_quadrant <- lmer (depressed ~ contract + native + age + gndr + feeling_income +  education + quadrant +contract *quadrant + UE_rate + GDP_logged + contract_ratio
                                + (1 |  cntry), data = complete_cases1)

physical_health_quadrant <- lmer (health_issues ~ contract + native + age + gndr + feeling_income +  education + quadrant + contract *quadrant + UE_rate + GDP_logged + contract_ratio
                                  + (1 |  cntry), data = complete_cases1)

self_health_quadrant <- lmer (self_health ~ contract + native + age + gndr + feeling_income +  education + quadrant + contract *quadrant + UE_rate + GDP_logged + contract_ratio
                              + (1 |  cntry), data = complete_cases1) 

knitreg(list(mental_health_quadrant, physical_health_quadrant, self_health_quadrant),
        custom.model.names = c ("Mental health", "Physical health","Self-rated health"),
        custom.coef.names = c ("(Intercept)",
                               "Limited contract",
                               "Migrant",
                               "Age",
                               "Female",
                               "Coping on present income (ref. category: Coping well on present income)",
                               "Diffiult on present income",
                               "Very difficult on present income",
                               "Educational level: upper secondary (ref. category: Below upper secondary)",
                               "Educational level: vocational, subdegree",
                               "Educational level: tertiary education",
                               "Quadrant 2: High ALMP, High equal protection legislation (ref. category: Quadrant 1: Low ALMP, Low equal protection legislation)",
                               "Quadrant 3: Low ALMP, High equal protection legislation",
                               "Quadrant 4: High ALMP, Low equal protection legislation",
                               "Unemployment rate",
                               "GDP (logged)",
                               "Ratio of limited to unlimited contracts",
                               "Quadrant 2 x limited contract (ref. category: Quadrant 1 x limited contract)",
                               "Quadrant 3 x limited contract",
                               "Quadrant 4 x limited contract"))

#between contract and ALMPs
#mental health 
mental_health_ALMP_random <- lmer (depressed ~ contract + native + agea + gndr + feeling_income + education + ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score
                                   + (1 |  cntry), data = complete_cases1) 
mental_health_ALMP_random_control <- lmer (depressed ~ contract + native + agea + gndr + feeling_income + education+ ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score + UE_rate + GDP_logged + contract_ratio
                                           + (1 |  cntry), data = complete_cases1) 

#physical health 
physical_health_ALMP_random <- lmer (health_issues ~ contract + native + agea + gndr + feeling_income + education + ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score
                                     + (1 |  cntry), data = complete_cases1) 

physical_health_ALMP_random_control <- lmer (health_issues ~ contract + native + agea + gndr + feeling_income + education+ ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score + UE_rate + GDP_logged + contract_ratio
                                             + (1 |  cntry), data = complete_cases1) 

#self-rated health 
self_health_ALMP_random <- lmer (self_health~ contract + native + agea + gndr + feeling_income + education + ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score
                                 + (1 |  cntry), data = complete_cases1) 
self_health_ALMP_random_control <- lmer (self_health ~ contract + native + agea + gndr + feeling_income + education + ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score + UE_rate + GDP_logged + contract_ratio
                                         + (1 |  cntry), data = complete_cases1) 

#all together now
knitreg(list(mental_health_ALMP_random, mental_health_ALMP_random_control, physical_health_ALMP_random, physical_health_ALMP_random_control,self_health_ALMP_random, self_health_ALMP_random_control),
        custom.model.names = c ("Model 7", "Model 8", "Model 9", "Model 10",
                                "Model 11", "Model 12"),
        custom.coef.names = c ("(Intercept)",
                               "Limited contract",
                               "Migrant (ref. category: Native)",
                               "Age",
                               "Female (ref. category: Male)",
                               "Coping on present income (ref. category: Coping well on present income)",
                               "Diffiult on present income",
                               "Very difficult on present income",
                               "Educational level: upper secondary (ref. category: less than and lower secondary",
                               "Educational level: vocational, subdegree",
                               "Educational level: tertiary education",
                               "ALMP spending (% GDP per 1000 unemployed)",
                               "Equal treatment score",
                               "ALMP spending x limited contract",
                               "Unemployment rate",
                               "GDP (logged)",
                               "Ratio of limited to unlimited contracts"))


#btwn contract and EPL
# mental health
mental_health_EPL_random <- lmer (depressed ~ contract + native + agea + gndr + feeling_income +   education + EPL_score + contract *EPL_score + ALMP_adjusted_num
                                  + (1 |  cntry), data = complete_cases1) 
mental_health_EPL_random_control <- lmer (depressed ~ contract + native + agea + gndr + feeling_income +   education + EPL_score + contract *EPL_score + ALMP_adjusted_num + UE_rate + GDP_logged + contract_ratio
                                          + (1 |  cntry), data = complete_cases1)


#physical health 
physical_health_EPL_random <- lmer (health_issues ~ contract + native + agea + gndr + feeling_income +  education + EPL_score + contract *EPL_score + ALMP_adjusted_num
                                    + (1 |  cntry), data = complete_cases1) 

physical_health_EPL_random_control <- lmer (health_issues ~ contract + native + agea + gndr + feeling_income +   education + EPL_score + contract *EPL_score + ALMP_adjusted_num + UE_rate + GDP_logged + contract_ratio
                                            + (1 |  cntry), data = complete_cases1) 

#self reported health as outcome

self_health_EPL_random <- lmer (self_health ~ contract + native + agea + gndr + feeling_income + education + EPL_score + contract *EPL_score + ALMP_adjusted_num 
                                + (1 |  cntry), data = complete_cases1) 

self_health_EPL_random_control <- lmer (self_health ~ contract + native + agea + gndr + feeling_income +  education +  EPL_score + contract *EPL_score + ALMP_adjusted_num + UE_rate + GDP_logged + contract_ratio
                                        + (1 |  cntry), data = complete_cases1) 

#all together now
knitreg(list(mental_health_EPL_random, mental_health_EPL_random_control, physical_health_EPL_random, physical_health_EPL_random_control,self_health_EPL_random, self_health_EPL_random_control),
        custom.model.names = c ("Model 13", "Model 14", "Model 15", "Model 16",
                                "Model 17", "Model 18"),
        custom.coef.names = c ("(Intercept)",
                               "Limited contract",
                               "Migrant (ref. category: Native)",
                               "Age",
                               "Female (ref. category: Male)",
                               "Coping on present income (ref. category: Coping well on present income)",
                               "Diffiult on present income",
                               "Very difficult on present income",
                               "Educational level: upper secondary (ref. category: less than and lower secondary",
                               "Educational level: vocational, subdegree",
                               "Educational level: tertiary education",
                               "Equal treatment score",
                               "ALMP spending (% GDP per 1000 unemployed)",
                               "Equal treatment score x limited contract",
                               "Unemployment rate",
                               "GDP (logged)",
                               "Ratio of limited to unlimited contracts"))


# predicted probabilities
EPL_physical_predict <- ggpredict(model = physical_health_EPL_random_control, 
                                  terms = c("EPL_score [0:6]", "contract [limited, unlimited]"), 
                                  ci.lvl = NA)
plot(EPL_physical_predict) +
  # ylim sets the min and max for the y axis
  ylim(0, 1) 
#graphs of predicted probabilities 
#ALMP spending 
ALMP_mental<- ggpredict(mental_health_ALMP_random_control, terms = c("ALMP_adjusted_num", "contract") ) %>% plot()
ALMP_physical<- ggpredict(physical_health_ALMP_random_control, terms = c("ALMP_adjusted_num", "contract") ) %>% plot()
ALMP_self<- ggpredict(self_health_ALMP_random_control, terms = c("ALMP_adjusted_num", "contract") ) %>% plot()

#EPL spending 
EPL_mental <- ggpredict(mental_health_EPL_random_control, terms = c("EPL_score", "contract") ) %>% plot()
EPL_phyiscal <- ggpredict(physical_health_EPL_random_control, terms = c("EPL_score", "contract") ) %>% plot()
EPL_self <- ggpredict(self_health_EPL_random_control, terms = c("EPL_score", "contract") ) %>% plot()



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(ALMP_mental, EPL_mental, ALMP_phyiscal, EPL_phyiscal, ALMP_self, EPL_self, plotlist=NULL, file, cols=3, layout=NULL) {
  library(grid)
  library(ggplot2)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(ALMP_mental, EPL_mental, ALMP_phyiscal, EPL_phyiscal, ALMP_self, EPL_self), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

library(grid)
library(ggplot2)
multiplot(ALMP_mental, EPL_mental, ALMP_physical,EPL_phyiscal,  ALMP_self, EPL_self, cols=3)
#> `geom_smooth()` using method = 'loess'
par(mfrow=c(3,2))

#discussion: different effects over gender
#restricting the sample 
full_data_female<- subset(full_data,gndr=="Female") 
full_data_male<- subset(full_data,gndr=="Male") 

#NEW model w both ALMPs and EPL
#ALMPs
#mental health 
mental_health_ALMP_random_female <- lmer (depressed ~ contract + native + age + feeling_income + education + ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score
                                          + (1 |  cntry), data = full_data_female) 
mental_health_ALMP_random_control_female <- lmer (depressed ~ contract + native + age + feeling_income + education+ ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score + UE_rate + GDP_logged + contract_ratio
                                                  + (1 |  cntry), data = full_data_female) 

knitreg(list(mental_health_ALMP_random_female, mental_health_ALMP_random_control_female))

#physical health 
physical_health_ALMP_random_female <- lmer (health_issues ~ contract + native + age  + feeling_income + education + ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score 
                                            + (1 |  cntry), data = full_data_female) 

physical_health_ALMP_random_control_female <- lmer (health_issues ~ contract + native + age  + feeling_income + education + ALMP_adjusted_num + contract *ALMP_adjusted_num +EPL_score + UE_rate + GDP_logged + contract_ratio 
                                                    + (1 |  cntry), data = full_data_female) 

knitreg(list(physical_health_ALMP_random_female, physical_health_ALMP_random_control_female))

#self-rated health 
self_health_ALMP_random_female <- lmer (self_health~ contract + native + age + feeling_income + education + ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score
                                        + (1 |  cntry), data = full_data_female) 
self_health_ALMP_random_control_female <- lmer (self_health ~ contract + native + age + feeling_income + education + ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score + UE_rate + GDP_logged + contract_ratio
                                                + (1 |  cntry), data = full_data_female) 

knitreg(list(self_health_ALMP_random_female, self_health_ALMP_random_control_female))

#btwn contract and EPL
# mental health
mental_health_EPL_random_female <- lmer (depressed ~ contract + native + age + feeling_income +   education + EPL_score + contract *EPL_score + ALMP_adjusted_num
                                         + (1 |  cntry), data = full_data_female) 
mental_health_EPL_random_control_female <- lmer (depressed ~ contract + native + age  + feeling_income +   education + EPL_score + contract *EPL_score + ALMP_adjusted_num + UE_rate + GDP_logged + contract_ratio
                                                 + (1 |  cntry), data = full_data_female) 

knitreg(list(mental_health_EPL_random_female, mental_health_EPL_random_control_female))




#physical health 
physical_health_EPL_random_female <- lmer (health_issues ~ contract + native + age  + feeling_income +  education + EPL_score + contract *EPL_score + ALMP_adjusted_num + contract_ratio
                                           + (1 |  cntry), data = full_data_female) 

physical_health_EPL_random_control_female <- lmer (health_issues ~ contract + native + age  + feeling_income +   education + EPL_score + contract *EPL_score + ALMP_adjusted_num + UE_rate + GDP_logged + contract_ratio
                                                   + (1 |  cntry), data = full_data_female) 

knitreg(list(physical_health_EPL_random_female, physical_health_EPL_random_control_female))


#self reported health as outcome

self_health_EPL_random_female <- lmer (self_health ~ contract + native + age + feeling_income + education + EPL_score + contract *EPL_score + ALMP_adjusted_num + contract_ratio
                                       + (1 |  cntry), data = full_data_female) 

self_health_EPL_random_control_female <- lmer (self_health ~ contract + native + age  + feeling_income +  education +  EPL_score + contract *EPL_score + ALMP_adjusted_num + UE_rate + GDP_logged + contract_ratio
                                               + (1 |  cntry), data = full_data_female) 

knitreg(list(self_health_EPL_random_female, self_health_EPL_random_control_female))

knitreg(list(mental_health_ALMP_random_control_female, physical_health_ALMP_random_control_female, self_health_ALMP_random_control_female),
        custom.model.names = c ("Model 19", "Model 20", "Model 21"),
        custom.coef.names = c ("(Intercept)",
                               "Limited contract (ref. category: Unlimited contract)",
                               "Migrant (ref. category: Native)",
                               "Age",
                               "Coping on present income (ref. category: Coping well on present income)",
                               "Diffiult on present income",
                               "Very difficult on present income",
                               "Educational level: upper secondary (ref. category: less than and lower secondary",
                               "Educational level: vocational, subdegree",
                               "Educational level: tertiary education",
                               "ALMP spending (% GDP per 1000 unemployed)",
                               "Equal treatment score",
                               "ALMP spending x limited contract",
                               "Unemployment rate",
                               "GDP (logged)",
                               "Ratio of limited to unlimited contracts"))

knitreg(list(mental_health_EPL_random_control_female, physical_health_EPL_random_control_female, self_health_EPL_random_control_female),
        custom.model.names = c ("Model 22", "Model 23", "Model 24"),
        custom.coef.names = c ("(Intercept)",
                               "Limited contract (ref. category: Unlimited contract)",
                               "Migrant (ref. category: Native)",
                               "Age",
                               "Coping on present income (ref. category: Coping well on present income)",
                               "Diffiult on present income",
                               "Very difficult on present income",
                               "Educational level: upper secondary (ref. category: less than and lower secondary",
                               "Educational level: vocational, subdegree",
                               "Educational level: tertiary education",
                               "Equal treatment score",
                               "ALMP spending (% GDP per 1000 unemployed)",
                               "Equal treatment score x limited contract",
                               "Unemployment rate",
                               "GDP (logged)",
                               "Ratio of limited to unlimited contracts"))

#graphs of predicted probabilities 
#ALMP spending 
ALMP_mental_female<- ggpredict(mental_health_ALMP_random_control_female, terms = c("ALMP_adjusted_num", "contract") ) %>% plot()
ALMP_physical_female<- ggpredict(physical_health_ALMP_random_control_female, terms = c("ALMP_adjusted_num", "contract") ) %>% plot()
ALMP_self_female<- ggpredict(self_health_ALMP_random_control_female, terms = c("ALMP_adjusted_num", "contract") ) %>% plot()

#EPL spending 
EPL_mental_female <- ggpredict(mental_health_EPL_random_control_female, terms = c("EPL_score", "contract") ) %>% plot()
EPL_phyiscal_female <- ggpredict(physical_health_EPL_random_control_female, terms = c("EPL_score", "contract") ) %>% plot()
EPL_self_female <- ggpredict(self_health_EPL_random_control_female, terms = c("EPL_score", "contract") ) %>% plot()

par(mfrow=c(3,2))

library(grid)
library(ggplot2)
multiplot(ALMP_mental_female,  EPL_mental_female, ALMP_physical_female,EPL_phyiscal_female, ALMP_self_female, EPL_self_female, cols=3)
#> `geom_smooth()` using method = 'loess'

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(ALMP_mental_female, EPL_mental_female, ALMP_phyiscal_female, EPL_phyiscal_female, ALMP_self_female, EPL_self_female, plotlist=NULL, file, cols=3, layout=NULL) {
  library(grid)
  library(ggplot2)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(ALMP_mental_female, EPL_mental_female, ALMP_phyiscal_female, EPL_phyiscal_female, ALMP_self_female, EPL_self_female), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#NEW model w both ALMPs and EPL
#ALMPs
#mental health 
mental_health_ALMP_random_male <- lmer (depressed ~ contract + native + age + feeling_income + education + ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score
                                        + (1 |  cntry), data = full_data_male) 
mental_health_ALMP_random_control_male <- lmer (depressed ~ contract + native + age + feeling_income + education+ ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score + UE_rate + GDP_logged + contract_ratio
                                                + (1 |  cntry), data = full_data_male) 

knitreg(list(mental_health_ALMP_random_male, mental_health_ALMP_random_control_male))

#physical health 
physical_health_ALMP_random_male <- lmer (health_issues ~ contract + native + age  + feeling_income + education + ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score 
                                          + (1 |  cntry), data = full_data_male) 

physical_health_ALMP_random_control_male <- lmer (health_issues ~ contract + native + age  + feeling_income + education + ALMP_adjusted_num + contract *ALMP_adjusted_num +EPL_score + UE_rate + GDP_logged + contract_ratio 
                                                  + (1 |  cntry), data = full_data_male) 

knitreg(list(physical_health_ALMP_random_male, physical_health_ALMP_random_control_male))

#self-rated health 
self_health_ALMP_random_male <- lmer (self_health~ contract + native + age + feeling_income + education + ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score
                                      + (1 |  cntry), data = full_data_male) 
self_health_ALMP_random_control_male <- lmer (self_health ~ contract + native + age + feeling_income + education + ALMP_adjusted_num + contract *ALMP_adjusted_num + EPL_score + UE_rate + GDP_logged + contract_ratio
                                              + (1 |  cntry), data = full_data_male) 

knitreg(list(self_health_ALMP_random_male, self_health_ALMP_random_control_male))

#btwn contract and EPL
# mental health
mental_health_EPL_random_male <- lmer (depressed ~ contract + native + age + feeling_income +   education + EPL_score + contract *EPL_score + ALMP_adjusted_num
                                       + (1 |  cntry), data = full_data_male) 
mental_health_EPL_random_control_male <- lmer (depressed ~ contract + native + age  + feeling_income +   education + EPL_score + contract *EPL_score + ALMP_adjusted_num + UE_rate + GDP_logged + contract_ratio
                                               + (1 |  cntry), data = full_data_male) 

knitreg(list(mental_health_EPL_random_male, mental_health_EPL_random_control_male))

#physical health 
physical_health_EPL_random_male <- lmer (health_issues ~ contract + native + age  + feeling_income +  education + EPL_score + contract *EPL_score + ALMP_adjusted_num + contract_ratio
                                         + (1 |  cntry), data = full_data_male) 

physical_health_EPL_random_control_male <- lmer (health_issues ~ contract + native + age  + feeling_income +   education + EPL_score + contract *EPL_score + ALMP_adjusted_num + UE_rate + GDP_logged + contract_ratio
                                                 + (1 |  cntry), data = full_data_male) 

knitreg(list(physical_health_EPL_random_male, physical_health_EPL_random_control_male))


#self reported health as outcome

self_health_EPL_random_male <- lmer (self_health ~ contract + native + age + feeling_income + education + EPL_score + contract *EPL_score + ALMP_adjusted_num + contract_ratio
                                     + (1 |  cntry), data = full_data_male) 

self_health_EPL_random_control_male <- lmer (self_health ~ contract + native + age  + feeling_income +  education +  EPL_score + contract *EPL_score + ALMP_adjusted_num + UE_rate + GDP_logged + contract_ratio
                                             + (1 |  cntry), data = full_data_male) 

knitreg(list(self_health_EPL_random_male, self_health_EPL_random_control_male))

knitreg(list(mental_health_ALMP_random_control_male, physical_health_ALMP_random_control_male, self_health_ALMP_random_control_male),
        custom.model.names = c ("Model 25", "Model 26", "Model 27"),
        custom.coef.names = c ("(Intercept)",
                               "Limited contract (ref. category: Unlimited contract)",
                               "Migrant (ref. category: Native)",
                               "Age",
                               "Coping on present income (ref. category: Coping well on present income)",
                               "Diffiult on present income",
                               "Very difficult on present income",
                               "Educational level: upper secondary (ref. category: less than and lower secondary",
                               "Educational level: vocational, subdegree",
                               "Educational level: tertiary education",
                               "Equal treatment score",
                               "ALMP spending (% GDP per 1000 unemployed)",
                               "ALMP spending x limited contract",
                               "Unemployment rate",
                               "GDP (logged)",
                               "Ratio of limited to unlimited contracts"))

knitreg(list(mental_health_EPL_random_control_male, physical_health_EPL_random_control_male, self_health_EPL_random_control_male),
        custom.model.names = c ("Model 28", "Model 29", "Model 30"),
        custom.coef.names = c ("(Intercept)",
                               "Limited contract (ref. category: Unlimited contract)",
                               "Migrant (ref. category: Native)",
                               "Age",
                               "Coping on present income (ref. category: Coping well on present income)",
                               "Diffiult on present income",
                               "Very difficult on present income",
                               "Educational level: upper secondary (ref. category: less than and lower secondary",
                               "Educational level: vocational, subdegree",
                               "Educational level: tertiary education",
                               "Equal treatment score",
                               "ALMP spending (% GDP per 1000 unemployed)",
                               "Equal treatment score x limited contract",
                               "Unemployment rate",
                               "GDP (logged)",
                               "Ratio of limited to unlimited contracts"))
#graphs of predicted probabilities 
#ALMP spending 
ALMP_mental_male<- ggpredict(mental_health_ALMP_random_control_male, terms = c("ALMP_adjusted_num", "contract") ) %>% plot()
ALMP_physical_male<- ggpredict(physical_health_ALMP_random_control_male, terms = c("ALMP_adjusted_num", "contract") ) %>% plot()
ALMP_self_male<- ggpredict(self_health_ALMP_random_control_male, terms = c("ALMP_adjusted_num", "contract") ) %>% plot()

#EPL spending 
EPL_mental_male <- ggpredict(mental_health_EPL_random_control_male, terms = c("EPL_score", "contract") ) %>% plot()
EPL_phyiscal_male <- ggpredict(physical_health_EPL_random_control_male, terms = c("EPL_score", "contract") ) %>% plot()
EPL_self_male <- ggpredict(self_health_EPL_random_control_male, terms = c("EPL_score", "contract") ) %>% plot()

par(mfrow=c(3,2))

library(grid)
library(ggplot2)
multiplot(ALMP_mental_male, EPL_mental_male, ALMP_physical_male, EPL_phyiscal_male, ALMP_self_male, EPL_self_male, cols=3)
#> `geom_smooth()` using method = 'loess'



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(ALMP_mental_male, EPL_mental_male, ALMP_physical_male, EPL_phyiscal_male, ALMP_self_male, EPL_self_male, plotlist=NULL, file, cols=3, layout=NULL) {
  library(grid)
  library(ggplot2)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(ALMP_mental_male, EPL_mental_male, ALMP_physical_male, EPL_phyiscal_male, ALMP_self_male, EPL_self_male), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## regression diagnostics
# check for multicollinearity
vif(mental_health_ALMP_random)
vif(physical_health_ALMP_random)
vif(self_health_ALMP_random)


vif(mental_health_EPL_random)
vif(physical_health_EPL_random)
vif(self_health_EPL_random)

#checking for influential outliers
influence.measures(mental_health_ALMP_random)$is.inf[, "cook.d"] %>% sum()
influence.measures(physical_health_ALMP_random)$is.inf[, "cook.d"] %>% sum()
influence.measures(self_health_ALMP_random)$is.inf[, "cook.d"] %>% sum()

influence.measures(mental_health_EPL_random)$is.inf[, "cook.d"] %>% sum()
influence.measures(physical_health_EPL_random)$is.inf[, "cook.d"] %>% sum()
influence.measures(self_health_EPL_random)$is.inf[, "cook.d"] %>% sum()



