#
library(readxl) # Data read from XLXS
library(tidyverse) #--- Several package -- 
library(labelled) # ---Var Label
library(gtsummary) #--Descriptive Statistics using Psych Package epid
#
setwd("D:/abc")
dir()
#
mydata<-read_excel("MyBDHS.xlsx")
names(mydata)
#
B1<-select(mydata, main_id, v025,v190, v012, v106, v116, v437, v438, v744a, v744b, v744c, 
           v744d, v744e, b19, b4, v008, b3, m2a, m2b, m2c, m2e, m2j, m14, hw70, hw71, hw72)
#
names(B1)
#
B2<-B1%>%
  mutate(
           v437=ifelse(v437<9000, v437, NA),
           v438=ifelse(v438<9000, v438, NA),
           hw70=ifelse(hw70<9000, hw70, NA),
           hw71=ifelse(hw71<9000, hw71, NA),
           hw72=ifelse(hw72<9000, hw72, NA),
           mW=v437/10,
           mH= v438/10,
           haz=hw70/100,
           waz=hw71/100,
           whz=hw72/100,
           #Type of toilet facility
           toilet2=ifelse(v116==11|v116==12|v116==13|v116==21|v116==22,0, ifelse(v116==23|v116==31|v116==41|v116==42|v116==43|v116==97,1, NA)),
           toilet2=factor(toilet2, c(0,1), labels = c("Improved", "Unimproved"),exclude = NA), 
           toilet2= relevel(toilet2, ref = "Improved"),
           #Mother's education
           edu=ifelse(v106==0|v106==1,0, ifelse(v106==2|v106==3,1, NA)), 
           edu = factor(edu, c(0,1), labels=c("Below secondary",  "Secondary and above"),exclude = NA),
           edu = relevel(edu, ref = "Secondary and above"),
           #Attitudes to domestic Violence
           DomesticViolence=ifelse(v744a==1|v744b==1| v744c==1| v744d==1| v744e==1, 1, 0),
           DomesticViolence = factor(DomesticViolence,c(0,1), labels=c("No",  "Yes"),exclude = NA),
           DomesticViolence =relevel(DomesticViolence, ref = "No"),
           #At leas 4 ANC from Medically trained provider
           anc2=ifelse((m2a==1| m2b==1| m2c==1| m2e==1 |m2j==1)&(m14 >3 & m14 <= 20),1, ifelse(is.na(m2a), NA,0)),
           anc2 = factor(anc2, c(0,1), labels=c("No",  "Yes"),exclude = NA),
           anc2=relevel(anc2, ref = "Yes"),
           #Maternal BMI
           bmi=mW/(mH/100)^2,   
           bmiCAT=ifelse(bmi>=18.5, 0, ifelse(bmi<18.5, 1, NA)),
           bmiCAT = factor(bmiCAT, c(0,1), labels=c("Normal" , "Underweight")),
           bmiCAT = relevel(bmiCAT, ref = "Normal"),
           #Height-for age; Stunting
           stunting=ifelse(haz<(-2), 1, 0),
           stunting = factor(stunting, c(0,1), labels=c("Non-stunted", "stunted")),
           stunting = relevel(stunting, ref = "Non-stunted"),
           #Weight-for age; underweight
           underweight=ifelse(waz<(-2), 1, 0),
           underweight = factor(underweight, c(0,1), labels=c("Non-underweight", "underweight")),
           underweight = relevel(underweight, ref = "Non-underweight"),
           #Weight-for-Height; wasting
           wasting=ifelse(whz<(-2), 1, 0),
           wasting = factor(wasting, c(0,1), labels=c("Non-wasting", "wasting")),
           wasting = relevel(wasting, ref = "Non-wasting"),
           #Place of residence
           v025 = factor(v025,c(1, 2), labels=c("Urban", "Rural")),
           v025 = relevel(v025, ref = "Urban"),
           #Sex
           b4 = factor(b4,c(1, 2), labels=c("Male", "Female")),
)%>%
rename(
  sex = b4,
  DM = DomesticViolence)


#
var_label(B2) <- list(
  toilet2="Type of toilet facility",
  edu="Mother's education",
  DM="Attitudes to domestic Violence",
  anc2="At leas 4 ANC from Medically trained provider",
  mW="Maternal Weight in kg",
  mH="Maternal Height in cm",
  bmi="Maternal BMI",
  bmiCAT="Maternal underweight(BMI<18.5)",
  haz="Height-for-age",
  stunting="Childhood stunting",
  waz="Weight-for-age",
  underweight="Childhood underweight",
  whz="Weight-for-Height",
  wasting="Childhood wasting",
  v025="Place of residence",
  v012="Maternal Age",
  sex= "Sex of child"
) 
var_label(B2)
#
B3<-select(B2,main_id,v025, mW,  mH, haz, waz,  whz,  toilet2, edu, DM, anc2,  bmi, bmiCAT,  stunting,  underweight, wasting, sex)
#
MyStat <- list(all_continuous() ~ "{mean} ± {sd}", all_categorical() ~ "{n} ({p})")
MyDigit <- list( all_categorical() ~ c(0, 2), all_continuous() ~ c(2,2) )

MyTab<-B3 %>% 
  filter(!is.na(v025))%>%
  select(v025, mW,  mH, haz, waz,  whz,  toilet2, edu,  DM, anc2,  bmi, bmiCAT,  stunting,  underweight, wasting, sex) %>%
  tbl_summary(by = v025, missing = "no",statistic = MyStat, digits = MyDigit)%>%
  add_overall(last = TRUE)%>%
  add_p(list(all_continuous() ~ "t.test"), pvalue_fun = ~ style_pvalue(.x, digits = 3))%>%
  modify_caption("General characteristic of respondents")%>%
  add_stat_label()%>%
  bold_labels()
MyTab






