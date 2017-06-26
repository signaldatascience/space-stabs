# tabsSpaces redo:
  
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
# install.packages("plotrix")
library(plotrix)
library(dummies)
library(glmnet)
library(corrplot)
# install.packages("gbm")
library(gbm)
library(caret)
library(caretEnsemble)


#importing survey results: 51392 obs of 154 variables
pub=read.csv(file = "C:/Users/kim/Google Drive/001-SIGNAL/Week8/survey_results_public.csv",header = TRUE,sep = ',')
pub_orig=pub  #creating backup

#importing processed schema data with question IDs and base info (who the question was offered to)
schema=read.csv(file = "C:/Users/kim/Google Drive/001-SIGNAL/Week8/survey_results_schema_plus.csv",header = TRUE,sep = ',')


#---Restricting Dataset -----------------------------------------------------#

#restricting dataset to professional developers (Q100==1) who are currently employed (Q135 <= 3).  
pub = pub_orig[pub_orig$Professional == 'Professional developer' & pub_orig$EmploymentStatus %in% c("Employed full-time","Employed part-time","Independent contractor, freelancer, or self-employed"),]
#35326 obs of 154 variables

#restricting further to those who answered the question of tabs vs spaces: 
pub=pub[!is.na(pub$TabsSpaces),]
#28074 obs of 154 variables


#removing variables (questions) posed to folks who were not professional coders (Q100!=1)
colRem=c("Q100 == 2","Q100 == 3","Q100 == 4","Q100 == 5")
col=schema[schema$BASE %in% colRem,]
col$Column
colR=c('ExCoderReturn','ExCoderNotForMe','ExCoderBalance','ExCoder10Years','ExCoderBelonged','ExCoderSkills','ExCoderWillNotCode','ExCoderActive','ExpectedSalary')

pub=select(pub,-c(ExCoderReturn,ExCoderNotForMe,ExCoderBalance,ExCoder10Years,ExCoderBelonged,ExCoderSkills,ExCoderWillNotCode,ExCoderActive,ExpectedSalary))
#28074 obs of 145 variables
rm(colR)
rm(colRem)
rm(col)

#removing unwanted levels
pub=droplevels(pub)


#removing professional developer and nondveloper type columns (too few levels)
pub=select(pub,-c(NonDeveloperType,Professional))
#28074 obs of 143 variables

#removing rows without reported salary
pub=pub[!is.na(pub$Salary),]
#12426 obs of 143 variables



# pub$CousinEducation can be removed, as it was administered randomly and there's no indication of who it was administered to
pub$CousinEducation = NULL
#12426 obs of 142 variables


# Visually Inspecting the Data -------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------

summary(pub$TabsSpaces)
hist(pub$Salary,breaks = 5000)
length(pub[pub$Salary==0,])  #143


round(pub$Salary,2)
levels(pub$Country)
hist(pub$Salary[pub$Country == "United States"],breaks = 15)
hist(count(pub$TabsSpaces[pub$Country == "United States"]))
plot(table(pub$TabsSpaces[pub$Country == "United States"]))

length(pub$Salary[pub$Country == "United States"])  #3675

qplot(pub$Salary[pub$Country == "United States"], color = pub$TabsSpaces[pub$Country == "United States"])
#stacks the different answers vertically in a count. a bit hard to visualize. 

pub$TabsSpaces

Tabs= c(pub$Salary[pub$Country == "United States" & pub$TabsSpaces == 'Tabs'])
Both = c(pub$Salary[pub$Country == "United States" & pub$TabsSpaces == 'Both'])
Spaces= c(pub$Salary[pub$Country == "United States" & pub$TabsSpaces == 'Spaces'])


saltab=vector(list,c(Tabs, Both, Spaces))
pubUS= pub[pub$Country == "United States",] 

  
multhist(saltab, beside = TRUE)
hist(Tabs)

glm(formula = Salary~TabsSpaces,family = 'gaussian',data = pub[pub$Country == "United States",])

ggplot(pubUS,aes(x=pubUS$Salary,group=pubUS$TabsSpaces,fill=pubUS$TabsSpaces))+
  geom_histogram(position="dodge")+theme_bw()


# --- manual dummy processing----------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------

#per later notes, add MobileDeveloperType to this 


#creating Dummy Variables for the columns that didn't import well

pub_levels = pub %>% 
  sapply(levels)

num_levels = pub_levels  %>% 
  sapply(length)
names(num_levels)=p_lev
p_lev = names(pub_levels)

pub_lev=sort(num_levels, decreasing=TRUE)

rm(p_lev)
rm(num_levels)
rm(pub_levels)

head(pub_lev,25)
# WantWorkLanguage   HaveWorkedLanguage                  IDE    ImportantBenefits         MetricAssess 
# 11239                 8438                 4798                 3530                 2420 
# WantWorkPlatform   HaveWorkedPlatform        DeveloperType      SelfTaughtTypes          Methodology 
# 2198                 2006                 1823                 1149                  788 
# CousinEducation           JobProfile       EducationTypes    WantWorkFramework  HaveWorkedFramework 
# 735                  479                  447                  407                  285 
# WantWorkDatabase   HaveWorkedDatabase              Country                 Race StackOverflowDevices 
# 248                  237                  201                   97                   47 
# Gender            WorkStart         YearsProgram        YearsCodedJob    YearsCodedJobPast 
# 29                   24                   21                   21                   21 
# > 



pub$ha


# for HaveWorkedLanguage variable (imported with 8438 levels) -------------------------

languages = pub %>%
  unnest(HaveWorkedL_ = str_split(HaveWorkedLanguage, "; ")) %>%
  transmute(Respondent, HaveWorkedL_)


languages$HaveWorkedL_=as.factor(languages$HaveWorkedL_)  #changes this to a factor
View(languages)
levels(languages$HaveWorkedL_)

lang_d=dummy.data.frame(languages)  #makes dummy dataframe
View(lang_d)
lang_d = lang_d %>%
  aggregate(by = lang_d['Respondent'], FUN=sum)
#collapses them back into a single row per respondent.  respondent column is twice, but we can remove that column.

lang_d=lang_d[,-2]  #removes respondent sum row

lang_d = lang_d %>%
  mutate(TotalLangWorked = rowSums(lang_d)-lang_d$Respondent)    #adds sum of languages worked
ncol(lang_d)
lang_d=lang_d[,c(1,38,2:37)] #rearranges lang_d


rm(languages)


# for DeveloperType variable (imported with 1823 levels) -------------------------



dev_type= pub %>%
  unnest(Dev_Type_ = str_split(DeveloperType, "; ")) %>%
  transmute(Respondent, Dev_Type_)

dev_type$Dev_Type_=as.factor(dev_type$Dev_Type_)
levels(dev_type$Dev_Type_)
dev_d=dummy.data.frame(dev_type)   #makes dummy dataframe

d_dev = dev_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=dev_d['Respondent'], FUN=sum)
#12426 obs of 17 var.

d_dev=d_dev[,-2]  #removes respondent sum row

d_dev = d_dev %>%
  mutate(DevTypesTotal = rowSums(d_dev)-d_dev$Respondent)    #adds sum of developer types selected
ncol(d_dev)
d_dev=d_dev[,c(1,17,2:16)] #rearranges lang_d

View(d_dev)
rm(dev_type)
rm(dev_d)





# for ImportantBenefits variable (imported with 3530 levels) -------------------------

imp_ben= pub %>%
  unnest(benefit_ = str_split(ImportantBenefits, "; ")) %>%
  transmute(Respondent, benefit_)

imp_ben$benefit_=as.factor(imp_ben$benefit_)
levels(imp_ben$benefit_)
ben_d=dummy.data.frame(imp_ben)   #makes dummy dataframe

ben_d = ben_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=ben_d['Respondent'], FUN=sum)
#12426 obs of 20 var.

ben_d=ben_d[,-2]  #removes respondent sum row

ben_d = ben_d %>%
  mutate(TotalBenefitsImportant = rowSums(ben_d)-ben_d$Respondent-ben_d$benefit_NA)    #adds sum of important benefits
ncol(ben_d)  #20
ben_d=ben_d[,c(1,20,2:19)] #rearranges dummy_extra

View(ben_d)
rm(imp_ben)





# for WantWorkLanguage variable (imported with 11239 levels) -------------------------


w_lang= pub %>%
  unnest(LangWant_ = str_split(WantWorkLanguage, "; ")) %>%
  transmute(Respondent, LangWant_)

w_lang$LangWant_=as.factor(w_lang$LangWant_)
levels(w_lang$LangWant_)
w_lang_d=dummy.data.frame(w_lang)   #makes dummy dataframe

w_lang_d = w_lang_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=w_lang_d['Respondent'], FUN=sum)
#12426 obs of 38 var.

w_lang_d=w_lang_d[,-2]  #removes respondent sum row

w_lang_d = w_lang_d %>%
  mutate(TotalLangWanted = rowSums(w_lang_d)-w_lang_d$Respondent-w_lang_d$LangWant_NA)    #adds sum of languages wanted
ncol(w_lang_d)  #38
w_lang_d=w_lang_d[,c(1,38,2:37)] #rearranges dummy_extra

View(w_lang_d)
rm(w_lang)









# for IDE variable (imported with 4798 levels) -------------------------


ide= pub %>%
  unnest(IDE_ = str_split(IDE, "; ")) %>%
  transmute(Respondent, IDE_)

ide$IDE_=as.factor(ide$IDE_)
levels(ide$IDE_)
ide_d=dummy.data.frame(ide)   #makes dummy dataframe

ide_d = ide_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=ide_d['Respondent'], FUN=sum)
#12426 obs of 25 var.

ide_d=ide_d[,-2]  #removes respondent sum row

ide_d = ide_d %>%
  mutate(totalIDEused = rowSums(ide_d)-ide_d$Respondent-ide_d$IDE_NA)    #adds sum of ides used
ncol(ide_d)  #25
ide_d=ide_d[,c(1,25,2:24)] #rearranges dummy_extra

View(ide_d)
rm(ide)





# for MetricAssess variable (imported with 2420 levels) -------------------------


met_assess= pub %>%
  unnest(metric_ = str_split(MetricAssess, "; ")) %>%
  transmute(Respondent, metric_)

met_assess$metric_=as.factor(met_assess$metric_)
levels(met_assess$metric_)
met_d=dummy.data.frame(met_assess)   #makes dummy dataframe

met_d = met_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=met_d['Respondent'], FUN=sum)
#12426 obs of 25 var.

met_d=met_d[,-2]  #removes respondent sum row

met_d = met_d %>%
  mutate(TotalImpMetrics = rowSums(met_d)-met_d$Respondent-met_d$metric_NA)    #adds sum of important metrics chosen
ncol(met_d)  #16
met_d=met_d[,c(1,16,2:15)] #rearranges dummy_extra

View(met_d)
rm(met_assess)






# for WantWorkPlatform variable (imported with 2198 levels) -------------------------


ww_plat= pub %>%
  unnest(WantPlatform_ = str_split(WantWorkPlatform, "; ")) %>%
  transmute(Respondent, WantPlatform_)

ww_plat$WantPlatform_=as.factor(ww_plat$WantPlatform_)
levels(ww_plat$WantPlatform_)
ww_plat_d=dummy.data.frame(ww_plat)   #makes dummy dataframe

ww_plat_d = ww_plat_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=ww_plat_d['Respondent'], FUN=sum)
#12426 obs of 18 var.

ww_plat_d=ww_plat_d[,-2]  #removes respondent sum row

ww_plat_d = ww_plat_d %>%
  mutate(TotPlatformWanted = rowSums(ww_plat_d)-ww_plat_d$Respondent-ww_plat_d$WantPlatform_NA)    #adds sum of platforms wanted
ncol(ww_plat_d)  #18
ww_plat_d=ww_plat_d[,c(1,18,2:17)] #rearranges dummy_extra

View(ww_plat_d)
rm(ww_plat)





# for HaveWorkedPlatform variable (imported with 2006 levels) -------------------------


hw_plat= pub %>%
  unnest(HaveWorkedPlat_ = str_split(HaveWorkedPlatform, "; ")) %>%
  transmute(Respondent, HaveWorkedPlat_)

hw_plat$HaveWorkedPlat_=as.factor(hw_plat$HaveWorkedPlat_)
levels(hw_plat$HaveWorkedPlat_)
hw_plat_d=dummy.data.frame(hw_plat)   #makes dummy dataframe

hw_plat_d = hw_plat_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=hw_plat_d['Respondent'], FUN=sum)
#12426 obs of 18 var.

hw_plat_d=hw_plat_d[,-2]  #removes respondent sum row

hw_plat_d = hw_plat_d %>%
  mutate(TotPlatfWorked = rowSums(hw_plat_d)-hw_plat_d$Respondent-hw_plat_d$HaveWorkedPlat_NA)    #adds sum of platforms worked
ncol(hw_plat_d)  #18
hw_plat_d=hw_plat_d[,c(1,18,2:17)] #rearranges dummy_extra

View(hw_plat_d)
rm(hw_plat)






# for SelfTaughtTypes variable (imported with 1149 levels) -------------------------


selfTaught= pub %>%
  unnest(selfTaughtType_ = str_split(SelfTaughtTypes, "; ")) %>%
  transmute(Respondent, selfTaughtType_)

selfTaught$selfTaughtType_=as.factor(selfTaught$selfTaughtType_)
levels(selfTaught$selfTaughtType_)
selfT_d=dummy.data.frame(selfTaught)   #makes dummy dataframe

selfT_d = selfT_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=selfT_d['Respondent'], FUN=sum)
#12426 obs of 14 var.

selfT_d=selfT_d[,-2]  #removes respondent sum row

selfT_d = selfT_d %>%
  mutate(TotalSelfTaught = rowSums(selfT_d)-selfT_d$Respondent-selfT_d$selfTaughtType_NA)    #adds sum of self teaching methods used
ncol(selfT_d)  #14
selfT_d=selfT_d[,c(1,14,2:13)] #rearranges dummy_extra

View(selfT_d)
rm(selfTaught)












# for Methodology variable (imported with 788 levels) -------------------------


method= pub %>%
  unnest(Method_ = str_split(Methodology, "; ")) %>%
  transmute(Respondent, Method_)

method$Method_=as.factor(method$Method_)
levels(method$Method_)
method_d=dummy.data.frame(method)   #makes dummy dataframe

method_d = method_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=method_d['Respondent'], FUN=sum)
#12426 obs of 14 var.

method_d=method_d[,-2]  #removes respondent sum row

method_d = method_d %>%
  mutate(TotMethodsKnown = rowSums(method_d)-method_d$Respondent-method_d$Method_NA)    #adds sum of methods known
ncol(method_d)  #14
method_d=method_d[,c(1,14,2:13)] #rearranges dummy_extra

View(method_d)
rm(method)









# for JobProfile variable (imported with 479 levels) -------------------------


job_prof= pub %>%
  unnest(JobProf_ = str_split(JobProfile, "; ")) %>%
  transmute(Respondent, JobProf_)

job_prof$JobProf_=as.factor(job_prof$JobProf_)
levels(job_prof$JobProf_)
job_prof_d=dummy.data.frame(job_prof)   #makes dummy dataframe

job_prof_d = job_prof_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=job_prof_d['Respondent'], FUN=sum)
#12426 obs of 22 var.

job_prof_d=job_prof_d[,-2]  #removes respondent sum row

job_prof_d = job_prof_d %>%
  mutate(TotProfilesUsed = rowSums(job_prof_d)-job_prof_d$Respondent-job_prof_d$JobProf_NA)    #adds sum of Job profiles used
ncol(job_prof_d)  #22
job_prof_d=job_prof_d[,c(1,22,2:21)] #rearranges dummy_extra

View(job_prof_d)
rm(job_prof)






# for EducationTypes variable (imported with 447 levels) -------------------------


edu_type= pub %>%
  unnest(EduType_ = str_split(EducationTypes, "; ")) %>%
  transmute(Respondent, EduType_)

edu_type$EduType_=as.factor(edu_type$EduType_)
levels(edu_type$EduType_)
edu_type_d=dummy.data.frame(edu_type)   #makes dummy dataframe

edu_type_d = edu_type_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=edu_type_d['Respondent'], FUN=sum)
#12426 obs of 12 var.

edu_type_d=edu_type_d[,-2]  #removes respondent sum row

edu_type_d = edu_type_d %>%
  mutate(TotalEduTypes = rowSums(edu_type_d)-edu_type_d$Respondent-edu_type_d$EduType_NA)    #adds sum of education types used
ncol(edu_type_d)  #12
edu_type_d=edu_type_d[,c(1,12,2:11)] #rearranges dummy_extra

View(edu_type_d)
rm(edu_type)









# for HaveWorkedFramework variable (imported with 285 levels) -------------------------


wkd_frm= pub %>%
  unnest(Framework_ = str_split(HaveWorkedFramework, "; ")) %>%
  transmute(Respondent, Framework_)

wkd_frm$Framework_=as.factor(wkd_frm$Framework_)
levels(wkd_frm$Framework_)
wkd_frm_d=dummy.data.frame(wkd_frm)   #makes dummy dataframe

wkd_frm_d = wkd_frm_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=wkd_frm_d['Respondent'], FUN=sum)
#12426 obs of 12 var.

wkd_frm_d=wkd_frm_d[,-2]  #removes respondent sum row

wkd_frm_d = wkd_frm_d %>%
  mutate(TotalFrameworks = rowSums(wkd_frm_d)-wkd_frm_d$Respondent-wkd_frm_d$Framework_NA)    #adds sum of Frameworks worked
ncol(wkd_frm_d)  #12
wkd_frm_d=wkd_frm_d[,c(1,12,2:11)] #rearranges dummy_extra

View(wkd_frm_d)
rm(wkd_frm)







# for HaveWorkedDatabase variable (imported with 237 levels) -------------------------


wkd_db= pub %>%
  unnest(DB_ = str_split(HaveWorkedDatabase, "; ")) %>%
  transmute(Respondent, DB_)

wkd_db$DB_=as.factor(wkd_db$DB_)
levels(wkd_db$DB_)
wkd_db_d=dummy.data.frame(wkd_db)   #makes dummy dataframe

wkd_db_d = wkd_db_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=wkd_db_d['Respondent'], FUN=sum)
#12426 obs of 11 var.

wkd_db_d=wkd_db_d[,-2]  #removes respondent sum row

wkd_db_d = wkd_db_d %>%
  mutate(TotalDBsWorked = rowSums(wkd_db_d)-wkd_db_d$Respondent-wkd_db_d$DB_NA)    #adds sum of DBs worked
ncol(wkd_db_d)  #11
wkd_db_d=wkd_db_d[,c(1,11,2:10)] #rearranges dummy_extra

View(wkd_db_d)
rm(wkd_db)









# for Race variable (imported with 97 levels) -------------------------


race= pub %>%
  unnest(Race_ = str_split(Race, "; ")) %>%
  transmute(Respondent, Race_)

race$Race_=as.factor(race$Race_)
levels(race$Race_)
race_d=dummy.data.frame(race)   #makes dummy dataframe

race_d = race_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=race_d['Respondent'], FUN=sum)
#12426 obs of 12 var.

race_d=race_d[,-2]  #removes respondent sum row

race_d = race_d %>%
  mutate(MultiRacial = rowSums(race_d)-race_d$Respondent-race_d$Race_NA)  #adds sum of races ticked
race_d$MultiRacial =ifelse(race_d$MultiRacial <= 1,0,1) #converts MultiRacial to binary variable


ncol(race_d)  #12
race_d=race_d[,c(1,12,2:11)] #rearranges dummy_extra

View(race_d)
rm(race)






# for Gender variable (imported with  29 levels) -------------------------


gender= pub %>%
  unnest(Gender_ = str_split(Gender, "; ")) %>%
  transmute(Respondent, Gender_)

gender$Gender_=as.factor(gender$Gender_)
levels(gender$Gender_)
gender_d=dummy.data.frame(gender)   #makes dummy dataframe

gender_d = gender_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=gender_d['Respondent'], FUN=sum)
#12426 obs of 8 var.

gender_d=gender_d[,-2]  #removes respondent sum row

# gender_d = gender_d %>%
#   mutate(NB = rowSums(gender_d)-gender_d$Respondent-gender_d$Gender_NA,Cis = ifelse(gender_d$Gender_ %in% c("Female","Male"),1,0)) #converts MultiRacial to binary variable
# gender_d$NB =ifelse(gender_d$NB > 1,1,0) #converts MultiRacial to binary variable



ncol(gender_d)  #7
# gender_d=gender_d[,c(1,12,2:11)] #rearranges gender_d

View(gender_d)
rm(gender)







# for WantWorkFramework variable (imported with 407 levels) -------------------------


wnt_frm= pub %>%
  unnest(FrmWnted_ = str_split(WantWorkFramework, "; ")) %>%
  transmute(Respondent, FrmWnted_)

wnt_frm$FrmWnted_=as.factor(wnt_frm$FrmWnted_)
levels(wnt_frm$FrmWnted_)
wnt_frm_d=dummy.data.frame(wnt_frm)   #makes dummy dataframe

wnt_frm_d = wnt_frm_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=wnt_frm_d['Respondent'], FUN=sum)
#12426 obs of 12 var.

wnt_frm_d=wnt_frm_d[,-2]  #removes respondent sum row

wnt_frm_d = wnt_frm_d %>%
  mutate(TotFramwkWnted = rowSums(wnt_frm_d)-wnt_frm_d$Respondent-wnt_frm_d$FrmWnted_NA)    #adds sum of Frameworks worked
ncol(wnt_frm_d)  #12
wnt_frm_d=wnt_frm_d[,c(1,12,2:11)] #rearranges lang_d

View(wnt_frm_d)
rm(wnt_frm)







# for WantWorkDatabase variable (imported with 248 levels) -------------------------


ww_db= pub %>%
  unnest(DBWanted_ = str_split(WantWorkDatabase, "; ")) %>%
  transmute(Respondent, DBWanted_)

ww_db$DBWanted_=as.factor(ww_db$DBWanted_)
levels(ww_db$DBWanted_)
ww_db_d=dummy.data.frame(ww_db)   #makes dummy dataframe

ww_db_d = ww_db_d %>%     #collapses extra rows back into a single row per respondent.  respondent column is twice, but we can remove that column.
  aggregate(by=ww_db_d['Respondent'], FUN=sum)
#12426 obs of 11 var.

ww_db_d=ww_db_d[,-2]  #removes respondent sum row

ww_db_d = ww_db_d %>%
  mutate(TotalDBWanted = rowSums(ww_db_d)-ww_db_d$Respondent-ww_db_d$DBWanted_NA)    #adds sum of Frameworks worked
ncol(ww_db_d)  #11
ww_db_d=ww_db_d[,c(1,11,2:10)] #rearranges lang_d

View(ww_db_d)
rm(ww_db)




# Final pub processing before combining into df to pass into models -----------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------


#removing all the variables we will be turning into dummies manually:
pub_dummed_vars=c('WantWorkDatabase','WantWorkFramework','Gender','Race','HaveWorkedDatabase','HaveWorkedFramework','EducationTypes','JobProfile','Methodology','SelfTaughtTypes','HaveWorkedPlatform','WantWorkPlatform','MetricAssess','IDE','WantWorkLanguage','ImportantBenefits','DeveloperType','HaveWorkedLanguage')
#per later notes, add MobileDeveloperType to this list

length(pub_dummed_vars) #18

colnames(pub)

pub=pub[,!(names(pub) %in% pub_dummed_vars)]   #12426 obs of 124 var








extra_dum_sets=c(lang_d,d_dev,ben_d,w_lang_d,ide_d,met_d,ww_plat_d,hw_plat_d,selfT_d,method_d,job_prof_d,edu_type_d,wkd_frm_d,wkd_db_d,race_d,gender_d,wnt_frm_d,ww_db_d)


colnames(pub)

pub2=pub[,!(names(pub) %in% pub_dummed_vars)]   #12426 obs of 124 var


pub2_d=dummy.data.frame(pub2)
#12426 obs of 965 var

pub2_d_bkup=pub2_d


pub2_d=as.data.frame(cbind(pub2_d,d_dev,ben_d,w_lang_d,ide_d,met_d,ww_plat_d,hw_plat_d,selfT_d,method_d,job_prof_d,edu_type_d,wkd_frm_d,wkd_db_d,race_d,gender_d,wnt_frm_d,ww_db_d))
#12426 obs of 1244 var

colnames(pub2_d)

grep(pattern = "Respondent",x = colnames(pub2_d))
in_rem=grep(pattern = "Respondent",x = colnames(pub2_d))  #  1  966  983 1003 1041 1066 1082 1100 1118 1132 1146 1168 1180 1192 1203 1215 1222 1234
in_rem=in_rem[-1]
length(in_rem)   #17


#12426 obs of 1244 var
pub2_d[,in_rem]=NULL
#12426 obs of 1227 var




# ----  STILL NEED TO GO THROUGH AND REMOVE VARIABLES SO THAT THINGS ARE NOT DEPENDENT ON EACH OTHER ----#




pub_scaled=as.data.frame(scale(pub2_d))
Respondant=select(pub2_d,Respondent)
respondant_scaled=select(pub_scaled,Respondent)
target=pub2_d$Salary             #separates out Y
t_scaled=pub_scaled$Salary      #separates out Y

pub2_d=select(pub2_d,-Respondent)                #removes Respondent from df
pub_scaled=select(pub_scaled,-Respondent)        #removes Respondent from df
pub2_d=select(pub2_d,-Salary)                #removes predictor from df
pub_scaled=select(pub_scaled,-Salary)        #removes predictor from df



which(is.na(pub_scaled))
nacols <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) any(is.na(x))))]
}

nacols(pub_scaled) #[1] "CareerSatisfaction"        "JobSatisfaction"           "HoursPerWeek" [4] "StackOverflowSatisfaction"



#fixing CareerSatisfaction:

replace=which(is.na(pub_scaled$CareerSatisfaction))
rp_col=c(rep(0,length(pub_scaled$JobSatisfaction)))
rp_col[replace]=1
pub_scaled = pub_scaled %>%
  mutate(isNA_careerSatisfaction = rp_col)

#replaces career satisfaction NAs with the floor of the average career satisfaction, after adding a column to account for those who didn't report careeer satisfaction
pub_scaled$CareerSatisfaction[replace]=floor(mean(pub_scaled$CareerSatisfaction,na.rm = TRUE))  #7   
pub_scaled$CareerSatisfaction=round(pub_scaled$CareerSatisfaction,0)







#fixing pub_scaled$JobSatisfaction
pub_scaled$JobSatisfaction
replace=which(is.na(pub_scaled$JobSatisfaction))
rp_col=c(rep(0,length(pub_scaled$JobSatisfaction)))
rp_col[replace]=1
pub_scaled = pub_scaled %>%
  mutate(isNA_JobSatisfaction = rp_col)

#replaces job satisfaction NAs with the floor of the average job satisfaction, after adding a column to account for those who didn't report job satisfaction
pub_scaled$JobSatisfaction[replace]=floor(mean(pub_scaled$JobSatisfaction,na.rm = TRUE))  #7   



#fixing pub_scaled$HoursPerWeek
pub_scaled$HoursPerWeek
replace=which(is.na(pub_scaled$HoursPerWeek))
rp_col=c(rep(0,length(pub_scaled$HoursPerWeek)))
rp_col[replace]=1
pub_scaled = pub_scaled %>%
  mutate(isNA_HoursPerWeek = rp_col)



#replaces HoursPerWeek NAs with the floor of the average HoursPerWeek, after adding a column to account for those who didn't report HoursPerWeek spent looking for a job
pub_scaled$HoursPerWeek[replace]=floor(median(pub_scaled$HoursPerWeek,na.rm = TRUE))  #1   
#this one is funnily distributed and has a lot of outliers. so i picked median instead of mean to replace it with 


pub_scaled$StackOverflowSatisfaction=NULL  #GETTING RID OF  YOU
pub_scaled$StackOverflowDevicesDesktop=NULL  #AND YOU
pub_scaled[grepl('StackOverflowDevices',names(pub_scaled))]=NULL  #AND ALL Y'ALL
#12426 OBS OF x VAR

pub_scaled[grepl('StackOverflowAds',names(pub_scaled))]=NULL  #and ya'll
#12426 OBS OF 1183 VAR







# --   Passing into Models --------------------------------------------------------------
#----------------------------------------------------------------------------------------

# -----  Trying cv.glmnet again




# m_cvglm=cv.glmnet(x=as.matrix(pub2_d),y=pub2_d$Salary,nfolds=5)


# sm_cvglm=cv.glmnet(x=as.matrix(pub_scaled),y=pub_scaled$Salary,nfolds=5)

# which(is.na(pub_scaled))
# nacols <- function(df) {
#   colnames(df)[unlist(lapply(df, function(x) any(is.na(x))))]
# }
# 
# nacols(pub_scaled)







coef(sm_cvglm,s='lambda.min')  #all predictive on salary, including it in calculations. no good. 
t_scaled=pub_scaled$Salary      #separates out Y
pub_scaled=select(pub_scaled,-Salary)        #removes predictor from df
grep(pattern = "Salary",x = colnames(pub_scaled)) #only one column

sm_cvglm=cv.glmnet(x=as.matrix(pub_scaled),y=t_scaled,nfolds=5)
sm_cvglm
coef(sm_cvglm,s='lambda.min')
cv_coefs=as.matrix(coef(sm_cvglm,s='lambda.min'))
cv_coefs=as.data.frame(cv_coefs)
colnames(cv_coefs)='moo'
cv_coefs=round(cv_coefs[order(-abs(cv_coefs$moo)), ,drop=FALSE],5)
grep(pattern = "Tabs",x = rownames(cv_coefs))  #60 869 870
cv_coefs=mutate(cv_coefs,"Var"=rownames(cv_coefs))
cv_coefs[60,] #0.02222 TabsSpacesSpaces
cv_coefs[869,] #0 TabsSpacesBoth
cv_coefs[870,] #0 TabsSpacesTabs


what=sort(abs(cv_coefs),decreasing = TRUE)
cv_coefs=order(abs(cv_coefs),decreasing = TRUE)
cv_coefs
grep(pattern = "intercept",x = rownames(cv_coefs))


#co-correlations
pubb=cbind(pub_scaled,t_scaled)

pub_cor=cor(pubb)
pub_cor=round(pub_cor,5)
pub_cor=as.data.frame(pub_cor)
items=cbind('Spaces'=pub_cor$TabsSpacesSpaces,'Tabs'=pub_cor$TabsSpacesTabs,'Both'=pub_cor$TabsSpacesBoth,'Salary'=pub_cor$t_scaled)
rownames(items)=rownames(pub_cor)
corrplot(items,details=FALSE, cex.col=8)
items=cbind(rownames(items),items)
rownames(items)=NULL





#trying gradient boosted trees

#defining hyperparameter grids

ntrees=500
shrink=seq(.01,.1,.03)
depth=c(1,5,10,20,40,60)
minobs=1:3


#per robert, no-do random forest since you didn't learn this



#random forest

 
grid=expand.grid(mtry= c(34, 394, 1182))


caret_reg = function(x, y, method, grid, ...) {  #x=an object where samples are in rows and features are in 
  # columns.  y=a numeric or factor vector containing the outcome for each sample.
  set.seed(1)
  control = trainControl(method="repeatedcv", 
                         repeats=1,
                         number=3, 
                         verboseIter=TRUE)
  train(x=x, 
        y=y, 
        method=method, 
        tuneGrid=grid,
        trControl=control, 
        metric="RMSE",
        preProcess=c("center", "scale"), ...)
}



rforest=caret_reg(x=pub_scaled, y=t_scaled, method='ranger', grid=grid,importance='impurity')



#Something is wrong; all the RMSE metric values are missing:
US_scaled=pub_scaled[pub_scaled$`CountryUnited States`>=pub_scaled$`CountryUnited States`[913],]  #create df with only US info
index=which(pub_scaled$`CountryUnited States`>=pub_scaled$`CountryUnited States`[913])  #get index of which results rows to include in regression
pub_scaled$`CountryUnited States` #check the index against the original info rows
t_us=t_scaled[index]  #minimize target to same length as df
colrem=grep(pattern = "Country",x = colnames(US_scaled))
colnames(US_scaled)[131]  #"CountryUnited States"
cn_rem=colnames(US_scaled)[colrem]
cn_rem[127]  #[1] "CountryUnited States"
cn_rem=cn_rem[-127]
US_scaled=US_scaled[,!(names(US_scaled) %in% cn_rem)]  #getting rid of all countries except US
#3675 obs of 1048 var


colrem=grep(pattern = "MobileDev",x = colnames(US_scaled))
colnames(US_scaled)[124]  #MobileDeveloperTypeiOS
cn_rem=colnames(US_scaled)[colrem]
US_scaled=US_scaled[,!(names(US_scaled) %in% cn_rem)]  #getting rid of mobile developer variable cuz it also caused problems
#3675obs of 1039 var

#also currency is problem variable
colrem=grep(pattern = "Currency",x = colnames(US_scaled))
colnames(US_scaled)[415]  #[1] "CurrencyBrazilian reais (R$)"
colnames(US_scaled)[colrem]
cn_rem=colnames(US_scaled)[colrem]  #[17] "CurrencyU.S. dollars ($)"
cn_rem[17]
cn_rem=cn_rem[-17]
US_scaled=US_scaled[,!(names(US_scaled) %in% cn_rem)]  #getting rid of extra currency variables cuz it also caused problems

#These variables have zero variances: CountryUnited States, YearsCodedJobPast1 to 2 years, YearsCodedJobPast11 to 12 years, YearsCodedJobPast6 to 7 years, YearsCodedJobPastLess than a year, YearsCodedJobPastNA, TimeAfterBootcampI haven't gotten a job as a developer yet, WorkStart2:00 AM, WorkStartMidnight, StackOverflowDescribesI've heard of Stack Overflow, but have never visited, JobProf_CW_Jobs, JobProf_JobSite.co.uk, JobProf_Naukri, JobProf_Pracuj, JobProf_Reed.co.uk, JobProf_Remix_jobs, JobProf_StepStone, JobProf_Talent.io, JobProf_Total_Jobs, JobProf_Workopolis, JobProf_Xing

#get rid of all of these
cn_rem=c('CountryUnited States', 'YearsCodedJobPast1 to 2 years', 'YearsCodedJobPast11 to 12 years', 'YearsCodedJobPast6 to 7 years', 'YearsCodedJobPastLess than a year', 'YearsCodedJobPastNA', 'TimeAfterBootcampI haven\'t gotten a job as a developer yet', 'WorkStart2:00 AM', 'WorkStartMidnight', 'StackOverflowDescribesI\'ve heard of Stack Overflow, but have never visited', 'JobProf_CW_Jobs', 'JobProf_JobSite.co.uk', 'JobProf_Naukri', 'JobProf_Pracuj', 'JobProf_Reed.co.uk', 'JobProf_Remix_jobs', 'JobProf_StepStone', 'JobProf_Talent.io', 'JobProf_Total_Jobs', 'JobProf_Workopolis', 'JobProf_Xing')
US_scaled=US_scaled[,!(names(US_scaled) %in% cn_rem)]  #3675 obs of 1001 var


#and these
# These variables have zero variances: YearsProgramNA, ChallengeMyselfStrongly disagree, WorkStart1:00 AM, WorkStart11:00 PM, WorkStart3:00 AM, Dev_Type_NA
cn_rem=c('YearsProgramNA', 'ChallengeMyselfStrongly disagree', 'WorkStart1:00 AM', 'WorkStart11:00 PM', 'WorkStart3:00 AM', 'Dev_Type_NA')
US_scaled=US_scaled[,!(names(US_scaled) %in% cn_rem)]  #3675 obs of 995 var


rforest=caret_reg(x=US_scaled, y=t_us, method='ranger', grid=grid,importance='impurity')

var(US_scaled$PronounceGIFNA)  #0.7516627




# 
# model=glm(Salary~., family = gaussian, data=pub_scaled)
# 
# model$coefficients
# 
# lm_coefs=model$coefficients
# lm_coefs=sort(abs(lm_coefs),decreasing = TRUE)
# lm_coefs=round(lm_coefs,5)
# options("max.print"=1500)
# 
# grep('TabsSpaces',names(lm_coefs))  #454  1033
# lm_coefs[454]  #TabsSpacesSpaces  0.01759
# lm_coefs[1033]  #TabsSpacesBoth  0.00121
# 
