library(tidyverse)
library(gtsummary)
library(lubridate)
library(mice)

rm(list=ls())
# get data #
# path_files <- c("/Users/asm8744/Desktop/DML_Data/mimic-iv-1.0/")
path_files <- c("/Users/axel/Desktop/NYU/Rotation2/DML/MIMIC_Data/mimic-iv-1.0/")
# core data #
admin <- read.csv(paste0(path_files,"core/admissions.csv"))
patients <- read.csv(paste0(path_files,"core/patients.csv"))
transfers <- read.csv(paste0(path_files,"core/transfers.csv"))

# getting info for subsets #
# sepsis #
dict_diagnosis <- read.csv(paste0(path_files,"hosp/d_icd_diagnoses.csv"))
diagnosis <- read.csv(paste0(path_files,"hosp/diagnoses_icd.csv"))

sepsis_codes <- unique(as.character(unlist(dict_diagnosis %>% 
                                             filter(grepl("sepsis", long_title)) %>% 
                                             select(icd_code))))
# remove newborn sepsis #
# Septicemia [sepsis] of newborn "77181"
# Bacterial sepsis of newborn, unspecified "P36", "P368","P369"
# Puerperal sepsis "O85","67020","67022","67024",

sepsis_codes <- sepsis_codes[!(sepsis_codes %in% c("O85","67020","67022","67024","P36", "P368","P369","77181"))]
sepsis_patients <- diagnosis %>% 
  filter(icd_code %in% sepsis_codes)
sepsis_patients <- left_join(sepsis_patients, dict_diagnosis %>% select(icd_code, long_title), by = "icd_code") %>% 
  select(-one_of("seq_num", "icd_version"))

# get the first time they had sepsis #

admin_sepsis <- admin %>% 
  filter(subject_id %in% sepsis_patients$subject_id,
         hadm_id %in% sepsis_patients$hadm_id) %>% 
  select(subject_id, hadm_id, admittime, dischtime, deathtime, insurance ,marital_status, ethnicity)

sa_patients <- left_join(sepsis_patients, 
                         admin_sepsis,
                         by = c("subject_id","hadm_id")) %>% 
  rowwise() %>% 
  mutate(
    year_adm = strsplit(admittime,"-")[[1]][1]#,
    # year_dish = strsplit(dischtime,"-")[[1]][1],
    # year_death = strsplit(deathtime,"-")[[1]][1],
    # anchor_year = year_adm
  ) 

# keep first sepsis event #
sav_patients_first_sepsis <- admin_sepsis %>% 
  group_by(subject_id) %>% 
  mutate(admin_time_stamp = min(admittime)
  ) %>% 
  ungroup() %>% 
  filter(admittime == admin_time_stamp
  ) %>% 
  # select(subject_id, hadm_id, admittime, dischtime, deathtime) %>% 
  distinct()

#### Adding ICU times ####
icustay <- read.csv(paste0(path_files,"icu/icustays.csv"))
sav_patients_first_d_icu <- left_join(
  sav_patients_first_sepsis,
  icustay %>% select(subject_id,hadm_id,stay_id, intime,outtime),
  by = c("subject_id", "hadm_id")
)

sav_patients_first_d <- sav_patients_first_d_icu %>% 
  group_by(subject_id) %>% 
  mutate(intime_stamp = min(intime)
  ) %>% 
  ungroup() %>% 
  filter(intime == intime_stamp
  ) %>% 
  distinct()

#### adding ICU ventilation times #####
t1 <- read.table(paste0(path_files,"icu/ventilation_icu_1.txt"), sep= ",")
t2 <- read.table(paste0(path_files,"icu/ventilation_icu_2.txt"), sep= ",")

icu_vent <- rbind(t1,t2)
colnames(icu_vent) <- c("subject_id","hadm_id","stay_id","charttime",
                        "storetime","itemid","value","valuenum","valueuom","warning")

icu_vent_sepsis <- icu_vent %>% 
  filter(subject_id %in% sav_patients_first_d$subject_id,
         hadm_id %in% sav_patients_first_d$hadm_id,
         itemid %in% c("223848" ,"223849")) %>% 
  select(subject_id, hadm_id, charttime,storetime)

# merge with data #
savdd_outcome_icu <- left_join(
  sav_patients_first_d, 
  icu_vent_sepsis,
  by = c("subject_id", "hadm_id")
) %>% 
  mutate(ventilation = ifelse(is.na(charttime),0,1))

# keep first time of ventilation for those who had it #
no_vent_patients <- savdd_outcome_icu %>% 
  filter(ventilation == 0)
vent_patients <- savdd_outcome_icu %>% 
  filter(ventilation == 1)


vent_patients_uniq <- vent_patients %>% 
  group_by(subject_id) %>% 
  mutate(vent_time_stamp = min(charttime)
  ) %>% 
  ungroup() %>% 
  filter(charttime == vent_time_stamp
  ) %>% 
  select(-one_of("vent_time_stamp")) %>% 
  distinct()

sav_patients_first <- rbind(no_vent_patients,vent_patients_uniq)

sav_patients_first_d <- sav_patients_first

# data for zeqi #
# zeqi_data <- sav_patients_first_d %>%
#   select(subject_id, hadm_id, ventilation, charttime, intime) %>%
#   mutate(time_stamp = ifelse(ventilation == 1, charttime, intime)) 
# write.csv(zeqi_data, "/Users/axel/Desktop/NYU/Rotation2/DML/MIMIC_Data/data/vzeqi_data.csv")


#### Adding Covariates ####
# time fixed confounders #
# sex/gender + age #
sav_patients_first_d_d <- left_join(sav_patients_first_d ,
                                    patients %>% select(subject_id, gender, anchor_age, anchor_year),
                                    by = "subject_id") %>% 
  rowwise()%>% 
  mutate(
    year_adm = strsplit(admittime, "-")[[1]][1],
    age = anchor_age + (as.numeric(year_adm) - as.numeric(anchor_year))
  )



# time varying #
# these are in the ed folder #

triage <- read.csv("/Users/axel/Desktop/NYU/Rotation2/DML/MIMIC_Data/mimic-iv-1.0/mimic-iv-ed-1.0/ed/triage.csv")
edstay <- read.csv("/Users/axel/Desktop/NYU/Rotation2/DML/MIMIC_Data/mimic-iv-1.0/mimic-iv-ed-1.0/ed/edstays.csv")


# get hadmid for triage #
triage_hadmid <- left_join(triage,
                           edstay %>% select(subject_id, hadm_id, stay_id,intime,outtime) %>% 
                             rename(intime_ed = intime, outtime_ed = outtime),
                           by = c("subject_id","stay_id"))

temp <- triage_hadmid %>% filter(subject_id %in% sav_patients_first_d_d$subject_id,
                                 hadm_id %in% sav_patients_first_d_d$hadm_id)

# sum(unique(triage$subject_id) %in% sav_patients_first_d_d$subject_id)
# sum(unique(temp$hadm_id) %in% sav_patients_first_d_d$hadm_id) # 4314
# 
# apply(temp, 2, function(x){sum(!is.na(x))})

# merge #
sav_patients_first_d_d_t <- left_join(
  sav_patients_first_d_d,
  temp %>% 
    select(subject_id, hadm_id, temperature,
           heartrate, resprate, o2sat, sbp, dbp, pain, acuity,intime_ed,outtime_ed),
  by = c("subject_id","hadm_id")
)

# get the closest obs for these before start time #
sav_patients_first_d_d_t_uniq <-
  sav_patients_first_d_d_t %>%
  group_by(subject_id) %>%
  mutate(time_diff = ifelse(
    ventilation == 1,
    difftime(charttime,intime_ed, units = 'hours'),
    difftime(intime,intime_ed, units = 'hours')
  ),
  ed_time_stamp = min(time_diff)
  ) %>%
  ungroup() 

sav_patients_first_d_d_t_uniq_1 <- sav_patients_first_d_d_t_uniq %>% 
  filter(is.na(ed_time_stamp))
sav_patients_first_d_d_t_uniq_2 <- sav_patients_first_d_d_t_uniq %>%
  filter(!is.na(ed_time_stamp),
         time_diff == ed_time_stamp)

sav_patients_first_d_d_t <- rbind(
  sav_patients_first_d_d_t_uniq_1,
  sav_patients_first_d_d_t_uniq_2
) %>% 
  select(-one_of("stay_id","storetime", "anchor_age","anchor_year","year_adm",
                 "intime_ed","outtime_ed","time_diff","ed_time_stamp")) %>% 
  distinct()

#### outcomes ####
savdd_outcome <- sav_patients_first_d_d_t %>% 
  rowwise() %>% 
  mutate(
    time = as.numeric(ifelse(ventilation == 1 & deathtime != "", 
                             difftime(deathtime,charttime, units = 'hours' ),
                             ifelse(deathtime != "",
                                    difftime(deathtime,intime, units = 'hours' ),
                                    difftime(outtime,intime, units = 'hours' )
                             )
    )),
    death_40H = ifelse(time <= 40, 1, 0),
    death_60H = ifelse(time <= 60, 1, 0),
    death_80H = ifelse(time <= 80, 1, 0),
    death_100H = ifelse(time <= 100, 1, 0),
    death_120H = ifelse(time <= 120, 1, 0)
  )



#######
#### summary ####

savdd_outcome %>% 
  mutate(
    ventilation = ifelse(ventilation == 1, "yes","no")
  ) %>% 
  select(ventilation,temperature,
         heartrate, resprate, o2sat, sbp, dbp, pain, acuity) %>% 
  tbl_summary(by = ventilation)


savdd_outcome %>% 
  mutate(
    ventilation = ifelse(ventilation == 1, "yes","no")
  ) %>% 
  select(ventilation, age, gender, insurance, 
         ethnicity, death_40H, death_60H, death_80H,death_100H, death_120H,temperature,
         heartrate, resprate, o2sat, sbp, dbp, pain, acuity) %>% 
  tbl_summary(by = ventilation)

### fix data to numeric ###
final <- savdd_outcome %>% 
  select(subject_id,hadm_id,ventilation,age, gender, insurance, ethnicity, temperature,
         heartrate, resprate, o2sat, sbp, dbp, pain, acuity,
         death_40H, death_60H, death_80H,death_100H, death_120H) %>% 
  mutate(
    # gender #
    gender = ifelse(gender == "F", 0, 1),
    # insurance #
    medicaid = ifelse(insurance == "Medicaid",1,0),
    medicare = ifelse(insurance == "Medicare",1,0),
    # martial status #
    # divorced = ifelse(marital_status == "DIVORCED", 1, 0),
    # married = ifelse(marital_status == "MARRIED", 1, 0),
    # single = ifelse(marital_status == "SINGLE", 1, 0),
    # widowed = ifelse(marital_status == "WIDOWED", 1, 0),
    # ethinicty #
    native = ifelse(ethnicity == "AMERICAN INDIAN/ALASKA NATIVE", 1, 0),
    asian = ifelse(ethnicity == "ASIAN", 1, 0),
    afro_american = ifelse(ethnicity == "BLACK/AFRICAN AMERICAN", 1, 0),
    hispanic = ifelse(ethnicity == "HISPANIC/LATINO", 1, 0),
    other = ifelse(ethnicity == "OTHER", 1, 0),
    white = ifelse(ethnicity == "WHITE", 1, 0)
  ) %>% 
  select(-one_of(c("insurance",#"marital_status",
                   "ethnicity")))

final %>% 
  tbl_summary(by = ventilation)

write.csv(final, "/Users/axel/Desktop/NYU/Rotation2/DML/MIMIC_Data/data/analysis_set.csv")

### Impute missing data ###
md.pattern(final)
tempData <- mice(final[,-c(1,2)],m=5,maxit=50,seed=500)
completedData <- complete(tempData,1)

final_imp <- cbind(final[, 1:2],completedData)
anyNA(final_imp)

final_imp %>% 
  tbl_summary(by = ventilation)
write.csv(final_imp, "/Users/axel/Desktop/NYU/Rotation2/DML/MIMIC_Data/data/imp_analysis_set.csv")
dim(final[complete.cases(final),])


##################################################



##################################################



##################################################
# get ventilation #
dict_procedure <- read.csv(paste0(path_files, "hosp/d_icd_procedures.csv"))
procedure <- read.csv(paste0(path_files, "hosp/procedures_icd.csv"))

vent_codes <- unique(as.character(unlist(dict_procedure %>% 
                                           filter(grepl("ventilation", long_title)) %>% 
                                           select(icd_code))))
# merge ventilation events with sepsis patients #
merge_procedure <- procedure %>% 
  filter(icd_code %in% vent_codes,
         subject_id %in% sepsis_patients$subject_id,
         hadm_id %in% sepsis_patients$hadm_id) %>% 
  select(subject_id, hadm_id, icd_code,chartdate)


merge_procedure_name <- left_join(merge_procedure, dict_procedure %>% select(icd_code, long_title), by = "icd_code") %>% 
  select(-one_of("icd_code")) %>% rename(ventilation_detail = long_title)


sav_patients <- left_join(sa_patients,
                          merge_procedure_name,
                          by = c("subject_id", "hadm_id"))
# add age #
sav_patients <- left_join(sav_patients, 
                          patients %>% select(subject_id, anchor_age, anchor_year),
                          by = "subject_id"
) %>% 
  mutate(
    age = anchor_age + (as.numeric(year_adm) - as.numeric(anchor_year))
  )



# sav_patients <- left_join(sav_patients %>% 
#             mutate(anchor_year = as.numeric(anchor_year)), 
#           patients, 
#           by = c("subject_id", "anchor_year") ) 

# keep first sepsis event #
sav_patients_first_sepsis <- sav_patients %>% 
  mutate(ventilation = ifelse(is.na(ventilation_detail),0, 1)) %>% 
  group_by(subject_id) %>% 
  mutate(admin_time_stamp = min(admittime)#,
         # vent_time_stamp = min(chartdate)
  ) %>% 
  ungroup() %>% 
  filter(admittime == admin_time_stamp#,
         # chartdate == vent_time_stamp
  ) %>% 
  select(subject_id, hadm_id, ventilation, chartdate, admittime, dischtime, deathtime,age) %>% 
  distinct()

## still duplicates from ventilation ##
sav_patients_novent <- sav_patients_first_sepsis %>% filter(ventilation == 0)
sav_patients_vent <- sav_patients_first_sepsis %>% filter(ventilation == 1)

sav_patients_vent_uniq <- sav_patients_vent %>% 
  group_by(subject_id) %>% 
  mutate(vent_time_stamp = min(chartdate)
  ) %>% 
  ungroup() %>% 
  filter(chartdate == vent_time_stamp
  ) %>% 
  select(subject_id, hadm_id, ventilation, chartdate, admittime, dischtime, deathtime,age) %>% 
  distinct()

sav_patients_first <- rbind(sav_patients_novent,sav_patients_vent_uniq)

sav_patients_first_d <- sav_patients_first #%>% 
# rowwise() %>% 
# mutate(
#   chartdate = ifelse(!is.na(chartdate),paste(chartdate, "12:00:00"), chartdate),
#   time = ifelse(ventilation == 1 & deathtime != "", 
#                 difftime(deathtime,chartdate, units = 'hours' ),
#                 NA
#   ),
#   death_40H = ifelse(is.na(time), 0, 
#                      ifelse(time <= 40, 1, 0)
#   ),
#   death_60H = ifelse(is.na(time), 0, 
#                      ifelse(time <= 60, 1, 0)
#   ),
#   death_80H = ifelse(is.na(time), 0, 
#                      ifelse(time <= 80, 1, 0)
#   )
# )

### add icu entry ###
icustay <- read.csv(paste0(path_files,"icu/icustays.csv"))
sav_patients_first_d_icu <- left_join(
  sav_patients_first_d,
  icustay %>% select(subject_id,hadm_id, intime,outtime),
  by = c("subject_id", "hadm_id")
)

sav_patients_first_d <- sav_patients_first_d_icu %>% 
  group_by(subject_id) %>% 
  mutate(intime_stamp = min(intime)#,
         # vent_time_stamp = min(chartdate)
  ) %>% 
  ungroup() %>% 
  filter(intime == intime_stamp
  ) %>% 
  distinct()

#### add demographics #####
# sex/gender #
sav_patients_first_d_d <- left_join(sav_patients_first_d ,
                                    patients %>% select(subject_id, gender),
                                    by = "subject_id")

# others #
sav_patients_first_d_d <- left_join(sav_patients_first_d_d, 
                                    admin %>% 
                                      select(subject_id,hadm_id,insurance, language, marital_status, ethnicity),
                                    by = c("subject_id", "hadm_id"))

# write.csv(sav_patients_first_d_d, "/Users/axel/Desktop/NYU/Rotation2/DML/MIMIC_Data/data/vent_time.csv")
# zeqi_data <- sav_patients_first_d_d %>% 
#   select(subject_id, hadm_id, ventilation, chartdate, intime) %>% 
#   mutate(time_stamp = ifelse(ventilation == 1, chartdate, intime))
# write.csv(zeqi_data, "/Users/axel/Desktop/NYU/Rotation2/DML/MIMIC_Data/data/vzeqi_data.csv")

# recreate outcome #

savdd_outcome <- sav_patients_first_d_d %>% 
  rowwise() %>% 
  mutate(
    time = as.numeric(ifelse(ventilation == 1 & deathtime != "", 
                             difftime(deathtime,chartdate, units = 'hours' ),
                             ifelse(deathtime != "",
                                    difftime(deathtime,intime, units = 'hours' ),
                                    difftime(outtime,intime, units = 'hours' )
                             )
    )),
    death_40H = ifelse(time <= 40, 1, 0),
    death_60H = ifelse(time <= 60, 1, 0),
    death_80H = ifelse(time <= 80, 1, 0),
    death_100H = ifelse(time <= 100, 1, 0),
    death_120H = ifelse(time <= 120, 1, 0)
  )

timedifference <- difftime(savdd_outcome$intime,savdd_outcome$chartdate , units = 'hours' )
timedifference <- as.numeric(timedifference[!is.na(timedifference)])
hist(timedifference)
summary(timedifference)
quantile(timedifference, c(0.01, 0.05, 0.10, 0.25,0.5, 0.75, 0.9, 0.95, 0.99))

#### adding ICU ventilation times #####
t1 <- read.table(paste0(path_files,"icu/ventilation_icu_1.txt"), sep= ",")
t2 <- read.table(paste0(path_files,"icu/ventilation_icu_2.txt"), sep= ",")

icu_vent <- rbind(t1,t2)
colnames(icu_vent) <- c("subject_id","hadm_id","stay_id","charttime",
                        "storetime","itemid","value","valuenum","valueuom","warning")

icu_vent_sepsis <- icu_vent %>% 
  filter(subject_id %in% savdd_outcome$subject_id,
         hadm_id %in% savdd_outcome$hadm_id,
         itemid %in% c("223848" ,"223849")) %>% 
  select(subject_id, hadm_id, charttime,storetime)

# merge with data #
savdd_outcome_icu <- left_join(
  savdd_outcome, 
  icu_vent_sepsis,
  by = c("subject_id", "hadm_id")
)

temp_dat <- savdd_outcome_icu %>% 
  mutate(temp_time = as.numeric(difftime(intime,charttime , units = 'hours' ))) %>% 
  filter(temp_time > 0)








d_items <- read.csv(paste0(path_files,"icu/d_items.csv"))
vent_codes_icu <- unique(as.character(unlist(d_items %>% 
                                           filter(grepl("Ventilator", label)) %>% 
                                           select(itemid))))
vent_codes_icu <- c("223848" ,"223849")
icu_procedures <- read.csv(paste0(path_files,"icu/procedureevents.csv"))
icu_chartevents <- read.csv(paste0(path_files,"icu/chartevents.csv"))


icu_procedures %>% 
  filter(subject_id %in% savdd_outcome$subject_id,
         itemid %in% vent_codes_icu)

### summary ###

sav_patients_first_d_d %>% 
  mutate(
    ventilation = ifelse(ventilation == 1, "yes","no")
  ) %>% 
  select(ventilation, age, gender, insurance, language, marital_status, 
         ethnicity, death_40H, death_60H, death_80H) %>% 
  tbl_summary(by = ventilation)

### fix data to numeric ###
final <- sav_patients_first_d_d %>% 
  select(ventilation,age, gender, insurance, language, marital_status, ethnicity, death_40H, death_60H, death_80H) %>% 
  mutate(
    # gender #
    gender = ifelse(gender == "F", 0, 1),
    # insurance #
    medicaid = ifelse(insurance == "Medicaid",1,0),
    medicare = ifelse(insurance == "Medicare",1,0),
    # language #
    language = ifelse(language == "ENGLISH",1,0),
    # martial status #
    divorced = ifelse(marital_status == "DIVORCED", 1, 0),
    married = ifelse(marital_status == "MARRIED", 1, 0),
    single = ifelse(marital_status == "SINGLE", 1, 0),
    widowed = ifelse(marital_status == "WIDOWED", 1, 0),
    # ethinicty #
    native = ifelse(ethnicity == "AMERICAN INDIAN/ALASKA NATIVE", 1, 0),
    asian = ifelse(ethnicity == "ASIAN", 1, 0),
    afro_american = ifelse(ethnicity == "BLACK/AFRICAN AMERICAN", 1, 0),
    hispanic = ifelse(ethnicity == "HISPANIC/LATINO", 1, 0),
    other = ifelse(ethnicity == "OTHER", 1, 0),
    white = ifelse(ethnicity == "WHITE", 1, 0)
  ) %>% 
  select(-one_of(c("insurance","marital_status", "ethnicity")))

final %>% 
  tbl_summary(by = ventilation)

write.csv(final, "/Users/axel/Desktop/NYU/Rotation2/DML/MIMIC_Data/data/analysis_set.csv")
###################################


















##################################


sav_patients_first_d <- sav_patients_first %>% 
  rowwise() %>% 
  mutate(
    time = ifelse(ventilation == 1 & deathtime != "", 
                  difftime(deathtimechartdate, units = 'hours' ),
                  NA
    ),
    death_40H = ifelse(is.na(time), 0, 
                       ifelse(time <= 40, 1, 0)
    ),
    death_60H = ifelse(is.na(time), 0, 
                       ifelse(time <= 60, 1, 0)
    ),
    death_80H = ifelse(is.na(time), 0, 
                       ifelse(time <= 80, 1, 0)
    )
  )


difftime(sav_patients_first$dischtime[1],sav_patients_first$admittime[1], units = 'hours' )

difftime("2128-09-08 15:13:00","2128-09-08 11:13:00", units = 'hours' )


sav_patients_first$dischtime[1]

as_date(sav_patients_first$dischtime[1]) - 
  as_date(sav_patients_first$admittime[1] )

dups <- sav_patients_first$subject_id[duplicated(sav_patients_first$subject_id)]

########################################################



dups <- sav_patients_first_sepsis$subject_id[duplicated(sav_patients_first_sepsis$subject_id)]

sav_patients_first_sepsis %>% filter(subject_id == "11281568") %>% 
  select(subject_id, admittime, ventilation_detail, chartdate)
sav_patients %>% filter(subject_id == "11281568") %>% 
  select(subject_id, admittime, ventilation_detail, chartdate)

sav_patients_first_sepsis %>% filter(subject_id == "19509298") %>% 
  select(subject_id, admittime, ventilation_detail, chartdate)
sav_patients %>% filter(subject_id == "19509298") %>% 
  select(subject_id, admittime, ventilation_detail, chartdate)

filter(admittime == time_stamp) %>% 
  ungroup()

# subset to their first sepsis event? #
# subset to their first ventilation event? #




#########################################

# get age of patients #
saa_patients <- left_join(sa_patients,
                          patients,
                          by = "subject_id")

# ventilation data #
dict_procedure <- read.csv(paste0(path_files, "hosp/d_icd_procedures.csv"))
procedure <- read.csv(paste0(path_files, "hosp/procedures_icd.csv"))

vent_codes <- unique(as.character(unlist(dict_procedure %>% 
                                           filter(grepl("ventilation", long_title)) %>% 
                                           select(icd_code))))
# merge ventilation events with sepsis patients #
merge_procedure <- procedure %>% 
  filter(icd_code %in% vent_codes,
         subject_id %in% sepsis_patients$subject_id,
         hadm_id %in% sepsis_patients$hadm_id) %>% 
  select(subject_id, hadm_id, icd_code)


merge_procedure_name <- left_join(merge_procedure, dict_procedure %>% select(icd_code, long_title), by = "icd_code") %>% 
  select(-one_of("icd_code")) %>% rename(ventilation_detail = long_title)


saav_patients <- left_join(saa_patients,
                           merge_procedure_name,
                           by = c("subject_id", "hadm_id"))
