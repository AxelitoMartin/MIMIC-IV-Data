library(tidyverse)
library(gtsummary)
library(lubridate)

rm(list=ls())
# get data #
path_files <- c("/Users/asm8744/Desktop/DML_Data/mimic-iv-1.0/")
# core data #
admin <- read.csv(paste0(path_files,"core/admissions.csv"))
patients <- read.csv(paste0(path_files,"core/patients.csv"))
transfers <- read.csv(paste0(path_files,"core/transfers.csv"))

# getting info for subsets #
# sepsis #
dict_diagnosis <- read.csv(paste0(path_files,"hosp/d_icd_diagnoses.csv"))
diagnosis <- read.csv(paste0(path_files,"hosp/diagnoses_icd.csv"))
# get all sepsis codes #
sepsis_codes <- unique(as.character(unlist(dict_diagnosis %>% 
                                             filter(grepl("sepsis", long_title)) %>% 
                                             select(icd_code))))
# get all patients that had sepsis at some point #
sepsis_patients <- diagnosis %>% 
  filter(icd_code %in% sepsis_codes)
sepsis_patients <- left_join(sepsis_patients, dict_diagnosis %>% select(icd_code, long_title), by = "icd_code")
# write.csv(sepsis_patients, file = paste0("../DML_Data/processed_data/","sepsis_patients.csv"))

sepsis_patients <- sepsis_patients %>% 
  select(subject_id, hadm_id, seq_num, long_title) %>% 
  rename(sepsis_detail = long_title) %>% 
  distinct()

# what to do with multiple assessement of sepsis? #
# sepsis_patients %>% 
#   group_by(subject_id, hadm_id) %>% 
#   tally() %>% 
#   arrange(-n)

###########################################
# add transfers (not sure how we want to deal with these patients) #

# sepsis_patients <- left_join(sepsis_patients,
#                              transfers %>% select(subject_id, transfer_id, eventtype),
#                              by = "subject_id")

###########################################
# get ventilation events #
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
# length(unique(merge_procedure_name$subject_id))
# 2519 patients received ventilation at some point #

sv_patients <- full_join(sepsis_patients,
                         merge_procedure_name,
                         by = c("subject_id", "hadm_id"))
# checks #
# dim(sv_patients)
# length(unique(as.character(unlist(sv_patients %>% filter(!is.na(ventilation_detail)) %>% select(subject_id)))))
# 2519 --> match #

# fix data, propose treatments, summarise #
sv_data <- sv_patients %>% 
  mutate(ventilation_detail = ifelse(is.na(ventilation_detail), "None", ventilation_detail))

# get number of patients with multiple entries #
sv_data %>% 
  group_by(subject_id) %>% 
  tally(name = "data_entries") %>% 
  ungroup() %>% 
  group_by(data_entries) %>% 
  tally(name = "num_patients")

sv_data %>% 
  group_by(subject_id) %>% 
  tally(name = "data_entries") %>% 
  ungroup() %>% arrange(-data_entries)

sv_data %>% filter(subject_id == "11281568")
procedure %>% filter(subject_id == "11281568")

sv_data %>% filter(subject_id == "19509298")
procedure %>% filter(subject_id == "19509298")

sv_data %>% 
  select(sepsis_detail, ventilation_detail) %>% 
  tbl_summary()

#################################################
### CONFOUNDERS ###

#################################################
### OUTCOMES ###
admin_sv <- admin %>% 
  filter(subject_id %in% sv_data$subject_id,
         hadm_id %in% sv_data$hadm_id) %>% 
  select(subject_id, hadm_id, admittime, dischtime, deathtime, insurance, ethnicity) %>% 
  mutate(
    status = ifelse(deathtime != "", 1, 0), 
    time = ifelse(status == 1,
                  as.numeric(as.Date(deathtime) - as.Date(admittime)),
                  as.numeric(as.Date(dischtime) - as.Date(admittime))),
  )








#################################################

temp <- procedure %>%
  filter(icd_code %in% vent_codes, subject_id %in% sepsis_patients$subject_id)

sv_patients <- left_join(sepsis_patients %>% 
                           rename(icd_code_sepsis = icd_code, icd_version_sepsis = icd_version), 
                         merge_procedure, 
                         by = c("subject_id", "hadm_id", "seq_num"))

sv_patients <- full_join(sepsis_patients %>% 
                           rename(icd_code_sepsis = icd_code, icd_version_sepsis = icd_version), 
                         merge_procedure, 
                         by = c("subject_id", "hadm_id", "seq_num"))

###########################################
# add demographics & time outcomes #
