options(tidyverse.quiet = TRUE)
library(tidyverse)
library(stringr)

# Fixed Patient Attributes

## Subject Characteristics
socio_econ <- read_csv("raw-data/_Subject_Characteristics/Socio-Economics.csv",
                       col_types = cols(
                         REC_ID = col_integer(),
                         F_STATUS = col_character(),
                         PATNO = col_integer(),
                         EVENT_ID = col_character(),
                         PAG_NAME = col_character(),
                         INFODT = col_character(),
                         EDUCYRS = col_integer(),
                         HANDED = col_integer(),
                         ORIG_ENTRY = col_character(),
                         LAST_UPDATE = col_datetime(format = ""),
                         QUERY = col_character(),
                         SITE_APRV = col_character()
                       )) %>% 
  select(PATNO, Education=EDUCYRS, Handed=HANDED) %>% 
  mutate(Handed = factor(Handed,levels=1:3,labels=c("Right","Left","Mixed")))

determine_race <- function(df) {
  determine_row <- function(lt) {
    lt<-as.list(lt)
    if (sum(unlist(lt),na.rm=TRUE)>1) {"Other"}
    else if (!is.na(lt$RAINDALS) & lt$RAINDALS) {"Other"}
    else if (!is.na(lt$RAHAWOPI) & lt$RAHAWOPI) {"Other"}
    else if (!is.na(lt$RANOS) & lt$RANOS) {"Other"}
    else if (!is.na(lt$RAASIAN) & lt$RAASIAN) { "Asian" }
    else if (!is.na(lt$RABLACK) & lt$RABLACK) { "Black" }
    else if (!is.na(lt$RAWHITE) & lt$RAWHITE) { "White" }
    else NA
  }
  apply(df,1,determine_row) %>% unlist
}



screening <- read_csv("raw-data/_Subject_Characteristics/Screening___Demographics.csv",
                      col_types = cols(
                        .default = col_integer(),
                        F_STATUS = col_character(),
                        EVENT_ID = col_character(),
                        PAG_NAME = col_character(),
                        CONSNTDT = col_character(),
                        PRJENRDT = col_character(),
                        REFERRAL = col_character(),
                        RSNDEC = col_character(),
                        RSNEXC = col_character(),
                        ORIG_ENTRY = col_character(),
                        LAST_UPDATE = col_datetime(format = ""),
                        QUERY = col_character(),
                        SITE_APRV = col_character()
                      )) %>% 
  select(-REC_ID,-F_STATUS,-EVENT_ID,-PAG_NAME,-SIGNCNST,-REFERRAL,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY, -SITE_APRV,-PRJENRDT,-BIRTHDT) %>% 
  mutate(Sex = factor(replace(GENDER,GENDER==1,0),levels=c(0,2),c("Female","Male"))) %>% 
  mutate(GENDER = factor(GENDER,levels=0:2,c("Female of child bearing potential",
                                    "Female of non-child bearing potential","Male"))) %>% 
  mutate(APPRDX = factor(APPRDX),CURRENT_APPRDX=factor(CURRENT_APPRDX)) %>% 
  mutate_at(vars(HISPLAT:RAWHITE), function(x) {replace(x,x==2,NA)}) %>% 
  mutate(Race = determine_race(select_(.,"RAINDALS:RANOS")),
         Race2 = case_when(
           pmap_int(list(RAINDALS,RAHAWOPI,RANOS,RAASIAN,RABLACK,RAWHITE),sum,na.rm=TRUE) > 1 ~ "Other",
           RAINDALS==1 | RAHAWOPI==1 | RANOS==1 ~ "Other",
           RAASIAN==1 ~ "Asian",
           RABLACK==1 ~ "Black",
           RAWHITE==1 ~ "White",
           TRUE ~ NA_character_
         )
  )


center_subject <- read_csv("raw-data/_Subject_Characteristics/Center-Subject_List.csv",col_types = 
                             cols(
                               CNO = col_character(),
                               PATNO = col_integer()
                             )) %>% 
  rename(Center=CNO)

patient_status <- read_csv("raw-data/_Subject_Characteristics/Patient_Status.csv",col_types=cols(
  PATNO = col_integer(),
  RECRUITMENT_CAT = col_character(),
  IMAGING_CAT = col_character(),
  ENROLL_DATE = col_character(),
  ENROLL_CAT = col_character(),
  ENROLL_STATUS = col_character(),
  DESCRP_CAT = col_character(),
  STATUS_DATE = col_character()
)) %>% 
  select(-STATUS_DATE)

family_history <- read_csv("raw-data/_Subject_Characteristics/Family_History__PD_.csv", col_types=
                             cols(
                               .default = col_integer(),
                               F_STATUS = col_character(),
                               EVENT_ID = col_character(),
                               PAG_NAME = col_character(),
                               INFODT = col_character(),
                               ORIG_ENTRY = col_character(),
                               LAST_UPDATE = col_datetime(format = ""),
                               QUERY = col_character(),
                               SITE_APRV = col_character()
                             )) %>% 
  group_by(PATNO) %>% 
  slice(n()) %>% # Use latest measurement
  select(-REC_ID,-F_STATUS,-PAG_NAME,-INFODT,-ORIG_ENTRY, -LAST_UPDATE, -QUERY, -SITE_APRV) %>% 
  mutate(`Family history` = (BIOMOMPD | BIODADPD | FULSIBPD | HAFSIBPD | MAGPARPD | PAGPARPD | MATAUPD | PATAUPD | KIDSPD)) %>% 
  select(PATNO,`Family history`)

## Genetics
biospec_cat <- read_csv("raw-data/Biospecimen/Current_Biospecimen_Analysis_Results.csv", col_types=
                          cols(
                            PATNO = col_integer(),
                            GENDER = col_character(),
                            DIAGNOSIS = col_character(),
                            CLINICAL_EVENT = col_character(),
                            TYPE = col_character(),
                            TESTNAME = col_character(),
                            TESTVALUE = col_character(),
                            UNITS = col_character(),
                            RUNDATE = col_date(format = ""),
                            PROJECTID = col_integer(),
                            PI_NAME = col_character(),
                            PI_INSTITUTION = col_character(),
                            update_stamp = col_datetime(format = "")
                          )) %>% 
  filter(is.na(UNITS)) %>% 
  mutate(TESTNAME=replace(TESTNAME,TESTNAME==toupper("ApoE Genotype"),"ApoE Genotype")) %>% 
  select(PATNO,TESTNAME,TESTVALUE,RUNDATE) %>%
  arrange(PATNO,TESTNAME,desc(RUNDATE)) %>% #Some measurements have been repeated 
  distinct(PATNO,TESTNAME,.keep_all=TRUE) %>%
  select(-RUNDATE) %>% 
  spread(TESTNAME,TESTVALUE) %>% 
  mutate(`ApoE e4 status`=stringr::str_count(`ApoE Genotype`,"e4") %>% 
           factor(levels=0:2,labels=c("No","Heterozygous","Homozygous")))

## PD Features
# TODO: what to do with later events than BL or SC (is this revised information?)
pd_features <- read_csv("raw-data/Medical_History/PD_Features.csv",
                        col_types=cols(
                          .default = col_character(),
                          REC_ID = col_integer(),
                          PATNO = col_integer(),
                          SXMO = col_integer(),
                          SXYEAR = col_integer(),
                          LAST_UPDATE = col_datetime(format = "")
                        )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  filter(EVENT_ID %in% c("BL","SC")) %>% 
  select(-EVENT_ID)



random <- read_csv("raw-data/Study_Enrollment/Randomization_table.csv",col_types=cols(
  REC_ID = col_integer(),
  F_STATUS = col_character(),
  PATNO = col_integer(),
  EVENT_ID = col_character(),
  PAG_NAME = col_character(),
  INFODT = col_character(),
  ENROLLDT = col_character(),
  BIRTHDT = col_character(),
  GENDER = col_integer(),
  CONSNTDT = col_character(),
  ORIG_ENTRY = col_character(),
  LAST_UPDATE = col_datetime(format = ""),
  QUERY = col_character(),
  SITE_APRV = col_character()
)) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  select(PATNO,BIRTHDT)


#read_csv("raw-data/Medical_History/Clinical_Diagnosis_and_Management.csv")
#read_csv("raw-data/Medical_History/Diagnostic_Features.csv") %>% count(DFSTROKE)

## Combine
subject_characteristics <- 
  Reduce(function(x,y) {full_join(x,y,by="PATNO")},
       list(center_subject,patient_status,socio_econ,screening,
            family_history,biospec_cat,pd_features,random))

## Derived
convert_monthyear_num <- function(x) {
  matches <- str_match(x,"([0-9]*)/([0-9]*)")
  as.numeric(matches[,3]) + as.numeric(matches[,2])/12
}

determine_group <- function(apprx,enrolldt) {
  new <- as.integer(apprx) *!is.na(enrolldt)
  new <- replace(new,new==5 | new==6, 5)
  new <- replace(new,new==7 | new==8, 6)
  factor(new,levels=0:6,labels=c(NA,"PD","Healthy Control","SWEDD","Prodromal","Genetic Cohort","Genetic Subject"))
}

subject_characteristics <- subject_characteristics %>% 
  mutate(Age = convert_monthyear_num(ENROLL_DATE) - convert_monthyear_num(BIRTHDT)) %>%
  mutate(AgeYear = floor(convert_monthyear_num(ENROLL_DATE)) - floor(convert_monthyear_num(BIRTHDT))) %>% 
  mutate(`Disease duration` = convert_monthyear_num(ENROLL_DATE)-
                              convert_monthyear_num(PDDXDT)) %>% 
  mutate(Group = determine_group(APPRDX,ENROLL_DATE))
  
# Repeated Measurements

## Motor
### MDS_UPDRS
mds_1 <- read_csv("raw-data/Motor___MDS-UPDRS/MDS_UPDRS_Part_I.csv",col_types=cols(
  REC_ID = col_integer(),
  F_STATUS = col_character(),
  PATNO = col_integer(),
  EVENT_ID = col_character(),
  PAG_NAME = col_character(),
  INFODT = col_character(),
  NUPSOURC = col_integer(),
  NP1COG = col_integer(),
  NP1HALL = col_integer(),
  NP1DPRS = col_integer(),
  NP1ANXS = col_integer(),
  NP1APAT = col_integer(),
  NP1DDS = col_integer(),
  ORIG_ENTRY = col_character(),
  LAST_UPDATE = col_datetime(format = ""),
  QUERY = col_character(),
  SITE_APRV = col_character()
)) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV,-NUPSOURC)

mds_1 <- read_csv("raw-data/Motor___MDS-UPDRS/MDS_UPDRS_Part_I__Patient_Questionnaire.csv",col_types=
                    cols(
                      REC_ID = col_integer(),
                      F_STATUS = col_character(),
                      PATNO = col_integer(),
                      EVENT_ID = col_character(),
                      PAG_NAME = col_character(),
                      INFODT = col_character(),
                      NUPSOURC = col_integer(),
                      NP1SLPN = col_integer(),
                      NP1SLPD = col_integer(),
                      NP1PAIN = col_integer(),
                      NP1URIN = col_integer(),
                      NP1CNST = col_integer(),
                      NP1LTHD = col_integer(),
                      NP1FATG = col_integer(),
                      ORIG_ENTRY = col_character(),
                      LAST_UPDATE = col_datetime(format = ""),
                      QUERY = col_character(),
                      SITE_APRV = col_character()
                    )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  left_join(mds_1, by=c("PATNO", "EVENT_ID")) %>% 
  mutate(MDS_UPDRS_I = NP1COG + NP1HALL + NP1DPRS + NP1ANXS + NP1APAT + NP1DDS + NP1SLPN + NP1SLPD + NP1PAIN + NP1URIN + NP1CNST + NP1LTHD + NP1FATG)
  
mds_2 <- read_csv("raw-data/Motor___MDS-UPDRS/MDS_UPDRS_Part_II__Patient_Questionnaire.csv", col_types=
                    cols(
                      .default = col_integer(),
                      F_STATUS = col_character(),
                      EVENT_ID = col_character(),
                      PAG_NAME = col_character(),
                      INFODT = col_character(),
                      ORIG_ENTRY = col_character(),
                      LAST_UPDATE = col_datetime(format = ""),
                      QUERY = col_character(),
                      SITE_APRV = col_character()
                    )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  mutate(MDS_UPDRS_II = NP2SPCH + NP2SALV + NP2SWAL + NP2EAT + NP2DRES + NP2HYGN + NP2HWRT + NP2HOBB + NP2TURN + NP2TRMR + NP2RISE + NP2WALK + NP2FREZ)

mds_3 <- read_csv("raw-data/Motor___MDS-UPDRS/MDS_UPDRS_Part_III.csv",col_types=
                    cols(
                      .default = col_integer(),
                      F_STATUS = col_character(),
                      EVENT_ID = col_character(),
                      PAG_NAME = col_character(),
                      INFODT = col_character(),
                      CMEDTM = col_time(format = ""),
                      EXAMTM = col_time(format = ""),
                      ANNUAL_TIME_BTW_DOSE_NUPDRS = col_double(),
                      ORIG_ENTRY = col_character(),
                      LAST_UPDATE = col_datetime(format = ""),
                      QUERY = col_character(),
                      SITE_APRV = col_character()
                    )) %>% 
  #select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
#         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  mutate(MDS_UPDRS_III = NP3SPCH + NP3FACXP + NP3RIGN + NP3RIGRU + NP3RIGLU + PN3RIGRL + 
           NP3RIGLL + NP3FTAPR + NP3FTAPL + NP3HMOVR + NP3HMOVL + NP3PRSPR + NP3PRSPL + 
           NP3TTAPR + NP3TTAPL + NP3LGAGR + NP3LGAGL + NP3RISNG + NP3GAIT + NP3FRZGT + 
           NP3PSTBL + NP3POSTR + NP3BRADY + NP3PTRMR + NP3PTRML + NP3KTRMR + NP3KTRML + 
           NP3RTARU + NP3RTALU + NP3RTARL + NP3RTALL + NP3RTALJ + NP3RTCON)

mds_4 <- read_csv("raw-data/Motor___MDS-UPDRS/MDS_UPDRS_Part_IV.csv",col_types=
                    cols(
                      REC_ID = col_integer(),
                      F_STATUS = col_character(),
                      PATNO = col_integer(),
                      EVENT_ID = col_character(),
                      PAG_NAME = col_character(),
                      INFODT = col_character(),
                      NP4WDYSK = col_integer(),
                      NP4DYSKI = col_integer(),
                      NP4OFF = col_integer(),
                      NP4FLCTI = col_integer(),
                      NP4FLCTX = col_integer(),
                      NP4DYSTN = col_integer(),
                      ORIG_ENTRY = col_character(),
                      LAST_UPDATE = col_datetime(format = ""),
                      QUERY = col_character(),
                      SITE_APRV = col_character()
                    )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  mutate(MDS_UPDRS_IV = NP4WDYSK + NP4DYSKI + NP4OFF + NP4FLCTI + NP4FLCTX + NP4DYSTN)

determine_subtype <- function(tremor,pigd) {
  ratio <- tremor/pigd
  if (is.na(tremor) | is.na(pigd)) NA
  else if (pigd==0 & tremor==0) "Indeterminate"
  else if ((pigd==0 & tremor>0) | ratio>1.15) "TD"
  else if (ratio<=0.9) "PIGD"
  else if (is.na(ratio)) NA
  else "Indeterminate"
}



# Check calculation of subtype
mds_measurements <- Reduce(function(x,y) {left_join(x,y,by=c("PATNO","EVENT_ID"))},
                    list(mds_1,mds_2,mds_3,mds_4)) %>% 
  filter(PATNO!=54854) %>% 
  mutate(MDS_TOTAL = MDS_UPDRS_I + MDS_UPDRS_II + MDS_UPDRS_III) %>% 
  mutate(Tremorscore = (NP2TRMR + NP3PTRMR + NP3PTRML + NP3KTRMR + NP3KTRML + 
                          NP3RTARU + NP3RTALU + NP3RTARL + NP3RTALL + NP3RTALJ + NP3RTCON)/11) %>% 
  mutate(PIGDscore = (NP2WALK + NP2FREZ + NP3GAIT + NP3FRZGT + NP3PSTBL)/5) %>% 
  mutate(Subtype = map2_chr(Tremorscore,PIGDscore,determine_subtype)) %>% 
  mutate(ratio = Tremorscore/PIGDscore) %>%
  mutate(Subtype2 = case_when(
    PIGDscore==0 & Tremorscore==0 ~ "Indeterminate",
    (PIGDscore==0 & Tremorscore>0) | ratio>1.15 ~ "TD",
    ratio<=0.9 ~ "PIGD",
    (is.na(Tremorscore) | is.na(PIGDscore)) ~ NA_character_,
    is.na(ratio) ~ NA_character_,
    TRUE ~ "Indeterminate"
  )) %>% 
  mutate(Month = case_when(
    EVENT_ID=="SC" ~ -1,
    EVENT_ID=="BL" ~ 0,
    EVENT_ID=="V01" ~ 3,
    EVENT_ID=="V02" ~ 6,
    EVENT_ID=="V03" ~ 9,
    EVENT_ID=="V04" ~ 12,
    EVENT_ID=="V05" ~ 18,
    EVENT_ID=="V06" ~ 24,
    EVENT_ID=="V07" ~ 30,
    EVENT_ID=="V08" ~ 36,
    EVENT_ID=="V09" ~ 42,
    EVENT_ID=="V10" ~ 48,
    EVENT_ID=="V11" ~ 54,
    EVENT_ID=="V12" ~ 60,
    TRUE ~ NA_real_
  )) %>% 
  mutate(PD_MED_USE=factor(PD_MED_USE,levels=0:7,
                           labels=c("None","Lv","Ag","Other","Lv+Other","Lv+Ag","Ag+Other","Lv+Ag+Other"))) %>%
  mutate(ON_OFF_DOSE=factor(ON_OFF_DOSE,levels=1:2,labels=c(">=6hr","<6hr"))) %>% 
  select(PATNO,EVENT_ID,starts_with("NP1"),starts_with("NP2"),starts_with("NP3"), MDS_TOTAL, MDS_UPDRS_I, 
         MDS_UPDRS_II, MDS_UPDRS_III, MDS_UPDRS_IV, Subtype, 
         ANNUAL_TIME_BTW_DOSE_NUPDRS, ON_OFF_DOSE, PD_MED_USE, Month, PAG_NAME, NHY)

mds_total <- mds_measurements %>% 
  select(PATNO,EVENT_ID,MDS_TOTAL, MDS_UPDRS_I, 
         MDS_UPDRS_II, MDS_UPDRS_III, MDS_UPDRS_IV, Subtype, 
         ANNUAL_TIME_BTW_DOSE_NUPDRS, ON_OFF_DOSE, PD_MED_USE, Month, PAG_NAME, NHY)



  
modified_schwab <- read_csv("raw-data/Motor___MDS-UPDRS/Modified_Schwab_+_England_ADL.csv",col_types = 
                              cols(
                                REC_ID = col_integer(),
                                F_STATUS = col_character(),
                                PATNO = col_integer(),
                                EVENT_ID = col_character(),
                                PAG_NAME = col_character(),
                                INFODT = col_character(),
                                MSEADLG = col_integer(),
                                ORIG_ENTRY = col_character(),
                                LAST_UPDATE = col_datetime(format = ""),
                                QUERY = col_character(),
                                SITE_APRV = col_character()
                              )) %>% 
  select(PATNO, EVENT_ID, MSEADLG)

pase <- read_csv("raw-data/Motor___MDS-UPDRS/PASE_-_Household_Activity.csv",col_types=
                   cols(
                     REC_ID = col_integer(),
                     F_STATUS = col_character(),
                     PATNO = col_integer(),
                     EVENT_ID = col_character(),
                     PAG_NAME = col_character(),
                     LTHSWRK = col_integer(),
                     HVYHSWRK = col_integer(),
                     HMREPR = col_integer(),
                     LAWNWRK = col_integer(),
                     OUTGARDN = col_integer(),
                     CAREGVR = col_integer(),
                     WRKVL = col_integer(),
                     WRKVLHR = col_integer(),
                     WRKVLACT = col_integer(),
                     ORIG_ENTRY = col_character(),
                     LAST_UPDATE = col_datetime(format = ""),
                     QUERY = col_character(),
                     SITE_APRV = col_datetime(format = "")
                   ))
pase %>% count(EVENT_ID)

## Non-motor

### Benton_Judgment_of_Line_Orientation
# There is one sample that has measurements for both sides
# Comments state that sometimes the wrong test was applied
benton <- read_csv("raw-data/Non-motor_Assessments/Benton_Judgment_of_Line_Orientation.csv",col_types=
                     cols(
                       .default = col_integer(),
                       F_STATUS = col_character(),
                       EVENT_ID = col_character(),
                       PAG_NAME = col_character(),
                       INFODT = col_character(),
                       COMM = col_character(),
                       DVS_JLO_MSSAE = col_double(),
                       ORIG_ENTRY = col_character(),
                       LAST_UPDATE = col_datetime(format = ""),
                       QUERY = col_character(),
                       SITE_APRV = col_character()
                     )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  mutate(Benton = rowSums(select_(.,"BJLOT1:BJLOT30"),na.rm = TRUE)) %>% 
  select(PATNO,EVENT_ID,Benton,DVS_JLO_MSSAE,AGE_ASSESS_JLO, DVS_JLO_MSSA)

### GDS
# Subjects with GDS >=5 are "Depressed".  Subjects with GDS <5 are "Not Depressed".
geriatricdepression <- read_csv("raw-data/Non-motor_Assessments/Geriatric_Depression_Scale__Short_.csv",col_types=
                                  cols(
                                    .default = col_integer(),
                                    F_STATUS = col_character(),
                                    EVENT_ID = col_character(),
                                    PAG_NAME = col_character(),
                                    INFODT = col_character(),
                                    ORIG_ENTRY = col_character(),
                                    LAST_UPDATE = col_datetime(format = ""),
                                    QUERY = col_character(),
                                    SITE_APRV = col_character()
                                  )) %>% 
  mutate(GDS = (1-GDSSATIS) + GDSDROPD + GDSEMPTY + GDSBORED + (1-GDSGSPIR) + 
           GDSAFRAD + (1-GDSHAPPY) + GDSHLPLS + GDSHOME + GDSMEMRY +
           (1-GDSALIVE) + GDSWRTLS + (1-GDSENRGY) + GDSHOPLS + GDSBETER) %>% 
  select(PATNO,EVENT_ID,GDS)

### Cognitive Categorization
cognitive_cat <- read_csv("raw-data/Non-motor_Assessments/Cognitive_Categorization.csv",col_types=
                            cols(
                              REC_ID = col_integer(),
                              F_STATUS = col_character(),
                              PATNO = col_integer(),
                              EVENT_ID = col_character(),
                              PAG_NAME = col_character(),
                              INFODT = col_character(),
                              PTCGBOTH = col_integer(),
                              COGDECLN = col_integer(),
                              FNCDTCOG = col_integer(),
                              COGSTATE = col_integer(),
                              COGDXCL = col_integer(),
                              RVWNPSY = col_integer(),
                              ORIG_ENTRY = col_character(),
                              LAST_UPDATE = col_datetime(format = ""),
                              QUERY = col_character(),
                              SITE_APRV = col_character()
                            )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV)

### Epworth Sleepiness Scale
# Information source also available
epps <- read_csv("raw-data/Non-motor_Assessments/Epworth_Sleepiness_Scale.csv", col_types=
                   cols(
                     REC_ID = col_integer(),
                     F_STATUS = col_character(),
                     PATNO = col_integer(),
                     EVENT_ID = col_character(),
                     PAG_NAME = col_character(),
                     INFODT = col_character(),
                     PTCGBOTH = col_integer(),
                     ESS1 = col_integer(),
                     ESS2 = col_integer(),
                     ESS3 = col_integer(),
                     ESS4 = col_integer(),
                     ESS5 = col_integer(),
                     ESS6 = col_integer(),
                     ESS7 = col_integer(),
                     ESS8 = col_integer(),
                     ORIG_ENTRY = col_character(),
                     LAST_UPDATE = col_datetime(format = ""),
                     QUERY = col_character(),
                     SITE_APRV = col_character()
                   )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  mutate(Epworth = rowSums(select_(.,"ESS1:ESS8"))) %>% 
  select(PATNO,EVENT_ID,Epworth)

### REM Behaviour Disorder Diagnosis
# Drop: Very small dataset, not used
remdiag <- read_csv("raw-data/Non-motor_Assessments/Features_of_REM_Behavior_Disorder.csv",col_types=
                      cols(
                        .default = col_integer(),
                        F_STATUS = col_character(),
                        EVENT_ID = col_character(),
                        PAG_NAME = col_character(),
                        INFODT = col_character(),
                        REMONSDT = col_character(),
                        RBDDXDT = col_character(),
                        ORIG_ENTRY = col_character(),
                        LAST_UPDATE = col_datetime(format = ""),
                        QUERY = col_character(),
                        SITE_APRV = col_character()
                      )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV)

### Hopkins Verbal Learning Test
# Drop: the derived measures in the dataset such as Derived-Total Recall T-Score
hopkins <- read_csv("raw-data/Non-motor_Assessments/Hopkins_Verbal_Learning_Test.csv", col_types=
                      cols(
                        .default = col_integer(),
                        F_STATUS = col_character(),
                        EVENT_ID = col_character(),
                        PAG_NAME = col_character(),
                        INFODT = col_character(),
                        comm = col_character(),
                        AGE_ASSESS_HVLT = col_double(),
                        DVT_TOTAL_RECALL = col_double(),
                        DVT_DELAYED_RECALL = col_double(),
                        DVT_RETENTION = col_double(),
                        DVT_RECOG_DISC_INDEX = col_double(),
                        ORIG_ENTRY = col_character(),
                        LAST_UPDATE = col_datetime(format = ""),
                        QUERY = col_character(),
                        SITE_APRV = col_character()
                      )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  mutate(TotalRec=HVLTRT1 + HVLTRT2 + HVLTRT3 +HVLTRDLY,
         `HVLT Immediate/Total Recall` = HVLTRT1 + HVLTRT2 + HVLTRT3,
         `HVLT Discrimination Recognition` =	HVLTREC - (HVLTFPRL + HVLTFPUN),
         `HVLT Retention` =	HVLTRDLY / pmax(HVLTRT2, HVLTRT3)) %>%
  select(PATNO,EVENT_ID,`HVLT Immediate/Total Recall`,
         `HVLT Discrimination Recognition`,`HVLT Retention`,
         DVT_TOTAL_RECALL,HVLTVRSN, TotalRec,DVT_DELAYED_RECALL,DVT_RETENTION, 
         AGE_ASSESS_HVLT, DVT_RECOG_DISC_INDEX,HVLTRDLY,HVLTRT1,HVLTRT2, HVLTRT3)


### Letter Number Sequencing
lns <- read_csv("raw-data/Non-motor_Assessments/Letter_-_Number_Sequencing__PD_.csv", col_types=
                  cols(
                    .default = col_integer(),
                    F_STATUS = col_character(),
                    EVENT_ID = col_character(),
                    PAG_NAME = col_character(),
                    INFODT = col_character(),
                    ORIG_ENTRY = col_character(),
                    LAST_UPDATE = col_datetime(format = ""),
                    QUERY = col_character(),
                    SITE_APRV = col_character()
                  )
                  ) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  mutate(LNS = rowSums(select_(.,"LNS1A:LNS7C"),na.rm=TRUE)) %>% 
  select(PATNO,EVENT_ID,LNS,DVS_LNS,AGE_ASSESS_LNS)


### Montreal Cognitive Assessment
# TODO: If EDUCYRS <=12 and Unadjusted Score < 30, add 1 more point to score.  If EDUCYRS > 12, do not add any more points to score.
moca <- read_csv("raw-data/Non-motor_Assessments/Montreal_Cognitive_Assessment__MoCA_.csv", col_types=
                   cols(
                     .default = col_integer(),
                     F_STATUS = col_character(),
                     EVENT_ID = col_character(),
                     PAG_NAME = col_character(),
                     INFODT = col_character(),
                     ORIG_ENTRY = col_character(),
                     LAST_UPDATE = col_datetime(format = ""),
                     QUERY = col_character(),
                     SITE_APRV = col_character()
                   )) %>% 
  #select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
  #       -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  mutate(info_date = lubridate::parse_date_time(INFODT,"m/y")) %>%
  mutate(original_date = lubridate::parse_date_time(ORIG_ENTRY,"m/y")) %>% 
  mutate(MOCA_unadjusted = MCAALTTM + MCACUBE + MCACLCKC + MCACLCKN + MCACLCKH + 
           MCALION +  MCARHINO + MCACAMEL + MCAFDS + MCABDS + MCAVIGIL + 
           MCASER7 + MCASNTNC + MCAVF + MCAABSTR + MCAREC1 + MCAREC2 + 
           MCAREC3 + MCAREC4 + MCAREC5 + MCADATE + MCAMONTH + MCAYR + 
           MCADAY + MCAPLACE + MCACITY) %>% 
  
  #filter(info_date<lubridate::parse_date_time("04/2015","m/y")) %>% 
  #filter(original_date<lubridate::parse_date_time("06/2014","m/y")) %>%
  left_join(socio_econ, by="PATNO") %>% 
  mutate(MOCA_adjusted=if_else(Education<=12 & MOCA_unadjusted<30, MOCA_unadjusted+1L, MOCA_unadjusted)) %>% 
  select(PATNO,EVENT_ID,MOCA_adjusted,MCATOT)



### QUIP
# Drop medication
quip <- read_csv("raw-data/Non-motor_Assessments/QUIP_Current_Short.csv", col_types=
                   cols(
                     .default = col_integer(),
                     F_STATUS = col_character(),
                     EVENT_ID = col_character(),
                     PAG_NAME = col_character(),
                     INFODT = col_character(),
                     TMDISMED = col_character(),
                     CNTRLDSM = col_character(),
                     ORIG_ENTRY = col_character(),
                     LAST_UPDATE = col_datetime(format = ""),
                     QUERY = col_character(),
                     SITE_APRV = col_character()
                   )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  mutate(QUIP = (CNTRLGMB | TMGAMBLE) + (CNTRLSEX | TMSEX) + (CNTRLBUY | TMBUY) +
           (CNTRLEAT|TMEAT) + TMTORACT + TMTMTACT + TMTRWD) %>% 
  mutate(TMDISMED=parse_integer(TMDISMED,na="N"),
         CNTRLDSM=parse_integer(CNTRLDSM,na="N")) %>% 
  mutate(TMDISMED=replace(TMDISMED,is.na(TMDISMED),0L),
         CNTRLDSM=replace(CNTRLDSM,is.na(CNTRLDSM),0L)) %>% 
  mutate(QUIP8 = (CNTRLGMB | TMGAMBLE) + (CNTRLSEX | TMSEX) + (CNTRLBUY | TMBUY) +
           (CNTRLEAT|TMEAT) + (CNTRLDSM|TMDISMED) + TMTORACT + TMTMTACT + TMTRWD) %>% 
  mutate(QUIPany=QUIP8>0) %>% 
  select(PATNO,EVENT_ID,QUIP,QUIP8,QUIPany)
  

## REM Sleep Behaviour Disorder
# TODO: unsure about the meaning of the calculation of the score. 
# Subjects with score >=5 are RBD Positive.  Subjects with score <5 are RBD Negative.
remsleep <- read_csv("raw-data/Non-motor_Assessments/REM_Sleep_Disorder_Questionnaire.csv",
                     col_types=cols(
                       .default = col_integer(),
                       F_STATUS = col_character(),
                       EVENT_ID = col_character(),
                       PAG_NAME = col_character(),
                       INFODT = col_character(),
                       CNSOTHCM = col_character(),
                       ORIG_ENTRY = col_character(),
                       LAST_UPDATE = col_datetime(format = ""),
                       QUERY = col_character(),
                       SITE_APRV = col_character()
                     )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  #filter(!(ORIG_ENTRY %in% c("12/2015", "12/2016"))) %>% 
  mutate(RBD = DRMVIVID + DRMAGRAC + DRMNOCTB + SLPLMBMV + SLPINJUR + DRMVERBL + 
           DRMFIGHT + DRMUMV + DRMOBJFL + MVAWAKEN + DRMREMEM + SLPDSTRB +
           (STROKE |  HETRA |  PARKISM |  RLS |  NARCLPSY |  DEPRS |  EPILEPSY |  BRNINFM |  CNSOTH)) %>% 
  mutate(RBDSQ = rowSums(select_(.,"DRMVIVID:CNSOTH"))) %>% 
  select(PATNO,EVENT_ID,RBD,RBDSQ)

### Scopa-AUT
# Drop: there are more scopa scores than used in the summary
# Note: NAs are counted as 0 in calculation of SCOPA-AUT
# SCAU1 - SCAU25.  For questions 1-21 (SCAU1 - SCAU21), add 3 points for each response of "9". Otherwise, add the number of points in response.  For questions 22-25 (SCAU22 - SCAU25), add 0 points for each response of "9". Otherwise, add the number of points in response.
scopa <- read_csv("raw-data/Non-motor_Assessments/SCOPA-AUT.csv", col_types=
                    cols(
                      .default = col_integer(),
                      F_STATUS = col_character(),
                      EVENT_ID = col_character(),
                      PAG_NAME = col_character(),
                      INFODT = col_character(),
                      SCAU23AT = col_character(),
                      SCAU26AT = col_character(),
                      SCAU26BT = col_character(),
                      SCAU26CT = col_character(),
                      SCAU26DT = col_character(),
                      ORIG_ENTRY = col_character(),
                      LAST_UPDATE = col_datetime(format = ""),
                      QUERY = col_character(),
                      SITE_APRV = col_character()
                    )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  mutate_at(vars(SCAU1:SCAU21),function(x){replace(x,x==9,3)}) %>% 
  mutate_at(vars(SCAU22:SCAU25),function(x){replace(x,x==9,0)}) %>%
  mutate_at(vars(SCAU22:SCAU25),function(x){replace(x,is.na(x),0)}) %>%
  select(-SCAU23A,-SCAU23AT) %>% 
  mutate(`SCOPA-AUT` = rowSums(select(.,SCAU1:SCAU25))) %>% 
  select(PATNO,EVENT_ID,`SCOPA-AUT`)

### Semantic Fluency (SFT)
semanticfluency <- read_csv("raw-data/Non-motor_Assessments/Semantic_Fluency.csv",col_types=
                              cols(
                                REC_ID = col_integer(),
                                F_STATUS = col_character(),
                                PATNO = col_integer(),
                                EVENT_ID = col_character(),
                                PAG_NAME = col_character(),
                                INFODT = col_character(),
                                VLTANIM = col_integer(),
                                VLTVEG = col_integer(),
                                VLTFRUIT = col_integer(),
                                AGE_ASSESS_SFTANIM = col_integer(),
                                DVS_SFTANIM = col_integer(),
                                DVT_SFTANIM = col_integer(),
                                ORIG_ENTRY = col_character(),
                                LAST_UPDATE = col_datetime(format = ""),
                                QUERY = col_character(),
                                SITE_APRV = col_character()
                              )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  mutate(SFT = VLTANIM + VLTVEG + VLTFRUIT) %>% 
  select(PATNO,EVENT_ID,DVS_SFTANIM,DVT_SFTANIM,SFT,AGE_ASSESS_SFTANIM,
         DVS_SFTANIM,VLTANIM)

### State Trait Anxiety Inventory
statetrait <- read_csv("raw-data/Non-motor_Assessments/State-Trait_Anxiety_Inventory.csv",col_types=
                         cols(
                           .default = col_integer(),
                           F_STATUS = col_character(),
                           EVENT_ID = col_character(),
                           PAG_NAME = col_character(),
                           INFODT = col_character(),
                           ORIG_ENTRY = col_character(),
                           LAST_UPDATE = col_datetime(format = ""),
                           QUERY = col_character(),
                           SITE_APRV = col_character()
                         )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  mutate(STAI_trait = STAIAD22 + STAIAD24 + STAIAD25 + STAIAD28 + STAIAD29 + 
                      STAIAD31 + STAIAD32 + STAIAD35 + STAIAD37 + STAIAD38 + STAIAD40 + 
                (5-STAIAD21) + (5-STAIAD23) + (5-STAIAD26) + (5-STAIAD27) + (5-STAIAD30) + 
                (5-STAIAD33) + (5-STAIAD34) + (5-STAIAD36) + (5-STAIAD39)) %>% 
  mutate(STAI_state = STAIAD3 + STAIAD4 + STAIAD6 + STAIAD7 + STAIAD9 + STAIAD12 + 
           STAIAD13 + STAIAD14 + STAIAD17 + STAIAD18 + (5-STAIAD1) + (5-STAIAD2) + 
           (5-STAIAD5) + (5-STAIAD8) + (5-STAIAD10) + (5-STAIAD11) + (5-STAIAD15) + 
           (5-STAIAD16) + (5-STAIAD19) + (5-STAIAD20)) %>% 
  mutate(STAI = STAI_state+STAI_trait) %>% 
  select(PATNO,EVENT_ID,STAI_state,STAI_trait,STAI)

### Symbol Digit Modalities
sdm <- read_csv("raw-data/Non-motor_Assessments/Symbol_Digit_Modalities.csv",
                col_types=cols(
                  REC_ID = col_integer(),
                  F_STATUS = col_character(),
                  PATNO = col_integer(),
                  EVENT_ID = col_character(),
                  PAG_NAME = col_character(),
                  INFODT = col_character(),
                  SDMTOTAL = col_integer(),
                  SDMTVRSN = col_integer(),
                  AGE_ASSESS_SDM = col_double(),
                  DVSD_SDM = col_double(),
                  DVT_SDM = col_double(),
                  COMM = col_character(),
                  ORIG_ENTRY = col_character(),
                  LAST_UPDATE = col_datetime(format = ""),
                  QUERY = col_character(),
                  SITE_APRV = col_character()
                )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  select(PATNO,EVENT_ID,DVT_SDM,SDMTOTAL,AGE_ASSESS_SDM,DVSD_SDM,SDMTVRSN)

### UPSIT
upsit <- read_csv("raw-data/Non-motor_Assessments/University_of_Pennsylvania_Smell_ID_Test.csv", col_types=
                    cols(
                      REC_ID = col_integer(),
                      F_STATUS = col_character(),
                      PATNO = col_integer(),
                      EVENT_ID = col_character(),
                      PAG_NAME = col_character(),
                      INFODT = col_character(),
                      UPSITBK1 = col_integer(),
                      UPSITBK2 = col_integer(),
                      UPSITBK3 = col_integer(),
                      UPSITBK4 = col_integer(),
                      COMM = col_character(),
                      ORIG_ENTRY = col_character(),
                      LAST_UPDATE = col_datetime(format = ""),
                      QUERY = col_character(),
                      SITE_APRV = col_character()
                    )) %>% 
  select(-REC_ID, -F_STATUS,-PAG_NAME,-INFODT,
         -ORIG_ENTRY, -LAST_UPDATE, -QUERY,-SITE_APRV) %>% 
  mutate(UPSIT = UPSITBK1 + UPSITBK2 + UPSITBK3 + UPSITBK4) %>% 
  select(PATNO,EVENT_ID,UPSIT)

### Combine
# NOTE: We consider screening visits as baseline visits here
non_motor <- 
  Reduce(function(x,y) {full_join(x %>% mutate(EVENT_ID = replace(EVENT_ID,EVENT_ID=="SC","BL")),y%>% mutate(EVENT_ID = replace(EVENT_ID,EVENT_ID=="SC","BL")),by=c("PATNO","EVENT_ID"))},
         list(upsit,sdm,statetrait,semanticfluency,scopa,remsleep,quip,
              moca,lns,hopkins,epps,benton,geriatricdepression,cognitive_cat))

### Derive Mild Cognitive Impairment
non_motor <- non_motor %>% 
  mutate(`Cognitive Decline` = COGDECLN &
           ((DVT_TOTAL_RECALL <=35) + (DVT_RECOG_DISC_INDEX <=35) + (DVS_JLO_MSSAE <=6) + (DVS_LNS <=6) + 
              (DVT_SFTANIM<= 35) + (DVT_SDM <=35))>1 & !FNCDTCOG) %>% 
  mutate(`Mild Cognitive Impairment` = 
           ((DVT_TOTAL_RECALL <=35) + (DVT_RECOG_DISC_INDEX <=35) + (DVS_JLO_MSSAE <=6) + (DVS_LNS <=6) + 
           (DVT_SFTANIM<= 35) + (DVT_SDM <=35))>1 & !FNCDTCOG) %>% 
  mutate(`Cognitive Impairment` = COGDECLN &
           ((DVT_TOTAL_RECALL <=35) + (DVT_RECOG_DISC_INDEX <=35) + (DVS_JLO_MSSAE <=6) + (DVS_LNS <=6) + 
              (DVT_SFTANIM<= 35) + (DVT_SDM <=35))>1 & FNCDTCOG)

## DaTSCAN
datscan <- read_csv("raw-data/Imaging/DATScan_Analysis.csv", col_types=
                      cols(
                        PATNO = col_integer(),
                        EVENT_ID = col_character(),
                        CAUDATE_R = col_double(),
                        CAUDATE_L = col_double(),
                        PUTAMEN_R = col_double(),
                        PUTAMEN_L = col_double()
                      )) %>% 
  mutate(`Mean Putamen`=(PUTAMEN_R+PUTAMEN_L)/2,
         `Mean Caudate`=(CAUDATE_R+CAUDATE_L)/2,
         `Mean Striatum`=(`Mean Putamen`+`Mean Caudate`)/2,
         `Count Density Ratio`=`Mean Caudate`/`Mean Putamen`,
         `Putaminal asymmetry`=pmax(PUTAMEN_L,PUTAMEN_R)/pmin(PUTAMEN_L,PUTAMEN_R),
         `Caudate asymmetry`=pmax(CAUDATE_L,CAUDATE_R)/pmin(CAUDATE_L,CAUDATE_R)
  )

datscan_visual <- read_csv("raw-data/Imaging/DaTSCAN_SPECT_Visual_Interpretation_Assessment.csv", col_types=
                             cols(
                               PATNO = col_integer(),
                               VISINTRP = col_character(),
                               SCANDATE = col_character()
                             ))

# TODO: contralateral, ipsilateral

## Biospecimen

### Biospecimen
# Drops: below and above threshold become NA
# Drop: select the most recent measurements in case of mutiple measurements
biospecimen <- read_csv("raw-data/Biospecimen/Current_Biospecimen_Analysis_Results.csv", col_types=
                          cols(
                            PATNO = col_integer(),
                            GENDER = col_character(),
                            DIAGNOSIS = col_character(),
                            CLINICAL_EVENT = col_character(),
                            TYPE = col_character(),
                            TESTNAME = col_character(),
                            TESTVALUE = col_character(),
                            UNITS = col_character(),
                            RUNDATE = col_date(format = ""),
                            PROJECTID = col_integer(),
                            PI_NAME = col_character(),
                            PI_INSTITUTION = col_character(),
                            update_stamp = col_datetime(format = "")
                          )) %>% 
  filter(!is.na(UNITS)) %>% 
  mutate(TESTVALUE=as.numeric(TESTVALUE)) %>% 
  rename(EVENT_ID=CLINICAL_EVENT,Measurement=TESTNAME,Score=TESTVALUE) %>% 
  mutate(Measurement = paste(Measurement,UNITS)) %>% 
  group_by(PATNO,EVENT_ID,Measurement) %>% 
  arrange(RUNDATE) %>% 
  filter(row_number()==n()) %>% 
  ungroup %>% 
  select(PATNO, EVENT_ID,Measurement,Score) %>% 
  spread(Measurement,Score)
  

### Blood Chemistry
# Drop: Introduces some NA when character description of problem with measurement is given
# Drop: When multiple measurements as available
blood <- read_csv("raw-data/Biospecimen/Blood_Chemistry___Hematology.csv",col_types=
                    cols(
                      .default = col_character(),
                      PATNO = col_integer(),
                      COLLTM = col_time(format = ""),
                      RECTM = col_time(format = ""),
                      RPTTM = col_time(format = ""),
                      LSILORNG = col_double(),
                      LSIHIRNG = col_double(),
                      LUSLORNG = col_double(),
                      LUSHIRNG = col_double()
                    ))  %>% 
  select(-PAG_NAME, -LCOLLDT, -COLLTM, -LRECDT, -RECTM,-RPTTM,
         -LABCODE,-LVISTYPE) %>% 
  mutate(Date = convert_monthyear_num(LRPTDT)) %>% 
  group_by(PATNO,EVENT_ID,LTSTNAME) %>% 
  arrange(Date) %>% 
  filter(row_number() == n()) %>% 
  ungroup %>% 
  select(PATNO, EVENT_ID, LTSTNAME, LSIRES) %>% 
  mutate(LSIRES=as.numeric(LSIRES)) %>% 
  spread(LTSTNAME,LSIRES)

# read_csv("raw-data/Biospecimen/Genetic_Testing_Results.csv")
# 

# Only relates to: "U01" "U02" "V02" "V09" "V05" "V11"
# lab_meta <- read_csv("raw-data/Biospecimen/iPSC_Blood_Sample.csv") %>% 
#   select(-REC_ID,-F_STATUS,-PAG_NAME,-INFODT,-ORIG_ENTRY, -LAST_UPDATE, -QUERY, -SITE_APRV)
# blood <- left_join(blood,lab_meta,by=c("PATNO","EVENT_ID"))

pd_med_use <- read_csv("raw-data/Medical_History/Use_of_PD_Medication.csv",col_types = cols(QUERY="c",FULNUPDR="c")) %>% 
  select(-REC_ID,-F_STATUS,-PAG_NAME,-INFODT,-ORIG_ENTRY, -LAST_UPDATE, -QUERY, -SITE_APRV)

# 
# read_csv("raw-data/Biospecimen/Lumbar_Puncture_Sample_Collection.csv")
# 
# read_csv("raw-data/Biospecimen/Skin_Biopsy.csv")
# 
# read_csv("raw-data/Biospecimen/Whole_Blood_Sample_Collection.csv")

# Events
#read_csv("raw-data/Medical_History/Adverse_Event_Log.csv")
#read_csv("raw-data/Medical_History/Current_Medical_Conditions_Log.csv") %>% View()

conmed <- read_csv("raw-data/Medical_History/Concomitant_Medications.csv",col_types = cols(
  .default = col_character(),
  REC_ID = col_integer(),
  PATNO = col_integer(),
  CMSEQ = col_integer(),
  CMDOSE = col_double(),
  ROUTE = col_integer(),
  ONGOING = col_integer(),
  DISMED = col_integer(),
  LAST_UPDATE = col_datetime(format = "")
)) %>% 
  mutate(ld33 = (LEDD=="LD x 0.33")) %>%
  mutate(ld5 = (LEDD=="LD x 0.5")) %>% 
  mutate(LEDD = parse_double(LEDD,na = c("LD x 0.5","LD x 0.33"))) %>% 
  mutate(LEDD = replace(LEDD,is.na(LEDD),0)) %>% 
  mutate(ld33 = replace(ld33,is.na(ld33),FALSE)) %>% 
  mutate(ld5 = replace(ld5,is.na(ld5),FALSE)) %>% 
  #filter(!is.na(LEDD)) %>% 
  mutate(STARTDT=convert_monthyear_num(STARTDT),
         STOPDT=convert_monthyear_num(STOPDT))

sig <- read_csv("raw-data/Study_Enrollment/Signature_Form.csv", col_types=
                  cols(
                    REC_ID = col_integer(),
                    F_STATUS = col_character(),
                    PATNO = col_integer(),
                    EVENT_ID = col_character(),
                    PAG_NAME = col_character(),
                    INFODT = col_character(),
                    VISSTAT = col_integer(),
                    VISSTCM = col_character(),
                    VISMSRSN_CODE = col_character(),
                    ASSESCMP = col_integer(),
                    LGCMED2 = col_integer(),
                    LGAE2 = col_integer(),
                    LGCONDTN = col_integer(),
                    COMM = col_character(),
                    ORIG_ENTRY = col_character(),
                    LAST_UPDATE = col_datetime(format = ""),
                    QUERY = col_character(),
                    SITE_APRV = col_character()
                  )) %>% 
  mutate(INFODT=convert_monthyear_num(INFODT)) 

conmedpd <- conmed %>% filter(DISMED==1)
ledd <- numeric(nrow(sig))
for (i in 1:nrow(sig)) {
  meds_at_i <-   
    conmedpd %>%
    filter(PATNO==sig$PATNO[[i]],
           is.na(STARTDT) | STARTDT<sig$INFODT[[i]], # Note: We use strictly smaller, to encode that starting today does not affect today's measurements.
           is.na(STOPDT) | STOPDT>=sig$INFODT[[i]])
  
  if (any(meds_at_i$ld33) | any(meds_at_i$ld5)) {
    # Count total levodopa LEDD
    total_levo <- meds_at_i$LEDD %>% sum
    meds_at_i %>% mutate(LEDD=case_when(ld33==TRUE~total_levo/3, ld5==TRUE~total_levo/2, TRUE~LEDD))
  }
  ledd[[i]] <- meds_at_i$LEDD %>% sum
}


ledds <- sig %>% mutate(LEDD=ledd) %>% 
  select(PATNO,EVENT_ID,LEDD)

vital <- read_csv("raw-data/Medical_History/Vital_Signs.csv",
                  col_types = cols(
  .default = col_integer(),
  F_STATUS = col_character(),
  EVENT_ID = col_character(),
  PAG_NAME = col_character(),
  INFODT = col_character(),
  WGTKG = col_double(),
  TEMPC = col_double(),
  COMM = col_character(),
  ORIG_ENTRY = col_character(),
  LAST_UPDATE = col_datetime(format = ""),
  QUERY = col_character(),
  SITE_APRV = col_character()
)) %>% 
  select(-REC_ID,-F_STATUS,-PAG_NAME,-INFODT,-ORIG_ENTRY, -LAST_UPDATE, -QUERY, -SITE_APRV) %>% 
  mutate(BMI = WGTKG/(HTCM/100)^2)
  
conditions <- read_csv("raw-data/Medical_History/Current_Medical_Conditions_Log.csv", col_types=
                         cols(
                           .default = col_character(),
                           REC_ID = col_integer(),
                           PATNO = col_integer(),
                           SEQNO = col_integer(),
                           DIAGYR = col_integer(),
                           RESOLVD = col_integer(),
                           RESYR = col_integer(),
                           PT_CODE = col_integer(),
                           VMEDDRA = col_double(),
                           LAST_UPDATE = col_datetime(format = "")
                         )) %>% 
  mutate(Disease = case_when(
    HLT_NAME %in% c("Elevated cholesterol") ~ "Hyperlipidaemia",
    HLT_NAME == "Lipid metabolism and deposit disorders NEC" ~ "Dyslipidaemia",
    HLT_NAME == "Diabetes mellitus (incl subtypes)" ~ "Diabetes",
    HLT_NAME == "Vascular hypertensive disorders NEC" ~ "Hypertension",
    HLT_NAME %in% c("Coronary artery disorders NEC","Ischaemic coronary artery disorders","Supraventricular arrhythmias") ~ "Heart disease",
    TRUE ~ NA_character_
  )) %>% 
  filter(!is.na(Disease)) %>% 
  filter(is.na(RESOLVD) | RESOLVD==0) %>% 
  group_by(PATNO,Disease) %>% 
  arrange(DIAGYR) %>% 
  slice(1) %>% 
  ungroup %>% 
  select(PATNO,Disease,DIAGYR) %>% 
  spread(Disease,DIAGYR)

cv_terms <- c(
  'CORONARY ARTERY DISEASE',
  'MYOCARDIAL INFARCTION',
  'ANGINA',
  'HEART ATTACK',
  'TRANSIENT ISCHEMIC ATTACK',
  'CABG',
  'CAD',
  'CHRONIC ISCHEMIC HEART DISEASE',
  'CORONARY ARTERY BYPASS GRAFT SURGERY',
  'CORONARY ARTERY DISEASE (CAD)',
  'CORONARY ARTERY DISEASE WITH STENT PLACEMENT',
  'TRANSIENT ISCHEMIC ATTACK',
  'TIA'
)

gmh <- read_csv("raw-data/Medical_History/General_Medical_History.csv",
                col_types = cols(
                  .default = col_character(),
                  REC_ID = col_double(),
                  PATNO = col_double(),
                  MHROW = col_double(),
                  MHHX = col_double(),
                  MHACTRES = col_double(),
                  MHDIAGYR = col_double(),
                  PT_CODE = col_double(),
                  LAST_UPDATE = col_datetime(format = "")
                )) %>% 
  mutate(CV_HISTORY = case_when(
    str_detect(VERBATIM, 'STROKE') | VERBATIM %in% cv_terms ~ 1,
    TRUE ~ 0
  )) %>%
  distinct(PATNO, CV_HISTORY) %>%
  group_by(PATNO) %>% # if somebody has cvhist and non cv diseases in MH, register as somebody with cvhist
  arrange(desc(CV_HISTORY)) %>%
  slice(1) %>%
  ungroup

subject_characteristics <- left_join(subject_characteristics,gmh,by="PATNO")
# read_csv("raw-data/Medical_History/Current_Medical_Conditions_Log.csv") %>%
#   .$PT_NAME %>% str_extract(".*choles.*") %>% unique 
# 
# read_csv("raw-data/Medical_History/Current_Medical_Conditions_Log.csv") %>%
#   .$PT_NAME %>% str_extract(".*dial.*") %>% unique 
# 
# read_csv("raw-data/Medical_History/Current_Medical_Conditions_Log.csv") %>% 
#   filter(HLT_NAME=="Hyperlipidaemias NEC") %>% .$CONDTERM
# 
# "Lipid metabolism and deposit disorders NEC"
# read_csv("raw-data/Medical_History/Current_Medical_Conditions_Log.csv") %>% 
#   filter(PT_NAME=="Hypercholesterolaemia") %>% .$HLT_NAME
# Diabetes mellitusstr_detect(PT_NAME,".*2 diabetes mellitus.*")
# read_csv("raw-data/Medical_Hist ory/Current_Medical_Conditions_Log.csv") %>%
#   .$CONDTERM %>% str_extract("DIABETE.*") %>% unique
# read_csv("raw-data/Medical_History/Current_Medical_Conditions_Log.csv") %>% 
#   .$CONDTERM



save(subject_characteristics,non_motor,mds_total,datscan,datscan_visual,mds_1,mds_2,mds_3,mds_4, mds_measurements,
     biospecimen,blood,pd_med_use,vital,conditions,modified_schwab,ledds,moca,scopa,sig,ledds,conmed,file="data/data.RData")



