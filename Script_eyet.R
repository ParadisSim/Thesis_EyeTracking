#install.packages("tidyverse")
library(tidyverse)
library(eyetrackingR)
#install.packages("devtools")
library(devtools)
#install_github("tmalsburg/saccades/saccades", dependencies=TRUE)
library(saccades)
#install.packages('eyetrackingR')
library(eyetrackingR)


data <- analyse_prelim_mai_2025
data_for_condi <- data %>% mutate(condi = paste(voice_int, face_int, emotion))


#---------------------------- SACCADES PACKAGE ----------------
#Analyses are threefold. One with silence, one with neutral voices and one with emotional voices. 
#As of 2025-05-22 we will experiment on the later (emotional) block

#To note, more on validity etc needs to be read. The first analyses will be simple to test the saccades package.

View(samples)

#Filtering for stimulus also messes up the time?
#data <- analyse_prelim_mai_2025%>% filter(CurrentObject == "Stimulus")


#We must create a file from our raw data having the 1) time, 2) x pos of the eyes, 3) y pos of the eyes, 4) trial

#Assuming that !0 is invalid (to verify).
#data <- data %>% filter(ValidityLeftEye == 0 & ValidityRightEye == 0)
#This removes accurate times


#1 we will use TET_time with a stable frequency needed for the velocity based algorithms to be valid
#2 & 3 we will average left and right eyes position for x and y data
#4 we will filter for emotional conditions and  the trial number will be one of the 100 (4 emotions x 5 voice intensities and 5 face intensities)

#Here we proceed to have condition numbers as a product of a unique number associated with every possible conditions
data_for_condi <- data %>% mutate(condi = paste(voice_int, face_int, emotion))

data_numbertrials <- as.data.frame(data_numbertrials %>% mutate(condi = unique(data_for_condi$condi)))
data_numbertrials <- data_numbertrials %>% mutate(numberCondi = row_number())
data_numbertrials <- data_numbertrials %>% filter(numberCondi != 141) %>% select(condi,numberCondi)

#The merge here is responsible to associate the unique number everytime the character concatenation is seen in the original
data_with_trials <- merge(data_numbertrials, data_for_condi, by = "condi")

#avg to have x and y coordinates
#data_emotional_conditions <- data_with_trials %>% mutate(x = rowMeans(pick(c(XGazePosLeftEye,XGazePosRightEye))),y = rowMeans(pick(c(YGazePosLeftEye,YGazePosRightEye))))
#data_emotional_conditions %>% group_by(x) %>% summary()

#We will use the camera relative data for now as it seems standardized between -1 and 1, whilst the raw gives weird x data.
#data_emotional_conditions_debug <- data_with_trials %>% mutate(x = rowMeans(pick(c(XCameraPosLeftEye,XCameraPosRightEye))),y = rowMeans(pick(c(YCameraPosLeftEye,YCameraPosRightEye))))
#data_emotional_conditions_debug %>% group_by(x) %>% summary()

#
data_emotional_conditions_debug_cursor <- data_with_trials %>% mutate(x = CursorX,y = CursorY)


#prob, maybe take relative on screen ?

#here, we will look at participant 1, session 1. This is so we can assess if the package works properly before adjusting trials for our design
data_emotional_conditions_p1 <- data_emotional_conditions_debug_cursor %>% filter(Subject == 1, Session == 1)

#On filtre pour garder condi emotionelles seulement et enlever les gazes autres que lors des présentation de stimulus
data_fixation_detection <- data_emotional_conditions_p1 %>% rename(time = RTTime, trial = TrialId)
data_fixation_detection <- data_fixation_detection %>% arrange(trial, time)
data_fixation_detection <- data_fixation_detection %>% mutate(time = time-190000)


fixed <- detect.fixations(data_fixation_detection)
event_fixed <- fixed %>% filter(event == "fixation")

diagnostic.plot(data_fixation_detection,event_fixed,interactive = FALSE,start.time=1000, duration=12000)

ok <- saccades::calculate.summary(fixed)
#Ici, on dirait que cest les croix de fixations qui sont longues. On pourrait essayer de les preprocess en enlevant

#Now, since the package cannot handle ++ subjects and condition, we will test other methods before trying to revert find what were condition and participant numbers.


#----------------- Eyetracking R------------------

#Validity right et left - 0 c'est bon. Si jamais trop peu de données ainsi, voir : https://researchwiki.solo.universiteitleiden.nl/xwiki/wiki/researchwiki.solo.universiteitleiden.nl/download/Software/E-Prime/E-Prime%20and%20Tobii/WebHome/eet_2_user_guide14052019.pdf?rev=1.1
#Faire validity right == 0 & validity left == 0
#Ensuite voir si TETTime est ok

#Clean data by adding a trackloss column (False if 0, 0 and true otherwise)

data_trackloss <- data_for_condi %>% mutate(TrackL = ValidityLeftEye != 0 | ValidityRightEye != 0)
data_trackloss %>% filter(TrackL == TRUE)

#Add 2 AOIcolumns (Neutral, Emotional) et mettre true si neutral est regardé, true si emotional est regardé

data_processed <- data_trackloss %>% mutate(Neutral = AOIStimulus == "Neutral", Emotional = AOIStimulus == "Emotional")


data_processed_no_cross <- data_processed %>% filter(CurrentObject == "Stimulus") 

unique(data_processed_no_cross %>% filter(Session != 1 & Session !=2) %>% select(Subject, Session))
#Make trials go from 1 to 1120 for each part.
#Lets temprarily not fix and just not analyse those

#9 = 1121, 14,18,19,21,22,25 on plusieurs restart a fix
data_processed_no_cross <- data_processed_no_cross %>% filter(Subject != 9,Subject != 14, Subject != 18 ,Subject != 19, Subject != 21, Subject != 22, Subject != 25)

data_processed_no_cross <- data_processed_no_cross %>% mutate(unique_trial = as.numeric(TrialId) + 560*(as.numeric(Session)-1))

data_processed_no_cross %>% group_by(Subject,TrialId) %>% summarise(count = length(unique(TrialId))) %>% summary()
data_processed_no_cross %>% group_by(Subject,unique_trial) %>% summarise(count = length(unique(unique_trial))) %>% summary()



data_processed_no_cross %>% group_by(Subject)TrialIddata_processed_no_cross %>% group_by(Subject) %>% summary(count = unique_trial)

data_eye <- make_eyetrackingr_data(data_processed_no_cross, 
                               participant_column = "Subject",
                               trial_column =  "unique_trial",
                               time_column = "RTTime",
                               trackloss_column = "TrackL",
                               aoi_columns = c('Neutral','Emotional'),
                               treat_non_aoi_looks_as_missing = TRUE)


