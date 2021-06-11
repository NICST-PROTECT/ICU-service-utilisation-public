# reading data in.
load("data/Bed information.RData")
load("data/Main dataset.RData")

######################################### Processing patient information.
# converting the dates to date format.
# removing negative lengths of stay.
data <- data %>% 
  mutate(admission.date_of_admission = ymd(admission.date_of_admission)) %>% 
  mutate(discharge.date_of_discharge = ymd(discharge.date_of_discharge)) %>%
  arrange(admission.date_of_admission) %>% 
  # The time strings need some cleaning up.
  # I'm going to replace everything which doesn't match with midnight, for both admission and discharge times so that I don't end up 
  # with negative lengths of stay if they are admitted and discharged on the same day.
  mutate(admission.time_of_admission = word(admission.time_of_admission, 1, 2, ":"),
         discharge.time_of_discharge = word(discharge.time_of_discharge, 1, 2, ":")) %>% 
  mutate(admission.time_of_admission = if_else(str_detect(admission.time_of_admission, 
                                                          "^[0-9][0-9]:[0-9][0-9]$") & 
                                                 str_detect(discharge.time_of_discharge, 
                                                            "^[0-9][0-9]:[0-9][0-9]$"), 
                                               admission.time_of_admission, "00:00", "00:00"), 
         discharge.time_of_discharge = if_else(str_detect(discharge.time_of_discharge, 
                                                          "^[0-9][0-9]:[0-9][0-9]$") & 
                                                 str_detect(admission.time_of_admission, 
                                                            "^[0-9][0-9]:[0-9][0-9]$"), 
                                               discharge.time_of_discharge, "00:00", "00:00")) %>% 
  mutate(los = as.numeric(difftime(ymd_hm(paste(discharge.date_of_discharge, 
                                                discharge.time_of_discharge)),
                                   ymd_hm(paste(admission.date_of_admission, 
                                                admission.time_of_admission)), units = "days"))) %>% 
  # Getting rid of wrong lengths of stay.
  filter(los >= 0) %>% 
  # Getting time from hospital to ICU admission.
  mutate(hospital_to_icu = as.numeric(difftime(ymd_hm(paste(admission.date_of_admission, admission.time_of_admission)), 
                                               ymd_hm(paste(admission.date_of_admission_hospital, 
                                                            if_else(admission.time_of_admission_hospital == "", "00:00", admission.time_of_admission_hospital))), 
                                                      units = "hours"))) %>% 
  # Filtering out data before October 2019. 
  filter(admission.date_of_admission >= "2019-10-01")

# splitting the dates into week long periods. 
# I also create a variable for the beginning of the month, so that I can join to number of available beds.
data <- data %>% 
  mutate(weekwise = ceiling_date(admission.date_of_admission, unit = "week", week_start = getOption("lubridate.week.start", 1))) %>% 
  mutate(monthwise = floor_date(admission.date_of_admission, unit = "month", week_start = getOption("lubridate.week.start", 1))) 

# I'm doing this again for dishcarge dates, so that I can get turnover. 
data <- data %>% 
  mutate(weekwise_discharge = ceiling_date(discharge.date_of_discharge, unit = "week", week_start = getOption("lubridate.week.start", 1))) %>% 
  mutate(monthwise_discharge = floor_date(discharge.date_of_discharge, unit = "month", week_start = getOption("lubridate.week.start", 1)))

# Joining the number of available beds.
data <- left_join(data, n_beds, by = c("monthwise" = "month"))

###################################################################################################################
# Summarising variables per week.
# Occupancy = number of admission*mean los/(number_of_beds * number_of_days.). Join the unit IDs to the dataset. 
# Formula for bed occupancy = (number of admissions * average length of stay)/(bed capacity * number of days)
# Formula for bed availability = denominator of above calculation - numerator.
# Formula for turnover = number of admissions / bed capacity.
weekly <- data %>% 
  group_by(weekwise) %>% 
  summarise(number_of_admissions = n(), 
            number_of_beds = mean(beds)*7, # Take mean here, to deal with the problem of a week spanning 2 months.
            occupancy = 100*(n()*mean(los))/(mean(beds)*7),
            bed_availability = (mean(beds)*7 - (n()*mean(los)))/(7*mean(number_of_units)), 
            mechanically_vent = 100*sum(admissionAssessment.mechanically_ventilated %in% c("mechanical_vent", "Mechanical vent") 
                                    & admissionAssessment.mechanically_ventilated_source != "Non invasive vent")/sum(admissionAssessment.mechanically_ventilated != ""),
            niv = 100*sum(admissionAssessment.mechanically_ventilated == "Non invasive vent" 
                      | admissionAssessment.mechanically_ventilated_source == "Non invasive vent")/sum(admissionAssessment.mechanically_ventilated != ""),
            cardiovascular_support = 100*sum(!admissionAssessment.vasoactive_drugs %in% c("None", "null", ""))/sum(!
              (admissionAssessment.cardiovascular_support == "" & (admissionAssessment.vasoactive_drugs == "" | 
                                                                    admissionAssessment.vasoactive_drugs == "null"))),
            renal_replacement = 100*sum(admissionAssessment.renal_replacement == "Yes")/n(),
            surgical_total = 100*sum((admission.diagnosis_type == "Post operative" | 
                                          admission.diagnosis_type2 %in% c("Post operative", "Post operative2")))/n(),
            surgical_planned = 100*sum((admission.diagnosis_type == "Post operative" | 
                                 admission.diagnosis_type2 %in% c("Post operative", "Post operative2")) & 
                                   admission.admission_type == "Planned")/sum((admission.diagnosis_type == "Post operative" | 
                                                                                 admission.diagnosis_type2 %in% c("Post operative", "Post operative2"))),
            surgical_unplanned = 100*sum((admission.diagnosis_type == "Post operative" | 
                                           admission.diagnosis_type2 %in% c("Post operative", "Post operative2")) &
                                           admission.admission_type == "Unplanned")/sum((admission.diagnosis_type == "Post operative" | 
                                                                                           admission.diagnosis_type2 %in% c("Post operative", "Post operative2"))),
            # A patient is non-surgical if they haven't got a post operative condition recorded at some point.
            non_surgical = 100*sum(admission.diagnosis_type == "Non operative" & admission.diagnosis_type2 != "Post operative2")/n(), 
            apache_II = median(admissionAssessment.apache_score, na.rm = TRUE),
            
            ####################################### Outcome variables
            discharged_alive = 100*sum(discharge.discharge_status == "Alive")/sum(discharge.discharge_status %in% c("Alive", "Dead")),
            discharged_dead = 100*sum(discharge.discharge_status == "Dead")/sum(discharge.discharge_status %in% c("Alive", "Dead")),
            discharged_home = 100*sum(discharge.discharge_destination %in% c("Home", " Home", 
                                                                         "Discharge home for end of life care"))/sum(discharge.discharge_status == "Alive"),
            discharged_ward = 100*sum(discharge.discharge_destination %in% c("Ward", " Ward"))/sum(discharge.discharge_status == "Alive"),
            discharged_icu = 100*sum(discharge.discharge_destination %in% c("ICU", " ICU"))/sum(discharge.discharge_status == "Alive"),
            discharged_transfer = 100*sum(discharge.discharge_destination %in% c("Transfer for specialist care", "Other hospital"))/sum(discharge.discharge_status == "Alive"),
            discharged_ama = 100*sum(discharge.discharge_destination %in% c("Discharged against medical advice") |
                                       discharge.left_against_medical_advice == "Yes")/sum(discharge.discharge_status == "Alive"),
            discharged_other = 100*sum(discharge.discharge_destination %in% c("others", " others", "Others"))/sum(discharge.discharge_status == "Alive"),
            readmission = 100*sum(admission.readmission %in% c("Yes"))/n(),
            los = median(los),
            time_to_icu = median(hospital_to_icu, na.rm = TRUE),
            ######################################## Adding amount of missingness for each of the base variables.
            paying_fees_m = sum(admission.fee_paying == ""), 
            mechanically_vent_m = sum(admissionAssessment.mechanically_ventilated == ""),
            cardiovascular_support_m = sum(admissionAssessment.cardiovascular_support == "" & 
                                                 (admissionAssessment.vasoactive_drugs == "" | 
                                                    admissionAssessment.vasoactive_drugs == "null")),
            renal_replacement_m = sum(admissionAssessment.renal_replacement == ""),
            surgical_m = sum(admission.diagnosis_type == ""),
            planned_m = sum(admission.admission_type == ""), 
            apache_m = sum(is.na(admissionAssessment.apache_score)),
            #### Missingness for outcomes.
            discharge_status_m = sum(discharge.discharge_status %in% c("", "null")),
            discharge_destination_m = sum(discharge.discharge_destination %in% c("", "null") & discharge.discharge_status == "Alive"),
            readmission_m = sum(admission.readmission == ""),
            time_to_icu_m = sum(is.na(hospital_to_icu)))

# Some variables are only recorded after specific dates. Making them NA before that.
weekly <- weekly %>% 
  mutate(niv = if_else(weekwise > ymd("2019-10-06"), niv, as.numeric(NA))) %>% 
  mutate(renal_replacement = if_else(weekwise > ymd("2020-05-01"), renal_replacement, as.numeric(NA))) %>% 
  mutate(renal_replacement_m = if_else(weekwise > ymd("2020-05-01"), renal_replacement_m, as.integer(NA))) %>% 
  mutate(time_to_icu = if_else(weekwise > ymd("2019-11-01"), time_to_icu, as.numeric(NA))) %>% 
  mutate(time_to_icu_m = if_else(weekwise > ymd("2019-11-01"), time_to_icu_m, as.integer(NA))) %>% 
  mutate(readmission = if_else(weekwise > ymd("2019-11-01"), readmission, as.numeric(NA))) %>% 
  mutate(readmission_m = if_else(weekwise > ymd("2019-11-01"), readmission_m, as.integer(NA)))
  
# Getting turnover using the discharge dates.
weekly_turnover <- data %>% 
  group_by(weekwise_discharge) %>% 
  summarise(turnover = 100*n()/(mean(beds)*7))

# Joining turnover to weekly data
weekly <- left_join(weekly, weekly_turnover, by = c("weekwise" = "weekwise_discharge")) %>% 
  # rearranging the variables so that it does not cause problems later on.
  select(weekwise, number_of_admissions, number_of_beds, occupancy, turnover, everything())

# adding a variable to indicate which monthly category each row falls into. I need to add the factor bit to make sure 
# the month-year combinations get ordered properly.
levels <- c("2019", "Jan 2020", "Feb 2020", "Mar 2020", "Apr 2020", 
            "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020")

weekly <- weekly %>% 
  mutate(monthwise = if_else(year(weekwise) == 2019, "2019", 
                             paste0(as.character(lubridate::month(weekwise, label = TRUE)), " 2020"))) %>% 
  mutate(monthwise = factor(x = monthwise, levels = levels))

save(weekly, file = "data/01_weekly.RData")
save(data, file = "data/02_cleaned_data.RData")