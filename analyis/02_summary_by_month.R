# Loading the data in.
load("data/01_weekly.RData")

# Defining some functions I can use to summarise the data more nicely. 
get_median_iqr <- function(data, variable, name, output) {
  # I get the median and IQR for whichever variable, paste them together as characters, and save it all the the output 
  # data frame.
  
  by_month <- data %>% 
    group_by(monthwise) %>% 
    summarise(clean = paste0(round(median(get(variable), na.rm = TRUE),2 ), " (", 
                             round(IQR(get(variable), na.rm = TRUE), 2), ")")) %>% 
    select(clean) %>% 
    t()
  
  # Now, putting the Jan and Feb medians and IQRs on the end.
  jan_feb <- data %>% 
    filter(monthwise %in% c("Jan 2020", "Feb 2020")) %>% 
    summarise(clean = paste0(round(median(get(variable), na.rm = TRUE),2 ), " (", 
                             round(IQR(get(variable), na.rm = TRUE), 2), ")")) %>% 
    select(clean) %>% 
    t()
  
  # Filling the first variable in with the row label.
  by_month <- c(name, by_month, jan_feb)
  
  # Renaming the variables to let the 2 data frames stack on top of each other.
  output <- rbind(output, by_month, stringsAsFactors = FALSE)
  output
  colnames(output) <- c("Variable", levels)
  output
}

get_percentage_missingness <- function(data, variable, name, output) {
  # I get the median and IQR for whichever variable, paste them together as characters, and save it all the the output 
  # data frame.
  
  by_month <- data %>% 
    group_by(monthwise) %>% 
    summarise(clean = paste0(sum(get(variable), na.rm = TRUE), " (", 
                             round(100*sum(get(variable), na.rm = TRUE)/sum(number_of_admissions), 2), ")")) %>% 
    select(clean) %>% 
    t()
  
  # Now, putting the Jan and Feb medians and IQRs on the end.
  jan_feb <- data %>% 
    filter(monthwise %in% c("Jan 2020", "Feb 2020")) %>% 
    summarise(clean = paste0(sum(get(variable), na.rm = TRUE), " (", 
                             round(100*sum(get(variable), na.rm = TRUE)/sum(number_of_admissions), 2), ")")) %>% 
    select(clean) %>% 
    t()
  
  # Filling the first variable in with the row label.
  by_month <- c(name, by_month, jan_feb)
  
  # Renaming the variables to let the 2 data frames stack on top of each other.
  output <- rbind(output, by_month, stringsAsFactors = FALSE)
  output
  colnames(output) <- c("Variable", levels)
  output
}

################## Getting monthly summaries.

# This is the main output table. It's empty for now. 
levels <- c("2019", "Jan 2020", "Feb 2020", "Mar 2020", "Apr 2020", 
            "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020", "Jan and Feb 2020")
output <- data.frame(matrix(ncol = 1 + length(levels), nrow = 0))
colnames(output) <- c("Variable", levels)

# I'm going to fill the table in with the summaries of each variable. 
output <- rbind(output, c("General Variables", rep("", length(levels))), stringsAsFactors = FALSE)
output <- get_median_iqr(weekly, "number_of_admissions", "Number of admissions", output)
output <- get_median_iqr(weekly, "occupancy", "% Unit occupancy ", output)
output <- get_median_iqr(weekly, "turnover", "% Unit turnover", output)
output <- get_median_iqr(weekly, "bed_availability", "Number of beds free per unit per day", output)
output <- get_median_iqr(weekly, "mechanically_vent", "% Mechanical ventilation on admission", output)
output <- get_median_iqr(weekly, "niv", "% Non invasive ventilation on admission", output)
output <- get_median_iqr(weekly, "cardiovascular_support", "% Cardiovascular support on admission", output)
output <- get_median_iqr(weekly, "renal_replacement", "% Renal replacement on admission", output)
output <- get_median_iqr(weekly, "surgical_total", "% All surgical admissions", output)
output <- get_median_iqr(weekly, "surgical_planned", "% Planned surgical admissions", output)
output <- get_median_iqr(weekly, "surgical_unplanned", "% Unplanned surgical admissions", output)
output <- get_median_iqr(weekly, "non_surgical", "% Non surgical admissions", output)
output <- get_median_iqr(weekly, "apache_II", "Median APACHE II", output)

output <- rbind(output, c("Outcome Variables", rep("", length(levels))), stringsAsFactors = FALSE)
output <- get_median_iqr(weekly, "discharged_dead", "% Dead", output)
output <- get_median_iqr(weekly, "discharged_alive", "% Alive", output)
output <- get_median_iqr(weekly, "discharged_home", "% Discharged home", output)
output <- get_median_iqr(weekly, "discharged_ward", "% Discharged ward", output)
output <- get_median_iqr(weekly, "discharged_icu", "% Discharged ICU", output)
output <- get_median_iqr(weekly, "discharged_transfer", "% Transferred to another hospital", output)
output <- get_median_iqr(weekly, "discharged_ama", "% Discharged against medical advice", output)
output <- get_median_iqr(weekly, "discharged_other", "% Discharged Other", output)
output <- get_median_iqr(weekly, "readmission", "% Readmitted to ICU", output)
output <- get_median_iqr(weekly, "los", "Median Length of Stay (days)", output)
output <- get_median_iqr(weekly, "time_to_icu", "Median time from hospital to ICU admission (hours)", output)

output <- rbind(output, c("Missingness", rep("", length(levels))), stringsAsFactors = FALSE)
output <- get_percentage_missingness(weekly, "mechanically_vent_m", "n (%) missing Mechanical ventilation", output)
output <- get_percentage_missingness(weekly, "cardiovascular_support_m", "n (%) missing cardiovascular support", output)
output <- get_percentage_missingness(weekly, "renal_replacement_m", "n (%) missing renal replacement", output)
output <- get_percentage_missingness(weekly, "surgical_m", "n (%) missing surgical status", output)
output <- get_percentage_missingness(weekly, "planned_m", "n (%) missing planned status", output)
output <- get_percentage_missingness(weekly, "apache_m", "n (%) missing APACHE score", output)

output <- rbind(output, c("Missingness outcome variables", rep("", length(levels))), stringsAsFactors = FALSE)
output <- get_percentage_missingness(weekly, "discharge_status_m", "n (%) missing discharge status", output)
output <- get_percentage_missingness(weekly, "discharge_destination_m", "n (%) missing discharge destination", output)
output <- get_percentage_missingness(weekly, "readmission_m", "n (%) missing readmission", output)
output <- get_percentage_missingness(weekly, "time_to_icu_m", "n (%) missing hospital admission date", output)

# writing the output data frame to an excel file
write.xlsx(output, file = "output/01_output.xlsx", borders = c("all"), colWidths = c("auto"), 
           na.string = "-")