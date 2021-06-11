# Getting pre-and post pandemic summaries and doing non-parametric stats tests to compare them. 
# Pre-pandemic is defined as jan and feb, and post pandemic is after.

load("data/01_weekly.RData")

# Getting selecting only the data we need for the pandemic comparision.
pandemic_comparison <- weekly %>% 
  mutate(pandemic = if_else(monthwise %in% c("2019", "Jan 2020", "Feb 2020"), "Oct 2019 to Feb 2020", "Mar - Sep 2020"))

# making the pandemic variable a factor so that the order of variables is preserved.
levels <- c("Oct 2019 to Feb 2020", "Mar - Sep 2020")
pandemic_comparison$pandemic = factor(pandemic_comparison$pandemic, levels = levels)

# Defining some functions I can use to summarise the data more nicely. 
get_median_iqr_test <- function(data, variable, name, output) {
  # I get the median and IQR for whichever variable, paste them together as characters, and save it all the the output 
  # data frame.
  
  by_month <- data %>% 
    group_by(pandemic) %>% 
    summarise(clean = paste0(round(median(get(variable), na.rm = TRUE),2 ), " (", 
                             round(quantile(get(variable), probs = 0.25, na.rm = TRUE), 2), ", ",
                             round(quantile(get(variable), probs = 0.75, na.rm = TRUE)), ")")) %>% 
    select(clean) %>% 
    t()
  
  # Now, putting the pre-pandemic data on the end.
  test <- data %>% 
    summarise(clean = format(round(wilcox.test(get(variable) ~ pandemic)$p.value, 5), nsmall = 5, scientific = FALSE)) %>% 
    select(clean) %>% 
    t()
  
  # Filling the first variable in with the row label.
  by_month <- c(name, by_month, test)
  
  # Renaming the variables to let the 2 data frames stack on top of each other.
  output <- rbind(output, by_month, stringsAsFactors = FALSE)
  output
  colnames(output) <- c("Variable", levels)
  output
}

################## Getting monthly summaries.

# This is the main output table. It's empty for now. 
levels <- c("Oct 2019 to Feb 2020", "Mar - Sep 2020", "p-value")
output <- data.frame(matrix(ncol = 1 + length(levels), nrow = 0))
colnames(output) <- c("Variable", levels)

# I'm going to fill the table in with the summaries of each variable. 
output <- rbind(output, c("General Variables", rep("", length(levels))), stringsAsFactors = FALSE)
output <- get_median_iqr_test(pandemic_comparison, "number_of_admissions", "Number of admissions", output)
output <- get_median_iqr_test(pandemic_comparison, "occupancy", "% Unit occupancy ", output)
output <- get_median_iqr_test(pandemic_comparison, "turnover", "% Unit turnover", output)
output <- get_median_iqr_test(pandemic_comparison, "bed_availability", "Number of beds free per unit per day", output)
output <- get_median_iqr_test(pandemic_comparison, "mechanically_vent", "% Mechanical ventilation on admission", output)
output <- get_median_iqr_test(pandemic_comparison, "niv", "% Non invasive ventilation on admission", output)
output <- get_median_iqr_test(pandemic_comparison, "cardiovascular_support", "% Cardiovascular support on admission", output)
output <- get_median_iqr_test(pandemic_comparison, "surgical_total", "% All surgical admissions", output)
output <- get_median_iqr_test(pandemic_comparison, "surgical_planned", "% Planned surgical admissions", output)
output <- get_median_iqr_test(pandemic_comparison, "surgical_unplanned", "% Unplanned surgical admissions", output)
output <- get_median_iqr_test(pandemic_comparison, "non_surgical", "% Non surgical admissions", output)
output <- get_median_iqr_test(pandemic_comparison, "apache_II", "APACHE II on admission", output)


output <- rbind(output, c("Outcome Variables", rep("", length(levels))), stringsAsFactors = FALSE)
output <- get_median_iqr_test(pandemic_comparison, "discharged_dead", "% Dead", output)
output <- get_median_iqr_test(pandemic_comparison, "discharged_alive", "% Alive", output)
output <- get_median_iqr_test(pandemic_comparison, "discharged_home", "% Discharged home", output)
output <- get_median_iqr_test(pandemic_comparison, "discharged_ward", "% Discharged ward", output)
output <- get_median_iqr_test(pandemic_comparison, "discharged_icu", "% Discharged ICU", output)
output <- get_median_iqr_test(pandemic_comparison, "discharged_transfer", "% Transferred to another hospital", output)
output <- get_median_iqr_test(pandemic_comparison, "discharged_ama", "% Discharged against medical advice", output)
output <- get_median_iqr_test(pandemic_comparison, "discharged_other", "% Discharged Other", output)
output <- get_median_iqr_test(pandemic_comparison, "readmission", "% Readmitted to ICU", output)
output <- get_median_iqr_test(pandemic_comparison, "los", "Median Length of Stay (days)", output)

# We also want the risk ratio and confidence interval for survival.
# Need to use the raw data
load("data/02_cleaned_data.RData")

# creating a table 
pandemic_dead <- data %>% 
  filter(discharge.discharge_status == "Dead" & pandemic == "Mar - Sep 2020") %>% 
  tally() %>% 
  pull()

pandemic_alive <- data %>% 
  filter(discharge.discharge_status == "Alive" & pandemic == "Mar - Sep 2020") %>% 
  tally() %>% 
  pull()

pre_pandemic_dead <- data %>% 
  filter(discharge.discharge_status == "Dead" & pandemic == "Oct 2019 to Feb 2020") %>% 
  tally() %>% 
  pull()

pre_pandemic_alive <- data %>% 
  filter(discharge.discharge_status == "Alive" & pandemic == "Oct 2019 to Feb 2020") %>% 
  tally() %>% 
  pull()

contigency <- data.frame(c(pre_pandemic_alive, pandemic_alive), c(pre_pandemic_dead, pandemic_dead))

riskratio(c(c(pre_pandemic_alive, pandemic_alive), c(pre_pandemic_dead, pandemic_dead)))

# Writing the output out. 
wb <- loadWorkbook("output/01_output.xlsx")
addWorksheet(wb, "3_compare_pandemic_2019")
writeData(wb, sheet = "3_compare_pandemic_2019", x = output, borders = "columns")
setColWidths(wb, "3_compare_pandemic_2019", cols = 1:6, widths = "auto")
saveWorkbook(wb, "output/01_output.xlsx", overwrite = TRUE)

############# Doing a separate version where pandemic is just the lockdown, not the bit after that.
pandemic_comparison <- pandemic_comparison %>% 
  filter(monthwise %in% c("Mar 2020", "Apr 2020", "May 2020") | pandemic == "Oct 2019 to Feb 2020")

# This is the main output table. It's empty for now. 
levels <- c("Oct 2019 to Feb 2020", "Mar - May 2020", "p-value")
output <- data.frame(matrix(ncol = 1 + length(levels), nrow = 0))
colnames(output) <- c("Variable", levels)

# I'm going to fill the table in with the summaries of each variable. 
output <- rbind(output, c("General Variables", rep("", length(levels))), stringsAsFactors = FALSE)
output <- get_median_iqr_test(pandemic_comparison, "number_of_admissions", "Number of admissions", output)
output <- get_median_iqr_test(pandemic_comparison, "occupancy", "% Unit occupancy ", output)
output <- get_median_iqr_test(pandemic_comparison, "turnover", "% Unit turnover", output)
output <- get_median_iqr_test(pandemic_comparison, "bed_availability", "Number of beds free per unit per day", output)
output <- get_median_iqr_test(pandemic_comparison, "mechanically_vent", "% Mechanical ventilation on admission", output)
output <- get_median_iqr_test(pandemic_comparison, "niv", "% Non invasive ventilation on admission", output)
output <- get_median_iqr_test(pandemic_comparison, "cardiovascular_support", "% Cardiovascular support on admission", output)
output <- get_median_iqr_test(pandemic_comparison, "surgical_total", "% All surgical admissions", output)
output <- get_median_iqr_test(pandemic_comparison, "surgical_planned", "% Planned surgical admissions", output)
output <- get_median_iqr_test(pandemic_comparison, "surgical_unplanned", "% Unplanned surgical admissions", output)
output <- get_median_iqr_test(pandemic_comparison, "non_surgical", "% Non surgical admissions", output)
output <- get_median_iqr_test(pandemic_comparison, "apache_II", "APACHE II on admission", output)


output <- rbind(output, c("Outcome Variables", rep("", length(levels))), stringsAsFactors = FALSE)
output <- get_median_iqr_test(pandemic_comparison, "discharged_dead", "% Dead", output)
output <- get_median_iqr_test(pandemic_comparison, "discharged_alive", "% Alive", output)
output <- get_median_iqr_test(pandemic_comparison, "discharged_home", "% Discharged home", output)
output <- get_median_iqr_test(pandemic_comparison, "discharged_ward", "% Discharged ward", output)
output <- get_median_iqr_test(pandemic_comparison, "discharged_icu", "% Discharged ICU", output)
output <- get_median_iqr_test(pandemic_comparison, "discharged_transfer", "% Transferred to another hospital", output)
output <- get_median_iqr_test(pandemic_comparison, "discharged_ama", "% Discharged against medical advice", output)
output <- get_median_iqr_test(pandemic_comparison, "discharged_other", "% Discharged Other", output)
output <- get_median_iqr_test(pandemic_comparison, "readmission", "% Readmitted to ICU", output)
output <- get_median_iqr_test(pandemic_comparison, "los", "Median Length of Stay (days)", output)

# Writing the output out. 
wb <- loadWorkbook("output/01_output.xlsx")
addWorksheet(wb, "4_compare_lockdown_2019")
writeData(wb, sheet = "4_compare_lockdown_2019", x = output, borders = "columns")
setColWidths(wb, "4_compare_lockdown_2019", cols = 1:6, widths = "auto")
saveWorkbook(wb, "output/01_output.xlsx", overwrite = TRUE)
