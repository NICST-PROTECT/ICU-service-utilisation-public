# Loading the data in.
load("data/02_cleaned_data.RData")


# Creating a variable to indicate if the data is pandemic or pre-pandemic.
data <- data %>% 
  mutate(pandemic = if_else(monthwise < "2020-03-01", "Oct 2019 to Feb 2020", "Mar - Sep 2020"))

output <- cbind("Variable", "Value")

# Number of patients
new_value <- nrow(data)
new_line <- cbind("Number of admissions", new_value)
output <- rbind(output, new_line)

# years of admission
new_value <- nrow(data %>% filter(year(weekwise) == "2019"))
new_line <- cbind("Number of 2019 admissions", new_value)
output <- rbind(output, new_line)

new_value <- nrow(data %>% filter(year(weekwise) == "2020"))
new_line <- cbind("Number of 2020 admissions", new_value)
output <- rbind(output, new_line)

# pandemic vs pre-pandemic.
new_value <- nrow(data %>% filter(pandemic == "Oct 2019 to Feb 2020"))
new_line <- cbind("Pre pandemic Oct 2019 to Feb 2020", new_value)
output <- rbind(output, new_line)

new_value <- nrow(data %>% filter(pandemic == "Mar - Sep 2020"))
new_line <- cbind("Pandemic Mar - Sep 2020", new_value)
output <- rbind(output, new_line)

# Number of units
new_value <- nrow(data %>% distinct(unitId))
new_line <- cbind("Number of units", new_value)
output <- rbind(output, new_line)

# Reading the unit data in and getting a list of the units.
units <- data %>% 
  group_by(unitId) %>% 
  tally()

names <- read_csv("data/unit_names.csv")

units <- left_join(units, names, by = "unitId") %>% 
  select(-X1)

save(data, file = "data/02_cleaned_data.RData")
# Writing the output out. 
wb <- loadWorkbook("output/01_output.xlsx")
addWorksheet(wb, "2_summarise_cohort")
writeData(wb, sheet = "2_summarise_cohort", x = output, borders = "columns")
setColWidths(wb, "2_summarise_cohort", cols = 1:6, widths = "auto")
saveWorkbook(wb, "output/01_output.xlsx", overwrite = TRUE)