# Loading the data in.
load("data/01_weekly.RData")

####################### Fitting a loess regresssion.
# Listing the variables we will want on the graph
graph_variables <- c("number_of_admissions", "number_of_beds", "occupancy", "turnover",
                     "bed_availability", 
                     "mechanically_vent", "niv", "cardiovascular_support", "surgical_total",
                     "surgical_planned",
                     "apache_II", "discharged_dead", "discharged_home", "discharged_ward",
                     "discharged_icu", "discharged_transfer", "discharged_ama", "discharged_other",
                     "los", "readmission")
data <- weekly %>% 
  select(weekwise, all_of(graph_variables))

# For the loess function, creating an index variable so that the order of the data is clear. 
# Since there's no explanatory variable, I will need to use it in the loess function later.
data <- data %>% 
  arrange(weekwise) %>% 
  mutate(index = 1:nrow(data))

### Getting predictions and standard errors for the predictions for all of the variables. 
# Saving those bits to separate datasets, and joining them together to get one large dataset of all the variables.
# Doing readmisison separately, since it has NAs.
data_preds <- 
  data %>% 
  mutate_at(vars(!matches(c("weekwise", "index", "readmission"))), ~loess(. ~ index, data = data)$fitted) %>% 
  rename_at(vars(!matches(c("weekwise", "index", "readmission"))), ~paste0(., "_pred")) %>% 
  select(!c(readmission))


data_sds <- 
  data %>% 
  mutate_at(vars(!matches(c("weekwise", "index", "readmission"))), ~predict(loess(. ~ index, data = data), se = TRUE)$se.fit) %>% 
  rename_at(vars(!matches(c("weekwise", "index", "readmission"))), ~paste0(., "_se")) %>% 
  select(!c(readmission))

# Dealing with readmission because it has missing data
deal_with_nas <- data %>% 
  select(c("weekwise", "index", "readmission")) %>% 
  na.omit() %>% 
  mutate(readmission_pred = loess(readmission ~ index, data = data)$fitted) %>% 
  mutate(readmission_se = predict(loess(readmission ~ index, data = data), se = TRUE)$se.fit) %>% 
  select(!c(readmission))

# Labelling variables from the original data so they don't get mixed up.
data <- 
  data %>% 
  rename_at(vars(!matches(c("weekwise", "index"))), ~paste0(., "_orig"))
  
data_all <- 
  left_join(data, data_preds, by = c("weekwise", "index")) %>% 
  left_join(., data_sds, by = c("weekwise", "index")) %>% 
  left_join(., deal_with_nas, by = c("weekwise", "index"))

### The data needs to be in long format to be plotted. 
data_all <- as.data.table(data_all)

data_long = melt(data_all, id.vars = c("weekwise", "index"),
                    measure.vars = patterns("_orig", "_pred", "_se"), value.name = c("orig", "pred", "se"))
data_long[, variable := factor(variable, levels = 1:length(graph_variables), labels = graph_variables)]

# saving the full datasets of predictions and standard errors for later. 
save(data_all, file = "data/03_loess_predictions.RData")

######### plotting the predicted and actual values of the variables per week.
input_graph_vars <- c("number_of_admissions", "occupancy", "turnover", "bed_availability",
                 "mechanically_vent", "niv", "cardiovascular_support", "surgical_total",
                 "surgical_planned", "apache_II")

output_graph_vars <- c("discharged_dead", "discharged_home", "discharged_ward", "discharged_icu",
                       "discharged_transfer", 
                       "discharged_ama", "discharged_other", "los", "readmission")

## Plotting the same graph for both inputs and outputs.
data_long_input <- data_long %>% 
  filter(variable %in% input_graph_vars)

data_long_output <- data_long %>% 
  filter(variable %in% output_graph_vars)

# Inputs.
# Creating nice lables
nice_names <- as_labeller(c(`number_of_admissions` = "Admissions (N)", `occupancy` = "Unit occupancy (%)", 
                            `turnover` = "Unit turnover (%)", `bed_availability` = "Beds free per unit per day (N)", 
                            `mechanically_vent` = "Mechanical ventilation on admission (%)", `niv` = "Non-invasive ventilation on admission (%)",
                            `cardiovascular_support` = "Cardiovascular support on admission (%)", `surgical_total` = "Surgical admissions (%)",
                            `surgical_planned` = "Planned surgical admissions (%)", `apache_II` = "APACHE II score on admission "))

ggplot() + 
  geom_point(data = data_long_input, aes(x = weekwise, y = orig), size = 0.5) + 
  geom_line(data = data_long_input, aes(x = weekwise, y = pred), colour = "darkblue") +
  geom_errorbar(data = data_long_input, aes(x = weekwise, ymin = pred - se, ymax = pred + se), width = .2, colour = "darkblue") +
  facet_wrap(~variable, ncol = 3, scales = "free", labeller = nice_names) + 
  xlab("") +
  ylab("Weekly trends") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "months") + 
  geom_vline(xintercept = ymd("2020-03-11"), colour = "blue") + 
  geom_vline(xintercept = ymd("2020-03-23"), colour = "red") + 
  geom_vline(xintercept = ymd("2020-06-01"), colour = "red") + 
  theme_classic() + 
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 60, hjust = 1)) 
ggsave("output/02_loess_trends_inputs.png", width = 9, height = 10)

# Outputs.
# Creating nice lables
nice_names <- as_labeller(c(`discharged_dead` = "Dead (%)", `discharged_home` = "Discharged home (%)", 
                            `discharged_ward` = "Discharged ward (%)", `discharged_icu` = "Discharged ICU (%)", 
                            `discharged_transfer` = "Transferred to another hospital (%)", `discharged_ama` = "Discharged against medical advice (%)",
                            `discharged_other` = "Discharged other (%)", `los` = "Median Length of Stay (days)",
                            `readmission` = "Readmitted to ICU (%)"))

ggplot() + 
  geom_point(data = data_long_output, aes(x = weekwise, y = orig), size = 0.5) + 
  geom_line(data = data_long_output, aes(x = weekwise, y = pred), colour = "darkblue") +
  geom_errorbar(data = data_long_output, aes(x = weekwise, ymin = pred - se, ymax = pred + se), width = .2, colour = "darkblue") +
  facet_wrap(~variable, ncol = 3, scales = "free", labeller = nice_names) + 
  xlab("") +
  ylab("Weekly trends") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "months") + 
  geom_vline(xintercept = ymd("2020-03-11"), colour = "blue") + 
  geom_vline(xintercept = ymd("2020-03-23"), colour = "red") + 
  geom_vline(xintercept = ymd("2020-06-01"), colour = "red") + 
  theme_classic() + 
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 60, hjust = 1)) 
ggsave("output/03_loess_trends_outputs.png", width = 9, height = 10)
