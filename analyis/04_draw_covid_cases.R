# Drawing graph of ICU numbers and covid cases.
covid_cases <- read.xlsx("data/Phases_timeline .xlsx") %>% 
  mutate(date = excel_numeric_to_date(date))

# getting weekly number of ICU admissions, along with unit occupancy.
load("data/01_weekly.RData") 
weekly <- weekly %>% 
  filter(weekwise > as.Date("2020-01-01")) 

# Drawing a graph with a dual axis. 
# coefficent of multiplication for the second axis
coef <- max(covid_cases$cases)/max(weekly$number_of_admissions)
# There's no space for both the start and end labels. So making it only the starts.
phase_labels <- covid_cases %>% 
  filter(!str_detect(phase, "End")) %>% 
  filter(phase != "Janta curfew") %>% 
  mutate(phase = str_replace(phase, "Start", ""))

ggplot() +
  geom_line(data = weekly, aes(x = weekwise, y = number_of_admissions, colour = "Number of ICU admissions")) + 
  geom_line(data = covid_cases, aes(x = date, y = cases/coef, colour = "Number of covid cases")) + # Divide by coef to get the same range as the number of admissions
  scale_color_manual(values = c("Number of ICU admissions" = "darkblue", "Number of covid cases" = "darkgreen")) +
  scale_y_continuous(
   # Features of the first axis
    name = "Weekly number of admissions to ICU",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coef, name = "Number of covid cases recorded")
  ) + 
  geom_vline(data = phase_labels, mapping = aes(xintercept = date), size = 0.5, colour = "darkorange", alpha = 0.75) + 
  geom_text(data = phase_labels, mapping = aes(x = date, label = phase, y = 0.5), angle = 80, hjust = 0, 
            vjust = 1, check_overlap = TRUE, colour = "darkorange", alpha = 0.75, fontface = "italic") + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "months") +  
  xlab("Date") + 
  theme_classic() + 
  theme(legend.position = "bottom",legend.title = element_blank(), 
        text = element_text(size = 8),
        axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("output/04_phases_of_lockdown.png")