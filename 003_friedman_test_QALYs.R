#  friedman tests to test difference between QALYs in repeated measures in groups
# 20.9.24

# Split the data by group
control_data <- filter(long_data, Assigned == "Control")
intervention_data <- filter(long_data, Assigned == "Intervention")

# Participants with missing data excluded
control_data_clean <- control_data %>%
  group_by(Participant) %>%
  filter(all(complete.cases(QALY))) %>%
  ungroup()

intervention_data_clean <- intervention_data %>%
  group_by(Participant) %>%
  filter(all(complete.cases(QALY))) %>%
  ungroup()

# Run the Friedman test for the control group
control_friedman <- friedman.test(QALY ~ Time | Participant, data = control_data_clean)
print(control_friedman)

# Run the Friedman test for the intervention group
intervention_friedman <- friedman.test(QALY ~ Time | Participant, data = intervention_data_clean)
print(intervention_friedman)




