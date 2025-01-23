# descriptive statistics
# 20.9.24

# Reshape the data to long format
long_data <- data %>%
  pivot_longer(cols = starts_with("EQ5D"), 
               names_to = "Time", 
               values_to = "QALY")

# Calculate descriptive statistics for each group at each time point
descriptive_stats <- long_data %>%
  group_by(Time, Assigned) %>%
  summarise(
    count = n(),
    mean_QALY = mean(QALY, na.rm = TRUE),
    median_QALY = median(QALY, na.rm = TRUE),
    sd_QALY = sd(QALY, na.rm = TRUE),
    Q1_QALY = quantile(QALY, na.rm = TRUE, probs = 0.25),
    Q3_QALY = quantile(QALY, na.rm = TRUE, probs = 0.75)
  ) %>%
  mutate(
    mean_QALY = round(mean_QALY, 2),
    median_QALY = round(median_QALY, 2),
    sd_QALY = round(sd_QALY, 2),
    Q1_QALY = round(Q1_QALY, 2),
    Q3_QALY = round(Q3_QALY, 2),
    IQR_QALY = paste0(Q1_QALY, "-", Q3_QALY)
         ) %>%
  mutate(
    mean_SD = paste0(mean_QALY, " (", sd_QALY, ")"),
    median_IQR = paste0(median_QALY, " (", IQR_QALY, ")")
  ) %>%  select(1:2, 9:10)

# Create a flextable
descriptive_flextable <- descriptive_stats %>%
  flextable() %>%
  set_caption(caption = "Descriptive Statistics for QALYs by Group and Time Point") %>%
  theme_vanilla()  # Optional: Apply a vanilla theme for better formatting

# Display the table in R to preview
descriptive_flextable

# Create a Word document and add the flextable
doc <- read_docx() %>%
  body_add_flextable(descriptive_flextable) %>%
  body_add_par("Table: Descriptive Statistics") # Optional: Add caption

# Save the document
print(doc, target = "output/descriptive_statistics_QALYs_by_group.docx")


# create overall plots

# Arrange data
mean_qaly_data_overall <- long_data %>%
  group_by(Assigned, Time) %>%
  summarise(mean_QALY = mean(QALY, na.rm = TRUE))

mean_qaly_data_overall$Time <- factor(mean_qaly_data_overall$Time, 
                              levels = c("EQ5D_BL", "EQ5D_12WK", "EQ5D_6M"))

# Create the plot
ggplot(mean_qaly_data_overall, aes(x = Time, y = mean_QALY, group = Assigned, color = Assigned)) +
  geom_line(size = 1) +      # Line for each group
  geom_point(size = 3) +     # Points at each time point
  labs(title = "Mean QALYs at each time point in control and intervention groups",
       x = "Time Point",
       y = "Mean QALYs",
       color = "Group") +
  scale_x_discrete(labels = c("Baseline", "12 weeks", "6 months")) +
  theme_minimal()

long_data$Time <- factor(long_data$Time, levels = c("EQ5D_BL", "EQ5D_12WK", "EQ5D_6M"), 
                         labels = c("Baseline", "12 weeks", "6 months"))

ggplot(long_data, aes(x = Time, y = QALY, fill = Assigned)) +
  geom_boxplot() +
  labs(title = "Distribution of QALYs at each time point in control and intervention groups", 
       x = "Time Point", 
       y = "QALYs") +
  scale_x_discrete(labels = c("Baseline", "12 weeks", "6 months")) +
  theme_minimal()

#distribution of QALYs

hist(long_data$QALY, main = "Histogram of QALYs", xlab = "QALY", breaks = 10)
shapiro.test(long_data$QALY)

library(lme4)
library(ggplot2)

# Fit a mixed-effects model
model <- lmer(QALY ~ Assigned * Time + (1 | Participant), data = long_data)

# Extract residuals
residuals <- resid(model)

# Plot a Q-Q plot to check for normality
qqnorm(residuals)
qqline(residuals)

# Alternatively, use a histogram of residuals
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")

#plot(density(long_dataplot(density(long_data$QALY), main = "Density Plot of QALYs", xlab = "QALY")
boxplot(long_data$QALY, main = "Boxplot of QALYs", ylab = "QALY")


shapirowilk <- long_data %>%
  group_by(Assigned, Time) %>%
  summarise(shapiro_p = shapiro.test(QALY)$p.value)

# total unique participants 
# control
length(unique(long_data$Participant[long_data$Assigned == "Control"]))
# intervention
length(unique(long_data$Participant[long_data$Assigned == "Intervention"]))


# total unique participants 
# control
length(unique(control_data_clean$Participant))
# intervention
length(unique(intervention_data_clean$Participant))



