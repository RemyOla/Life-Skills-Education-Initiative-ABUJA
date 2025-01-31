data<-Case_Study_C_LIID_pre_LSE_sessions_self_efficacy_AIM_and_IAM
str(data)
summary(data)
install.packages(c("ggplot2","dplyr","tidyr"))
library(ggplot2)
library(dplyr)
library(tidyr)

# Likert response mapping
likert_mapping <- list(
  "Not sure at all" = 1,
  "A little sure" = 2,
  "Somewhat sure" = 3,
  "Quite sure" = 4,
  "Very sure" = 5
)

# Apply mapping to self-efficacy question columns
self_efficacy_columns <- names(data)[grep("^\\d", names(data))]  # Select all question columns
data[self_efficacy_columns] <- lapply(data[self_efficacy_columns], function(x) recode(x, !!!likert_mapping))

# Define domains for scoring
domains <- list(
  essence = c("1.1. I can find meaning in my life, even when things are hard.",
              "1.2. I can set goals that are important to me.",
              "1.3. I can use my beliefs to help me when I face problems."),
  adversities = c("2.1. I can handle stress when life is difficult.",
                  "2.2. I can break big problems into smaller, easier parts.",
                  "2.3. I can recognize when I'm feeling bad and do something about it."),
  resilience = c("3.1. I can recover when things go wrong.",
                 "3.2. I can use my strengths to overcome challenges.",
                 "3.3. I can stay hopeful even when problems last a long time."),
  decision_making = c("12.1. I can gather good information before making big decisions.",
                      "12.2. I can think about how my choices will affect things now and later.",
                      "12.3. I can learn from my choices to make better ones next time.")
)

# Calculate domain scores
for (domain in names(domains)) {
  data[[paste0(domain, "_score")]] <- rowMeans(data[domains[[domain]]], na.rm = TRUE)
}

# Prepare data for plotting
# Gather domain scores into long format for faceting
plot_data <- data %>%
  pivot_longer(
    cols = ends_with("_score"),  # Select all domain score columns
    names_to = "Domain",
    values_to = "Score"
  )
View(plot_data)
# Create a faceted boxplot
ggplot(plot_data, aes(x = `Camp Name`, y = Score, fill = `Camp Name`)) +
  geom_boxplot() +
  facet_wrap(~ Domain, scales = "free_y") +  # Create a panel for each domain
  labs(
    title = "Comparison of LSE Outcomes Across Delivery Models",
    x = "Delivery Model (Camp Name)",
    y = "Domain Score"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )
ggplot(plot_data, aes(x = `Camp Name`, y = Score, fill = `Camp Name`)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +  # Bar plot showing mean score
  facet_wrap(~ Domain, scales = "free_y") +  # Create a panel for each domain
  labs(
    title = "Comparison of LSE Outcomes Across Delivery Models",
    x = "Delivery Model (Camp Name)",
    y = "Mean Domain Score"
  ) +  # Add the labs() function
  scale_fill_manual(values = c("#1D3557", "#6B4F31")) +  # Dark blue and earthy brown in hex codes
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )
plot_data
str(plot_data)
View(plot_data)
# Load necessary libraries
library(lme4)        # For mixed-effects models
library(dplyr)       # For data manipulation
library(tidyr)       # For tidying data
library(ggplot2)     # For visualizations
library(lmerTest)    # For p-values in mixed effects models

# Load your dataset
# dataset <- read.csv("path/to/your/dataset.csv")  # Uncomment to load your dataset

# Make sure categorical variables are factors
plot_data$`Camp Name` <- as.factor(plot_data$`Camp Name`)
plot_data$Domain <- as.factor(plot_data$Domain)

# Create a new variable for the intervention measures (AIM & IAM)
# Assuming AIM and IAM variables are directly related to the responses
# You can combine the variables if necessary (e.g., means, sums, or different aggregation)

# Example for AIM
plot_data$AIM_Score <- rowMeans(plot_data[, c("The Life Skills Education.LSE.intervention.meets.my.approval",
                                          "The.Life.Skills.Education.LSE.intervention.is.appealing.to.me",
                                          "I.like.the.Life.Skills.Education.LSE.intervention",
                                          "I.welcome.the.Life.Skills.Education.LSE.intervention")], na.rm = TRUE)

# Example for IAM
dataset$IAM_Score <- rowMeans(dataset[, c("The.Life.Skills.Education.LSE.intervention.seems.fitting.for.internally.displaced.persons",
                                          "The.Life.Skills.Education.LSE.intervention.seems.suitable.for.our.community",
                                          "The.Life.Skills.Education.LSE.intervention.seems.applicable.to.our.mental.health.needs",
                                          "The.Life.Skills.Education.LSE.intervention.seems.like.a.good.match.for.our.situation")], na.rm = TRUE)

# Create a mixed model for AIM (dependent variable AIM_Score)
aim_model <- lmer(AIM_Score ~ Essence + Adversities + Resilience + National.Identity + 
                    National.Purpose + Respect.for.Diversity + Creativity + Critical.Thinking + 
                    Problem.Solving + Cooperation + Negotiation + Decision.Making + (1 | CampName), 
                  data = dataset)

# Display the summary of AIM model
summary(aim_model)

# Create a mixed model for IAM (dependent variable IAM_Score)
iam_model <- lmer(IAM_Score ~ Essence + Adversities + Resilience + National.Identity + 
                    National.Purpose + Respect.for.Diversity + Creativity + Critical.Thinking + 
                    Problem.Solving + Cooperation + Negotiation + Decision.Making + (1 | CampName), 
                  data = dataset)

# Display the summary of IAM model
summary(iam_model)

# Diagnostics - Check residuals and random effects
# Plot residuals for AIM model
plot(residuals(aim_model))
qqnorm(residuals(aim_model))
qqline(residuals(aim_model))

# Plot residuals for IAM model
plot(residuals(iam_model))
qqnorm(residuals(iam_model))
qqline(residuals(iam_model))

# Optional: Visualize the coefficients for AIM model
coef_plot_aim <- ggplot(as.data.frame(coef(aim_model)), aes(x = rownames(.), y = `(Intercept)`)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "AIM Model Coefficients", x = "Variables", y = "Coefficients")

print(coef_plot_aim)

# Optional: Visualize the coefficients for IAM model
coef_plot_iam <- ggplot(as.data.frame(coef(iam_model)), aes(x = rownames(.), y = `(Intercept)`)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "IAM Model Coefficients", x = "Variables", y = "Coefficients")

print(coef_plot_iam)

# To further analyze and refine models, you can try adding interaction terms or transformations if necessary.
