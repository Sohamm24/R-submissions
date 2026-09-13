# Palmer Penguins Statistical Analysis
# 1. Setup and Package Installation
if(!require(palmerpenguins)) install.packages("palmerpenguins")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(e1071)) install.packages("e1071")
if(!require(effsize)) install.packages("effsize")
if(!require(car)) install.packages("car")

library(palmerpenguins)
library(dplyr)
library(ggplot2)
library(e1071)
library(effsize)
library(car)

# Load data and omit NAs
data("penguins")
df <- na.omit(penguins)

# ---------------------------------------------------------
# Task 1: Descriptive Statistical Analysis
# ---------------------------------------------------------
print("--- Task 1: Descriptive Statistical Analysis ---")

calc_stats <- function(x) {
  c(
    Mean = mean(x),
    Median = median(x),
    Min = min(x),
    Max = max(x),
    Var = var(x),
    SD = sd(x),
    Q1 = quantile(x, 0.25),
    Q3 = quantile(x, 0.75),
    IQR = IQR(x),
    Skewness = skewness(x),
    Kurtosis = kurtosis(x)
  )
}

# Overall descriptive statistics for body mass
overall_stats <- calc_stats(df$body_mass_g)
print("Overall Statistics for Body Mass:")
print(overall_stats)

# Species-wise descriptive statistics
species_stats <- df %>%
  group_by(species) %>%
  summarise(
    Mean = mean(body_mass_g),
    Median = median(body_mass_g),
    Min = min(body_mass_g),
    Max = max(body_mass_g),
    Var = var(body_mass_g),
    SD = sd(body_mass_g),
    Q1 = quantile(body_mass_g, 0.25),
    Q3 = quantile(body_mass_g, 0.75),
    IQR = IQR(body_mass_g),
    Skewness = skewness(body_mass_g),
    Kurtosis = kurtosis(body_mass_g)
  )
print("Species-wise Statistics for Body Mass:")
print(species_stats)

# Visualizations for Task 1
# Histogram
ggplot(df, aes(x = body_mass_g)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  theme_minimal() +
  labs(title = "Histogram of Body Mass", x = "Body Mass (g)", y = "Frequency")

# Boxplot
ggplot(df, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Body Mass by Species", x = "Species", y = "Body Mass (g)")

# Density plot
ggplot(df, aes(x = body_mass_g, fill = species)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Body Mass by Species", x = "Body Mass (g)", y = "Density")


# ---------------------------------------------------------
# Task 2: Hypothesis Testing (Male vs Female)
# ---------------------------------------------------------
print("--- Task 2: Hypothesis Testing ---")
# H0: Mean body mass of males = Mean body mass of females
# H1: Mean body mass of males != Mean body mass of females

male_mass <- df$body_mass_g[df$sex == "male"]
female_mass <- df$body_mass_g[df$sex == "female"]

# Normality check
shapiro_male <- shapiro.test(male_mass)
shapiro_female <- shapiro.test(female_mass)
print("Shapiro-Wilk test (Male):")
print(shapiro_male)
print("Shapiro-Wilk test (Female):")
print(shapiro_female)

# QQ Plots
# Adjust margins and plot side-by-side to prevent "figure margins too large" error
par(mfrow=c(1,2), mar=c(4,4,2,1))
qqnorm(male_mass, main = "QQ Plot - Male Body Mass")
qqline(male_mass, col = "red")

qqnorm(female_mass, main = "QQ Plot - Female Body Mass")
qqline(female_mass, col = "red")
# Reset to default
par(mfrow=c(1,1), mar=c(5,4,4,2) + 0.1)

# Independent Two-Sample t-test
ttest_res <- t.test(body_mass_g ~ sex, data = df)
print("T-test Result:")
print(ttest_res)

# Effect size (Cohen's d)
cohen_d <- cohen.d(body_mass_g ~ sex, data = df)
print("Cohen's d:")
print(cohen_d)


# ---------------------------------------------------------
# Task 3: One-Way ANOVA
# ---------------------------------------------------------
print("--- Task 3: One-Way ANOVA ---")
# Check homogeneity of variance
levene_res <- leveneTest(body_mass_g ~ species, data = df)
print("Levene's Test:")
print(levene_res)

# One-way ANOVA
anova_res <- aov(body_mass_g ~ species, data = df)
print("One-Way ANOVA Summary:")
summary(anova_res)

# Tukey HSD Post-hoc
tukey_res <- TukeyHSD(anova_res)
print("Tukey HSD Post-hoc Test:")
print(tukey_res)

# Group Comparison Plot
plot(tukey_res)


# ---------------------------------------------------------
# Task 4: Non-Parametric Analysis
# ---------------------------------------------------------
print("--- Task 4: Kruskal-Wallis Test ---")
kruskal_res <- kruskal.test(body_mass_g ~ species, data = df)
print("Kruskal-Wallis Test:")
print(kruskal_res)


# ---------------------------------------------------------
# Task 5: Two-Way ANOVA
# ---------------------------------------------------------
print("--- Task 5: Two-Way ANOVA ---")
twoway_anova <- aov(body_mass_g ~ species * sex, data = df)
print("Two-Way ANOVA Summary:")
summary(twoway_anova)

# Sex-wise boxplot
ggplot(df, aes(x = species, y = body_mass_g, fill = sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Body Mass by Species and Sex", x = "Species", y = "Body Mass (g)")


# ---------------------------------------------------------
# Task 6: Additional Analysis (Flipper Length)
# ---------------------------------------------------------
print("--- Task 6: Additional Analysis (Flipper Length) ---")
anova_flipper <- aov(flipper_length_mm ~ species, data = df)
print("One-Way ANOVA (Flipper Length):")
summary(anova_flipper)

tukey_flipper <- TukeyHSD(anova_flipper)
print("Tukey HSD (Flipper Length):")
print(tukey_flipper)

# Species-wise flipper-length comparison
ggplot(df, aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Flipper Length by Species", x = "Species", y = "Flipper Length (mm)")

print("Analysis Complete.")
