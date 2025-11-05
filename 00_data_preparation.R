library(dplyr)
library(tidyr)

# === 1. Demographics ===

age_data <- data.frame(
  Age.Group = c("Under 18", "18-20", "21-23", "24-26", "27 and above"),
  [cite_start]Count = c(6, 91, 286, 69, 19) # Approximated from chart [cite: 8-10]
)
age_data$Age.Group <- factor(age_data$Age.Group, levels = age_data$Age.Group)

gender_data <- data.frame(
  Gender = c("Male", "Female"),
  Percentage = c(73.2, 26.8),
  [cite_start]Count = c(345, 126) # Based on N=471 [cite: 13]
)

university_data <- data.frame(
  University = c("AAU", "AASTU", "Hawassa Univ", "BahirDar Univ", "Haramaya Univ", "ASTU", "Debre Birhan", "Jimma Univ", "Wachemo Univ", "Admas Univ", "HiLCoE College", "St. Mary's", "Arba Minch", "Gondar Univ", "Mekelle Univ"),
  Enrollment = c(121, 57, 36, 31, 27, 24, 21, 16, 10, 9, 9, 7, 7, 6, 5) #
)
university_data$University <- factor(university_data$University, levels = university_data$University)

program_data <- data.frame(
  Program = c("SE", "BSc", "CS", "CSE", "CS", "Information Systems", "ECE", "MSc", "Bsc", "MD", "Engineering", "Information Technology", "Biomedical Engineering", "Accounting and Finance", "Tourism management"),
  Count = c(111, 52, 50, 26, 14, 14, 12, 7, 7, 5, 5, 5, 4, 4, 4) #
)
program_data$Program <- factor(program_data$Program, levels = rev(program_data$Program))

field_data <- data.frame(
  Field = c("STEM", "Other", "Bus & Econ", "Health & Med", "Soc Sci & Hum", "Arts & Design"),
  Percentage = c(65, 27.2, 4.46, 1.27, 1.27, 0.849) #
)

gpa_hist_data <- data.frame(
  GPA.Bin = c("2.25", "2.50", "2.75", "3.00", "3.25", "3.50", "3.75", "4.00"),
  Count = c(1, 5, 11, 10, 32, 34, 47, 75) # Approximated from histogram bins
)
[cite_start]gpa_mean <- 3.46 # [cite: 27]
[cite_start]gpa_median <- 3.50 # [cite: 27]

study_level_data <- data.frame(
  Study.Level = c("1", "2", "3", "4", "5", "6", "Grade 11", "Graduated", "Masters"),
  Count = c(34, 63, 94, 146, 71, 2, 5, 53, 3) #
)
study_level_data$Study.Level <- factor(study_level_data$Study.Level, levels = study_level_data$Study.Level)

study_level_agg_data <- data.frame(
  Study.Level = c("Upper", "Lower", "Graduated"),
  Count = c(218, 191, 62), # From "Level of Study Distribution" chart
  Percentage = c(46.3, 40.6, 13.2)
)
study_level_agg_data$Study.Level <- factor(study_level_agg_data$Study.Level, levels = c("Lower", "Upper", "Graduated"))

# === 2. Exposure Participation ===

exposure_type_data <- data.frame(
  Exposure.Type = c("Internships", "Social nets", "Coursework", "Mentorship", "Career fairs", "Job shadowing", "Other"),
  [cite_start]No.Students = c(266, 164, 180, 144, 72, 52, 28) # [cite: 50-52]
)
exposure_type_data$Exposure.Type <- factor(exposure_type_data$Exposure.Type, levels = exposure_type_data$Exposure.Type[order(-exposure_type_data$No.Students)])

exposure_count_data <- data.frame(
  Exposures = c("1", "2", "3", "4", "5", "6"),
  [cite_start]Students = c(258, 88, 67, 33, 11, 14) # [cite: 55-57]
)
exposure_count_data$Exposures <- factor(exposure_count_data$Exposures, levels = exposure_count_data$Exposures)

gender_exposure_data <- data.frame(
  Num.Exposures = rep(c("1", "2", "3", "4", "5", "6"), 2),
  Gender = c(rep("Male", 6), rep("Female", 6)),
  Student.Count = c(205, 57, 37, 24, 8, 14, 53, 31, 30, 9, 3, 0) # Approximated from chart
)

avg_gpa_exposure_data <- data.frame(
  Exposures = 1:6,
  [cite_start]GPA = c(3.41, 3.50, 3.47, 3.57, 3.46, 3.48) # Approximated from chart [cite: 73]
)

avg_exp_uni_data <- data.frame(
  University = c("BahirDar Univ", "Unity University", "ACT American College", "Salale University", "Kotebe University", "Mekelle University", "Wollo University", "Madda walabu University", "Wollega University", "Gambella University"),
  Avg.Exposures = c(1.94, 2.0, 2.0, 2.0, 2.0, 2.2, 3.0, 4.5, 5.0, 5.0) #
)
avg_exp_uni_data$University <- factor(avg_exp_uni_data$University, levels = avg_exp_uni_data$University[order(avg_exp_uni_data$Avg.Exposures)])

avg_exp_gender_data <- data.frame(
  Gender = c("Female", "Male"),
  [cite_start]Avg.Exposures = c(2.07, 1.87) # [cite: 88]
)

avg_exp_field_data <- data.frame(
  Field = c("Arts & Design", "STEM", "Other", "Bus & Econ", "Health & Med", "Soc Sci & Hum"),
  [cite_start]Avg.Exposures = c(2.5, 1.96, 1.93, 1.67, 1.17, 1.17) # [cite: 94-97]
)
avg_exp_field_data$Field <- factor(avg_exp_field_data$Field, levels = avg_exp_field_data$Field[order(-avg_exp_field_data$Avg.Exposures)])

avg_exp_level_data <- data.frame(
  Study.Level = c("Lower", "Upper", "Graduated"),
  [cite_start]Avg.Exposures = c(1.79, 1.95, 2.23) # [cite: 100]
)
avg_exp_level_data$Study.Level <- factor(avg_exp_level_data$Study.Level, levels = avg_exp_level_data$Study.Level)

# === 3. Exposure Influence ===

influence_type_data <- data.frame(
  Exposure.Type = c("Internships", "Univ Course", "Mentorship", "Job Shadow", "Career Fairs", "Social Networks", "Family", "Friends"),
  Avg.Influence = c(3.34, 3.18, 3.26, 2.88, 2.87, 3.05, 2.65, 2.67) #
)
influence_type_data$Exposure.Type <- factor(influence_type_data$Exposure.Type, levels = influence_type_data$Exposure.Type)

influence_gpa_data <- data.frame(
  GPA.Range = rep(c("2.0-2.5", "2.5-3.0", "3.0-3.5", "3.5-4.0"), each = 8),
  Exposure.Type = rep(c("Internships", "Univ coursework", "Mentorship prog", "Job shadowing", "Career fairs", "Social networks", "Family", "Friends"), 4),
  Influence = c(
    3.6, 3.6, 3.6, 3.1, 3.4, 3.6, 2.8, 3.1, # 2.0-2.5 (Approx from chart)
    3.5, 3.2, 3.5, 2.8, 3.1, 3.2, 2.8, 2.8, # 2.5-3.0 (Approx from chart)
    3.6, 3.4, 3.5, 3.2, 3.0, 3.4, 2.9, 2.9, # 3.0-3.5 (Approx from chart)
    3.6, 3.5, 3.5, 3.1, 3.1, 3.5, 2.9, 3.0  # 3.5-4.0 (Approx from chart)
  )
) #
influence_gpa_data$Exposure.Type <- factor(influence_gpa_data$Exposure.Type, levels = c("Internships", "Univ coursework", "Mentorship prog", "Job shadowing", "Career fairs", "Social networks", "Family", "Friends"))

mean_influence_type_data <- data.frame(
  Exposure.Type = c("Formal", "Informal"),
  [cite_start]Mean.Score = c(3.10, 2.65) # [cite: 126]
)

mean_influence_gender_data <- data.frame(
  Gender = rep(c("Male", "Female"), each = 2),
  Exposure.Type = rep(c("Formal", "Informal"), 2),
  [cite_start]Mean.Score = c(3.10, 2.71, 3.10, 2.50) # [cite: 130-131]
)

mean_influence_field_data <- data.frame(
  Academic.Field = rep(c("STEM", "Other", "Biz & Econ", "Arts & Design", "Health & Med", "Soc Sci & Hum"), each = 2),
  Exposure.Type = rep(c("Formal", "Informal"), 6),
  Mean.Score = c(
    [cite_start]3.09, 2.62, # STEM [cite: 137]
    [cite_start]3.18, 2.87, # Other [cite: 136]
    [cite_start]2.68, 2.19, # Biz & Econ [cite: 138]
    3.15, 2.00, # Arts & Design (Approx from chart)
    [cite_start]3.50, 2.43, # Health & Med [cite: 135]
    3.00, 2.58  # Soc Sci & Hum (Approx from chart)
  )
)

mean_influence_level_data <- data.frame(
  Study.Level = rep(c("Upper", "Lower", "Graduated"), each = 2),
  Exposure.Type = rep(c("Formal", "Informal"), 3),
  Mean.Score = c(
    [cite_start]3.18, 2.62, # Upper [cite: 142]
    [cite_start]3.00, 2.68, # Lower [cite: 143-144]
    [cite_start]3.10, 2.55  # Graduated [cite: 143]
  )
)
mean_influence_level_data$Study.Level <- factor(mean_influence_level_data$Study.Level, levels = c("Lower", "Upper", "Graduated"))

# === 4. Career Guidance ===

guidance_freq_sat_data <- data.frame(
  Metric = c("Avg Frequency", "Avg Satisfactn"),
  [cite_start]Score = c(2.72, 2.66) # [cite: 155-156]
)

guidance_gpa_data <- data.frame(
  GPA.Group = c("Low", "Medium", "High"),
  Usage.Frequency = c(2.58, 2.65, 2.72) # Approx from chart
)
guidance_gpa_data$GPA.Group <- factor(guidance_gpa_data$GPA.Group, levels = c("Low", "Medium", "High"))

guidance_uni_data <- data.frame(
  University = rep(c("AAU", "Jimma Univ", "HiLCoE College", "ASTU", "AASTU", "Wachemo Univ", "Debre Birhan", "Hawassa Univ", "Haramaya Univ", "BahirDar Univ"), 2),
  Metric = c(rep("Frequency", 10), rep("Satisfaction", 10)),
  Rating = c(
    [cite_start]1.89, 2.00, 2.44, 2.33, 2.78, 2.89, 2.89, 3.33, 3.67, 4.00, # Freq [cite: 166-168]
    [cite_start]2.22, 2.33, 2.22, 2.44, 2.56, 3.00, 3.11, 2.89, 3.56, 3.67  # Sat [cite: 166-168]
  )
)
guidance_uni_data$University <- factor(guidance_uni_data$University, levels = rev(c("AAU", "Jimma Univ", "HiLCoE College", "ASTU", "AASTU", "Wachemo Univ", "Debre Birhan", "Hawassa Univ", "Haramaya Univ", "BahirDar Univ")))

guidance_gender_data <- data.frame(
  Gender = rep(c("Male", "Female"), each = 2),
  Metric = rep(c("Frequency", "Satisfaction"), 2),
  [cite_start]Rating = c(2.76, 2.71, 2.62, 2.51) # [cite: 170-171]
)

guidance_field_data <- data.frame(
  Field = rep(c("STEM", "Other", "Bus & Econ", "Arts & Design", "Health & Med", "Soc Sci & Hum"), each = 2),
  Metric = rep(c("Frequency", "Satisfaction"), 6),
  Score = c(
    [cite_start]2.61, 2.48, # STEM [cite: 174-175]
    2.91, 2.93, # Other
    2.81, 3.14, # Bus & Econ
    [cite_start]3.75, 2.5,  # Arts & Design [cite: 174-175]
    [cite_start]2.67, 3.00, # Health & Med [cite: 174-175]
    [cite_start]3.50, 3.17  # Soc Sci & Hum [cite: 174-175]
  )
)

guidance_level_data <- data.frame(
  Level = rep(c("Graduated", "Lower", "Upper"), each = 2),
  Metric = rep(c("Frequency", "Satisfaction"), 3),
  Score = c(
    [cite_start]2.98, 3.13, # Graduated [cite: 179]
    2.70, 2.62, # Lower (Approx from chart)
    2.70, 2.55  # Upper (Approx from chart)
  )
)
guidance_level_data$Level <- factor(guidance_level_data$Level, levels = c("Lower", "Upper", "Graduated"))

guidance_usage_sat_data <- data.frame(
  Usage.Freq = c(1, 2, 3, 4, 5),
  Usage.Label = c("Never", "Rarely", "Sometimes", "Often", "Always"),
  [cite_start]Avg.Satisfaction = c(1.55, 2.04, 2.80, 3.62, 4.26) # [cite: 184-188]
)

# === 5. Career Direction ===

clarity_exposure_count_data <- data.frame(
  Num.Exposures = c(1, 2, 3, 4, 5, 6),
  Career.Clarity = c(3.24, 3.36, 3.57, 3.88, 3.18, 3.86) #
)

clarity_exposure_type_data <- data.frame(
  Exposure.Type = c("University", "Social Search", "Internships", "Career Fairs", "Mentorship", "Job Shadowing"),
  Avg.Rating = c(3.44, 3.47, 3.52, 3.43, 3.50, 3.65) #
)
clarity_exposure_type_data$Exposure.Type <- factor(clarity_exposure_type_data$Exposure.Type, levels = clarity_exposure_type_data$Exposure.Type)


# === 6. Qualitative Analysis (Word Cloud Data) ===
# [cite_start]Data is reconstructed from key themes [cite: 221, 223, 225]

shaping_exp_words <- c(
  "career", "experience", "work", "academic", "field", "job", "internship",
  "program", "skill", "university", "helped", "choice", "like", "impact",
  "path", "interest", "development", "mentorship", "projects", "realworld"
)
shaping_exp_freq <- c(
  100, 90, 80, 75, 70, 65, 60,
  55, 50, 45, 40, 35, 30, 28,
  25, 23, 20, 18, 15, 12
)
shaping_exp_data <- data.frame(word = shaping_exp_words, freq = shaping_exp_freq)

improvements_words <- c(
  "student", "career", "guidance", "job", "university", "practical",
  "program", "opportunities", "internship", "skill", "more", "better",
  "support", "industry", "help", "workshops", "mentorship", "personalized"
)
improvements_freq <- c(
  100, 90, 85, 80, 75, 70,
  65, 60, 55, 50, 45, 40,
  35, 30, 28, 25, 20, 18
)
improvements_data <- data.frame(word = improvements_words, freq = improvements_freq)

additional_exp_words <- c(
  "student", "experience", "practical", "work", "program", "internship",
  "industry", "university", "career", "skill", "job", "opportunities",
  "training", "mentorship", "project", "networking", "workshops"
)
additional_exp_freq <- c(
  100, 95, 90, 85, 80, 75,
  70, 65, 60, 55, 50, 45,
  40, 35, 30, 25, 20
)
additional_exp_data <- data.frame(word = additional_exp_words, freq = additional_exp_freq)


# === 7. Correlation Analysis ===

correlation_matrix <- matrix(
  c(1.00, 0.70, 0.37,
    0.70, 1.00, 0.34,
    0.37, 0.34, 1.00),
  nrow = 3,
  dimnames = list(c("Usage Freq", "Satisfaction", "Career Clarity"),
                  c("Usage Freq", "Satisfaction", "Career Clarity"))
[cite_start]) # [cite: 237]


# === Save all objects to a file ===
save.image(file = "reconstructed_data.RData")

print("Data preparation complete. 'reconstructed_data.RData' saved.")
