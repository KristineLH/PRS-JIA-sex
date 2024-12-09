###############################################################################
## Title: Distribution of JIA PRS
## Output: Figure 2
##
## Author: Hamid Khoshfekr Rudsari
## Date created: 2024.08.27
## Date modified: 2024.10.20

## Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)


## Load and prepare data
# df_final <- Data frame with samples (rows) by variables (columns). Should include the following variables:
#       - case_JIA (JIA diagnosis, binary yes/no)
#       - sex (binary male/female)
#       - PRS (numeric)
#       - PC1-10 (numeric)
#       - yob (year of birth, numeric)


# Normalization
df_final$PRS <- (df_final$PRS - mean(df_final$PRS))/sd(df_final$PRS)


####################################
## Plot Figure 2-A and Figure 2-B ##
####################################

df_final$Sex <- ifelse(df_final$sex == 1, "Male", "Female") 
df_final$Status <- ifelse(df_final$case_JIA == 1, "Case", "Control") 
data_cases <- df_final[df_final$Status == "Case",]
data_controls <- df_final[df_final$Status == "Control",]


# Distribution of PRS for controls --- Figure 2-A 
ggplot(data_controls, aes(x = PRS)) +
  geom_histogram(color="black", fill="lightblue") +
  labs(y = "Count",
       x = "PRS (JIA)",
       title = "Controls") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5))
ggsave("Figure2-A.JPEG", width = 10, height = 10, units = "cm", device = "jpg", dpi = 900)

# Distribution of PRS for cases --- Figure 2-B
ggplot(data_cases, aes(x = PRS, fill= Sex, color = Sex)) +
  geom_histogram(position="identity", alpha = 0.5) + 
  labs(x = "PRS (JIA)",
       y = "Count",
       title = "JIA Cases") +
  theme_classic() +
  scale_fill_manual(values = c("Male" = "#009E73", "Female" = "#E69F00")) +  
  theme(axis.ticks.x = element_blank(),
    legend.background = element_rect(color = "black", size = .5)) + 
  theme(plot.title = element_text(hjust=0.5))
ggsave("Figure2-B.JPEG", width = 10, height = 10, units = "cm", device = "jpg", dpi = 900)
