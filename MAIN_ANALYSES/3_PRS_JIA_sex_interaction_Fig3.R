###############################################################################
## Title: Investigate the interaction between a PRS for JIA and sex
## Output: Figure 3
##
## Author: Kristine L Haftorn
## Date created: 2024.08.27
## Date modified: 2024.11.26

## Load packages
library(mgcv)
library(dplyr)
library(haven)
library(ggplot2)
library(ROCR)

## Load and prepare data
# df_final <- Data frame with samples (rows) by variables (columns). Should include the following variables:
#       - case_JIA (JIA diagnosis, binary yes/no)
#       - sex (binary male/female)
#       - PRS (numeric)
#       - PC1-10 (numeric)
#       - yob (year of birth, numeric)

###########################################
## Simple logit-linear interaction model ##
###########################################
GLM_interaction <- glm(case_JIA ~ PRS*sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + yob, 
                       data = df_final, 
                       family = "binomial")
summary(GLM_interaction)

#############################################
## "Semi-stratified" GAM interaction model ##
#############################################
# Create sex dummy variables and calculate products
df_final$girl <- ifelse((df_final$sex == 2), 1, 0)
df_final$boy <- ifelse((df_final$sex == 1), 1, 0)
df_final$PRS_girl <- df_final$PRS*df_final$girl
df_final$PRS_boy <- df_final$PRS*df_final$boy

# Run model
GAM_interaction <- gam(case_JIA ~ girl + s(PRS_girl) + s(PRS_boy) + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + yob, 
                       data = df_final, 
                       family = "binomial")
summary(GAM_model)

# Create simulation dataset based on GAM model predictions
set.seed(1105)
newdata <- data.frame(PRS = seq(-4.5,4.5, 0.1))
newdata$PC1 <- mean(df_final$PC1)
newdata$PC2 <- mean(df_final$PC2)
newdata$PC3 <- mean(df_final$PC3)
newdata$PC4 <- mean(df_final$PC4)
newdata$PC5 <- mean(df_final$PC5)
newdata$PC6 <- mean(df_final$PC6)
newdata$PC7 <- mean(df_final$PC7)
newdata$PC8 <- mean(df_final$PC8)
newdata$PC9 <- mean(df_final$PC9)
newdata$PC10 <- mean(df_final$PC10)
newdata$yob <- mean(df_final$yob)

# Create separate datasets for males and females
newdata_girl <- newdata
newdata_girl$girl <- 1
newdata_girl$boy <- 0
newdata_girl$PRS_girl <- newdata_girl$PRS*newdata_girl$girl
newdata_girl$PRS_boy <- newdata_girl$PRS*newdata_girl$boy

newdata_boy <- newdata
newdata_boy$girl <- 0
newdata_boy$boy <- 1
newdata_boy$PRS_girl <- newdata_boy$PRS*newdata_boy$girl
newdata_boy$PRS_boy <- newdata_boy$PRS*newdata_boy$boy

pred_girl <- predict(GAM_model, type = 'link', newdata = newdata_girl, se.fit = TRUE)
newdata_girl$pred <- pred_girl$fit
newdata_girl$se <- pred_girl$se.fit
newdata_girl$conf_min <- newdata_girl$pred-(1.96*newdata_girl$se)
newdata_girl$conf_max <- newdata_girl$pred+(1.96*newdata_girl$se)

pred_boy <- predict(GAM_model, type = 'link', newdata = newdata_boy, se.fit = TRUE)
newdata_boy$pred <- pred_boy$fit
newdata_boy$se <- pred_boy$se.fit
newdata_boy$conf_min <- newdata_boy$pred-(1.96*newdata_boy$se)
newdata_boy$conf_max <- newdata_boy$pred+(1.96*newdata_boy$se)

# Transform y-values to get probabilities on the y-axis
newdata_girl$pred <- exp(newdata_girl$pred)/(1 + exp(newdata_girl$pred))
newdata_girl$conf_min <- exp(newdata_girl$conf_min)/(1 + exp(newdata_girl$conf_min))
newdata_girl$conf_max <- exp(newdata_girl$conf_max)/(1 + exp(newdata_girl$conf_max))
newdata_boy$pred <- exp(newdata_boy$pred)/(1 + exp(newdata_boy$pred))
newdata_boy$conf_min <- exp(newdata_boy$conf_min)/(1 + exp(newdata_boy$conf_min))
newdata_boy$conf_max <- exp(newdata_boy$conf_max)/(1 + exp(newdata_boy$conf_max))

###################
## Plot Figure 3 ##
###################
fig_data <- rbind(newdata_boy, newdata_girl)
fig3 <- ggplot(fig_data, aes(x = PRS, y = pred, color = factor(boy), fill=factor(boy))) +
  geom_ribbon(aes(ymin=conf_min, ymax=conf_max), alpha=0.1,show.legend = FALSE) +
  geom_line(size=1.1) +
  coord_trans(y = "log", ylim=c(0.0003,0.20)) +
  scale_y_continuous(breaks = c(0.001,0.01,0.02,0.03,0.04,0.05,0.10, 0.15)) +
  scale_x_continuous(limits=c(-4.5,4.5), breaks=c(-4,-3,-2,-1,0,1,2,3,4)) +
  #guides(y = "axis_logticks") +
  scale_color_manual(name = "Sex", labels= c("Female", "Male"), values = c("#E69F00", "#009E73")) +
  xlab("PRS (JIA)") +
  ylab("Log odds (JIA)") +
  theme_classic(base_size = 15) +
  theme(axis.text = element_text(face="bold"),
        legend.position = c(0.8,0.2),
        legend.background = element_rect(color = "black", linetype = "solid"))
fig3
ggsave("Figure3.JPEG", fig3, width = 180, height = 150, units = "mm", dpi = 600)
