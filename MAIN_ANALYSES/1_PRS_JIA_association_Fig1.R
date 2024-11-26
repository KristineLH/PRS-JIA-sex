###############################################################################
## Title: Investigate the association between PRS and JIA with GLM and GAM models
## Output: Figure 1
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

###################
## Run GLM model ##
###################
GLM_model <- glm(case_JIA ~ PRS + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex + yob, 
                 data = df_final, 
                 family = "binomial")
summary(GLM_model)

# Create simulation dataset based on GLM model predictions
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
newdata$sex <- mean(df_final$sex)
temp <- predict(GLM_model, type = 'link', newdata = newdata, se.fit = TRUE)
newdata$pred <- temp$fit
newdata$se <- temp$se.fit
newdata$conf_min <- newdata$pred-(1.96*newdata$se)
newdata$conf_max <- newdata$pred+(1.96*newdata$se)
newdata$model <- factor("glm")
newdata_glm <- newdata

# Transform y-values to get probabilities on the y-axis
newdata_glm$pred <- exp(newdata_glm$pred)/(1 + exp(newdata_glm$pred))
newdata_glm$conf_min <- exp(newdata_glm$conf_min)/(1 + exp(newdata_glm$conf_min))
newdata_glm$conf_max <- exp(newdata_glm$conf_max)/(1 + exp(newdata_glm$conf_max))

###################
## Run GAM model ##
###################
GAM_model <- gam(case_JIA ~ s(PRS) + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + sex + yob,
                 data = df_final, 
                 family = "binomial")
summary(GLM_model)

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
newdata$sex <- mean(df_final$sex)
temp <- predict(GAM_model, type = 'link', newdata = newdata, se.fit = TRUE)
newdata$pred <- temp$fit
newdata$se <- temp$se.fit
newdata$conf_min <- newdata$pred-(1.96*newdata$se)
newdata$conf_max <- newdata$pred+(1.96*newdata$se)
newdata$model <- factor("gam")
newdata_gam <- newdata

# Transform y-values to get probabilities on the y-axis
newdata_gam$pred <- exp(newdata_gam$pred)/(1 + exp(newdata_gam$pred))
newdata_gam$conf_min <- exp(newdata_gam$conf_min)/(1 + exp(newdata_gam$conf_min))
newdata_gam$conf_max <- exp(newdata_gam$conf_max)/(1 + exp(newdata_gam$conf_max))

###################
## Plot Figure 1 ##
###################
fig_data <- rbind(newdata_glm, newdata_gam)
fig1 <- ggplot(fig_data, aes(x = PRS, y =pred, color = factor(model), fill=factor(model))) +
  geom_line(size=1.1) +
  coord_trans(y = "log", ylim=c(0.0003,0.10)) +
  scale_y_continuous(breaks = c(0.001,0.01,0.02,0.03,0.04,0.05,0.10)) +
  scale_x_continuous(limits=c(-4.5,4.5), breaks=c(-4,-3,-2,-1,0,1,2,3,4)) +
  guides(y = "axis_logticks") +
  geom_ribbon(aes(ymin=conf_min, ymax=conf_max), alpha=0.1,show.legend = FALSE) +
  scale_color_manual(name = "Model", labels= c("GLM", "GAM"), values = c("#E69F00","#009E73")) +
  xlab("PRS (JIA)") +
  ylab("Probability (JIA)") +
  theme_classic(base_size = 15) +
  theme(axis.text = element_text(face="bold"),
        legend.position = c(0.8,0.2),
        legend.background = element_rect(color = "black", linetype = "solid"))

ggsave("Figure1.JPEG", fig1, width = 180, height = 150, units = "mm", dpi = 600)
