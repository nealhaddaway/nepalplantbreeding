#' Meta-analysis for Nepal plant breeding meta-analysis

# import data
library(tidyverse)
library(readxl)
library(metafor)
library(orchaRd)

data <- readRDS(here::here("data/Full_data_extraction_sheet.RDS"))

# Introduction - Domestication
# Hybridisation - Introduction
# subset data
hyb_int_data <- subset(data, `hyb-int`==1)

# calculate effect sizes
ES_data <- data.frame(ID=NULL, short_citation=NULL, crop=NULL, baseline=NULL, ES=NULL, PSD=NULL, CA_judgement=NULL)
for (i in 1:length(unique(hyb_int_data$ID))){
  subset_data <- subset(hyb_int_data, ID==unique(hyb_int_data$ID)[i])
  # prepare data
  short_cit <- subset_data$short_citation[1]
  crop <- subset_data$crop[1]
  CA_judgement <- subset_data$CA_judgement[1]
  # subset interventions
  hybrid <- subset(subset_data, `intervention main category`=="hybridization") 
  introd <- subset(subset_data, `intervention main category`=="introduction")
  baseline <- introd$`mean (kg/ha)`
  #calculate ES and PSD
  ES <- hybrid$`mean (kg/ha)` - introd$`mean (kg/ha)`
  # Calculate correct sampling variance of the mean difference
  V <- (hybrid$SD^2 / hybrid$n) + (introd$SD^2 / introd$n)
  #PSD <- sqrt((((hybrid$n-1)*(hybrid$SD^2))+((introd$n-1)*(introd$SD^2))) / (hybrid$n+introd$n-2))
    
  # generate df
  new_data <- data.frame(ID=unique(hyb_int_data$ID)[i], 
                         short_citation=short_cit, 
                         crop=crop, 
                         baseline=baseline,
                         ES=ES, 
                         V=V,
                         CA_judgement=CA_judgement)
  ES_data <- rbind(ES_data, new_data)
}
ES_data$comparison <- rep("Hybridization - Introduction", nrow(ES_data))

#sort by crop and short citation
ES_data <- ES_data[order(ES_data$crop, rev(ES_data$short_citation)),]

#set up model
model1 <- rma.mv(yi=ES,
                 V=V,
                 data=ES_data,
                 mods=~factor(crop)*baseline,
                 method="ML",
                 random=~1|ID)

# redundant predictors dropped from model

table(ES_data$crop, ES_data$baseline)
# model1 <- rma.mv(yi=ES, 
#                  V=V, 
#                  data=ES_data, 
#                  mods=~factor(crop):baseline,
#                  method="ML", 
#                  random=~1|ID)


model1b <- rma.mv(yi=ES, 
                  V=V, 
                  data=ES_data, 
                  method="ML", 
                  random=~1|ID)
model_results <- orchaRd::mod_results(model1b, mod = "1", at = NULL, group = "ID")
summary(model1)

# meta-regression plot
ES_data$colour <- ES_data$crop
ES_data$colour <- gsub("wheat", "#F71735", ES_data$colour)
ES_data$colour <- gsub("maize", "#41EAD4", ES_data$colour)
ES_data$colour <- gsub("rice", "#FDFFFC", ES_data$colour)
ES_data$colour <- gsub("potato", "#FF9F1C", ES_data$colour)
regplot(model1, mod="baseline", pi=TRUE, refline=1, legend=FALSE,
        label="piout", labsize=0.8,
        bg=ES_data$colour, xlab="Baseline yield (kg/ha)", ylab="Yield difference (kg/ha)") 

# orchard plot
I2 <- orchaRd::i2_ml(model1b)
orchaRd::orchard_plot(model1b, group = "ID", xlab = "Raw mean effect size (kg/ha)",
                      transfm = "none") +
  annotate(geom = "text", x = 0.8, y = -3000, 
           label = paste0("italic(I)^{2} == ", round(I2[1],4), "*\"%\""), 
           color = "black", parse = TRUE, size = 5) 
# caterpillar plot
orchaRd::caterpillars(model1b, mod = "1", xlab = "Standardised mean difference", group="ID")

## ADDED INTERACTION ##

### a little helper function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, x) {
  list(bquote(paste(.(text),
                    " (Q = ", .(fmtx(x$QE, digits=2)),
                    ", df = ", .(x$k - x$p), ", ",
                    .(fmtp(x$QEp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)), "; ",
                    I^2, " = ", .(fmtx(x$I2, digits=1)), "%, ",
                    tau^2, " = ", .(fmtx(x$tau2, digits=2)), ")")))}

# fit models for each group
# Not enough data for cauliflower
# subset_caul <- subset(ES_data, crop=="cauliflower")
# res.caul <- rma.mv(yi=ES, 
#                    V=V, 
#                    data=subset_caul, 
#                    method="ML", 
#                    random=~ID|1)
subset_maize <- subset(ES_data, crop=="maize")
res.maize <- rma.mv(yi=ES, 
                    V=V, 
                    data=subset_maize, 
                    method="ML", 
                    random=~ID|1)
# Not enough data 
#subset_potato <- subset(ES_data, crop=="potato")
#res.potato <- rma.mv(yi=ES, 
#                     V=V, 
#                     data=subset_potato, 
#                     method="ML", 
#                     random=~ID|1)
subset_rice <- subset(ES_data, crop=="rice")
res.rice <- rma.mv(yi=ES, 
                   V=V, 
                   data=subset_rice, 
                   method="ML", 
                   random=~ID|1)
subset_wheat <- subset(ES_data, crop=="wheat")
res.wheat <- rma.mv(yi=ES, 
                    V=V, 
                    data=subset_wheat, 
                    method="ML", 
                    random=~ID|1)

# forest plot
# calculate groups
#n_cauliflower <- nrow(subset(ES_data, crop=="cauliflower"))
n_maize <- nrow(subset(ES_data, crop=="maize"))
#n_potato <- nrow(subset(ES_data, crop=="potato"))
n_rice <- nrow(subset(ES_data, crop=="rice"))
n_wheat <- nrow(subset(ES_data, crop=="wheat"))

# plot
forest(model1, addfit=FALSE, cex=0.45, xlab="Effect size (kg/ha)",
       cex.lab=0.6, 
       header="First author (Year)",
       slab=short_citation,
       ilab=crop,
       ilab.xpos=-10500,
       ylim=c(-1,79),
       xlim=c((-16000), (8000)),
       rows=c(74:58, 53:53, 49:32, 27:3)
)
# replace CA text with coloured text
CA_col <- ES_data$CA_judgement
CA_col <- gsub('HIGH', 'darkred', CA_col)
CA_col <- gsub('LOW', 'darkgreen', CA_col)
CA_col <- gsub('MEDIUM', 'darkorange', CA_col)
CA_col <- gsub('UNCLEAR', 'darkgrey', CA_col)
text(-8500, 
     c(74:58, 53:53, 49:32, 27:3), 
     ES_data$CA_judgement, col=c(CA_col), cex=0.45, font=2)
text(-10500, 78, 'Crop', cex=0.45, font=2)
text(-8500, 78, 'CA Judgement', cex=0.45, font=2)
### add text for the subgroups
text(-16000, c(75.1, 54.1, 50.1, 28.1), pos=4, c(
                 "Maize",
                 "Potato",
                 "Rice",
                 "Wheat"), cex=0.5, font=2)
### add summary polygons for the crop subgroups
#addpoly(res.caul, row=18.5, mlab=mlabfun("RE Model for Subgroup", res.caul))
addpoly(res.maize, 
        row=56.5, 
        mlab=mlabfun("RE Model for Subgroup", res.maize), cex=0.45, col="darkgrey", border="darkgrey")
#addpoly(res.potato, row=(n_wheat+6+n_rice+5)-1.5, mlab=mlabfun("RE Model for Subgroup", res.potato))
addpoly(res.rice, 
        row=30.5, 
        mlab=mlabfun("RE Model for Subgroup", res.rice), cex=0.45, col="darkgrey", border="darkgrey")
addpoly(res.wheat, 
        row=1.5, 
        mlab=mlabfun("RE Model for Subgroup", res.wheat), cex=0.45, col="darkgrey", border="darkgrey")
addpoly(model1b, 
        row=-1.5, 
        mlab=mlabfun("RE Model for all studies", model1), cex=0.45, col="lightgrey", border="lightgrey")


## testing model
#Cook's Distance plot for influential studies
x<-cooks.distance(model1)
plot(x,type='o',pch=19,xlab="Study number",ylab="Cook's Distance")

#Studies 10 and 11 are highly influential. Their inclusion likely drives the meta-analytic effect size and/or heterogeneity substantially.

influential_studies <- which(x > 1)
print(influential_studies)

ES_data_sens <- ES_data[-influential_studies, ]
model_sens <- rma.mv(yi=ES, V=V, data=ES_data_sens, 
                     mods=~factor(crop)*baseline,
                     method="ML", random=~ID|1)
summary(model_sens)
summary(model1)
# model1 <- rma.mv(yi=ES,
#                  V=V,
#                  data=ES_data,
#                  mods=~factor(crop)*baseline,
#                  method="ML",
#                  random=~1|ID)
# Build a sensitivity table

# For full model
pred_full <- predict(model1)
full_est <- pred_full$pred
full_ci_lb <- pred_full$ci.lb
full_ci_ub <- pred_full$ci.ub

# Between-study variance
tau2_full <- model1b$sigma2  # or model1b$tau2 if you have it

# Average within-study sampling variance
mean_V_full <- mean(ES_data$V, na.rm = TRUE)

# I²
I2_full <- (tau2_full / (tau2_full + mean_V_full)) * 100

# For sensitivity model (after removing studies)
pred_sens <- predict(model_sens)
sens_est <- pred_sens$pred
sens_ci_lb <- pred_sens$ci.lb
sens_ci_ub <- pred_sens$ci.ub

tau2_sens <- model_sens$sigma2
mean_V_sens <- mean(ES_data_sens$V, na.rm = TRUE)

I2_sens <- (tau2_sens / (tau2_sens + mean_V_sens)) * 100

# Assemble table
results_table <- data.frame(
  Model = c("All studies", "Sensitivity (excluding studies 6 & 7)"),
  Estimate_kg_ha = round(c(full_est, sens_est), 2),
  CI_lower = round(c(full_ci_lb, sens_ci_lb), 2),
  CI_upper = round(c(full_ci_ub, sens_ci_ub), 2),
  Tau2 = round(c(model1$sigma2, model_sens$sigma2), 1),
  I2 = round(c(I2_full, I2_full), 1)
)

results_table


# Including CA_judgement as a moderator has no significant effect
model1b <- rma.mv(yi=ES, 
                 V=V, 
                 data=ES_data, 
                 mods=~factor(crop)+CA_judgement,
                 method="ML", 
                 random=~ID|1)
model1b

#publication bias
# funnel(model1)
# regtest(rma(yi=ES,vi=PSD,data=ES_data,method="ML"))

# Extract residuals and standard errors
ES_data$resid <- resid(model1, type = "response")
ES_data$sei <- sqrt(ES_data$V)  # V is the sampling variance per effect size

# 3. Regress residuals on standard errors (Nakagawa-style bias test)
bias_test <- lm(resid ~ sei, data = ES_data)


# 4. Output summary
summary(bias_test)
## if sei is significant then there is small study bias

# Plot it
plot(ES_data$sei, ES_data$resid,
     xlab = "Standard Error", ylab = "Residuals",
     main = "Small-Study Effects Test (Nakagawa)")
abline(bias_test, col = "red", lwd = 2)

