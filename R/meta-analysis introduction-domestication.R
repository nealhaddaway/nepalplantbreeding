#' Meta-analysis for Nepal plant breeding meta-analysis

# import data
library(readxl)
library(metafor)

data <- read_excel("Full data extraction sheet.xlsx", 
                  sheet = "full")

# Introduction - Domestication
# subset data
int_dom_data <- subset(data, `int-dom`==1)

# calculate effect sizes
ES_data <- data.frame(ID=NULL, short_citation=NULL, crop=NULL, ES=NULL, PSD=NULL, CA_judgement=NULL)
for (i in 1:length(unique(int_dom_data$ID))){
  subset_data <- subset(int_dom_data, ID==unique(int_dom_data$ID)[i])
  # prepare data
  short_cit <- subset_data$short_citation[1]
  crop <- subset_data$crop[1]
  CA_judgement <- subset_data$CA_judgement[1]
  # subset interventions
  domest <- subset(subset_data, `intervention main category`=="domestication") 
  introd <- subset(subset_data, `intervention main category`=="introduction")
  #calculate ES and PSD
  ES <- introd$`mean (kg/ha)` - domest$`mean (kg/ha)`
  PSD <- sqrt((((introd$n-1)*(introd$SD^2))+((domest$n-1)*(domest$SD^2))) / (introd$n+domest$n-2))
    
  # generate df
  new_data <- data.frame(ID=unique(int_dom_data$ID)[i], 
                         short_citation=short_cit, 
                         crop=crop, 
                         ES=ES, 
                         PSD=PSD,
                         CA_judgement=CA_judgement)
  ES_data <- rbind(ES_data, new_data)
}
ES_data$comparison <- rep("Introducton - Domestication", nrow(ES_data))

#sort by crop and short citation
ES_data <- ES_data[order(ES_data$crop, rev(ES_data$short_citation)),]

# set up model
model1 <- rma.mv(yi=ES, 
                V=PSD, 
                data=ES_data, 
                mods=~factor(crop),
                method="ML", 
                random=~ID|1)
model1b <- rma.mv(yi=ES, 
                 V=PSD, 
                 data=ES_data, 
                 method="ML", 
                 random=~ID|1)
summary(model1)

### a little helper function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, x) {
  list(bquote(paste(.(text),
                    " (Q = ", .(fmtx(x$QE, digits=2)),
                    ", df = ", .(x$k - x$p), ", ",
                    .(fmtp(x$QEp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)), "; ",
                    I^2, " = ", .(fmtx(x$I2, digits=1)), "%, ",
                    tau^2, " = ", .(fmtx(x$tau2, digits=2)), ")")))}

# fit models for each group
subset_caul <- subset(ES_data, crop=="cauliflower")
res.caul <- rma.mv(yi=ES, 
                   V=PSD, 
                   data=subset_caul, 
                   method="ML", 
                   random=~ID|1)
subset_maize <- subset(ES_data, crop=="maize")
res.maize <- rma.mv(yi=ES, 
                    V=PSD, 
                    data=subset_maize, 
                    method="ML", 
                    random=~ID|1)
subset_potato <- subset(ES_data, crop=="potato")
res.potato <- rma.mv(yi=ES, 
                     V=PSD, 
                     data=subset_potato, 
                     method="ML", 
                     random=~ID|1)
subset_rice <- subset(ES_data, crop=="rice")
res.rice <- rma.mv(yi=ES, 
                   V=PSD, 
                   data=subset_rice, 
                   method="ML", 
                   random=~ID|1)
subset_wheat <- subset(ES_data, crop=="wheat")
res.wheat <- rma.mv(yi=ES, 
                    V=PSD, 
                    data=subset_wheat, 
                    method="ML", 
                    random=~ID|1)

# forest plot
# calculate groups
n_cauliflower <- nrow(subset(ES_data, crop=="cauliflower"))
n_maize <- nrow(subset(ES_data, crop=="maize"))
n_potato <- nrow(subset(ES_data, crop=="potato"))
n_rice <- nrow(subset(ES_data, crop=="rice"))
n_wheat <- nrow(subset(ES_data, crop=="wheat"))

# plot
forest(model1, addfit=FALSE, cex=0.45, xlab="Effect size (kg/ha)",
       cex.lab=0.6, 
       header="First author (Year)",
       slab=short_citation,
       ilab=crop,
       ilab.xpos=-3900,
       ylim=c(-1,36),
       xlim=c((-6200), (4200)),
       rows=c(31:23, 18:17, 12:8, 3:3),
       at=c(seq(-2000, 2000, 1000))
)
text(1500, 17, "point not displayed    ", cex=0.35)

# replace CA text with coloured text
CA_col <- ES_data$CA_judgement
CA_col <- gsub('HIGH', 'darkred', CA_col)
CA_col <- gsub('LOW', 'darkgreen', CA_col)
CA_col <- gsub('MEDIUM', 'darkorange', CA_col)
CA_col <- gsub('UNCLEAR', 'darkgrey', CA_col)
text(-2900, 
     c(31:23, 18:17, 12:8, 3:3), 
     ES_data$CA_judgement, col=c(CA_col), cex=0.45, font=2)
text(-3900, 35, 'Crop', cex=0.45, font=2)
text(-2900, 35, 'CA Judgement', cex=0.45, font=2)
### add text for the subgroups
text(-6200, c(32.1, 19.1, 13.1, 4.1), pos=4, c(
                 "Maize",
                 "Potato",
                 "Rice",
                 "Wheat"), cex=0.5, font=2)
### add summary polygons for the crop subgroups
#addpoly(res.caul, row=18.5, mlab=mlabfun("RE Model for Subgroup", res.caul))
addpoly(res.maize, 
        row=21.5, 
        mlab=mlabfun("RE Model for Subgroup", res.maize), cex=0.45, col="darkgrey", border="darkgrey")
addpoly(res.potato, 
        row=15.5, 
        mlab=mlabfun("RE Model for Subgroup", res.potato), cex=0.45, col="white", border="white", alpha=0)
text(3157, 15.5, "_", cex=18, col="white")
text(3080, 15.5, "9213.33 [ -5331.95, 23758.61]", cex=0.465)
addpoly(res.rice, 
        row=6.5, 
        mlab=mlabfun("RE Model for Subgroup", res.rice), cex=0.45, col="darkgrey", border="darkgrey")
#addpoly(res.wheat, row=1.5, mlab=mlabfun("RE Model for Subgroup", res.wheat), cex=0.45, col="darkgrey", border="darkgrey")
addpoly(model1b, 
        row=0, 
        mlab=mlabfun("RE Model for all studies", model1), cex=0.45, col="lightgrey", border="lightgrey")
text(3157, 0, "-", cex=22, col="white")
text(3157, 0, "1565.95 [ -623.71, 3755.62]", cex=0.465)

## testing model
#Cook's Distance plot for influential studies
x<-cooks.distance(model1)
plot(x,type='o',pch=19,xlab="Study number",ylab="Cook's Distance")

# Including CA_judgement as a moderator has no significant effect
model1b <- rma.mv(yi=ES, 
                 V=PSD, 
                 data=ES_data, 
                 mods=~factor(crop)+CA_judgement,
                 method="ML", 
                 random=~ID|1)
model1b

#publication bias
funnel(model1)
regtest(rma(yi=ES,vi=PSD,data=ES_data,method="ML"))

