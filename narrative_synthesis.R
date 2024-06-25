#' Meta-data/Data Visualisation (Narrative synthesis)
#' 
#' Load libraries and data
library(tidyverse)
library(networkD3)
library(readxl)
data <- read_excel("~/Library/CloudStorage/GoogleDrive-nealhaddaway@gmail.com/My Drive/contracts/CABI-Juno/Nepal/Data extraction/Meta-data extraction MASTER.xlsx", 
                                          sheet = "Data")

#' Crops
crop_sum <- data %>% 
  group_by(Crop) %>% 
  summarise(n = n())
crop_sum <- subset(crop_sum, Crop!='')
crop_sum <- rbind(c("cauliflower", 0), crop_sum)

ggplot(crop_sum, aes(x=Crop, y=as.numeric(n))) +
  geom_bar(stat="identity", fill="darkcyan") +
  xlab('Crop') +
  ylab('Studies') +
  scale_y_continuous(limits = c(0, 80))


#' Breeding methods and comparisons
#' Create breeding category labels
#`Breeding method` <- c('Domestication > Landrace (no selection)', 'Domestication > Landrace (selection)', 'Introduction > Multinational companies', 'Introduction > Int Agricultural Org', 'Introduction > Registered varieties (from another country)', 'Hybridisation > modified pedigree', 'Hybridisation > bulking method', 'Hybridisation > backcross method ', 'Hybridisation > synthetic varieties', 'Hybridisation > composite varieties', 'Other > Other (specify)')

data$control <- paste0(data$`Control (main breeding method)`, " > ", data$`Control (sub-category)`)
data$intervention <- paste0(data$`Intervention (main breeding method)`, " > ", data$`Intervention (sub-category)`)

#' Build a connection dataframe for Sankey
links <- data %>% 
  group_by(`Control (sub-category)`, `Intervention (sub-category)`) %>% 
  summarise(n = n())
    
# remove blank rows
links <- subset(links, `Control (sub-category)`!='') 
links <- subset(links, `Intervention (sub-category)`!='') 
# rename columns
colnames(links) <- c("target", "source", "value")
# specify source (intervention) or target (control) in variable value
links$target <- paste0("control = ", links$target)
links$source <- paste0("intervention = ", links$source)
    
#' Build nodes
nodes <- data.frame(name=c(as.character(links$source), 
                           as.character(links$target)) %>% unique())
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
links$group <- as.factor(links$source) # Add a 'group' column to each connection:
nodes$group <- c("a", "b", "c", "d", "e", "f", "g", 
                 "b", "g", "h", "d", "e", "f", "c", "i")  # Add a 'group' column to each node
#my_color <- 'd3.scaleOrdinal() .domain(["intervention = Multinational companies", "intervention = backcross method", "intervention = modified pedigree", "intervention = Bulking", "intervention = Int Agricultural Org", "intervention = Other (specify)", "intervention = composite varieties"]) .range(["blue","blue","blue","blue","green","blue","red"])' # Give a color for each group:
nodes$label <- tolower(sub("control = ", "", sub("intervention = ", '', nodes$name)))

sankey_widget <- sankeyNetwork(Links = links, Nodes = nodes,
                               Source = "IDsource", Target = "IDtarget",
                               Value = "value", NodeID = "label", 
                               LinkGroup="group", NodeGroup="group",
                               sinksRight=FALSE)
javascript_string <- 
  'function(el) {
     d3.select(el).selectAll(".link")
       .style("stroke", d => d.source.color);
  }'
library(htmlwidgets)
htmlwidgets::onRender(x = sankey_widget, jsCode = javascript_string)


#' Outcome
outcome_sum <- data %>% 
  group_by(Outcome, Resistance) %>% 
  summarise(n = n())

outcome_sum <- rbind(outcome_sum[1:5,], data.frame(Outcome="Yield", Resistance=NA, n=135)) #manually creating dataframe where Yield incorrectly has resistance subdata
outcome_sum$Resistance <- factor(outcome_sum$Resistance, levels=c("Cold", "Drought", "Insect/arthropod", "Pathogen", "Other (specify)", NA))

ggplot(outcome_sum, aes(x=Outcome, y=n, fill=Resistance)) +
  geom_bar(stat="identity") +
  xlab('Outcome') +
  ylab('Studies')


#' Crops by outcome
outcome_crop_sum <- data %>% 
  group_by(Outcome, Crop) %>% 
  summarise(n = n())
outcome_crop_sum <- rbind(outcome_crop_sum, data.frame(Outcome=c("Yield", "Resistance"), Crop=c("cauliflower", "cauliflower"), n=c(0,0)))
outcome_crop_sum$Crop <- factor(outcome_crop_sum$Crop, levels=rev(c("cauliflower", "maize", "potato", "rice", "wheat")))

ggplot(outcome_crop_sum, aes(x = Outcome, y = Crop, fill = n)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "darkcyan") +
  geom_text(aes(label = n), color = "black", size = 4) +
  coord_fixed() +
  xlab('Outcome') +
  ylab('Crop') +
  guides(fill = guide_colourbar(title = "Studies",
                                barwidth = 1,
                                barheight = 10))


#' Combined plot
#Study scale
scale_sum <- data %>% 
  group_by(`Study scale`) %>% 
  summarise(n = n())
scale_sum$`Study scale` <- factor(scale_sum$`Study scale`, levels=c("Plot", 
                                                                    "Field",
                                                                    "Farm",
                                                                    "Sub-national",
                                                                    "National",
                                                                    "Regional"))
scale_sum <- rbind(scale_sum, data.frame(`Study scale`="National", n=0, check.names=FALSE))
ggplot(scale_sum, aes(x=`Study scale`, y=n)) +
  geom_bar(stat="identity", fill="darkcyan") +
  xlab('Study scale') +
  ylab('Studies')

#Study length
length_sum <- data %>% 
  group_by(`Study length (measurement period in months)`) %>% 
  summarise(n = n())
length_sum$`Study length (measurement period in months)` <- as.numeric(length_sum$`Study length (measurement period in months)`)

ggplot(length_sum, aes(x=`Study length (measurement period in months)`)) +
  geom_histogram(colour="white", fill="darkcyan") +
  ylab('Studies') 

#Sample size
samplesize_sum <- data %>% 
  group_by(`Sample size (Number of true replicates)`) %>% 
  summarise(n = n())
samplesize_sum$`Sample size (Number of true replicates)` <- as.numeric(samplesize_sum$`Sample size (Number of true replicates)`)

ggplot(samplesize_sum, aes(x=`Sample size (Number of true replicates)`, y=n)) +
  geom_bar(stat="identity", fill="darkcyan") +
  ylab('Studies') +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))


#' Critical appraisal
CA_data <- read_excel("~/Library/CloudStorage/GoogleDrive-nealhaddaway@gmail.com/My Drive/contracts/CABI-Juno/Nepal/Critical appraisal/CriticalAppraisal MASTER.xlsx", 
                                                  sheet = "forR")

CA_summary <- data.frame(question=NULL, rating=NULL, n=NULL)
for (i in 6:length(colnames(CA_data))){
  high <- sum(CA_data[,i] == "HIGH", na.rm=TRUE)
  medium <- sum(CA_data[,i] == "MEDIUM", na.rm=TRUE)
  low <- sum(CA_data[,i] == "LOW", na.rm=TRUE)
  unclear <- sum(CA_data[,i] == "UNCLEAR", na.rm=TRUE)
  new_df <- data.frame(question=rep(colnames(CA_data[,i]), 4), 
                       rating=c("high", "medium", "low", "unclear"), 
                       n=c(high, medium, low, unclear))
  CA_summary <- rbind(CA_summary, new_df)
}
CA_summary$rating <- factor(CA_summary$rating, levels=c("high", "medium", "low", "unclear"))
CA_summary$question <- factor(CA_summary$question, 
                              levels=c("Appropriate replication level", "Replicate randomisation", "Replication degree", "Treatment matching", "Management differences", "Outcome measured directly", "Measurement extrapolation", "Central line measurement", "Number of plant measurements", "Random plant selection", "Reported drop out", "Reported drop out bias risk", "Missing sites, plots, samples", "Missing sites, plots, samples bias risk", "Selective reporting of results", "Selective reporting of results bias risk", "Control of unexpected events", "FINAL JUDGEMENT"))

ggplot(CA_summary, aes(fill=rating, y=n, x=question)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values = c("#de5e50", "#f0b96c", "#88e394", "#b3b5b3"), name = "Rating") +
  coord_flip() +
  xlab('Question') +
  ylab('% of studies') +
  scale_x_discrete(limits=rev)
