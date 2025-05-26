library(readxl)
library(ggplot2)
library(tidyverse)

data <- read_excel("~/Library/CloudStorage/GoogleDrive-nealhaddaway@gmail.com/My Drive/contracts/CABI-Juno/Nepal/synthesis/all abstract screening decisions.xlsx", 
                                               sheet = "includes")

crops <- c("maize", "corn", "Zea mays", "rice", "paddy", "paddies", "Oryza sativa", "wheat", "Triticum aestivum", "cauliflower", "Brassica oleracea", "potato", "Solanum tuberosum")
yield <- c("yield", "crop production", "harvest", "biomass", "quality", "dry matter", "test weight", "loss", "performance", "crop diversity", "varietal richness", "varietal diversity", "allelic diversity", "genetic diversity", "food security", "crop improvement", "nutrition security", "nutritional security", "food scarcity", "new varieties")
resistance <- c("stress", "disease", "pathogen", "blast", "blight", "gall midge", "false smut", "rust", "loose smut", "insect", "pest", "fungus", "fungal", "fungi", "bacteri", "virus", "viral", "cold", "frost", "temperature", "heat", "heatwave", "submerg", "flood", "water damage", "climate", "drought", "water limitation", "salinity", "lodging", "water logg", "waterlogg")
countries <- c("Nepal", "South Asia", "Afghanistan", "Bangladesh", "Bhutan", "India", "Pakistan", "Sri Lanka")

f <- function(pattern, data) (sum(grepl(pattern, data)))

# plot crop terms
# crops
df <- set
for (i in 1:length(crops)){
  t <- !!rowSums(sapply(set, grepl, pattern = crops[i])) 
  df$new <- t
  colnames(df)[colnames(df) == 'new'] <- crops[i]
}
crops_df <- data.frame(term = crops, 
                       title = unlist(lapply(crops, f, data=data$title)), 
                       abstract = unlist(lapply(crops, f, data=data$abstract)),
                       keywords = unlist(lapply(crops, f, data=data$keywords)),
                       t_a_c = colSums(Filter(is.logical, df)))
#crops_sum <- crops_df[,1:4] %>% 
#  pivot_longer(
#   -term,
#    names_to = "location",
#   values_to = "hits")
crops_df$term <- factor(crops_sum$term, levels = c("maize", "corn", "Zea mays", "rice", "paddy", "paddies", "Oryza sativa", "wheat", "Triticum aestivum", "cauliflower", "Brassica oleracea", "potato", "Solanum tuberosum"))
ggplot(data=crops_df, aes(y=term, x=t_a_c)) +
  geom_bar(stat="identity") +
  scale_y_discrete(limits = rev) +
  ylab("Term") + xlab("Records") +
  theme_minimal() +
  geom_hline(yintercept = 2.5) +
  geom_hline(yintercept = 4.5) +
  geom_hline(yintercept = 6.5) +
  geom_hline(yintercept = 10.5)

crops_sum2 <- data.frame(hits=crops_df[,5])
crops_sum2$crop <- c("maize", "maize", "maize", "rice", "rice", "rice", "rice", "wheat", "wheat", "cauliflower", "cauliflower", "potato", "potato")
crops_sum2 <- crops_sum2 %>% 
  group_by(crop) %>% 
  summarise(hits = sum(hits))
ggplot(data=crops_sum2, aes(y=crop, x=hits)) +
  geom_bar(stat="identity") +
  scale_y_discrete(limits = rev) +
  ylab("Crop") + xlab("Records") +
  theme_minimal()


# plot country terms
# countries
df <- set
for (i in 1:length(countries)){
  t <- !!rowSums(sapply(set, grepl, pattern = countries[i])) 
  df$new <- t
  colnames(df)[colnames(df) == 'new'] <- countries[i]
}
countries_df <- data.frame(term = countries, 
                           title = unlist(lapply(countries, f, data=data$title)), 
                           abstract = unlist(lapply(countries, f, data=data$abstract)),
                           keywords = unlist(lapply(countries, f, data=data$keywords)),
                           t_a_c = colSums(Filter(is.logical, df)))

#countries_sum <- countries_df[,1:4] %>% 
#  pivot_longer(
#    -term,
#    names_to = "location",
#   values_to = "hits")
ggplot(data=countries_df, aes(y=term, x=t_a_c)) +
  geom_bar(stat="identity") +
  scale_y_discrete(limits = rev) +
  ylab("Country") + xlab("Records") +
  theme_minimal()


# plot yield terms
df <- set
for (i in 1:length(yield)){
  t <- !!rowSums(sapply(set, grepl, pattern = yield[i])) 
  df$new <- t
  colnames(df)[colnames(df) == 'new'] <- yield[i]
}
yield_df <- data.frame(term = yield, 
                       title = unlist(lapply(yield, f, data=data$title)), 
                       abstract = unlist(lapply(yield, f, data=data$abstract)),
                       keywords = unlist(lapply(yield, f, data=data$keywords)),
                       t_a_c = colSums(Filter(is.logical, df)))

#yield_sum <- yield_df[,1:4] %>% 
#  pivot_longer(
#    -term,
#    names_to = "location",
#    values_to = "hits")
ggplot(data=yield_df, aes(y=term, x=t_a_c)) +
  geom_bar(stat="identity") +
  scale_y_discrete(limits = rev) +
  ylab("Term") + xlab("Records") +
  theme_minimal()

# plot resistance terms
df <- set
for (i in 1:length(resistance)){
  t <- !!rowSums(sapply(set, grepl, pattern = resistance[i])) 
  df$new <- t
  colnames(df)[colnames(df) == 'new'] <- resistance[i]
}
resistance_df <- data.frame(term = resistance, 
                            title = unlist(lapply(resistance, f, data=data$title)), 
                            abstract = unlist(lapply(resistance, f, data=data$abstract)),
                            keywords = unlist(lapply(resistance, f, data=data$keywords)),
                            t_a_c = colSums(Filter(is.logical, df)))

#resistance_sum <- resistance_df[,1:4] %>% 
#  pivot_longer(
#   -term,
#    names_to = "location",
#    values_to = "hits")
ggplot(data=resistance_df, aes(y=term, x=t_a_c)) +
  geom_bar(stat="identity") +
  scale_y_discrete(limits = rev) +
  ylab("Term") + xlab("Records") +
  theme_minimal()


#-----------

# retrieved Nepal studies (n=392)
data <- read_excel("~/Library/CloudStorage/GoogleDrive-nealhaddaway@gmail.com/My Drive/contracts/CABI-Juno/Nepal/Screening/Full text screening/Nepal for retrieval.xlsx", 
                   sheet = "retrieved")
