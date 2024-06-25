data <- read.csv('combined plot.csv')
data$levels <- as.factor(data$levels)
data$labels <- paste0(data$levels, " (", signif(data$yaxis,3), "%)")

library(ggplot2)
library(scales)

#colours <- c('#f0fae6', '#aadfbc', '#66bbc7', '#3995c2', '#1570b0', '#244b8b')
#levels(data$levels)


ggplot(data, aes(fill=levels, y=yaxis, x=xaxis)) + 
  geom_bar(position="fill", stat="identity", fill=data$colours) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_text(aes(label = stringr::str_wrap(labels, 19), y = (labelloc)/100),
            size = 2.2,
            angle = data$rotate,
            color = data$textcol) +
  coord_flip() +
  scale_x_discrete(labels = label_wrap(18))





