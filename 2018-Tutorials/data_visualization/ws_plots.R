library (tidyverse)
library(ggthemes)

getwd ()

meta <- read_delim ("data/Centralia_temperature.txt", delim="\t")
data <- read_delim("data/gene_abundance_centralia.txt", delim = "\t")

##annotate data

data.annotated <-  data %>%
  left_join(meta, by = "Site")
write.table(data.annotated, file = "output/data_annotated.txt", 
            quote = FALSE, row.names = FALSE, sep = "\t")

##Subset

arsM <- data.annotate %>%
  subset(Gene == "arsM")

##Plotting

ggplot(arsM, aes(x = Fire_history, y = Normalized.abundance)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, size = 2, aes(color = Temperature)) +
      scale_color_continuous(low = "yellow", high = "red") +
      coord_flip()

(boxplot <- ggplot(data.annotated, aes(x = Fire_history, y = Normalized.abundance)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, size = 2, aes(color = Temperature)) +
      scale_color_continuous(low = "yellow", high = "red") +
      facet_wrap(~Gene, scales = "free_y") +
      theme_classic(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      ylab("Normalized Abundance (rplB)") +
      xlab("Fire History") + 
      labs(color = "Temperature (˚C)"))

ggsave(boxplot,filename = "figures/gene.boxplot.eps", units = "in", 
        width = 6, height = 4, dpi = 300)


ggplot(data.annotated, aes(x = Site, y = Normalized.abundance)) +
      geom_bar(stat = "identity", aes(fill = Gene), color = "black",
           position = "fill" )

###Challenge 1

ggplot (data.annotated, aes(x = Temperature, y = Normalized.abundance)) +
        geom_smooth(method = 'lm', se = TRUE, linetype= "dashed", color = "black") + 
        geom_point(aes(color = Gene, shape = Fire_history)) +
        facet_wrap(~Gene, scales = "free_y") +
        theme_classic() +
        xlab("Temperature (˚C)") +
        ylab("Normalized Abundance")

##Challenge 2

ggplot(data.annotated, aes(x = Site, y = Gene)) +
  geom_point(aes(size = Normalized.abundance, color = Temperature)) +
  geom_point(aes(size = Normalized.abundance), shape = 1, color = "black") +
  scale_color_continuous(low = "yellow", high = "red") +
  theme_bw() +
  coord_flip()
