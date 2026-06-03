ggplot(aes(x = sector, y = count), data = datalong_EmlenInd)+
  coord_polar(theta = "x", direction = 1)+
  geom_bar(stat = "identity", fill = "maroon4", width = 14)+
  scale_x_continuous("", limits=c(0,360),
                     breaks=seq(0,360-0.001,by=45),
                     labels=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
  geom_segment(data=mergedEmlen,
               mapping=aes(y = 0, x = Mean, xend = Mean, 
                           yend = (rho*max(datalong_EmlenInd$count))),
               arrow = arrow(type="closed",ends="last",length=unit(2,"mm")),
               color="black", size=0.7)+
  geom_vline(data=mergedEmlen, aes(xintercept = Lower_CI), linetype="dashed")+
  geom_vline(data=mergedEmlen, aes(xintercept = Upper_CI), linetype="dashed")+
  ggtitle("Circular Plot")+
  facet_grid(Ring~Treatment)+
  theme_minimal()


#___

library(ggplot2)
library(gridExtra)

# Create a unique PDF filename with the current date
pdf_filename <- paste0("Circular_Plot_Report_OttenbyTest1_", Sys.Date(), ".pdf")

# Open PDF device
pdf(pdf_filename, width = 8, height = 10)

# Get unique species list
species_list <- unique(datalong_EmlenInd$Species)

for (species in species_list) {
  # Filter data for the current species
  species_data <- datalong_EmlenInd[datalong_EmlenInd$Species == species, ]
  
  # Generate the plot
  p <- ggplot(aes(x = sector, y = count), data = species_data) +
    coord_polar(theta = "x", direction = 1) +
    geom_bar(stat = "identity", fill = "maroon4", width = 14) +
    scale_x_continuous("", limits=c(0,360),
                       breaks=seq(0,360-0.001,by=45),
                       labels=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
    geom_segment(data=mergedEmlen[mergedEmlen$Species == species, ],
                 mapping=aes(y = 0, x = Mean, xend = Mean, 
                             yend = (rho * max(species_data$count))),
                 arrow = arrow(type="closed", ends="last", length=unit(2,"mm")),
                 color="black", size=0.7) +
    geom_vline(data=mergedEmlen[mergedEmlen$Species == species, ], aes(xintercept = Lower_CI), linetype="dashed") +
    geom_vline(data=mergedEmlen[mergedEmlen$Species == species, ], aes(xintercept = Upper_CI), linetype="dashed") +
    ggtitle(paste("Circular Plot for", species)) +
    facet_grid(Ring ~ Treatment) +
    theme_minimal()
  
  # Print the plot on a new page
  print(p)
}

# Close the PDF device
dev.off()

# Message to confirm completion
cat("PDF report generated:", pdf_filename, "\n")

#-----

library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)

#THIS IS THE ONE i ENDED UP USING!
# Create a unique PDF filename with the current date
pdf_filename <- paste0("Circular_Plot_Report_OttenbyTest2_", Sys.Date(), ".pdf")

# Open PDF device
pdf(pdf_filename, width = 8, height = 10)

# Get unique species list
species_list <- unique(datalong_EmlenInd$Species)

for (species in species_list) {
  # Filter data for the current species
  species_data <- datalong_EmlenInd %>%
    filter(Species == species) %>%
    complete(Ring, Treatment, fill = list(count = NA))  # Ensure all treatments exist per ring
  
  # Get unique ring numbers
  ring_numbers <- unique(species_data$Ring)
  
  # Split into groups of 4 individuals per page
  ring_groups <- split(ring_numbers, ceiling(seq_along(ring_numbers) / 4))
  
  for (group in ring_groups) {
    # Filter only the current group of 4 rings
    subset_data <- species_data %>% filter(Ring %in% group)
    
    # Skip empty plots
    if (nrow(subset_data) == 0) next
    
    # Generate the plot
    p <- ggplot(aes(x = sector, y = count), data = subset_data) +
      coord_polar(theta = "x", direction = 1) +
      geom_bar(stat = "identity", fill = "maroon4", width = 14, na.rm = TRUE) +  # Handle missing data
      scale_x_continuous("", limits = c(0, 360),
                         breaks = seq(0, 360 - 0.001, by = 45),
                         labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
      geom_segment(data = mergedEmlen %>% filter(Species == species, Ring %in% group),
                   mapping = aes(y = 0, x = Mean, xend = Mean, 
                                 yend = (rho * max(subset_data$count, na.rm = TRUE))),
                   arrow = arrow(type = "closed", ends = "last", length = unit(2, "mm")),
                   color = "black", size = 0.7) +
      geom_vline(data = mergedEmlen %>% filter(Species == species, Ring %in% group), aes(xintercept = Lower_CI), linetype = "dashed") +
      geom_vline(data = mergedEmlen %>% filter(Species == species, Ring %in% group), aes(xintercept = Upper_CI), linetype = "dashed") +
      ggtitle(paste("Circular Plot for", species)) +
      facet_wrap(~Ring + Treatment, ncol = 3, scales = "free") +  # Control layout for 4 individuals per page
      theme_minimal()
    
    # Print the plot on a new page
    print(p)
  }
}

# Close the PDF device
dev.off()

# Message to confirm completion
cat("✅ PDF report generated:", pdf_filename, "\n")
