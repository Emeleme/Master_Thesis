#-----


library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)

# Create a unique PDF filename with the current date
pdf_filename <- paste0("Circular_Plot_Report_Stensoffa_Test1", Sys.Date(), ".pdf") 

# Open PDF device
pdf(pdf_filename, width = 8, height = 10)

# Get unique species list
species_list <- unique(stf_datalong_EmlenInd$Species)


for (species in species_list) {
  # Filter data for the current species
  species_data <- stf_datalong_EmlenInd %>%
    filter(Species == species) %>%
    complete(Ring, Treatment, Experiment, fill = list(count = NA))  # Ensure all treatments exist per ring
  
  # Get unique ring numbers
  ring_numbers <- unique(species_data$Ring)
  
  # Split into groups of 4 individuals per page
  ring_groups <- split(ring_numbers, ceiling(seq_along(ring_numbers) / 4))
  
  for (group in ring_groups) {
    # Filter only the current group of 4 rings
    subset_data <- species_data %>% filter(Ring %in% group)
    
    # Skip empty plots
    if (nrow(subset_data) == 0) next
    
    # Create clean facet labels: Ring, Treatment, Experiment stacked
    subset_data$facet_label <- with(subset_data, paste(Ring, Treatment, Experiment, sep = "\n"))
    
    # Generate the plot
    p <- ggplot(aes(x = sector, y = count), data = subset_data) +
      coord_polar(theta = "x", direction = 1) +
      geom_bar(stat = "identity", fill = "maroon4", width = 14, na.rm = TRUE) +
      scale_x_continuous("", limits = c(0, 360),
                         breaks = seq(0, 360 - 0.001, by = 45),
                         labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
      
      # Arrows
      geom_segment(data = stf_mergedEmlen %>% filter(Species == species, Ring %in% group),
                   mapping = aes(y = 0, x = Mean, xend = Mean, 
                                 yend = (rho * max(subset_data$count, na.rm = TRUE))),
                   arrow = arrow(type = "closed", ends = "last", length = unit(2, "mm")),
                   color = "black", size = 0.7) +
      
      # Confidence intervals
      geom_vline(data = stf_mergedEmlen %>% filter(Species == species, Ring %in% group),
                 aes(xintercept = Lower_CI), linetype = "dashed") +
      geom_vline(data = stf_mergedEmlen %>% filter(Species == species, Ring %in% group),
                 aes(xintercept = Upper_CI), linetype = "dashed") +
      
      # Title and layout
      ggtitle(paste("Circular Plot for", species)) +
      facet_wrap(~facet_label, ncol = 2, scales = "free") +
      theme_minimal()
    
    print(p)
  }
}

# Close the PDF device
dev.off()

# Message to confirm completion
cat("✅ PDF report generated:", pdf_filename, "\n")


























#THIS IS THE CLOSEST TO WORK
# Create a unique PDF filename with the current date
pdf_filename <- paste0("Circular_Plot_Report_Stensoffa_Test2", Sys.Date(), ".pdf")

# Open PDF device
pdf(pdf_filename, width = 8, height = 10)

# Get unique species list
species_list <- unique(stf_datalong_EmlenInd$Species)



for (species in species_list) {
  # Filter data for the current species
  species_data <- stf_datalong_EmlenInd %>%
    filter(Species == species) %>%
    complete(Ring, Treatment, Experiment, fill = list(count = NA))  # Ensure all treatments exist per ring
  
  # Get unique ring numbers
  ring_numbers <- unique(species_data$Ring)

  # Split into groups of 4 individuals per page
  ring_groups <- split(ring_numbers, ceiling(seq_along(ring_numbers) / 4))
  
  for (group in ring_groups) {
    # Filter only the current group of 4 rings
    subset_data <- species_data %>% filter(Ring %in% group)
    
    #Treatment new
    label_data <- subset_data %>%
      group_by(Ring, Experiment) %>%
      summarise(Treatment = unique(Treatment), 
                x = 0,  # center
                y = max(count, na.rm = TRUE) * 1.05,  # slightly above top bar
                .groups = "drop")

    
    # Skip empty plots
    if (nrow(subset_data) == 0) next
    
    # Generate the plot
    p <- ggplot(aes(x = sector, y = count), data = subset_data) +
      coord_polar(theta = "x", direction = 1) +
      geom_bar(stat = "identity", fill = "maroon4", width = 14, na.rm = TRUE) +  # Handle missing data
      geom_text(data = label_data, aes(x = x, y = y, label = Treatment),
                inherit.aes = FALSE, size = 3)+
      scale_x_continuous("", limits = c(0, 360),
                         breaks = seq(0, 360 - 0.001, by = 45),
                         labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
      geom_segment(data = stf_mergedEmlen %>% filter(Species == species, Ring %in% group),
                   mapping = aes(y = 0, x = Mean, xend = Mean, 
                                 yend = (rho * max(subset_data$count, na.rm = TRUE))),
                   arrow = arrow(type = "closed", ends = "last", length = unit(2, "mm")),
                   color = "black", size = 0.7) +
      geom_vline(data = stf_mergedEmlen %>% filter(Species == species, Ring %in% group), 
                 aes(xintercept = Lower_CI), linetype = "dashed") +
      geom_vline(data = stf_mergedEmlen %>% filter(Species == species, Ring %in% group), 
                 aes(xintercept = Upper_CI), linetype = "dashed") +
      ggtitle(paste("Circular Plot for", species)) +
      facet_wrap(~Ring + Experiment, ncol = 2, scales = "free") +  # Control layout for 4 individuals per page
      theme_minimal()
    
    # Print the plot on a new page
    print(p)
  }
}

# Close the PDF device
dev.off()

# Message to confirm completion
cat("✅ PDF report generated:", pdf_filename, "\n")



















# Create a unique PDF filename with the current date
pdf_filename <- paste0("Circular_Plot_Report_Stensoffa_Test3", Sys.Date(), ".pdf")

# Open PDF device
pdf(pdf_filename, width = 8, height = 10)

# Get unique species list
species_list <- unique(stf_datalong_EmlenInd$Species)



for (species in species_list) {
  # Filter data for the current species
  species_data <- stf_datalong_EmlenInd %>%
    filter(Species == species) %>%
    complete(Ring, Treatment, Experiment, fill = list(count = NA))  # Ensure all treatments exist per ring
  
  # Get unique ring numbers
  ring_numbers <- unique(species_data$Ring)
  
  # Split into groups of 4 individuals per page
  ring_groups <- split(ring_numbers, ceiling(seq_along(ring_numbers) / 4))
  
  for (group in ring_groups) {
    # Filter only the current group of 4 rings
    subset_data <- species_data %>% filter(Ring %in% group)
    
    #Treatment new
    label_data <- subset_data %>%
      group_by(Ring, Experiment, Treatment)
    #%>%
     # summarise(x = 360,  # center
      #          y = max(count, na.rm = TRUE) * 1.05,  # slightly above top bar
       #         .groups = "drop")
    
    
    # Skip empty plots
    if (nrow(subset_data) == 0) next
    
    # Generate the plot
    p <- ggplot(aes(x = sector, y = count), data = subset_data) +
      coord_polar(theta = "x", direction = 1) +
      geom_bar(stat = "identity", fill = "maroon4", width = 14, na.rm = TRUE) +  # Handle missing data
      ##geom_text(data = label_data, aes(x=360, y=1, label = paste(Ring, "\n", Treatment, "\n", Experiment)))+
      #geom_text(data = label_data, aes(x = x, y = y, label = Treatment),
       #         inherit.aes = FALSE, size = 3)+
      scale_x_continuous("", limits = c(0, 360),
                         breaks = seq(0, 360 - 0.001, by = 45),
                         labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
      geom_segment(data = stf_mergedEmlen %>% filter(Species == species, Ring %in% group),
                   mapping = aes(y = 0, x = Mean, xend = Mean, 
                                 yend = (rho * max(subset_data$count, na.rm = TRUE))),
                   arrow = arrow(type = "closed", ends = "last", length = unit(2, "mm")),
                   color = "black", size = 0.7) +
      geom_vline(data = stf_mergedEmlen %>% filter(Species == species, Ring %in% group), 
                 aes(xintercept = Lower_CI), linetype = "dashed") +
      geom_vline(data = stf_mergedEmlen %>% filter(Species == species, Ring %in% group), 
                 aes(xintercept = Upper_CI), linetype = "dashed") +
      ggtitle(paste("Circular Plot for", species)) +
      facet_wrap(~Ring + Treatment + Experiment, ncol = 2, nrow = 24, scales = "free") +  # Control layout for 4 individuals per page
      theme_minimal()
    
    # Print the plot on a new page
    print(p)
  }
}

# Close the PDF device
dev.off()

# Message to confirm completion
cat("✅ PDF report generated:", pdf_filename, "\n")
















#claude 4- FUNCION+O°!!!!!!!!
# Create a unique PDF filename with the current date
pdf_filename <- paste0("Circular_Plot_Report_Stensoffa_Test4", Sys.Date(), ".pdf")
# Open PDF device
pdf(pdf_filename, width = 8, height = 10)
# Get unique species list
species_list <- unique(stf_datalong_EmlenInd$Species)

for (species in species_list) {
  # Filter data for the current species
  species_data <- stf_datalong_EmlenInd %>%
    filter(Species == species) %>%
    complete(Ring, Treatment, Experiment, fill = list(count = NA))
  
  # Get unique ring numbers
  ring_numbers <- unique(species_data$Ring)
  
  # Count how many facets per ring (Treatment x Experiment combinations)
  facets_per_ring <- species_data %>%
    distinct(Ring, Treatment, Experiment) %>%
    count(Ring) %>%
    tibble::deframe()  # named vector: Ring -> n facets
  
  # Group rings so that total facets per page <= 8 (4 rows x 2 cols)
  page_groups <- list()
  current_group <- c()
  current_count <- 0
  
  for (ring in ring_numbers) {
    n <- facets_per_ring[as.character(ring)]
    if (current_count + n > 16 && length(current_group) > 0) {
      page_groups <- append(page_groups, list(current_group))
      current_group <- c(ring)
      current_count <- n
    } else {
      current_group <- c(current_group, ring)
      current_count <- current_count + n
    }
  }
  if (length(current_group) > 0) page_groups <- append(page_groups, list(current_group))
  
  for (group in page_groups) {
    # Filter only the current group of rings
    subset_data <- species_data %>% filter(Ring %in% group)
    
    # Treatment new
    label_data <- subset_data %>%
      group_by(Ring, Experiment, Treatment)
    
    # Skip empty plots
    if (nrow(subset_data) == 0) next
    
    # Generate the plot
    p <- ggplot(aes(x = sector, y = count), data = na.omit(subset_data)) +
      coord_polar(theta = "x", direction = 1) +
      geom_bar(stat = "identity", fill = "maroon4", width = 14, na.rm = TRUE) +
      scale_x_continuous("", limits = c(0, 360),
                         breaks = seq(0, 360 - 0.001, by = 45),
                         labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
      geom_segment(data = stf_mergedEmlen %>% filter(Species == species, Ring %in% group),
                   mapping = aes(y = 0, x = Mean, xend = Mean, 
                                 yend = (rho * max(subset_data$count, na.rm = TRUE))),
                   arrow = arrow(type = "closed", ends = "last", length = unit(2, "mm")),
                   color = "black", size = 0.7) +
      geom_vline(data = stf_mergedEmlen %>% filter(Species == species, Ring %in% group), 
                 aes(xintercept = Lower_CI), linetype = "dashed") +
      geom_vline(data = stf_mergedEmlen %>% filter(Species == species, Ring %in% group), 
                 aes(xintercept = Upper_CI), linetype = "dashed") +
      ggtitle(paste("Circular Plot for", species)) +
      facet_wrap(~Ring + Treatment + Experiment, ncol = 2, scales = "free", drop = TRUE) +
      theme_minimal()
    
    # Print the plot on a new page
    print(p)
  }
}
# Close the PDF device
dev.off()
# Message to confirm completion
cat("✅ PDF report generated:", pdf_filename, "\n")

