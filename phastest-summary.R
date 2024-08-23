library('dplyr')
library('tidyverse')
library('ggplot2')

# Define file paths and organisms -----------------------------------------
# List of summary.txt file paths with corresponding genus and species
file_paths <- list(
  list(file = "data/Banthracis.PHASTEST/summary.txt", genus = "Bacillus", species = "anthracis"),
  list(file = "data/Bcereus.PHASTEST/summary.txt", genus = "Bacillus", species = "cereus"),
  list(file = "data/Bthuringiensis.PHASTEST/summary.txt", genus = "Bacillus", species = "thuringiensis"),
  list(file = "data/Blicheniformis.PHASTEST/summary.txt", genus = "Bacillus", species = "licheniformis"),
  list(file = "data/Cbotulinum.PHASTEST/summary.txt", genus = "Clostridium", species = "botulinum"),
  list(file = "data/Cdifficile.PHASTEST/summary.txt", genus = "Clostridium", species = "difficile"),
  list(file = "data/Cperf.PHASTEST/summary.txt", genus = "Clostridium", species = "perfringens"),
  list(file = "data/Ctetani.PHASTEST/summary.txt", genus = "Clostridium", species = "tetani")
)


# Get Genome Length -------------------------------------------------------
# Function to extract the genome length from the summary.txt file
ExtractGenomeLength <- function(file_path, genus, species) {
  lines <- readLines(file_path)
  # RegEx to extract the genome length
  match <- regmatches(lines[32], regexpr("\\d+(?=, gc%)", lines[32], perl = TRUE))
  number <- as.numeric(match)
  return(list(genome_length = number, genus = genus, species = species))
}

# Create a data frame of genome lengths w genus and species names
genome_data <- do.call(rbind, lapply(file_paths, function(x) {
  result <- ExtractGenomeLength(x$file, x$genus, x$species)
  return(data.frame(
    genome_length = result$genome_length,
    genus = result$genus,
    species = result$species,
    stringsAsFactors = FALSE
  ))
}))

# Get Total Length of Phage Regions ---------------------------------------

# Function create a data frame from the summary.txt table
ExtractSummaryTable <- function(file_path, genus, species) {
  lines <- readLines(file_path)
  table_lines <- lines[35:length(lines)]  # define which lines contain the data
  header_lines <- lines[33]  # define which line has the headers
  # if the table is empty, return an empty data frame
  if (length(table_lines) == 0) {
    return(data.frame())
  }
  # make a frame for the header  
  header <- read.table(text = paste(header_lines, collapse = "\n"), nrows = 1, header = FALSE, stringsAsFactors = FALSE)
  # read in the table 
  table_data <- read.table(text = paste(table_lines, collapse = "\n"), header = FALSE, stringsAsFactors = FALSE)
  
  colnames(table_data) <- unlist(header)  # add column names
  table_data$genus <- genus
  table_data$species <- species 
  return(table_data)
}

# Combine the data frames
combined_data <- do.call(rbind, lapply(file_paths, function(y) {
  result2 <-  ExtractSummaryTable(y$file, y$genus, y$species)
}))

# Remove "Kb"
combined_data$REGION_LENGTH <- str_remove(combined_data$REGION_LENGTH, "Kb")
# Convert to numeric and multiply by 1000 to get bp (same units as genome size)
combined_data$REGION_LENGTH <- 1000*(as.numeric(combined_data$REGION_LENGTH))

# Sum of region lengths for each species
region_data <- combined_data %>%
  group_by(species) %>%
  summarise(region_length_bp = sum(REGION_LENGTH, na.rm = TRUE))

# Get % Genome Phage Derived ----------------------------------------------
#combine the region data and the genome data
species_summary <- merge(region_data,genome_data,by="species")

species_summary$perc_phage<- 100*(species_summary$region_length_bp/species_summary$genome_length)

# Plot --------------------------------------------------------------------

# Rename the species to make the plot look nicer
species_summary$Org <- case_when(
  species_summary$species == "anthracis"~"B. anthracis",
  species_summary$species  == "cereus"~"B. cereus",
  species_summary$species  == "licheniformis"~"B. licheniformis",
  species_summary$species  == "thuringiensis"~"B. thuringiensis",
  species_summary$species  == "botulinum"~"C. botulinum",
  species_summary$species  == "difficile"~"C. difficile",
  species_summary$species  == "perfringens"~"C. perfringens",
  species_summary$species  == "tetani"~"C. tetani",
  .default = as.character(species_summary$species )
)

# Plot
ggplot(species_summary, aes(y=perc_phage, x=Org, fill=Org)) + 
  geom_bar(stat = "identity")+
  ylab("Percent Genome Phage Derived") +xlab("Species") +
  theme(axis.text.x= element_text(face="italic")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.025)))

