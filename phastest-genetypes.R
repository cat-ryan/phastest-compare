library('ggplot2')
library('tidyverse')
library('paletteer')
library('dplyr')
library('jsonlite')

# Import and Set-Up -------------------------------------------------------------

# Function to put JSON files into a list
ImportPredictedGenes <- function(file_path, genus, species) {
  fromJSON(file_path, flatten = TRUE) %>%
    mutate(genus = genus, species = species, .before = index)
}

# Create list of file paths and corresponding genus and species
pred_genes_list <- list(
  list(file = "data/Banthracis.PHASTEST/predicted_genes.json", genus = "Bacillus", species = "anthracis"),
  list(file = "data/Bcereus.PHASTEST/predicted_genes.json", genus = "Bacillus", species = "cereus"),
  list(file = "data/Bthuringiensis.PHASTEST/predicted_genes.json", genus = "Bacillus", species = "thuringiensis"),
  list(file = "data/Blicheniformis.PHASTEST/predicted_genes.json", genus = "Bacillus", species = "licheniformis"),
  list(file = "data/Cbotulinum.PHASTEST/predicted_genes.json", genus = "Clostridium", species = "botulinum"),
  list(file = "data/Cdifficile.PHASTEST/predicted_genes.json", genus = "Clostridium", species = "difficile"),
  list(file = "data/Cperf.PHASTEST/predicted_genes.json", genus = "Clostridium", species = "perfringens"),
  list(file = "data/Ctetani.PHASTEST/predicted_genes.json", genus = "Clostridium", species = "tetani")
)

# Apply function to each organism
proph_data <- lapply(pred_genes_list, function(x) {
  ImportPredictedGenes(x$file, x$genus, x$species)
})

# Combine each organism into one data frame
phage <- do.call(rbind, proph_data[c(8, 7, 6, 5, 4, 1, 3, 2)])

# rm rows for genes not in a prophage region
phage <- phage[phage$region_index != "Bacterial", ]

# create objects for convenience 
genus=phage$genus
species=phage$species
type=phage$type


# Add Groups --------------------------------------------------------------

phage$Org <- case_when(
  species == "anthracis"~"B. anthracis",
  species == "cereus"~"B. cereus",
  species == "licheniformis"~"B. licheniformis",
  species == "thuringiensis"~"B. thuringiensis",
  species == "botulinum"~"C. botulinum",
  species == "difficile"~"C. difficile",
  species == "perfringens"~"C. perfringens",
  species == "tetani"~"C. tetani",
  .default = as.character(species)
)

phage$Type <- case_when(
  type == "Attachment_site"~"Attachment site",
  type == "Fiber_protein"~"Virion",
  type == "Head_protein"~"Virion",
  type == "Hypothetical_protein"~"Hypothetical",
  type == "Non_phage-like_protein"~"Bacterial",
  type == "Phage-like_protein"~"Phage-like",
  type == "Plate_protein"~"Virion",
  type == "Portal_protein"~"Virion",
  type == "Tail_protein"~"Virion",
  type == "Terminase"~"Other",
  type == "Protease"~"Other",
  type == "Transposase"~"Other",
  type == "tRNA"~"Other",
  type == "Integrase"~"Other",
  type == "Protease"~"Other",
  type == "Lysis_protein"~"Other",
  type == "Antirepressor"~"Other",
  type == "Crossover_junction_protein"~"Other",
  type == "Endodeoxyribonuclease"~"Other",
  type == "Endonuclease"~"Other",
  type == "Holin"~"Other",
  type == "Integrase"~"Other",
  type == "Membrane_protein"~"Other",
  type == "Neck_protein"~"Virion",
  type == "phage antirepressor KilAC domain-containing protein"~"Other",
  type == "phage DNA-binding protein"~"Other",
  type == "phage exported protein"~"Other",
  type == "phage family Tn916-like,CTn6-Orf1"~"Other",
  type == "phage holin family protein"~"Other",
  type == "phage infection protein"~"Other",
  type == "phage integrase"~"Other",
  type == "phage major capsid protein"~"Virion",
  type == "phage packaging/lysis transcriptional regulator"~"Other",
  type == "phage portal protein"~"Virion",
  type == "phage protein"~"Other",
  type == "phage protein (partial)"~"Other",
  type == "phage RecT protein"~"Other",
  type == "phage related N-acetylmuramoyl-L-alanine amidase"~"Other",
  type == "phage related protein"~"Other",
  type == "phage replication protein"~"Other",
  type == "phage repressor protein"~"Other",
  type == "phage shock protein"~"Other",
  type == "phage shock protein (rhodanese)"~"Other",
  type == "phage surface protein"~"Other",
  type == "phage tail spike protein"~"Virion",
  type == "phage terminase small subunit"~"Other",
  type == "phage tyrosine recombinase"~"Other",
  type == "phage-like protein"~"Phage-like",
  type == "phage-related cell wall hydrolase"~"Other",
  type == "phage-related deoxyuridylate hydroxymethyltransferase"~"Other",
  type == "phage-related protein"~"Phage-like",
  type == "phage-related repressor"~"Other",
  type == "phage-related single-strand DNA binding protein"~"Other",
  type == "phage-type"~"Other",
  type == "Regulatory_protein"~"Other",
  type == "Repressor"~"Other",
  type == "Replication_protein"~"Other",
  .default = as.character(type)
)


# Plot --------------------------------------------------------------------

cbPalette <- c("#018EF9", "#F76902", "#74B944","#D62728FF","#F6BE00", "#9467BDFF", "#A3D4D4","#DB4A72")

ggplot(phage, aes(fill=Type, x=Org)) + 
  geom_bar() +
  ylab("Number of predicted genes") +xlab("Species") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(axis.text.x= element_text(face="italic")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.025))) +
  scale_fill_manual(values=cbPalette)
