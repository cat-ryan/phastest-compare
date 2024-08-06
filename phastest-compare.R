library('ggplot2')
library('tidyverse')
library('paletteer')
library('dplyr')
library('jsonlite')
#####Setup######
BAproph <- fromJSON("PHASTEST/Banthracis.PHASTEST/predicted_genes.json", flatten=TRUE)
BAproph<-BAproph%>%
  mutate(genus=('Bacillus'),species=('anthracis'),.before=index)

BCproph <- fromJSON("PHASTEST/Bcereus.PHASTEST/predicted_genes.json", flatten=TRUE)
BCproph<-BCproph%>%
  mutate(genus=('Bacillus'),species=('cereus'),.before=index)

BTproph <- fromJSON("PHASTEST/Bthuringiensis.PHASTEST/predicted_genes.json", flatten=TRUE)
BTproph<-BTproph%>%
  mutate(genus=('Bacillus'),species=('thuringiensis'),.before=index)

BLproph <- fromJSON("PHASTEST/Blicheniformis.PHASTEST/predicted_genes.json", flatten=TRUE)
BLproph<-BLproph%>%
  mutate(genus=('Bacillus'),species=('licheniformis'),.before=index)

CBproph <- fromJSON("PHASTEST/Cbotulinum.PHASTEST/predicted_genes.json", flatten=TRUE)
CBproph<-CBproph%>%
  mutate(genus=('Clostridium'),species=('botulinum'),.before=index)

CDproph <- fromJSON("PHASTEST/Cdifficile.PHASTEST/predicted_genes.json", flatten=TRUE)
CDproph<-CDproph%>%
  mutate(genus=('Clostridium'),species=('difficile'),.before=index)

CPproph <- fromJSON("PHASTEST/Cperf.PHASTEST/predicted_genes.json", flatten=TRUE)
CPproph<-CPproph%>%
  mutate(genus=('Clostridium'),species=('perfringens'),.before=index)

CTproph <- fromJSON("PHASTEST/Ctetani.PHASTEST/predicted_genes.json", flatten=TRUE)
CTproph<-CTproph%>%
  mutate(genus=('Clostridium'),species=('tetani'),.before=index)

#combine the data frames for each organism
phage<-rbind(CTproph,CPproph,CDproph,CBproph,BLproph,BAproph,BTproph,BCproph)

#rm rows for genes not in a prophage region
phage <- phage[phage$region_index != "Bacterial", ]

#create objects
genus=phage$genus
species=phage$species
type=phage$type


####Add Groups ######

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


#####Plot#####
cbPalette <- c("#018EF9", "#F76902", "#74B944","#D62728FF","#F6BE00", "#9467BDFF", "#A3D4D4","#DB4A72")

ggplot(phage, aes(fill=Type, x=Org)) + 
  geom_bar() +
  ylab("Number of predicted genes") +xlab("Species") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(axis.text.x= element_text(face="italic")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.025))) +
  scale_fill_manual(values=cbPalette)
