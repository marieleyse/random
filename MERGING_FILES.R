

setwd("C:/Users/Marie-e.Lafaille/Documents/MAVAN/data files/")
NEW8 = read.csv("Complete_MAVAN7.csv")

MAVAN <- read.csv("Complete_MAVAN7.csv", header=T)

ainsworth <- read.csv("data_ainsworth_best_.CSV", header=T)
#names(ainsworth)
#names(ainsworth) = paste0("Robert_", names(ainsworth), "_Bob")

PCERA6 <- read.csv("PCERA6.csv", header=T)

PCERA18 <- read.csv("PCERA18.csv", header=T)

PCERA36 <- read.csv("PCERA36.csv", header=T)

PCERA60 <- read.csv("PCERA60.csv", header=T)

MAVAN_AINSWORTH <- merge(MAVAN , ainsworth, by = "PSCID", all=TRUE)

write.csv(MAVAN_AINSWORTH, file = "Complete_MAVAN9.csv",row.names=FALSE, na="") ####CHECK IT

PCERA_NEW <- merge(PCERA6, PCERA18, by = "PSCID", all=TRUE)

PCERA_NEW2 <- merge(PCERA_NEW, PCERA36, by = "PSCID", all=TRUE)

PCERA_NEW3 <- merge(PCERA_NEW2, PCERA60, by = "PSCID", all=TRUE)

NEW <- merge(MAVAN_AINSWORTH, PCERA_NEW3, by = "PSCID", all=TRUE)

write.csv(NEW, file = "Complete_MAVAN8.csv",row.names=FALSE, na="") ####CHECK IT

#total <- merge(data frameA,data frameB,by=c("ID","Country"))
#total <- rbind(data frameA, data frameB)
#rbind( )

head(ainsworth)
head(ainsworth)

DOM <- read.csv("Dominic_data.csv", header=T)
CBCL <- read.csv("CBCL_data.csv", header=T)
NEW <-  merge(CBCL, DOM, by = "PSCID", all=TRUE)

GENE <- read.csv("Genes_2020_02_05.csv", header=T)
FULL <- read.csv("ADHD_factorscores_MAVAN_fix_wo_nan2.csv", header=T)

NEW_GENE <-  merge(GENE, NEW, by = "PSCID", all=TRUE)
NEW_FULL <-  merge(FULL, NEW_GENE, by = "PSCID", all=TRUE)

write.csv(NEW_FULL, file = "MAVAN_48M_and_up_april2020.csv",row.names=FALSE, na="") ####CHECK IT

setwd("/Users/Marie-Elyse/Downloads")

NEW <- read.csv("MAVAN_48M_and_up_april2020.csv", header=T)
MELM <- read.csv("MarieElyse.csv", header=T)

NEW_MELM <-  merge(NEW, MELM, by = "PSCID", all=FALSE)

AlphaPart::write.csv(NEW_MELM, file = "MAVAN_48M_and_up_jun2020.csv",row.names=FALSE, na="") ####CHECK IT

setwd("/Users/Marie-Elyse/Downloads")

NEW <- read.csv("NOV302020.csv", header=T)
MELM <- read.csv("genes.csv", header=T)

NEW_MELM <-  merge(NEW, MELM, by = "PSCID", all=FALSE)

write_csv(NEW_MELM, file = "FEB2021.csv", na="") ####CHECK IT

