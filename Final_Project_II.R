# Miles Spathelf
# Bio 694 - Final Project II
# Last updated 12/15/22

# Dataset used from https://opentraits.org/datasets/avianhwi
# Author Catherine Sheard - catherine.sheard@bristol.ac.uk
# Description Hand-wing index values for 10,338 species, together with a variety of ecological and environmental traits, matched to the Jetz et al. 2012 phylogeny and to IUCN Red List scientific names.
# Citation https://doi.org/10.1038/s41467-020-16313-6
# Website https://github.com/catherinesheard/Global-HWI

# Load libraries
library(dplyr)
library("corrplot")



#Set working directory and load bird data file
setwd("./GitHub/Final_project")
bird_traits <- read.csv("speciesdata-Table 1.csv", header = T)

#remove NAs from dataset
bird_traits_na_rem <- na.omit(bird_traits)

#remove most categorical data - I know Migration.2 is a ranked categorical variable and "1.2" (Sedentary but a little partially migratory?) might be a bit nonsensical but I'm interested if what might come out of it when looking at bird orders
bird_traits_analysis <- bird_traits_na_rem[,c("Order", "HWI", "Body.mass..log.", "Range.Size", "Island", "Migration.2", "Habitat", "Latitude", "AnnualTemp", "TempRange", "AnnualPrecip", "PrecipRange")]

#find means by orders
bird_traits_means <- bird_traits_analysis %>%
                    group_by(Order) %>%
                    summarise_at(vars("HWI", "Body.mass..log.", "Range.Size", "Island", "Migration.2", "Habitat", "Latitude", "AnnualTemp", "TempRange", "AnnualPrecip", "PrecipRange"), mean)

#Output csv of bird_traits_means
write.csv (bird_traits_means, file = "bird_traits_means.csv")

#Remove text based categorical variable for correlation plot
bird_traits_order <- bird_traits_means[,c("HWI", "Body.mass..log.", "Range.Size", "Island", "Migration.2", "Habitat", "Latitude", "AnnualTemp", "TempRange", "AnnualPrecip", "PrecipRange")]

#Look at potential correlations of variables
bird_order_cor <- cor(bird_traits_order, use="complete")

corrplot(bird_order_cor, method = "ellipse")
dev.copy2pdf(file = "./Correlation_Plot_Bird_Traits.pdf",
             width = 6, height = 4, bg = "white", compress = F, out.type = "pdf")

#Examine PCA of bird data by scaling first - Body mass and island status relationship is interesting but I suppose it follows the "island rule" for vertebrates. 
bird_order_std <- scale(bird_traits_order, center = T, scale = T)
bird_pca <- prcomp(na.omit(bird_order_std))
biplot(bird_pca)

png("Bird_traits_by_Order_PCA.png", width = 2200, height = 2200, pointsize = 16)
par(mar = c(5, 5, 3, 1.5))
biplot(bird_pca)
dev.off()

#Owls are cool - subset data for just the order of owls
Owl_data <- subset(bird_traits_na_rem, Order == "Strigiformes")

#Filter for partially migratory Owls
Owls_partially_migratory <- filter(Owl_data, Migration.2 == "2")

#Output csv for partially migratory Owls
write.csv (Owls_partially_migratory, file = "Owls_partially_migratory.csv")

#Examine Body Mass and Range size - Interesting how the largest owls have a smaller range. Perhaps energy expenditure of flight plays a role?
plot(Range.Size ~ Body.mass..log., data=Owls_partially_migratory,
     xlab = "Body Mass (g) log scale", ylab = "Range Size (Breeding/Resident)",
     main = "Partially Migratory Owls comparison of body mass and range size")
     axis(side = 1, labels = F, lwd = 1.5)
     axis(side = 2, labels = F, lwd = 1.5)
     axis(side = 3, labels = F, lwd = 1.5, tck = -0.02)
     axis(side = 4, labels = F, lwd = 1.5, tck = -0.02)
     axis(side = 1, labels = F, lwd = 1.5, tck = 0.02)
     axis(side = 2, labels = F, lwd = 1.5, tck = 0.02)
     axis(side = 3, labels = F, lwd = 1.5, tck = 0.02)
     axis(side = 4, labels = F, lwd = 1.5, tck = 0.02)
     box(lwd = 1.5)

dev.copy2pdf(file = "./Partially_Migratory_Owls.pdf",
                  width = 6, height = 4, bg = "white", compress = F, out.type = "pdf")
