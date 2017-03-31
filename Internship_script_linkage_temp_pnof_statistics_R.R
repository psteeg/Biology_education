### STATISTICAL ANALYSIS OF SPATIOTEMPORAL VARIATION IN TEMPERATURE AND FISH PNOF. 


## setting work directory, source and library
.libPaths("D://Stage//Rstudio//Libraries")
setwd("D://Stage//Rstudio//Data")


## load necessary packages
#library(gstat)
#library(RgoogleMaps)
#library(GISTools)
#library(raster)
#library(rgdal)
#library(sp)
#library(shapefiles)
#library(mapplots)


#### PART 1 - SPATIOTEMPORAL VARIATION IN WATER TEMPERATURE.


## load data: 
Anova_tmax_meuse <- read.csv("[name of dataset].csv")
Anova_tmax_meuse$Row <- as.factor(Anova_tmax_meuse$Row)
summary(Anova_tmax_meuse)
## We are going to analyse temperature (Tmax) variation with fixed factors Time and Depth.
## first, we analyse the interaction Depth*Time.
# Two-way anova of 6 maps: 3x Meuse 1 (10cm, 50cm, 100cm) and 3x Meuse 2 (10cm, 50cm, 100cm).
# H0 depth: the average Tmax is the same for each depth.
# H1 depth: the average Tmax is different between depths.
# H0 time: the average Tmax is the same for both measuring times.
# H1 time: the average Tmax is different for both measuring times.
# H0 depth*time: the differences in average Tmax between the three depths during the first measurement are the same as the differences in average Tmax between the three depths during the second measurement.
# H1 depth*time: the differences in average Tmax between the three depths during the first measurement are not the same as the differences in average Tmax between the three depths during the second measurement.
summary(aov(Tmax~Depth*Time, data=Anova_tmax_meuse))
# Depth: F=233, p<0.001 --> reject H0, assume H1.
# Time: F=6635, p<0.001 --> reject H0, assume H1.
# Depth*Time: F=7.7, p<0.001 --> reject H0, assume H1.
# --> Tmax varies between depths, and Tmax varies between the two measuring times. Tmax [vertical variation measurement 1] differs from [vertical variation measurement 2].
##
## depth: specify differences.
## use post-hoc test to determine where most of the Depth variation occurs:
TukeyHSD(aov(Tmax~Depth, data=Anova_tmax_meuse))
# depth in general: only significant variation between the highest & lowest layer (p<0.001). Most of this variation occurs between the middle & lowest layer (p=0.051), and less between the middle & highest layer (p=0.24). Why? --> look at the lower and upper values of the 95% confidence level for each comparison, there is high overlap.
## let's analyse the variation between depths within each of both measuring times.
Anova_tmax_meuse1 <- subset(Anova_tmax_meuse, Time=="Meuse1")
Anova_tmax_meuse2 <- subset(Anova_tmax_meuse, Time=="Meuse2")
summary(Anova_tmax_meuse1)
summary(Anova_tmax_meuse2)
summary(aov(Tmax~Depth, data=Anova_tmax_meuse1))
TukeyHSD(aov(Tmax~Depth, data=Anova_tmax_meuse1))
summary(aov(Tmax~Depth, data=Anova_tmax_meuse2))
TukeyHSD(aov(Tmax~Depth, data=Anova_tmax_meuse2))
# each measurement: significant variation between each depth (p<0.001 for each). Why? --> look at the lower and upper values of the 95% confidence level for each comparison, there is little to no overlap between the comparisons.
## What does this mean for Depth? --> in general, differences between depths are small. Now within each measuring time, the depth differences are significant because the value ranges are very small and there is little/no overlap between depths. However, when the two measurements are generalized, the value ranges become much larger and overlap more because of the small differences, resulting in higher p-values. So in short: Depth has a significant effect on Tmax variation, but differences are small. (this is why p-values must always be combined with plots of the values!).
##
## Time: specify differences.
## let's see how the temporal variation occurs on each depth.
Anova_tmax_meuse10cm  <- subset(Anova_tmax_meuse, Depth=="10cm")
Anova_tmax_meuse50cm  <- subset(Anova_tmax_meuse, Depth=="50cm")
Anova_tmax_meuse100cm <- subset(Anova_tmax_meuse, Depth=="100cm")
summary(aov(Tmax~Time, data=Anova_tmax_meuse10cm))
summary(aov(Tmax~Time, data=Anova_tmax_meuse50cm))
summary(aov(Tmax~Time, data=Anova_tmax_meuse100cm))
## Time: all depths significantly changed over time (increased Tmax). 
## --> Meuse: temporal variation was much larger than vertical variation (relatively; absolute differences were small!).
## 
## Summary Tmax Meuse:
# Small differences between depths (vertical)
# Larger differences between measurements (temporal)
# Interaction between depth and time: the vertical differences change over time (spatiotemporal). 
# Note that when we plot the values...
boxplot(Tmax~Depth*Time, data=Anova_tmax_meuse)
interaction.plot(Anova_tmax_meuse$Depth,Anova_tmax_meuse$Time,Anova_tmax_meuse$Tmax)
# ...we can see that, although the Depth differences within each measurement are statistically significant, the actual differences are small. Always plot the values in combination with the statistical results! 


#### END OF PART 1 - SPATIOTEMPORAL VARIATION IN WATER TEMPERATURE.


#### PART 2 - SPATIOTEMPORAL VARIATION IN FISH PNOF.


## now we repeat the analysis for the fish PNOF values to check if they follow the same spatiotemporal variation.
## Meuse:
Anova_pnof_meuse <- read.csv("Anova_pnof_meuse.csv")
Anova_pnof_meuse$Row <- as.factor(Anova_pnof_meuse$Row)
summary(Anova_pnof_meuse)
summary(aov(Pnof~Depth*Time, data=Anova_pnof_meuse))
#
## Depth:
summary(aov(Pnof~Depth, data=Anova_pnof_meuse))
# Depth: F=7.6, p<0.001
## Depth has a significant effect on the pnof variation.
## let's check if this variation between depths is present within each of both measuring times.
Anova_pnof_meuse1 <- subset(Anova_pnof_meuse, Time=="Meuse1")
Anova_pnof_meuse2 <- subset(Anova_pnof_meuse, Time=="Meuse2")
summary(aov(Pnof~Depth, data=Anova_pnof_meuse1))
# Depth: F=90, p<0.001
summary(aov(Pnof~Depth, data=Anova_pnof_meuse2))
# Depth: F=150, p<0.001
## yep, pnof is different between depths within each of both measuring times.
## use Post-hoc tukey test to analyse differences between the depths:
TukeyHSD(aov(Pnof~Depth, data=Anova_pnof_meuse))
# 10cm - 50cm:  p=0.26
# 50cm - 100cm: p=0.048
# 10cm - 100cm: p<0.001
## in general, there is significant variation between the highest & lowest layer, and most of this variation occurs between the middle & lowest layer (not so much between the middle & highest layer).
##
## Time:
summary(aov(Pnof~Time, data=Anova_pnof_meuse))
# Time: F=2139, p<0.001
## Time also has a significant effect on the pnof variation.
##
## Now let's analyse the significance of the Depth*Time interaction.
summary(aov(Pnof~Depth*Time, data=Anova_pnof_meuse))
# Depth*Time: F=11.29, p<0.001
## Depth*Time interaction is significant --> the Depth variation changes over time.
## side-note: why are the F and P values of Depth and Time different in this anova compared to the previous anova's described above??
## let's see how this spatiotemporal variation occurs on each depth.
Anova_pnof_meuse10cm  <- subset(Anova_pnof_meuse, Depth=="10cm")
Anova_pnof_meuse50cm  <- subset(Anova_pnof_meuse, Depth=="50cm")
Anova_pnof_meuse100cm <- subset(Anova_pnof_meuse, Depth=="100cm")
summary(aov(Pnof~Time, data=Anova_pnof_meuse10cm))
summary(aov(Pnof~Time, data=Anova_pnof_meuse50cm))
summary(aov(Pnof~Time, data=Anova_pnof_meuse100cm))
## for each depth, pnof changes over time -> temporal variation on each depth.
## to summarize: in general, pnof varies between depths, pnof varies between the measuring times, and the vertical variation between depths changes over time -> spatiotemporal variation.
## Note that when we plot the values...
boxplot(Pnof~Depth*Time, data=Anova_pnof_meuse)
interaction.plot(Anova_pnof_meuse$Depth,Anova_pnof_meuse$Time,Anova_pnof_meuse$Pnof)
## ...we can see that, although the Depth differences within each measurement are statistically significant, the actual differences are small. Always plot the values in combination with the statistical results! 


#### END OF PART 2 - SPATIOTEMPORAL VARIATION IN FISH PNOF.


##