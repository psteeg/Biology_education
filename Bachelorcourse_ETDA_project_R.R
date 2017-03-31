###############################################################
### TITLE:   Difference in germination rate between seeds, which are ingested by different fish species.


###############################################################
### ABSTRACT:   <a short (ca. 50 words) summary>
# In nature, fish often eat seeds. Some of these seeds can still germinate after being eaten. In this analysis, it is being studied what the differences between plant species and between fish treatments are when looking at germination ratio's. For this purpose the seeds of 20 different plant species have been fed to Tilapia, Karper or no fish. Afterwards the seeds were retrieved and it was observed whether they germinated in the next 100 days. Germination was significantly different between plant species. Overall germination was significantly lower after ingestion by Karper compared to control, but this was not the case for Tilapia. We speculate that overall seed germination has a large variation between plant species, and this variation is not influenced after ingestion by fish. Overall germination might be influenced by certain fish species such as Karper.


###############################################################
### SHORT INTRODUCTION: <between 100 and 200 words>
# In nature, many animals have seeds included in their diet. It is interesting to see that some of these seeds that have been ingested can still germinate, and possibly have a higher germination rate (Barnea et al. 1991). Ingestion by fish might influence the germination rate for several plant species because the seeds are exposed to the animal digestive system. This "fish treatment" can possibly affect the time (date) at which the seeds germinate, and therefore germination may occur too early or too late. If this happens, the environmental conditions might not be optimal for seeds to grow. Ultimately, ingestion by fish might result in a higher or lower percentage of seeds that germinate, compared to seeds that are not ingested by fish. In this analysis the percentage of germination will be analyzed, where variation between plant species will be analyzed between 2 fish species and a control treatment.


###############################################################
### RESEARCH QUESTION:  
# What is the difference in percentage of germination between Karper, Tilapia and Control (no fish),  between 20 plant species, and between 20 plant species in control, compared to Karper and Tilapia?


###############################################################
### HYPOTHESES AND EXPECTATIONS:
# 1) For each fish, compared to control: germination percentage is increased (Barnea et al. 1991).
# 2) Germination percentage will somewhat variate between plant species (we have no knowledge about the plant species).
# 3) Despite possible variation between plant species (in control group), plant species might react differently to the fish treatment  -->  In other words: germination percentage will vary between the plant seeds that are eaten by fish, compared to the seeds from the control group.


###############################################################
### METHODS: STUDY SYSTEM: <between 100 and 200 words>
# For this experiment, seeds from 20 plant species were used. Each plant species was subjected to 3 treatments: ingestion by karper, ingestion by tilapia, no ingestion (control). For each treatment, 10 fish tanks were used. And for each fish, a factor "Time" was added, which represents the moment the fish were fed with the seeds. And the factor block was also incorporated. A block is a timeblock, and each timeblock the same tanks and fish were used. Each fish was fed with a certain number of seeds. For each fish, the amount of seeds were counted for: feeding, ingestion and retrieval (after the treatment). Afterwards, all seeds were planted and grown to analyze germination speed. 


###############################################################
### METHODS: DATA ACQUISITION: <between 100 and 200 words>
# On the first day of analysis, the amount of seeds that germinated right away were counted. After that, for each seed the germination date was observed, up to 100 days after planting. After 100 days, the amount of seeds that did and did not germinate were counted, and the percentage of germination was calculated.


###############################################################
### METHODS: ANALYSES: <include R code in case of data manipulation>

# Data exploration
setwd ('[personal folder]')
germinationData <- read.csv("[first dataset]", na.strings = "")
View(germinationData)
summary(germinationData)
# Change 1 column to numeric (this is only needed in germinationData because of the 2 first rows)
germinationData$Kieming <- as.numeric(germinationData$Kieming)
germinationData[,15:ncol(germinationData)][is.na(germinationData[,15:ncol(germinationData)])]<-0


# Different dataset from excel with the first 2 rows excluded, so now all the rows are numeric. This new dataset can be explored and analyzed, and after analysis the first dataset can be used to identify the date for each column.
dataChanged <- read.csv("[second dataset]", na.strings = "")
# Transform all NA's into 0 (because it was a count of #seeds, 0 can be used because it means 0 seeds were found)
dataChanged[,14:ncol(dataChanged)][is.na(dataChanged[,14:ncol(dataChanged)])]<-0
View(dataChanged)
summary(dataChanged)
# All NA's are removed and replaced by 0, but some columns that are now an integer (they have values) need to be transformed to factor, when the column contains nominal data (i.e. fish species: species 3 =/= 3 * species 1)


class(dataChanged$Tank)
# Change tank, fish species, plant species and block from integer to factor 
dataChanged$Tank <- as.factor(dataChanged$Tank)
dataChanged$Fish.species <- as.factor(dataChanged$Fish.species)
dataChanged$Plant.block <- as.factor(dataChanged$Plant.block)
dataChanged$Plant.species <- as.factor(dataChanged$Plant.species)


# First step: analyze how many seeds have germinated (and how many have not)
# Rename last column "X" to "Total.germ" --> this column shows the total #seeds (#seeds = number of seeds) germinated for each plant in each fish
names(dataChanged)[names(dataChanged)=="X"] <- "Total.germ"
# Now we have to calculate the total amount of seeds that did and did not germinate


# Note: Column "Bij start gekiemd" shows the #seeds that germinated right at the start. The remaining #seeds that did not germinate at the start were analyzed during the rest of the experiment. These seeds are shown in the column "Ingezet". When these 2 columns are added, we have a new list with the total #seeds that were planted at the start and analyzed for germination.


# Create 3 lists: Start = number of seeds planted, Totalgerm = number of seeds germinated from grown seeds, Totalnongerm = number of seeds not germinated from grown seeds (Start-Totalgerm)
# First: change all 0 in "Start" to NA (0 seeds at start which means you divide bij 0 for percentage --> see below)
dataChanged$Start <- dataChanged$Ingezet + dataChanged$Bij.start.gekiemd
dim(subset(dataChanged,Start==0))
# 36 0's in Start that need to be removed
dataChanged$Start[dataChanged$Start==0] <- NA 
dataChanged <- dataChanged[!is.na(dataChanged$Start),]
summary(dataChanged)
# All 0's removed from Start, so all records with 0 starting seeds and 0 seeds that germinated on day 1 are removed
Start <- dataChanged$Start
Totalgerm <- dataChanged$Total.germ
Totalnongerm <- Start-Totalgerm
dataChanged$Total.nongerm <- Totalnongerm
# Create 2 new columns: Totalgermperc (percentage of germination) and Totalnongermperc (percentage of non germination)
Totalgermperc <- (Totalgerm/Start)*100
Totalnongermperc <- (Totalnongerm/Start)*100
dataChanged$Total.germperc <- Totalgermperc
dataChanged$Total.nongermperc <- Totalnongermperc
summary(dataChanged)
dim(subset(dataChanged,Total.nongerm<0))
# 0 negative values for Totalnongerm, so that is nice
# Last 4 columns: respectively total number of seeds germinated, total number of seeds not germinated, percentage of germination, percentage of non germination


###############################################################
### RESULTS: <describe your results between R code of all your analyses>


# New variable with 2 lists/columns: Total.germ and Total.nongerm
Y<-cbind(dataChanged$Total.germ,dataChanged$Total.nongerm)


# Create GLM with data type binomial, with Y as function of plant species (first explanatory factor)
GLM1 <- glm(Y~dataChanged$Plant.species, family=binomial)
plot(GLM1)
# Residuals vs Fitted = straight line --> nice. However, the Q-Q plot is not very well distributed.
# Quantiles do not matter because we use glm, so we do not assume a normal distribution of the residuals.
# How do we visualize this model? --> use barplot, using function predict for values and calculating means for each level of plant species.
yPredict <- predict(GLM1,type="response")  # type = response --> logit transformation is reversed
means1 <- tapply (yPredict, dataChanged$Plant.species, mean)
barplot (means1, xlab='Plant species', ylab='Percentage of seeds germinated',col=rainbow(20))
summary(GLM1)
# This function shows the rate of germination for each plant species (0 = 0%, 1 = 100% germination)


# Total germ as function of plant species
GLM2 <- glm (Total.germperc ~ Plant.species,data=dataChanged)
y2Predict <- predict (GLM2)
means2 <- tapply (y2Predict, dataChanged$Plant.species[!is.na(dataChanged$Total.germperc)], mean)
barplot (means2, ylab= 'Percentage of seeds germinated', xlab = 'Plant species',col=rainbow(20))
summary(GLM2)
# Average germination differs significantly for each plant species compared to species 1, except for plant species 15 and 20.


# Percentage germ as function of fish species
GLM3 <- glm (dataChanged$Total.germperc ~ dataChanged$Fish.species)
y3Predict <- predict (GLM3)
means3 <- tapply (y3Predict, dataChanged$Fish.species[!is.na(dataChanged$Total.germperc)], mean)
barplot (means3, ylab= 'Percentage of seeds germinated', xlab = 'Fish species',col=rainbow(3))
mean(dataChanged$Total.germperc[dataChanged$Fish.species=="2"])
summary(GLM3)
# Average germination is significantly lower in Karper compared to control. Tilapia is not significantly different from control.


# Now we look at the differences between the fish treatments with binomial data for the germination ratio.
GLM4 <- glm (Y~ dataChanged$Fish.species, family=binomial)
y4Predict <- predict (GLM4, type="response")
means4 <- tapply (y4Predict, dataChanged$Fish.species, mean)
barplot (means4, ylab= 'Number of seeds germinated', xlab = 'Fish species',col=rainbow(3))
summary(GLM4)
# This model too shows that average germination is significantly lower in Karper compared to control, and germination is not significantly changed in Tilapia.


# Percentage germ percentage as a function of fish * plant species
GLM5 <- glm(dataChanged$Total.germperc~ dataChanged$Fish.species * dataChanged$Plant.species,)
y5Predict <- predict (GLM5)
means5 <- tapply(y5Predict, dataChanged$Plant.species : dataChanged$Fish.species, mean)
barplot (means5, ylab="Percentage of seeds germinated", xlab = "Fish species for each plant species",col=rainbow(3))
# This plot does not give a nice view of any differences between plant or fish species, so we shall skip this plot
summary(GLM5)
# For many plant species, germination does not show a significant difference between plant species in both fish species, compared to the control. 


###############################################################
### DISCUSSION and CONCLUSION: <between 200 and 300 words>
# In this experiment, we have looked at differences in percentage of germination between Karper, Tilapia and control (no fish), between 20  plant species, and between 20 plant species in control, compared to Karper and Tilapia. Average germination was lower in Karper compared to the control group, however this was not the case for Tilapia compared to control (see glm3 and glm4). This result does not match with our hypothesis, where we stated that the overall germination rate is increased in both fishes compared to contol.Analysis of plant species shows that there was a significant variation between plant species 2-20 compared to species 1 (see glm1 and glm2). This matches with our hypothesis where we stated that the germination rate will somewhat differ between the plant species. When both plant and fish species are combined, almost no significant differences were found between variation between plant species with fish treatment compared to variation between plant species that had no fish treatment (see glm5). So even though there seems to be a large variation between plant species, this variation is not significantly altered when the seeds are ingested by fish. This does not match our hypothesis, where we stated that variation in germination between plant species that are eaten by fish will be different compared to the variation in germination of different species that are not ingested by fish. Ingestion by Karper seems to negatively affect the overall germination rate compared to no ingestion, however this is not the case for Tilapia. We speculate that overall seed germination (plant species combined) is possibly negatively influenced by certain fish species such as Karper, and some fish species such as Tilapia do not have a large effect on overall seed germination. For analysis of the germination percentage, a survival analysis was optional for extra results. Due to lack of time we had to skip this step. In further research, this survival analysis can improve the experiment and might elaborate on the results.


###############################################################
### LITERATURE: <just a few>
# Barnea et al. 1991. Does ingestion by birds affect seed germination? Functional Ecology, Vol. 5, No. 3 (1991), pp. 394-402