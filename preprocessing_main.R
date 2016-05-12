source("utils.R")
source("base.R")
source("melt.R")
loadAndInstallPackage("plyr")
loadAndInstallPackage("ggplot2")
loadAndInstallPackage("timeDate")
loadAndInstallPackage("reshape")
source("product_category_outlier_features.R")
source("day_rewardVergleich.R")
source("new_features.R")
source("customerprofiles.R")

Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "C")

# Begin of preprocessing program
train <- dmc15ReadTrainingSet(filename="../DMC_2015_orders_train.txt")
class <- dmc15ReadClassSet(filename="../DMC_2015_orders_class.txt")
melted <- dmc15MeltData(train)

# Preprocessing
preprocessedTrain <- getPreprocessedTrain(train) # Lukas
preprocessedTrain <- getFirstOrderFeatureTrain(preprocessedTrain) #Andrea
preprocessedTrain <- getPriceDiffTrain(preprocessedTrain) #Linchen (attention! duration > 10s)
preprocessedTrain <- getDayOfMonth(preprocessedTrain) #Mushi
preprocessedTrain <- getTimeBetweenReceivedAndOrdered(preprocessedTrain) #Lukas (Gruppe B)
preprocessedTrain <- getDayOfWeek(preprocessedTrain) #Lukas (Gruppe B)

preprocessedMelted <- getPreprocessedMelted(melted) # Lukas
preprocessedMelted <- getPriceDiffAndFrequency(preprocessedMelted) # Linchen (attention! duration > 10s)
preprocessedMelted <- getDayAndReward(preprocessedMelted) # Mushi

preprocessedTrainWithAllSingleCatIDs <- getAllCatIDsInSingleColumn(train,1)
preprocessedTrainWithAllSingleCatIDs <- getAllCatIDsInSingleColumn(preprocessedTrainWithAllSingleCatIDs,2)
preprocessedTrainWithAllSingleCatIDs <- getAllCatIDsInSingleColumn(preprocessedTrainWithAllSingleCatIDs,3)

# Write preprocessed data to csv
write.table(preprocessedMelted, file="../Preprocessed_Melted.csv", na="", sep="|", row.names = FALSE)
write.table(preprocessedTrain, file="../Preprocessed_Train.csv", na="", sep="|", row.names = FALSE)
write.table(preprocessedTrainWithAllSingleCatIDs, file="../preprocessedTrainWithAllSingleCatIDs.csv", na="", sep="|", row.names = FALSE)