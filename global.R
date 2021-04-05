library(readr)

# read the data
stroke <- read_csv("stroke.csv")
head(stroke)
strokeData = stroke[,2:12]
head(strokeData)

# Clean the data

strokeData[strokeData == "N/A"] <- NA
strokeData <- na.omit(strokeData)

strokeData$gender <- as.factor(strokeData$gender)
strokeData$hypertension <- as.factor(strokeData$hypertension)
strokeData$heart_disease <- as.factor(strokeData$heart_disease)
strokeData$ever_married <- as.factor(strokeData$ever_married)
strokeData$work_type <- as.factor(strokeData$work_type)
strokeData$Residence_type <- as.factor(strokeData$Residence_type)
strokeData$smoking_status <- as.factor(strokeData$smoking_status)
strokeData$bmi <- as.numeric(strokeData$bmi)