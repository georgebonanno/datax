#read all lines (with delims)
weightsHeightLines <- read.csv('weightheight_raw.csv',quote="",sep = "\n",row.names=NULL)
#remove duplicates
weightsHeightLines <- unique(weightsHeightLines)

#append a ',' in the front (to allow 'Mr. John' to fall in the name field))
#remove the extra ',' after 'Zach'
weightsHeightLines <- sapply(weightsHeightLines$name.gender.height.weight,
       
       function(x){
         x <- toString(x)
         if (startsWith(x,"Zack,,")) {
            newLine = gsub("^(Zack),,(.*)",",\\1,\\2",x,perl=TRUE);
         } else if (!startsWith(x,"Kros, Mr. John,")) {
            newLine = paste(",",sep="",x);     
         } else {
           newLine <- x
         }
         return (newLine)
      })

dataFieldsCorrected <- paste(weightsHeightLines,collapse="\n")

#data can now be parsed with fields in place
weightHeightStats <- read.csv(text=dataFieldsCorrected,quote="",header=FALSE,stringsAsFactors = FALSE)

#removed unwanted surname and extra field at end
colnames(weightHeightStats) <- c("surname","name","gender","height_m",
                                  "weight_kilos","not_needed")

weightHeightStats <- weightHeightStats[,!(names(weightHeightStats) %in% c("surname","not_needed"))]

#place marie-curie's height in correct place
height <- weightHeightStats[weightHeightStats$name=='marie-curie',]$weight_kilos
weightHeightStats[weightHeightStats$name=='marie-curie',]$height_m <- height
weightHeightStats[weightHeightStats$name=='marie-curie',]$weight_kilos <- -1

#Zach's height and weight values have been swapped
height <- weightHeightStats[weightHeightStats$name=='Zack',]$height_m
weightHeightStats[weightHeightStats$name=='Zack',]$height_m <- 
    weightHeightStats[weightHeightStats$name=='Zack',]$weight_kilos 
weightHeightStats[weightHeightStats$name=='Zack',]$weight_kilos <- height

#inputing Miralem Pjanic height and weight
# as taken from http://healthyceleb.com/miralem-pjanic-height-weight-body-statistics/49380
weightHeightStats[weightHeightStats$name=='Pjanic',]$height_m <- 1.80
weightHeightStats[weightHeightStats$name=='Pjanic',]$weight_kilos <- 74

#inputing Brad Pitt's  height and weight
# as taken from http://healthyceleb.com/brad-pitt-height-weight-body-statistics/9697

weightHeightStats[weightHeightStats$name=='Brad',]$weight_kilos <- 78

#correcting Stephen's height to 1.82 since a '1' was most probably omitted
# when inputting the data. From the other samples, a height of 1.82 makes
# sense for a male weight 73kg

weightHeightStats[weightHeightStats$name=="Stephen",]$height_m <- 1.82

weightHeightStats

#Michelle is a female
weightHeightStats[weightHeightStats$name=="Michelle",]$gender <- "F"

#Paul Pogba is a male
weightHeightStats[weightHeightStats$name=="'Paul Pogba'",]$gender <- "M"

#correcting all gender 'Male/male' to 'M'
weightHeightStats[(weightHeightStats$gender %in% c('male','Male','m')),]$gender <- 'M'

#correcting all gender 'Female' to 'F'
weightHeightStats[(weightHeightStats$gender %in% c('Female')),]$gender <- 'F'



#remove the 'kg' postfix in the 'weight_kg" fields
kgWeights <- na.omit(
              weightHeightStats[endsWith(weightHeightStats$weight_kilos,"kg"),]$weight_kilos)

weightHeightStats[na.omit(endsWith(weightHeightStats$weight_kilos,"kg")),]$weight_kilos <- gsub("kg","",kgWeights)

#convert grams to kilos
convertToKilos <- function (mass) {
  massGrams <- gsub("^([0-9]+)g$","\\1",mass,perl=TRUE)
  if (massGrams == mass) {
    massGrams <- as.numeric(mass)
  } else {
    massGrams <- as.numeric(massGrams)/1000
  }
  return (massGrams)
}


weightHeightStats$weight_kilos <- sapply(weightHeightStats$weight_kilos,convertToKilos)

#JP is probably incorrect (one could ask him ....). Setting to NA for now
weightHeightStats[(weightHeightStats$name == 'JP'),]$weight_kilos <- NA

#remove 'm' postfix in JP's height value
weightHeightStats[weightHeightStats$name=='JP',]$height_m=1.59

#convert to metres
convertToMetres <- function(height) {
  if (length((height)) > 0) {
    if (length(grep("^\\d+$",height,perl=TRUE,value=TRUE)) > 0) {
      height <- as.double(height)/100
    } else {
      inchPattern <- "^(\\d+)[\"'](\\d+)[\"']$"
      matches <- regexec(inchPattern,height,perl=TRUE)
      print(paste("matches",matches[1]))
      if ((matches[[1]][1]) == -1) {
        height <- as.double(height)
      } else {
        feetAndInches <- regmatches(height,matches)
        feet <- as.double(feetAndInches[[1]][2])
        inches <- as.double(feetAndInches[[1]][3])
        height <- (feet*0.3048)+(inches*0.0254)
      }
    }
  } else {
    height <- NA
  }
  return (height)
}

weightHeightStats$height_m <- sapply(weightHeightStats$height_m,convertToMetres)

# As seen below female all lie in 1.68-1.77 height range. The weight increases slightly 
#but gradually with the length. A weight of 62 kg for marie-curie fits well...
fem <- weightHeightStats[weightHeightStats$gender=='F',]
plot(fem$height_m,fem$weight_kilos)

weightHeightStats[weightHeightStats$name == 'marie-curie',]$weight_kilos <- 63

# finding Joey's height and JP's weight by extracting
# a best fit from the sample (w/o Jabba) by finding a 
# linear model for the relation between males weights
# and height (excluding Jabba)
males <- weightHeightStats[weightHeightStats$gender=='M',]
males <- males[complete.cases(males),]
malesNoJabba <- males[males$weight_kilos < 300,]

lm(malesNoJabba$height_m~malesNoJabba$weight_kilos)

# Call:
#  lm(formula = malesNoJabba$height_m ~ malesNoJabba$weight_kilos)

# Coefficients:
#  (Intercept)  malesNoJabba$weight_kilos  
#   1.149470                   0.008595  

# estimating Joey's height and JP's weight from linear model....
weightHeightStats[weightHeightStats$name=='Joey',]$height_m <- (77*0.008595)+1.149470
weightHeightStats[weightHeightStats$name=='JP',]$weight_kilos <- (1.590000-1.149470)/0.008595

weightHeightStats