library(dplyr)
library(tidyr)

# Reading File from CSV
rawcsv <- read.csv("titanic_original.csv",header = TRUE)
# changing  data frame to tbl
rawcsvtbl <- tbl_df(rawcsv)
# embarkation
# Find the missing values and replace them with S.
vemb <- as.character(rawcsvtbl$embarked)
ember_place <- function(item){
  if(item=="") (item<-"S")
  else{item=item} 
  return(item)
}
vemb <- vapply(vemb,ember_place,character(1))
rawcsvtbl$embarked <- vemb
# age - find mean of values,poplulation with empty values
eage <- as.numeric(rawcsvtbl$age)
mean_age <- mean(eage,na.rm = TRUE)
age_replace <- function(item,age){
  if(is.na(item)){item=age}
  else{item = item}
  return(item)
}
eage <- sapply(eage, age_replace,mean_age)
rawcsvtbl$age <- eage
# life boat - empty value with na
vlife <- as.character(rawcsvtbl$boat)
life_replace <-function(item){
  if (item==""){item = "NA"}
  else{item=item}
  return(item)
}
vlife <- sapply(vlife,life_replace)
rawcsvtbl$boat <- vlife
# if cabin value  has_cabin_number which has 1
vcabin <- as.character(rawcsvtbl$cabin)
cabin_replace <-function(item) {
  if (item==""){item = 1}
  else{item=0}
  return(item)
}
vcabin <- vapply(vcabin,cabin_replace,numeric(1))
rawcsvtbl <- cbind(rawcsvtbl,vcabin)
# removing last raw,
rawcsvtbl <- rawcsvtbl[-1310,]
# out put
write.csv(rawcsvtbl,file = "titanic_clean.csv",row.names=FALSE)

