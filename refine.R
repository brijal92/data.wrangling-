# Load the data in RStudio
library(tidyr)
library(dplyr)
# Reading File from CSV
rawcsv <- read.csv("refine_original.csv",header = TRUE)
# Clean up brand names
refine_original$company <- tolower(refine_original$company)
str(refine_original)
#  Separate product code and number
refine1 <- mutate(refine_original, Company = substr(refine_original$company, 1 , stop = 1))
refine1$Company <- grep("^[p]", "philips", refine1$Company)
refine1$Company <- grep("^[a]", "akzo", refine1$Company)
refine1$Company <- grep("^[u]", "unilever", refine1$Company)
refine1$Company <- grep("^[v]", "van houten", refine1$Company)
refine1 <- select(refine1, -company)
refine1 <- separate(refine1, Product.code.number, c("product_code", "product_number"), sep = "-")
# Add product catogeries
refine1 <- mutate(refine1, product_category = product_code)
refine1$product_category <- grep("^[p|f]", "Smartphone", refine1$product_category)
refine1$product_category <- grep("^[v]", "TV", refine1$product_category)
refine1$product_category <- grep("^[x]", "Laptop", refine1$product_category)
refine1$product_category <- grep("^[q]", "Tablet", refine1$product_category)
#Add full address for geocoding
refine1 <- unite(refine1, full_address, address : country, sep = "," )
refine1 <- mutate(refine1, company_philips = ifelse(Company == "philips", 1, 0))
refine1 <- mutate(refine1, company_akzo = ifelse(Company == "akzo", 1, 0))
refine1 <- mutate(refine1, company_unilever = ifelse(Company == "unilever", 1, 0))
refine1 <- mutate(refine1, company_van_houten = ifelse(Company == "van houten", 1, 0))
refine1 <- mutate(refine1, product_smartphone = ifelse(product_category == "Smartphone", 1, 0))
refine1 <- mutate(refine1, product_laptop = ifelse(product_category == "Laptop", 1, 0))
refine1 <- mutate(refine1, product_tv = ifelse(product_category == "TV", 1, 0))
refine1 <- mutate(refine1, product_tablet = ifelse(product_category == "Tablet", 1, 0))
refine_clean <- refine1
#output csv file
write.csv(refine_clean, file = "refine_clean.csv")

