#scratch

orange <- read.csv("inst/extdata/MyEBirdData_Orange_20220913.csv", header = TRUE)
head(orange)
any(is.na(orange$Date))
#no NA values for date

sample <- head(orange)
sample
library(mbbs)
#import_ebird_data("inst/extdata/MyEBirdData_Orange_20220913_wDurham.csv")
#that throws the same error so okay, so it's not a problem with having deleted the extraneous Durham data..

chat <- read.csv("inst/extdata/MyEBirdData_Chatham_20220913_sample.csv", header = TRUE)
unique(chat$County)
chat <- head(chat)
chat
#okay so there's some Durham in there. 
#need to delete out the Durham ones. Can do this either from eBird and then redownload or.....add something into the import_ebird_data code to fix this? Wrong to just edit the spreadsheet bc it's going to then be redownloaded with the same problems next year. 

write.csv(chat, file = "data/Goulden/mbbs_chatham_sample.csv")
chat2 <- import_ebird_data("data/Goulden/mbbs_chatham_sample.csv")

lubridate::mdy(chat$Date)
sample
lubridate::mdy(sample$Date) #works fine on the Orange data here..


orange <- read.csv("inst/extdata/MyEBirdData_Orange_20220913.csv", header = TRUE)
table(orange$County)
orange_only <- orange[orange$County == "Orange",]
table(orange_only$County)
head(orange_only)

write.csv(orange_only,"inst/extdata/MyEBirdData_Orange_20220913.csv")
orange_only2 <- read.csv("inst/extdata/MyEBirdData_Orange_20220913.csv", header = TRUE)

chatham <- read.csv("inst/extdata/MyEBirdData_Chatham_20220913.csv", header = TRUE)
table(chatham$County)
chatham_only <- chatham[chatham$County == "Chatham",]
table(chatham_only$County)
write.csv(chatham_only, "inst/extdata/MyEBirdData_Chatham_20220913.csv")

chatham <- chatham[,2:24]
chatham
table(chatham$Scientific.Name)
#Accipiter sp.
#Accipitridae sp. (hawk sp.)
#Anas platyrhynchos (Domestic type)
#Anatinae sp.
#Ardea herodias [herodias Group]
#Cairina moschata (Domestic type)
#Cardinalis cardinalis [cardinalis Group]
#Hirundinidae sp. 
#Hirundo rustica erythrogaster? same as Hirundo rustica?
#same q: Pandion haliaetus carolinensis vs pandion haliaetus
#Passeriformes sp.
#Picidae sp.
#Pipilo erythrophthalmus erythrophthalmus/canaster
# Setophaga dominica dominica/stoddardi 
# Sialia sialis sialis/bermudensis
#Sitta carolinensis carolinensis vs    Sitta carolinensis
# Thryothorus ludovicianus [ludovicianus Group]
#Turdus migratorius [migratorius Group] 
orange <- orange[,2:24]
table(orange$Scientific.Name)
# Accipitridae sp. (hawk sp.) 
#  Anas platyrhynchos (Domestic type)
#Cardinalis cardinalis [cardinalis Group] 
# Corvus sp. (crow sp.)
#Hirundinidae sp.
#Passeriformes sp.
#Picidae sp.
#Piranga rubra/olivacea 
#  Quiscalus quiscula quiscula/stonei 
durham <- read.csv("inst/extdata/MyEBirdData_Durham_20220913.csv", header = TRUE)
table(durham$Scientific.Name)
#Accipitridae sp. (hawk sp.)
#Anatinae sp. 
# Corvus sp. (crow sp.) 
#Picidae sp.
#Quiscalus quiscula quiscula/stonei
