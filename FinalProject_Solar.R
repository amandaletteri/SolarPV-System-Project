SolarAdopt <- read.csv("https://query.data.world/s/z7sgmrb66bsgxwjid6rtloauj3lq4b", header=TRUE, stringsAsFactors=FALSE);
View(SolarAdopt)

SolarConsider <- read.csv("https://query.data.world/s/h2uzczihps6wlgywy5ognxxw4bseaz", header=TRUE, stringsAsFactors=FALSE);
View(SolarConsider)

SolarSpecs <- read.csv("https://query.data.world/s/32lvde7ohhsda7rhljwd3uq5i4kre2", header=TRUE, stringsAsFactors=FALSE);
View(SolarSpecs)

head(SolarAdopt$REFERRAL_NUM)

#Creating vectors with specific themes and categories to be able to visualize
#them without other variables getting in the way. 
#The SolarAdopt dataframe is what I am subsetting in categories below. 

#This category provides demographics on the adopter and their answers to what prompted
#them to get solar
KeepPromptsAD <- c("CASE_ID", "STATE", "PEOPLE_TOT_3PLUS", "GENDER", "AGE_BINNED", "INCOME_BINNED","RETIRED", "PROMPT1", "PROMPT2", "PROMPT3", "PROMPT4", "PROMPT5", "PROMPT6",
  "PROMPT7", "PROMPT8", "PROMPT9", "PROMPT10", "PROMPT11", "PROMPT12", "PROMPT13", "PROMPT14", "PROMPT15")

##This category provides demographics on the adopter and their answers to what motivated
#them to get solar
KeepMotivesAD <- c("CASE_ID", "STATE", "PEOPLE_TOT_3PLUS", "GENDER","MOTIVE1", "MOTIVE2", "MOTIVE3", "MOTIVE4", "MOTIVE5", "MOTIVE6", "MOTIVE7",
  "MOTIVE8", "MOTIVE9")

##This category provides demographics on the adopter and their answers to what influenced
#them to get solar
KeepInfluencesAD <- c("CASE_ID", "STATE", "PEOPLE_TOT_3PLUS", "GENDER", "AGE_BINNED", "INCOME_BINNED","RETIRED",
  "INFLNC_FIRST","INFLNC_SAW", "INFLNC_PPLTALK", "INFLNC_ADS", "INFLNC_OTHKNEW", 
  "INFLNC_OTHKNEW_NUM", "INFLNC_OTH_MULT1", "INFLNC_OTH_MULT2", "INFLNC_OTH_MULT3", "INFLNC_OTH_MULT4", 
  "INFLNC_OTH_MULT5", "INFLNC_OTH_MULT6", "INFLNC_OTH_MULT7", "INFLNC_OTH_ONE1", "INFLNC_OTH_ONE2", 
  "INFLNC_OTH_ONE3", "INFLNC_OTH_ONE4", "INFLNC_OTH_ONE5", "INFLNC_OTH_ONE6")

##This category provides demographics on the adopter and their answers to how their
#experience of adopting solar went and how much the referred people to get solar. 
KeepExpAndRefsAD <- c("CASE_ID", "STATE", "PEOPLE_TOT_3PLUS", "GENDER", "AGE_BINNED", "INCOME_BINNED","RETIRED",
  "NPS_PANELS", "NPS_INSTALLER", "EXPERIENCE1", "EXPERIENCE2", 
  "EXPERIENCE3", "EXPERIENCE4", "EXPERIENCE5", "EXPERIENCE6", "REGRETS", "TOLD_OTHERS", "TOLD_OTHERS_NUM", 
  "TOLD_OTHERS_LOCAL", "REFERRAL_NUM", "REFERRAL_WHEN1", "REFERRAL_WHEN2", "REFERRAL_WHEN3", "REFERRAL_WHEN4")

##This category provides demographics on the adopter and their answers questions about
#their personal values and beliefs
KeepPersValuesAD <- c("CASE_ID", "STATE", "PEOPLE_TOT_3PLUS", "GENDER", "AGE_BINNED", "INCOME_BINNED","RETIRED",
  "PN1", "PN2", "E2", "BB1", "BB2", "BE13", "BE10", "CIJM1", "CIJM2", "CIJM3"
  )

#Created dataframes from the vector categories
ADprompt <- SolarAdopt[KeepPromptsAD]
ADmotives <- SolarAdopt[KeepMotivesAD]
ADinfluences <- SolarAdopt[KeepInfluencesAD]
ADexpANDrefs <- SolarAdopt[KeepExpAndRefsAD]
ADpersValues  <- SolarAdopt[KeepPersValuesAD]

View(ADprompt)
View(ADmotives)
View(ADinfluences)
View(ADexpANDrefs)
View(ADpersValues)

#Will add columns that have the categorical information for  better visualization.
ADprompt$StateR <- NA
ADprompt$StateR[ADprompt$STATE== 1]<- "New Jersey"
ADprompt$StateR[ADprompt$STATE== 2]<- "New York (state)"
ADprompt$StateR[ADprompt$STATE== 3]<- "Arizona"
ADprompt$StateR[ADprompt$STATE== 4]<- "California"

ADmotives$StateR <- NA
ADmotives$StateR[ADmotives$STATE== 1]<- "New Jersey"
ADmotives$StateR[ADmotives$STATE== 2]<- "New York (state)"
ADmotives$StateR[ADmotives$STATE== 3]<- "Arizona"
ADmotives$StateR[ADmotives$STATE== 4]<- "California"

ADinfluences$StateR <- NA
ADinfluences$StateR[ADinfluences$STATE== 1]<- "New Jersey"
ADinfluences$StateR[ADinfluences$STATE== 2]<- "New York (state)"
ADinfluences$StateR[ADinfluences$STATE== 3]<- "Arizona"
ADinfluences$StateR[ADinfluences$STATE== 4]<- "California"

ADexpANDrefs$StateR <- NA
ADexpANDrefs$StateR[ADexpANDrefs$STATE== 1]<- "New Jersey"
ADexpANDrefs$StateR[ADexpANDrefs$STATE== 2]<- "New York (state)"
ADexpANDrefs$StateR[ADexpANDrefs$STATE== 3]<- "Arizona"
ADexpANDrefs$StateR[ADexpANDrefs$STATE== 4]<- "California"

ADpersValues$StateR <- NA
ADpersValues$StateR[ADpersValues$STATE== 1]<- "New Jersey"
ADpersValues$StateR[ADpersValues$STATE== 2]<- "New York (state)"
ADpersValues$StateR[ADpersValues$STATE== 3]<- "Arizona"
ADpersValues$StateR[ADpersValues$STATE== 4]<- "California"

#Creating vectors with specific themes and categories to be able to visualize
#them without other variables getting in the way. 
#The SolarConsider dataframe is what I am subsetting in categories below. 

#This category provides demographics on the considerer and their answers to why they 
#are considering getting solar. 

KeepPromptsCN <- c("CASE_ID", "STATE", "PEOPLE_TOT_3PLUS", "GENDER", "AGE_BINNED", "INCOME_BINNED","RETIRED", "PROMPT1", "PROMPT2", "PROMPT3", "PROMPT4", "PROMPT5", "PROMPT6",
            "PROMPT7", "PROMPT8", "PROMPT9", "PROMPT10", "PROMPT11", "PROMPT12", "PROMPT13", "PROMPT14", "PROMPT15")

KeepMotivesCN <- c("CASE_ID", "STATE", "PEOPLE_TOT_3PLUS", "GENDER","MOTIVE1", "MOTIVE2", "MOTIVE3", "MOTIVE4", "MOTIVE5", "MOTIVE6", "MOTIVE7",
                   "MOTIVE8", "MOTIVE9")

KeepInfluencesCN <- c("CASE_ID", "STATE", "PEOPLE_TOT_3PLUS", "GENDER", "AGE_BINNED", "INCOME_BINNED","RETIRED",
                      "INFLNC_SAW", "INFLNC_PPLTALK", "INFLNC_ADS", "INFLNC_OTHKNEW", 
                      "INFLNC_OTHKNEW_NUM", "INFLNC_OTH_MULT1", "INFLNC_OTH_MULT2", "INFLNC_OTH_MULT3", "INFLNC_OTH_MULT4", 
                      "INFLNC_OTH_MULT5", "INFLNC_OTH_MULT6", "INFLNC_OTH_MULT7", "INFLNC_OTH_ONE1", "INFLNC_OTH_ONE2", 
                      "INFLNC_OTH_ONE3", "INFLNC_OTH_ONE4", "INFLNC_OTH_ONE5", "INFLNC_OTH_ONE6")

KeepConStopCN <- c("CASE_ID", "STATE", "PEOPLE_TOT_3PLUS", "GENDER", "AGE_BINNED", "INCOME_BINNED","RETIRED",
                   "CONCERN1_STOP", "CONCERN2_STOP", "CONCERN3_STOP", "CONCERN4_STOP", 
                   "CONCERN5_STOP", "CONCERN6_STOP", "CONCERN7_STOP", "CONCERN8_STOP",
                   "CONCERN9_STOP") 

KeepPersValuesCN <- c("CASE_ID", "STATE", "PEOPLE_TOT_3PLUS", "GENDER", "AGE_BINNED", "INCOME_BINNED","RETIRED",
                      "PN1", "PN2", "E2", "BB1", "BB2", "BE13", "BE10", "CIJM1", "CIJM2", "CIJM3")

CNprompt <- SolarConsider[KeepPromptsCN]
CNmotives <- SolarConsider[KeepMotivesCN]
CNinfluences <- SolarConsider[KeepInfluencesCN]
CNconcernStop <- SolarConsider[KeepConStopCN]
CNpersValues  <- SolarConsider[KeepPersValuesCN]

library("rcompanion")
library(ggplot2)

#Before adding the state string column, will continue on to the 3rd dataset and subset. 
#Creating vectors with specific themes and categories to be able to visualize
#them without other variables getting in the way. 
#The SolarSpecs dataframe is what I am subsetting in categories below. 

#This category provides demographics and values on control group and their answers to why they 
#are considering getting solar. 

#KeepTrigsGPS <- c("CASE_ID", "STATE", "PEOPLE_TOT_3PLUS", "GENDER", "AGE_BINNED", "INCOME_BINNED","RETIRED",
#                  "F1", "F2", "TRIG1", "TRIG3", "TRIG2", "TRIG4", "N1","N2")
#BELOW DIDN'T WORK: 
#x <- cbind(ADprompt$STATE,ADprompt$GENDER,ADprompt$PEOPLE_TOT_3PLUS,ADprompt$AGE_BINNED, ADprompt$INCOME_BINNED)
#y <- cbind(ADprompt$PROMPT1,ADprompt$PROMPT2,ADprompt$PROMPT3,ADprompt$PROMPT4, ADprompt$PROMPT5, ADprompt$PROMPT6)
#matplot(x,y,type="p")

#Export one dataframe to experiment with in Python
write.csv(ADprompt,"/Users/karina/Desktop/ADprompt.csv")


#Found that may need to merge Adopter and Considerer 
#dataframes to really test.

#First add column that indicates has solar 0:no / 1: yes

#x<-rep(c(1),times=1406)
#ADprompt1 <-ADprompt(x)
#ADprompt1

#Create vector/list to add new column indicating whether someone has 
# solar or not so we can join both adopter and considerer dataframes 
#for different categories

#below are the two vectors/variables for yes/no to solar PVs.
myList1 <- rep(c(1),times=1604)
print(myList1)

myList0 <- rep(c(0),times=588)
print(myList0)

library(tibble)

#The below code creates a new column named SPVs to the ADprompt and 
#CNprompt dataframes of indicating whether they have solar or not.
ADprompt$SPVs <- myList1
CNprompt$SPVs <- myList0

View(ADprompt)
View(CNprompt)

#Will actually append all rows into one dataframe.

#had to subset because had an additional column that CNprompt didnt have
ADprompt2 <- subset(ADprompt, select = -c(StateR))
View(ADprompt2)

#actual code to bind 2 dataframes with same column structure
PromptsDF <- rbind(ADprompt2, CNprompt)

View(PromptsDF)

#Now can look into doing stepwise regression 
#Backward Elimination - Summary to start
FitALl = lm(SPVs ~ . , data = PromptsDF)
summary(FitALl)

#Backward Elimination
step(FitALl, direction = 'backward')
