SolarAdopt <- read.csv("https://query.data.world/s/z7sgmrb66bsgxwjid6rtloauj3lq4b", header=TRUE, stringsAsFactors=FALSE);

library("rcompanion")
library(ggplot2)
library(tibble)

#below are the two vectors/variables for yes/no to solar PVs.
myList1 <- rep(c(1),times=1604)
print(myList1)

#The below code creates a new column named SPVs to the ADprompt and 
#CNprompt dataframes of indicating whether they have solar or not.
SolarAdopt$SPVs <- myList1

#Take out all states except California
SolarAdopt2 <- subset(SolarAdopt, SolarAdopt$STATE != 1 &
                              SolarAdopt$STATE != 2 & SolarAdopt$STATE != 3) 
View(SolarAdopt2)

#Subsetting the columns we want to create a new dataframe
KeepPromptsF <- c("CASE_ID", "AGE_BINNED", "PROMPT3", "PROMPT4","PROMPT12")
KeepPromptsPI <- c("CASE_ID", "AGE_BINNED", "PROMPT10", "PROMPT11","PROMPT14")

#Created dataframes from the vector categories
PromptsFinances <- SolarAdopt2[KeepPromptsF]
PromptsPersonalIn <- SolarAdopt2[KeepPromptsPI]

#Take out all NAs 
PromptsFinances1 <- na.omit(PromptsFinances)
PromptsPersonalIn1<- na.omit(PromptsPersonalIn)

View(PromptsFinances1)
View(PromptsPersonalIn1)

#Now run Independent Chi-Square in R 

library("gmodels")

#Question Setup: Will determine which prompt influenced people in CA 
#to get solar within the Finance and personal interaction categories. 

#Create a CrossTable for FINANCE PROMPTS BASED ON AGE
#AGE_BINNED: 1: 18-44 , 2: 45-54, 3: 55-64, 4: 65+ 
CrossTable(PromptsFinances1$PROMPT3, PromptsFinances1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05/ NO SIGNIFICANCE IN AGE RANGES CHOOSING SOLAR FOR REDUCING ENERGY BILL. BECAUSE ALL AGES ARE INFLUENCED PRETTY EVENLY
#BY THIS PROMPT.

CrossTable(PromptsFinances1$PROMPT4, PromptsFinances1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: < .05 / THERE IS A SIGNIFICANCE THAT AGE DOES HAVE PLANNING FOR RETIREMENT.

#Do a Post Hoc Analysis for those that are statistically significant:
#Fewer people than expected from the Age group 18-44 answered yes to being prompted to get solar due to planning for retirement.
#Fewer people than expected from the Age group 45-54 answered yes to being prompted to get solar due to planning for retirement.
#More people than expected from the Age group 55-64 answered yes to being prompted to get solar due to planning for retirement.

CrossTable(PromptsFinances1$PROMPT12, PromptsFinances1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05/ NO SIGNIFICANCE IN AGE RANGES CHOOSING SOLAR DUE TO LEARNING IT WAS MORE AFFORDABLE.


#Create CrossTables for PERSONAL INTERACTIONS PROMPTS BASED ON AGE
#AGE_BINNED: 1: 18-44 , 2: 45-54, 3: 55-64, 4: 65+ 
CrossTable(PromptsPersonalIn1$PROMPT10, PromptsPersonalIn1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05 / THERE IS A SIGNIFICANCE THAT AGE HAS ON WHETHER THEY WERE PROMPTED BY TALKING TO SOMEONE ABOUT.

CrossTable(PromptsPersonalIn1$PROMPT11, PromptsPersonalIn1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05 / THERE IS A SIGNIFICANCE THAT AGE HAS ON WHETHER THEY WERE PROMPTED BY IT BEING OFFERED AT A RETAIL STORE.

CrossTable(PromptsPersonalIn1$PROMPT14, PromptsPersonalIn1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05 / THERE IS A SIGNIFICANCE THAT AGE HAS ON WHETHER THEY WERE PROMPTED BY BEING APPROACHED BY A RETAILER.


#CHECK PATTERNS OF INFLUENCES BASED ON INCOME RANGES
#Data Wrangling 
KeepPromptsF_IB <- c("CASE_ID", "INCOME_BINNED", "PROMPT3", "PROMPT4","PROMPT12")
KeepPromptsPI_IB <- c("CASE_ID", "INCOME_BINNED", "PROMPT10", "PROMPT11","PROMPT14")

#Created dataframes from the vector categories
PromptsFinancesIB <- SolarAdopt2[KeepPromptsF_IB]
PromptsPersonalInIB <- SolarAdopt2[KeepPromptsPI_IB]

#Take out all NAs 
PromptsFinancesIB1 <- na.omit(PromptsFinancesIB)
PromptsPersonalInIB1<- na.omit(PromptsPersonalInIB)

View(PromptsFinancesIB1)
View(PromptsPersonalInIB1)

#Data now ready for CrossTable for FINANCE PROMPTS BASED ON INCOME RANGE
#INCOME_BINNED: 1: < 50,000, 2: 50,000-74,999, 3: 75,000-99,999, 
#               4: 100,000-149,999, 5: 150,000 or more, 95: Prefer not to answer

#All below analyses are running but getting the following error message: 
#Error in fisher.test(t, alternative = "two.sided") : 
#FEXACT error 7(location). LDSTP=18450 is too small for this problem,
#(pastp=147.259, ipn_0:=ipoin[itp=83]=734, stp[ipn_0]=148.256).
#Increase workspace or consider using 'simulate.p.value=TRUE'
#In addition: Warning message:
  #In chisq.test(t, correct = FALSE, ...) :
  #Chi-squared approximation may be incorrect

CrossTable(PromptsFinancesIB1$PROMPT3, PromptsFinancesIB1$INCOME_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05/ NO SIGNIFICANCE IN INCOME RANGES CHOOSING SOLAR FOR REDUCING ENERGY BILL. BECAUSE ALL AGES ARE INFLUENCED PRETTY EVENLY
#BY THIS PROMPT.

CrossTable(PromptsFinancesIB1$PROMPT4, PromptsFinancesIB1$INCOME_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE BETWEEN INCOME RANGES THAT PROMPTS TO BEGIN PLANNING FOR RETIREMENT.

CrossTable(PromptsFinancesIB1$PROMPT12, PromptsFinancesIB1$INCOME_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS", simulate.p.value=TRUE)
#RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE BETWEEN INCOME RANGES THAT PROMPTED THEM BASED ON HEARING IT WAS AFFORDABLE.

#Create CrossTables for PERSONAL INTERACTIONS PROMPTS BASED ON AGE
#INCOME_BINNED: 1: < 50,000, 2: 50,000-74,999, 3: 75,000-99,999, 
#               4: 100,000-149,999, 5: 150,000 or more, 95: Prefer not to answer

CrossTable(PromptsPersonalInIB1$PROMPT10, PromptsPersonalInIB1$INCOME_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE THAT INCOME RANGE HAS ON WHETHER THEY WERE PROMPTED BY TALKING TO SOMEONE ABOUT.

CrossTable(PromptsPersonalInIB1$PROMPT11, PromptsPersonalInIB1$INCOME_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE THAT INCOME RANGE HAS ON WHETHER THEY WERE PROMPTED BY IT BEING OFFERED AT A RETAIL STORE.

CrossTable(PromptsPersonalInIB1$PROMPT14, PromptsPersonalInIB1$INCOME_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: < .05 / THERE IS A SIGNIFICANCE THAT INCOME RANGE HAS ON WHETHER THEY WERE PROMPTED BY IT BEING OFFERED AT A RETAIL STORE.

#Do a Post Hoc Analysis for those that are statistically significant:
#More people than expected from the INCOME RANGE -Prefer not to say answered no to being prompted to get solar by it being offered at are retail store.
#More people than expected from the INCOME RANGE 150,000+ answered no to being prompted to get solar by it being offered at a retail store. 
#Fewer people than expected from the INCOME RANGE 75,000-99,9999 answered no to being prompted to get solar by seeing it offered at a retail store.

#CHECK PATTERNS IN THE INFLUENCES BASED ON GENDER
#Data Wrangling 
KeepPromptsF_GN <- c("CASE_ID", "GENDER", "PROMPT3", "PROMPT4","PROMPT12")
KeepPromptsPI_GN <- c("CASE_ID", "GENDER", "PROMPT10", "PROMPT11","PROMPT14")

#Created dataframes from the vector categories
PromptsFinancesGN <- SolarAdopt2[KeepPromptsF_GN]
PromptsPersonalInGN <- SolarAdopt2[KeepPromptsPI_GN]

#Take out all NAs 
PromptsFinancesGN1 <- na.omit(PromptsFinancesGN)
PromptsPersonalInGN1<- na.omit(PromptsPersonalInGN)

View(PromptsFinancesGN1)
View(PromptsPersonalInGN1)

#Data now ready for CrossTable for FINANCE PROMPTS BASED ON GENDER
#GENDER: 0: MALE, 1: FEMALE, 95: PREFER NOT TO ANSWER

CrossTable(PromptsFinancesGN1$PROMPT3, PromptsFinancesGN1$GENDER, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05/ NO SIGNIFICANT DIFFERNECE IN GENDER AND CHOOSING SOLAR FOR REDUCING ENERGY BILL. BECAUSE ALL AGES ARE INFLUENCED PRETTY EVENLY
#BY THIS PROMPT.

CrossTable(PromptsFinancesGN1$PROMPT4, PromptsFinancesGN1$GENDER, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE DIFF IN GENDER THAT PROMPTS TO BEGIN PLANNING FOR RETIREMENT.

CrossTable(PromptsFinancesGN1$PROMPT12, PromptsFinancesGN1$GENDER, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS", simulate.p.value=TRUE)
#RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE DIFF IN GENDER THAT PROMPTS BY BEING TOLD IT IS MORE AFFORDABLE.

#Create CrossTables for PERSONAL INTERACTIONS PROMPTS BASED ON GENDER
#GENDER: 0: MALE, 1: FEMALE, 95: PREFER NOT TO ANSWER

CrossTable(PromptsPersonalInGN1$PROMPT10, PromptsPersonalInGN1$GENDER, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE THAT GENDER HAS ON WHETHER THEY WERE PROMPTED BY TALKING TO SOMEONE ABOUT.

CrossTable(PromptsPersonalInGN1$PROMPT11, PromptsPersonalInGN1$GENDER, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE THAT GENDER HAS ON WHETHER THEY WERE PROMPTED BY IT BEING OFFERED AT A RETAIL STORE.

CrossTable(PromptsPersonalInGN1$PROMPT14, PromptsPersonalInGN1$GENDER, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: < .05 / THERE IS NO SIGNIFICANCE THAT GENDER HAS ON WHETHER THEY WERE PROMPTED BY IT BEING OFFERED AT A RETAIL STORE.


#CHECK PATTERNS IN THE INFLUENCES BASED ON NUMBER OF PEOPLE IN HOUSE
#PEOPLE_TOT_3PLUS: 0:2 OR LESS, 1: 3 OR MORE
#Data Wrangling 
KeepPromptsF_NP <- c("CASE_ID", "PEOPLE_TOT_3PLUS", "PROMPT3", "PROMPT4","PROMPT12")
KeepPromptsPI_NP <- c("CASE_ID", "PEOPLE_TOT_3PLUS", "PROMPT10", "PROMPT11","PROMPT14")

#Created dataframes from the vector categories
PromptsFinancesNP <- SolarAdopt2[KeepPromptsF_NP]
PromptsPersonalInNP <- SolarAdopt2[KeepPromptsPI_NP]

#Take out all NAs 
PromptsFinancesNP1 <- na.omit(PromptsFinancesNP)
PromptsPersonalInNP1<- na.omit(PromptsPersonalInNP)

View(PromptsFinancesNP1)
View(PromptsPersonalInNP1)

#Data now ready for CrossTable for FINANCE PROMPTS BASED ON NUMBER OF PEOPLE
#PEOPLE_TOT_3PLUS: 0:2 OR LESS, 1: 3 OR MORE

CrossTable(PromptsFinancesNP1$PROMPT3, PromptsFinancesNP1$PEOPLE_TOT_3PLUS, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05/ NO SIGNIFICANT DIFFERNECE IN NUMBER OF PEOPLE AND CHOOSING SOLAR FOR REDUCING ENERGY BILL. BECAUSE ALL AGES ARE INFLUENCED PRETTY EVENLY
#BY THIS PROMPT.

CrossTable(PromptsFinancesNP1$PROMPT4, PromptsFinancesNP1$PEOPLE_TOT_3PLUS, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: < .05 / THERE IS A SIGNIFICANCE DIFF IN NUMBER OF PEOPLE IN THE HOME THAT PROMPTS TO BEGIN PLANNING FOR RETIREMENT BY GETTING SOLAR.

#Post Hoc Analysis 
#More people than expected that have less than 3 people in their household said they were influenced by planning for retirement.
#Fewer people than expected that have more than 3 people in their household said they were influenced by planning for retirement. 

CrossTable(PromptsFinancesNP1$PROMPT12, PromptsFinancesNP1$PEOPLE_TOT_3PLUS, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS", simulate.p.value=TRUE)
#RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE DIFF IN NUMBER OF PEOPLE IN HOUSEHOLD THAT PROMPTS TO GET SOLAR BY HEARING IT IS MORE AFFORDABLE.

#Create CrossTables for PERSONAL INTERACTIONS PROMPTS BASED ON NUMBER OF PEOPLE LIVING IN HOUSEHOLD
#PEOPLE_TOT_3PLUS: 0:2 OR LESS, 1: 3 OR MORE

CrossTable(PromptsPersonalInNP1$PROMPT10, PromptsPersonalInNP1$PEOPLE_TOT_3PLUS, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE THAT NUMBER OF PEOPLE HAS ON WHETHER THEY WERE PROMPTED BY TALKING TO SOMEONE ABOUT.

CrossTable(PromptsPersonalInNP1$PROMPT11, PromptsPersonalInNP1$PEOPLE_TOT_3PLUS, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE THAT NUMBER OF PEOPLE HAS ON WHETHER THEY WERE PROMPTED BY IT BEING OFFERED AT A RETAIL STORE.

CrossTable(PromptsPersonalInNP1$PROMPT14, PromptsPersonalInNP1$PEOPLE_TOT_3PLUS, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
##RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE THAT NUMBER OF PEOPLE HAS ON WHETHER THEY WERE APPROACHED BY AN INSTALLER. 

#Do the same Independent Chi Square analysis as above for 2 types of motives.

#Subsetting the columns we want to create a new dataframe
KeepMotivesF <- c("CASE_ID", "AGE_BINNED", "MOTIVE1", "MOTIVE3","MOTIVE4")
KeepMotivesECO <- c("CASE_ID", "AGE_BINNED", "MOTIVE5", "MOTIVE7","MOTIVE8")

#Created dataframes from the vector categories
MotivesFinances <- SolarAdopt2[KeepMotivesF]
MotivesEco <- SolarAdopt2[KeepMotivesECO]

#Take out all NAs 
MotivesFinances1 <- na.omit(MotivesFinances)
MotivesEco1<- na.omit(MotivesEco)

View(MotivesFinances1)
View(MotivesEco1)

#Question Setup: Will determine which motive influenced people more in CA 
#to get solar within the Finance and eco categories. 

#Create CrossTable for FINANCIAL MOTIVES BASED ON AGE RANGE
#AGE_BINNED: 1: 18-44 , 2: 45-54, 3: 55-64, 4: 65+ 
CrossTable(MotivesFinances1$MOTIVE1, MotivesFinances1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE > .05

CrossTable(MotivesFinances1$MOTIVE3, MotivesFinances1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS: PVALUE < .05 HAVING A HARD TIME INTEREPRETING, but what I see is that most people of ages 55+ said importance of considering
#solar was moderately due to adding home's market value. 

CrossTable(MotivesFinances1$MOTIVE4, MotivesFinances1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE > .05

#Create CrossTable for ECO MOTIVES BASED ON AGE RANGE
#AGE_BINNED: 1: 18-44 , 2: 45-54, 3: 55-64, 4: 65+ 
CrossTable(MotivesEco1$MOTIVE5, MotivesEco1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE > .05

CrossTable(MotivesEco1$MOTIVE7, MotivesEco1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE < .05 SIGNIFICANT (WHAT DOES IT MEAN?)

CrossTable(MotivesEco1$MOTIVE8, MotivesEco1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE > .05



