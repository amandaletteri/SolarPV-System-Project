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

#Subsetting the columns we want to create a new dataframe / DID NOT WORK WITH SPVs
#KeepPromptsFSPVs <- c("CASE_ID", "SPVs", "PROMPT3", "PROMPT4","PROMPT12")
#KeepPromptsPISPVs <- c("CASE_ID", "SPVs", "PROMPT10", "PROMPT11","PROMPT14")

#Created dataframes from the vector categories
#PromptsFinancesSPVs <- SolarAdopt2[KeepPromptsFSPVs]
#PromptsPersonalInSPVs <- SolarAdopt2[KeepPromptsPISPVs]

#Take out all NAs 
#PromptsFinancesSPVs1 <- na.omit(PromptsFinancesSPVs)
#PromptsPersonalInSPVs1<- na.omit(PromptsPersonalInSPVs)

#Create a CrossTable for FINANCE PROMPTS BASED ON HAVING SPVs
#CrossTable(PromptsFinancesSPVs1$SPVs, PromptsFinancesSPVs1$PROMPT3, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#CrossTable(PromptsFinancesSPVs1$SPVs, PromptsFinancesSPVs1$PROMPT4, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#CrossTable(PromptsFinancesSPVs1$SPVs, PromptsFinancesSPVs1$PROMPT12, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#DOES NOT YIELD ANYTHING
#ERROR MESSAGE: Error in CST$residual[i, ] : incorrect number of dimensions


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
#RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE THAT AGE HAS ON WHETHER THEY WERE PROMPTED BY TALKING TO SOMEONE ABOUT.

CrossTable(PromptsPersonalIn1$PROMPT11, PromptsPersonalIn1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE THAT AGE HAS ON WHETHER THEY WERE PROMPTED BY IT BEING OFFERED AT A RETAIL STORE.

CrossTable(PromptsPersonalIn1$PROMPT14, PromptsPersonalIn1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE THAT AGE HAS ON WHETHER THEY WERE PROMPTED BY BEING APPROACHED BY A RETAILER.


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
#RESULTS P-VALUE: < .05 / THERE IS A SIGNIFICANCE THAT INCOME RANGE HAS ON WHETHER THEY WERE PROMPTED BY IBEING APPROACHED BY AN INSTALLER. 

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
#RESULTS P-VALUE: > .05 / THERE IS NO SIGNIFICANCE THAT GENDER HAS ON WHETHER THEY WERE PROMPTED BY IT BEING OFFERED AT A RETAIL STORE.


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

#Subsetting the columns we want to create a new dataframe for Financial Motives and Eco Related Motives

#Create datasets based on having SPVs
#KeepMotivesF_SPVs <- c("CASE_ID", "SPVs", "MOTIVE1", "MOTIVE3","MOTIVE4")
#KeepMotivesECO_SPVs <- c("CASE_ID", "SPVs", "MOTIVE5", "MOTIVE7","MOTIVE8")

#Created dataframes from the vector categories
#MotivesFinancesSPVs <- SolarAdopt2[KeepMotivesF_SPVs]
#MotivesEcoSPVs <- SolarAdopt2[KeepMotivesECO_SPVs]

#Take out all NAs 
#MotivesFinancesSPVs1 <- na.omit(MotivesFinancesSPVs)
#MotivesEco1SPVs1<- na.omit(MotivesEcoSPVs)

#Run CrossTable Anlaysis
#CrossTable(MotivesFinancesSPVs$SPVs, MotivesFinancesSPVs$MOTIVE1, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#SIMILAR RESULTS TO THE TOP WITH PROMPTS.
#WHAT IF AFTER PROJECT WE ADDED CONSIDERERS BACK AND SEE WHAT HAPPENS WITH AN ADDITINAL LEVEL IN OUR SPVS VARIABLE. 

#Create dataframes based on AGE RANGES
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
#RESULTS: PVALUE < .05 HOWEVER THERE IS NO SIGNIFICANCE IN ONE PARTICULAR GROUP.
#THE ONE THAT HAS SIGNIFICANCE DOES NOT MEET THE ASSUMPTION OF 5 VALUES.

CrossTable(MotivesFinances1$MOTIVE4, MotivesFinances1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE > .05

#Create CrossTable for ECO MOTIVES BASED ON AGE RANGE
#AGE_BINNED: 1: 18-44 , 2: 45-54, 3: 55-64, 4: 65+ 
CrossTable(MotivesEco1$MOTIVE5, MotivesEco1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE > .05

CrossTable(MotivesEco1$MOTIVE7, MotivesEco1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE < .05 SIGNIFICANT AGE RANGE HAS AN INFLUENCE ON HOW THEY RANK THE
#IMPORTANCE OF PERSONAL ENVIRONMENTAL IMPACT

CrossTable(MotivesEco1$MOTIVE8, MotivesEco1$AGE_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS P-VALUE > .05

#Data now ready for CrossTable for FINANCIAL PROMPTS BASED ON INCOME RANGE

#CHECK PATTERNS IN THE INFLUENCES BASED ON INCOME
#Data Wrangling 
#Create dataframes based on INCOME RANGES
KeepMotivesF_IB <- c("CASE_ID", "INCOME_BINNED", "MOTIVE1", "MOTIVE3","MOTIVE4")
KeepMotivesECO_IB <- c("CASE_ID", "INCOME_BINNED", "MOTIVE5", "MOTIVE7","MOTIVE8")

#Created dataframes from the vector categories
MotivesFinancesIB <- SolarAdopt2[KeepMotivesF_IB]
MotivesEcoIB <- SolarAdopt2[KeepMotivesECO_IB]

#Take out all NAs 
MotivesFinancesIB1 <- na.omit(MotivesFinancesIB)
MotivesEcoIB1<- na.omit(MotivesEcoIB)

View(MotivesFinancesIB1)
View(MotivesEcoIB1)

#Create CrossTable for FINANCIAL MOTIVES BASED ON INCOME
#INCOME_BINNED: 1: < 50,000, 2: 50,000-74,999, 3: 75,000-99,999, 
#               4: 100,000-149,999, 5: 150,000 or more, 95: Prefer not to answer

CrossTable(MotivesFinancesIB1$MOTIVE1, MotivesFinancesIB1$INCOME_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
CrossTable(MotivesFinancesIB1$MOTIVE3, MotivesFinancesIB1$INCOME_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
CrossTable(MotivesFinancesIB1$MOTIVE4, MotivesFinancesIB1$INCOME_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#NO SIGNIFICANCE PVALUE > .05 


#Create CrossTable for ECO MOTIVES BASED ON INCOME
#INCOME_BINNED: 1: < 50,000, 2: 50,000-74,999, 3: 75,000-99,999, 
#               4: 100,000-149,999, 5: 150,000 or more, 95: Prefer not to answer

CrossTable(MotivesEcoIB1$MOTIVE5, MotivesEcoIB1$INCOME_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS: PVALUE < .05 SHOWS THERE IS A SIGNIFICANCE IN INCOME AND HOW HOW THEY ARE MOTIVATED BY BEING ABLE TO USE RENEWABLE ENERGY

#Post Hoc analysis: 
#NEED TO STILL COMPLETE...

CrossTable(MotivesEcoIB1$MOTIVE7, MotivesEcoIB1$INCOME_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#NOT SIGNIFICANT
CrossTable(MotivesEcoIB1$MOTIVE8, MotivesEcoIB1$INCOME_BINNED, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#NOT SIGNIFICANT 


##Create CrossTable for FINANCIAL MOTIVES BASED ON GENDER

KeepMotivesF_GN <- c("CASE_ID", "GENDER", "MOTIVE1", "MOTIVE3","MOTIVE4")
KeepMotivesECO_GN <- c("CASE_ID", "GENDER", "MOTIVE5", "MOTIVE7","MOTIVE8")

#Created dataframes from the vector categories
MotivesFinancesGN <- SolarAdopt2[KeepMotivesF_GN]
MotivesEcoGN <- SolarAdopt2[KeepMotivesECO_GN]

#Take out all NAs 
MotivesFinancesGN1 <- na.omit(MotivesFinancesGN)
MotivesEcoGN1<- na.omit(MotivesEcoGN)

View(MotivesFinancesGN1)
View(MotivesEcoGN1)

#Run analysis
CrossTable(MotivesFinancesGN1$MOTIVE1, MotivesFinancesGN1$GENDER, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#NOT SIGNIFICANT

CrossTable(MotivesFinancesGN1$MOTIVE3, MotivesFinancesGN1$GENDER, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS: SIGNIFICANT P-VALUE < .05 
#Post Hoc Analysis
#Fewer males than expected selected extremely important that the motive for SPVs was to add home's market value. 
#Significantly More women than expected selected that motive was to add to home's market value. 
CrossTable(MotivesFinancesGN1$MOTIVE4, MotivesFinancesGN1$GENDER, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS: SIGNIFICANT P-VALUE < .05
#Post Hoc Analysis
#Fewer females than expected agreed that protection from rising electricity prices is moderately important.
#also with 1.8 and 1.7 being so close to 2... I would say that fewer females also said that 
#this motive was very and extremely important. 

#Create CrossTable for ECO MOTIVES BASED ON GENDER
CrossTable(MotivesEcoGN1$MOTIVE5, MotivesEcoGN1$GENDER, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS: SIGNIFICANT P-VALUE < .05
#Post Hoc Analysis
#Fewer females than expected selected being able to use renewable energy was not at all important, 
    #slightly, or moderately important. 
#More females than expected selected being able to use renewable energy as being extremely important. 
#Fewer males than expected selected being able to use renewable energy as being extremely important. 
CrossTable(MotivesEcoGN1$MOTIVE7, MotivesEcoGN1$GENDER, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#RESULTS: SIGNIFICANT P-VALUE < .05
#Post Hoc Analysis
#Fewer females than expected selected being able to reduce environmental impact was not at all important, 
#slightly, or moderately important. 
#More females than expected selected being able to reduce environmental impact as being extremely important. 
#Fewer males than expected selected being able to reduce environmental impact as being extremely important. 
#More men than expected seelected being able to reduce environmental impact as being slightly important.
CrossTable(MotivesEcoGN1$MOTIVE8, MotivesEcoGN1$GENDER, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#SIGNIFICANT P-VALUE < .05 
#More males than expected said that setting a good example for others in community is not at all important. 
#Fewer males than expected said that setting a good example for others in community is extremely important. 
#Fewer females than expected said that setting a good example for others in community is not at all important. 
#More females than expected said that setting a good example for others in community is exttremely important. 
#   33% of females said is extremely important vs. 18% of males said it was. 

##Create CrossTable for FINANCIAL MOTIVES BASED ON NUMBER OF PEOPLE IN HOUSEHLD 

KeepMotivesF_NP <- c("CASE_ID", "PEOPLE_TOT_3PLUS", "MOTIVE1", "MOTIVE3","MOTIVE4")
KeepMotivesECO_NP <- c("CASE_ID", "PEOPLE_TOT_3PLUS", "MOTIVE5", "MOTIVE7","MOTIVE8")

#Created dataframes from the vector categories
MotivesFinancesNP <- SolarAdopt2[KeepMotivesF_NP]
MotivesEcoNP <- SolarAdopt2[KeepMotivesECO_NP]

#Take out all NAs 
MotivesFinancesNP1 <- na.omit(MotivesFinancesNP)
MotivesEcoNP1<- na.omit(MotivesEcoNP)

View(MotivesFinancesNP1)
View(MotivesEcoNP1)


#library(ggplot2)
#ggplot(aes(MOTIVE5 + aes(geom_bar(MotivesEcoGN1)

#Create CrossTable for FINANCIAL MOTIVES BASED ON NUMBER OF PEOPLE IN HOUSEHOLD
##PEOPLE_TOT_3PLUS: 0:2 OR LESS, 1: 3 OR MORE

CrossTable(MotivesFinancesNP1$MOTIVE1, MotivesFinancesNP1$PEOPLE_TOT_3PLUS, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
CrossTable(MotivesFinancesNP1$MOTIVE3, MotivesFinancesNP1$PEOPLE_TOT_3PLUS, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
CrossTable(MotivesFinancesNP1$MOTIVE4, MotivesFinancesNP1$PEOPLE_TOT_3PLUS, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#NO SIGNIFICANCE 

#Create CrossTable for ECO MOTIVES BASED ON NUMBER OF PEOPLE IN HOUSEHOLD

CrossTable(MotivesEcoNP1$MOTIVE5, MotivesEcoNP1$PEOPLE_TOT_3PLUS, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
CrossTable(MotivesEcoNP1$MOTIVE7, MotivesEcoNP1$PEOPLE_TOT_3PLUS, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
CrossTable(MotivesEcoNP1$MOTIVE8, MotivesEcoNP1$PEOPLE_TOT_3PLUS, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#NO SIGNIFICANCE 
