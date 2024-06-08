#upload the National Student Clearinhouse + Student information files
nsc21 <- read.csv(file = '*', header = TRUE)
student <- read.csv(file = '*', header = TRUE)

# Upload relevant districts from 2021-2022 
#map <- read.csv("*", header = TRUE)

## Upload relevant districts from 2022-2023
map <- read.csv("*", header = TRUE)

#upload libraries
library(dplyr)
library(plyr)

#rename dataframes
names(nsc21)[names(nsc21) == 'SystemStudentId'] <- 'Student_Identifier'
names(student)[names(student) == 'ï..Student_Identifier'] <- 'Student_Identifier'
df <- left_join(nsc21, student, by = 'Student_Identifier')


names(map)[names(map) == 'RCDTS Text'] <- 'RCDTS'

df2 <- left_join(df,map, by = 'RCDTS')

sum(is.na(df2$Student_Identifier))
count(unique(df2$Student_Identifier))

#keep columns that aren't repeated
colnames(df2)
df3 <- df2[,c(3,4,6,1,2,11,12,13,14,15,16,17,34,36,37,38,39,44,46,48,49)]
sum(is.na(df3$Student_Identifier))

#remove student identifiers that are null
df4 <- subset(df3,!is.na(Student_Identifier))

#split enrollment begin by year, month, date
df4$Enrollment_Begin_Year <- substr(df4$EnrollmentBegin, 1,4)
df4$Enrolment_Begin_Month <- substr(df4$EnrollmentBegin, 5,6)
df4$Enrollment_Begin_Day <- substr(df4$EnrollmentBegin, 7,8)

#filter out Enrollment Begin Year > Graduation Year
df5 <- df4 %>% 
  filter(Enrollment_Begin_Year >= HighSchoolGradYear) 

x <- table(count(unique(df4$Student_Identifier)))
y <- table(count(unique(df5$Student_Identifier)))

x
y

sum(is.na(df5$EnrollmentBegin))
count(unique(df5$Student_Identifier))

#enrollment begin and end as date
df5$EnrollmentBegin <- as.character(df5$EnrollmentBegin)
df5$EnrollmentBegin <- as.Date(df5[["EnrollmentBegin"]], "%Y%m%d")
df5$EnrollmentEnd <- as.character(df5$EnrollmentEnd)
df5$EnrollmentEnd <- as.Date(df5[["EnrollmentEnd"]], "%Y%m%d")

#get rid of empty enrollment begins
df6 <- subset(df4,!is.na(EnrollmentBegin))

#group by Student_Identifier
df7 <- df6 %>%
  group_by(Student_Identifier) %>%
  filter(EnrollmentBegin == min(EnrollmentBegin))

#Drop enrollment begin (UPDATED)
df8 <- df7[,-c(10)]

#Drop duplicates
df9 <- df8[!duplicated(df8$Student_Identifier), ]

#get rid of empty enrollment ends - starting with df4 again, to group by max enrollment date + join in after
df10 <- df4

#group by max enrollment
df11 <- df10 %>% 
  group_by(Student_Identifier) %>%
  filter(EnrollmentEnd == max(EnrollmentEnd))

#only select student identifier and Enrollment Begin

df12 <- df11[,c(1,10)]

df13 <- df12[!duplicated(df12$Student_Identifier), ]

# join in min enrollment dataframe with max enrollment data frame
df14 <- join(df9,df13,by = "Student_Identifier", type = "left", match = "all")

#get rid of 'NULL' in enrollment begin
df14 <- df14 %>%
  filter(EnrollmentBegin != "NULL")

#calculate persistence
df14$EnrollmentBegin <- as.character(df14$EnrollmentBegin)
df14$EnrollmentBegin <- as.Date(df14[["EnrollmentBegin"]], "%Y%m%d")

df14$EnrollmentEnd <- as.character(df14$EnrollmentEnd)
df14$EnrollmentEnd <- as.Date(df14[["EnrollmentEnd"]], "%Y%m%d") 

df14$Diff <- difftime(df14$EnrollmentBegin, df14$EnrollmentEnd, unit = "days")

df14$Diff <- abs(df14$Diff)

df14$Persistence[df14$Diff > 365] <- 'Y'
df14$Persistence[df14$Diff <= 365] <- 'N'

#duplicating 0 duplicated student_identifiers
sum(duplicated(df14$Student_Identifier))

#unique
count(unique(df14$RCDTS))
sum(duplicated(df14$Student_Identifier))
sum(is.na(df14$Student_Identifier))
sum((df14$CollegeName == 'NULL'))
sum(is.na(df14$EnrollmentBegin))
sum(is.na(df14$EnrollmentEnd))

#subset columns to be in right order
df15 <- df14[,c(1,2,3,4,5,6,7,8,9,10,24,11,12,13,14,15,16,17,18,19,20,21,22,23,25,26)]

#filter out high schools not in rcdts file
df16 <- subset(df15, !is.na(df15$Facility Name))

#filter out persistence with NA

df16 <- subset(df15, !is.na(df15$Persistence))

#rename columns to be ready to push to BI Layer
#names(df16)[1] <- 'studentsystemid'
#names(df16)[2] <- 'firstname'
#names(df16)[3] <- 'lastname'
#names(df16)[4] <- 'requestyear'
#names(df16)[5] <- 'highschoolgradyear'
#names(df16)[6] <- 'collegename'
#names(df16)[7] <- 'collegestate'
#names(df16)[8] <- 'collegetype'
#names(df16)[9] <- 'publicprivate'
#names(df16)[10] <- 'enrollmentbegin'
#names(df16)[11] <- 'enrollmentend'
#names(df16)[12] <- 'enrollmentstatus'
#names(df16)[13] <- 'collegesequence'
#names(df16)[14] <- 'gender'
#names(df16)[15] <- 'raceethnicity'
#names(df16)[16] <- 'lowincome'
#names(df16)[17] <- 'ell'
#names(df16)[18] <- 'homelanguage'
#names(df16)[19] <- 'rcdts'
#names(df16)[20] <- 'highschool'
#names(df16)[21] <- 'city'
#names(df16)[22] <- 'districtid'
#names(df16)[23] <- 'district'
#names(df16)[24] <- 'days_persisted'
#names(df16)[25] <- 'persistence'

#export
write.csv(df16,"*", row.names = FALSE)




