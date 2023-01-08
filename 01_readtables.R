library(dplyr)

list.files(path = "/Users/moblakah/Downloads/Fitabase Data 4.12.16-5.12.16/")

setwd("/Users/moblakah/Downloads/Fitabase Data 4.12.16-5.12.16/")

#### Daily ####

sleep <- read.csv("sleepDay_merged.csv",header = T)
#intensity <- read.csv("dailyIntensities_merged.csv",header = T)
activity <- read.csv("dailyActivity_merged.csv",header = T)
#steps <- read.csv("dailySteps_merged.csv",header = T)
#calories <- read.csv("dailyCalories_merged.csv",header = T)

weight <- read.csv("weightLogInfo_merged.csv",header = T)

# check for duplicates in sleep table and remove
sleep %>% group_by(Id,SleepDay) %>% count() %>% View()
sleep$Id <- as.character(sleep$Id)
sleep <- sleep %>% unique()

# check how many ids per table
length(unique(weight$Id))

# merge tables 
colnames(activity)[2] <- "ActivityDay"
activity$Id <- as.character(activity$Id)
sleep$Id <- as.character(sleep$Id)
sleep$ActivityDay <- substring(sleep$SleepDay,1,9)
daily <- left_join(activity,sleep)

weight$Id <- as.character(weight$Id)
weight$ActivityDay <- substring(weight$Date,1,9)

daily <- left_join(daily, weight)
rm(activity,sleep,weight)

#remove unnecessary columns
daily <- daily %>% select(-c(Date,SleepDay))

#Create a new column called QualitySleep by dividing minutes asleep by time in bed
daily$QualitySleep <- daily$TotalMinutesAsleep / daily$TotalTimeInBed * 100
#### Hourly ####
calories <- read.csv("hourlyCalories_merged.csv")
intensities <- read.csv("hourlyIntensities_merged.csv")
steps <- read.csv("hourlySteps_merged.csv")

hourly <- left_join(calories,intensities)
hourly <- left_join(hourly,steps)
rm(steps,calories,intensities)

#### By minute ####

calories_narrow <- read.csv( "minuteCaloriesNarrow_merged.csv")
intensities_narrow <- read.csv( "minuteIntensitiesNarrow_merged.csv")
steps_narrow <- read.csv("minuteStepsNarrow_merged.csv")
sleep <- read.csv( "minuteSleep_merged.csv")
met <- read.csv("minuteMETsNarrow_merged.csv")


minute <- left_join(calories_narrow,intensities_narrow)
minute <- left_join(minute,steps_narrow)
minute <- left_join(minute,met)

rm(calories_narrow,intensities_narrow,steps_narrow,met)

sleep %>% group_by(Id,ActivityMinute) %>% count() %>% View()

colnames(sleep)[2] <- "ActivityMinute"
sleep <- sleep %>% unique()
minute <- left_join(minute,sleep)

rm(sleep)


#### heart rate #### 
heartrate <- read.csv("heartrate_seconds_merged.csv")
heartrate$numofcharacters <- nchar(heartrate$Time)

heartrate$minute <- substring(heartrate$Time,1,14)
heartrate$minute[heartrate$numofcharacters==21] <- substring(heartrate$Time[heartrate$numofcharacters==21],1,15)
heartrate$minute[heartrate$numofcharacters==19] <- substring(heartrate$Time[heartrate$numofcharacters==19],1,13)

heartrate_minute <- heartrate %>% group_by(Id,minute) %>% summarise(Heartrate_minute=mean(Value))
heartrate_minute$minute <- paste0(heartrate_minute$minute, ":00 AM")
colnames(heartrate_minute)[2] <- "ActivityMinute"

minute <- left_join(minute,heartrate_minute)


#to check with Marie
ggplot(data=hourly,aes(x=ActivityHour, y=AverageIntensity)) + geom_smooth(stat="identity") +
  theme_classic() + theme(axis.text.x = element_text(angle=45))+
  labs(title="Activity Hour vs Average Intensity")
       
       
write.csv(x = daily, file = "daily.csv")
getwd()

write.csv(x=heartrate, file = "heartrate.csv")
getwd()

write.csv(x=heartrate_minute, file = "heartrate_minute.csv")
getwd()

write.csv(x=hourly, file = "hourly.csv")

write.csv(x=minute,file = "minute.csv")

daily %>%
  summarise(average = mean(QualitySleep,na.rm = TRUE))
install.packages("here")
here()
