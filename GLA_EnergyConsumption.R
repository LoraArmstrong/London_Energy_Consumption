# Uses data on electricity consumption from a representative sample of London households
# to estimate what day in 2016 will have the peak usage for these households
# and what that peak usage will be.
# Data from: http://data.london.gov.uk/dataset/smartmeter-energy-use-data-in-london-households

library(data.table)
library(lubridate)
library(ggplot2)

GetTempData <- function(){
        # Downloads all available daily temperature data from WUnderground website,
        # starting in mid 1996 (earliest available) and ending yesterday (latest available),
        # for weather station at Heathrow airport
        # Merges data into one data table and returns it
        
        dailytemp_dt <- data.table()
        finalDate <- Sys.Date() - 1
        endMonth <- 12
        endDay <- 31
        
        for (y in 1996:year(finalDate)) {
                if (y == year(finalDate)) {
                        endMonth <- month(finalDate)
                        endDay <- day(finalDate)
                }
                URL <- paste('http://www.wunderground.com/history/airport/EGLL/', y, '/1/01/CustomHistory.html?dayend=', endDay, '&monthend=', endMonth, '&yearend=', y, '&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1', sep='')
                dailytemp_dt <- rbindlist(list(dailytemp_dt, fread(URL)))
        }
        return (dailytemp_dt)
}

energy_all_dt <- fread("Power-Networks-LCL-June2015(withAcornGps)v2.csv", header = TRUE, sep=',', stringsAsFactors = TRUE)

#Data cleaning
energy_all_dt <- unique(energy_all_dt)
setnames(energy_all_dt, "KWH/hh (per half hour)", "KWH_hh")
energy_all_dt$KWH_hh <- as.numeric(levels(energy_all_dt$KWH_hh))[energy_all_dt$KWH_hh]
energy_all_dt$Date <- as.Date(energy_all_dt$DateTime)
energy_all_dt <- energy_all_dt[complete.cases(energy_all_dt),]

# Does Ratio of ACORN groups remain constant over time?
total_houses_perday <- energy_all_dt[,.(total_houses=length(unique(LCLid))),by=Date]
bygroupbyday <- energy_all_dt[,list(num_houses=length(unique(LCLid))),by='Acorn_grouped,Date']
bygroupbyday <- merge(bygroupbyday, total_houses_perday, by = "Date", all = TRUE)
bygroupbyday$num_houses_percent <- 100*bygroupbyday$num_houses/bygroupbyday$total_houses

# Acorn group percentages change over time then stabilize in Oct, 2012
acornplot <- ggplot(data=bygroupbyday,aes(x=Date,y=num_houses_percent)) + 
                geom_line(aes(color=Acorn_grouped),size=1.25) + 
                xlab("Date") + scale_y_continuous("Percent of All Houses") +
                ggtitle("Acorn group percentages per day")
ggsave(filename="1_Acorn_percentages_per_day.pdf", plot=acornplot)

# Aggregate data for each house on each day
# Exclude dates before 12 October, 2012 when ACORN group ratios are more variable
byIDbyday <- energy_all_dt[Date>='2012-10-12',list(num_meas=length(KWH_hh), total_KWH=sum(KWH_hh)),by='LCLid,Date']

# If number of meas < 48 then exclude that house on that day due to missing meas
byIDbyday[,num_meas := 48]

# Aggredate data for all houses on each day, calc mean daily KWH & daily KWH for total houses
# Assume that total houses should be the total number in entire dataset
total_houses = length(unique(energy_all_dt$LCLid))
cat("There are",total_houses," houses in the dataset.")
agg <- byIDbyday[,list(num_houses=length(unique(LCLid)),mean_KWH=mean(total_KWH),all_KWH=total_houses*mean(total_KWH)),by=Date]

# Get csv file of daily temperatures at Heathrow, from WUnderground website
dailytemp_dt <- GetTempData()

#keep only useful cols & drop rows with NAs
colstokeep <- c("GMT", "Max TemperatureC", "Mean TemperatureC", "Min TemperatureC")
dailytemp_dt[, (setdiff(names(dailytemp_dt), colstokeep)) := NULL]
setnames(dailytemp_dt, c("GMT", "Max TemperatureC", "Mean TemperatureC", "Min TemperatureC"), c("Date", "MaxTemp", "MeanTemp", "MinTemp"))
dailytemp_dt <- dailytemp_dt[complete.cases(dailytemp_dt),]

#Make some extra date-related columns
dailytemp_dt$Date <- as.Date(dailytemp_dt$Date)
dailytemp_dt$Year <- year(dailytemp_dt$Date)
dailytemp_dt$Weekday <- wday(dailytemp_dt$Date, label=TRUE, abbr=FALSE)
dailytemp_dt$Weekend <- dailytemp_dt$Weekday == 'Saturday' | dailytemp_dt$Weekday == 'Sunday'
dailytemp_dt$Week <- week(dailytemp_dt$Date) #Week number
dailytemp_dt[Week==53]$Week <- 52 #Some yrs have leap weeks, for simplicity set last to 52
# Count winter holidays (Xmas, Boxing Day, New Years Day) as weekend days
dailytemp_dt[(month(Date) == '12' & day(Date) == '25') | (month(Date) == '12' & day(Date) == '26') | (month(Date) == '1' & day(Date) == '1')]$Weekend = TRUE

#Day number from 0 to 366
dailytemp_dt$DayNumber <- as.numeric(
        dailytemp_dt$Date - 
        as.Date(paste(as.character(dailytemp_dt$Year),"-01-01",sep='')))

#merge temperature data with energy use data
agg <- merge(agg, dailytemp_dt, by = "Date")
agg <- agg[all_KWH > 2000]

# Main observation: Temp vs Usage is linear at coldest temperatures
tempplot1 <- ggplot(data=agg,aes(x=MeanTemp,y=all_KWH)) + 
        geom_point(aes(color=Weekend),size=3) + 
        xlab("Mean Daily Temp (degrees C)") + scale_y_continuous("Aggregated Daily Usage (kWh)") +
        ggtitle("Temperature effect on energy usage")
ggsave(filename="2_Temp_vs_Usage_byWeekend.pdf", plot=tempplot1)

# In general, weekends use more energy than weekedays- Dec-Jan 13-14 example
tempplot2 <- ggplot(data=agg[415:455],aes(x=Date,y=all_KWH)) + 
        geom_line() + geom_point(aes(color=Weekend),size=3) + 
        xlab("Date") + scale_y_continuous("Aggregated Daily Usage (kWh)") +
        ggtitle("Energy usage by date")
ggsave(filename="3_Usage_byDate_late2013.pdf", plot=tempplot2)

# Sundays use the most energy on average of all days
weekdayplot <- ggplot(data=agg,aes(x=Weekday,y=all_KWH)) + 
        geom_point(aes(),size=3) + 
        xlab("Day of Week") + scale_y_continuous("Aggregated Daily Usage (kWh)") +
        ggtitle("Energy usage by weekday")
weekdayplot <- weekdayplot + geom_boxplot(aes(fill = Weekday))
ggsave(filename="4_Usage_by_Weekday.pdf", plot=weekdayplot)

# When is coldest week of the year on average?
# Find the coldest week of each year then average the numbers of those weeks
# Answer: 2nd week of January
week_year <- dailytemp_dt[,.(MeanTemp=mean(MeanTemp)),by='Week,Year']
week_year_coldest <- week_year[,.(Coldest_MeanTemp=min(MeanTemp)),by=Year]
week_year_coldest <- week_year_coldest[Year>1996 & Year<2015]

for(y in 1:18){
        currentyear <- y+1997
        current <- week_year[Year==currentyear]
        week_year_coldest$Week[y] <- max(current[MeanTemp == min(MeanTemp)]$Week)        
}

week_year_coldest$WeekAdjusted <- week_year_coldest$Week
week_year_coldest[Week<40]$WeekAdjusted <- week_year_coldest[Week<40]$WeekAdjusted + 52

coldest_week <- mean(week_year_coldest$WeekAdjusted)
if (coldest_week > 52){coldest_week <- coldest_week - 52}

print ("Average Coldest week number of the year from 1997-2014:")
print (coldest_week)

print("Predicted coldest week of 2016 is 2nd week of January")
print("Predicted day of peak usage is Sunday, Jan 10th")

# Coldest temperatures of each year are anomalously cold compared to historical avgs for each day. 
# Use average coldest temperature for all years rather than avg temp of coldest week
coldest_peryear_w <- mean(week_year_coldest$Coldest_MeanTemp)
cpyw_se <- sd(week_year_coldest$Coldest_MeanTemp)/sqrt(length(week_year_coldest$Coldest_MeanTemp))
print("Average temp during coldest week of year from 1997-2014 is:")
print(coldest_peryear_w)

# What is temp on Sunday of coldest week? Assume it is equal to avg. temp during coldest week
coldest_Sunday_temp <- coldest_peryear_w

# Find relationship between temp and agg energery usage for SUNDAY ONLY 
Sundaylm <- lm(all_KWH ~ MeanTemp, data=agg[Weekday=='Sunday' & MeanTemp < 20])
Sundaycoeffs <- coefficients(Sundaylm)

# Use linear model to predict energy usage on coldest Sunday
newdata <- data.frame(MeanTemp = coldest_Sunday_temp)
prediction <- predict(Sundaylm, newdata, interval="confidence")
cat("Peak energy usage is estimated to be",round(prediction[1]/1000, digits=2),"MWh.")
cat("The 95% confidence interval is ",round(prediction[2]/1000, digits=2),
    "-",round(prediction[3]/1000, digits=2),"MWH.")

sundayplot <- ggplot(data=agg[Weekday=='Sunday' & MeanTemp <= 15],aes(x=MeanTemp,y=all_KWH)) + 
        geom_point(aes(),size=3) + geom_abline(slope=Sundaycoeffs[2], 
                                               intercept=Sundaycoeffs[1], color='red') +
        xlab("Mean Daily Temperature (degrees C)") + 
        scale_y_continuous("Aggregated Daily Usage (kWh)") +
        ggtitle("Energy usage on Sundays")
ggsave(filename="5_Usage_vs_Temp_Sundays.pdf", plot=sundayplot)





