

#applying the corrections to density data and cleaning dataframes again (these are now per 40 meters)
#2018 season ignore right now
tick_comp_18 <- left_join(tick_comp_18, correct18, by = "Transect_ID")
tick_comp_18$Site.y <- NULL
tick_comp_18$Date.y <- NULL
colnames(tick_comp_18)[2:3] <- c("Site", "Date")
tick_comp_18 <- tick_comp_18 %>%
  mutate(AA_Adult = (AA_Adults/Distance_m)*40, AA_Nymph = (AA_Nymphs/Distance_m)*40, AA_Larva = (AA_Larvae/Distance_m)*40, IS_Adult = (IS_Adults/Distance_m)*40, IS_Nymph = (IS_Nymphs/Distance_m)*40, IS_Larva = (IS_Larvae/Distance_m)*40)
tick_comp_18[,4:9] <- NULL
#2019 season   



################################################################################
#make per transect summaries 2018 ignore
tick18_v2 <- tick_comp_18 %>%
  group_by(Site, Date) %>%
  summarise(mean_transect_aa.adults = sum(AA_Adult, na.rm = TRUE)/10, total_aa.adults = sum(as.numeric(AA_Adult)), mean_transect_aa.nymphs = sum(as.numeric(AA_Nymph))/10, total_aa.nymphs = sum(as.numeric(AA_Nymph)), mean_transect_aa.larvae = sum(as.numeric(AA_Larva))/10, total_aa.larvae = sum(as.numeric(AA_Larva)), mean_transect_is.adults = sum(as.numeric(IS_Adult))/10, total_is.adults = sum(as.numeric(IS_Adult)), mean_transect_is.nymphs = sum(as.numeric(IS_Nymph))/10, total_is.nymphs = sum(as.numeric(IS_Nymph)), mean_transect_is.larvae = sum(as.numeric(IS_Larva))/10, total_is.larvae = sum(as.numeric(IS_Larva)))

test <- both_corrected %>%
  group_by(Site, Date) %>%
  summarize(meanaaadults = mean(AA_Adults, na.rm = TRUE))

#per transect summary 2019 
tick19_v2 <- tick_comp_19 %>%
  group_by(Site, Date) %>%
  summarise(mean_transect_aa.adults= sum(AA_Adult, na.rm = TRUE)/10, total_aa.adults = sum(AA_Adult, na.rm = TRUE), mean_transect_aa.nymphs = sum(AA_Nymph, na.rm = TRUE)/10, total_aa.nymphs = sum(AA_Nymph, na.rm = TRUE), mean_transect_aa.larvae = sum(AA_Larva, na.rm = TRUE)/10, total_aa.larvae = sum(AA_Larva, na.rm = TRUE), mean_transect_is.adults = sum(IS_Adult, na.rm = TRUE)/10, total_is.adults = sum(IS_Adult, na.rm = TRUE), mean_transect_is.nymphs = sum(IS_Nymph, na.rm = TRUE)/10, total_is.nymphs = sum(IS_Nymph, na.rm = TRUE), mean_transect_is.larvae = sum(IS_Larva, na.rm = TRUE)/10, total_is.larvae = sum(IS_Larva, na.rm = TRUE),mean_transect_hl.adults = sum(HL_Adult, na.rm = TRUE)/10, total_hl.adults = sum(HL_Adult, na.rm = TRUE), mean_transect_hl.nymphs = sum(HL_Nymph, na.rm = TRUE)/10, total_hl.nymphs = sum(HL_Nymph, na.rm = TRUE), mean_transect_hl.larvae = sum(HL_Larva, na.rm = TRUE)/10, total_hl.larvae = sum(HL_Larva, na.rm = TRUE)) %>%
  filter(!Site %in% c("BH", "WB"))

tick19_v2$Site <- as.factor(tick19_v2$Site)

#making two season tick dataset ignore this tooo
both_tick <- rbind(as.numeric(tick18_v2), as.numeric(tick19_v2))
#adding year column
both_tickv2 <- both_tick %>%
  mutate(year = lubridate::year(Date), newdate = lubridate::month(Date)) %>%
  mutate(year = as.factor(year))

#############################################################################################
#OLD STUFF YOU NEED TO ORGANIZE
#############################################################################################

#creating daily mean values

#Arden Woods
AW <- read.csv("/Users/amandaweaver/Desktop/Columbia University/Diuk-Wasser Lab/thesis/Weather Data (SI, 2018)/HOBO/Arden_Woods_HOBO_clean.csv")
colnames(AW) <- c("Point", "Date", "Time", "Temp", "Hum")
AW$Date <- as.Date(AW$Date, format="%m/%d/%y")
head(AW)

#IF NECESSARY
#AW$Temp <- as.numeric(paste(AW$Temp))
#AW$Hum <- as.numeric(paste(AW$Hum))

#average
AW_AVG_TEMP <- tapply(AW$Temp, AW$Date, mean)
AW_AVG_HUM <- tapply(AW$Hum, AW$Date, mean)

head(AW_AVG_TEMP)
head(AW_AVG_HUM)

str(AW_AVG_TEMP)

AW_avg <- cbind(AW_AVG_TEMP, AW_AVG_HUM)
head(AW_avg)

#Blue Heron
BH <- read.csv("/Users/amandaweaver/Desktop/Columbia University/Diuk-Wasser Lab/thesis/Weather Data (SI, 2018)/HOBO/Blue_Heron_HOBO_clean.csv")
head(BH)
colnames(BH) <- c("Point", "Date", "Time", "Temp", "Hum")
BH$Date <- as.Date(BH$Date, format="%m/%d/%y")
BH_AVG_TEMP <- tapply(BH$Temp, BH$Date, mean)
BH_AVG_HUM <- tapply(BH$Hum, BH$Date, mean)
BH_avg <- cbind(BH_AVG_TEMP, BH_AVG_HUM)

#Clove Lakes
CL <- read.csv("/Users/amandaweaver/Desktop/Columbia University/Diuk-Wasser Lab/thesis/Weather Data (SI, 2018)/HOBO/Clove_Lakes_HOBO_clean.csv")
colnames(CL) <- c("Point", "Date", "Time", "Temp", "Hum")
CL$Date <- as.Date(CL$Date, format="%m/%d/%y")
CL_AVG_TEMP <- tapply(CL$Temp, CL$Date, mean)
CL_AVG_HUM <- tapply(CL$Hum, CL$Date, mean)
CL_avg <- cbind(CL_AVG_TEMP, CL_AVG_HUM)
head(CL_avg)

#Conference House
CH <- read.csv("/Users/amandaweaver/Desktop/Columbia University/Diuk-Wasser Lab/thesis/Weather Data (SI, 2018)/HOBO/Conference_House_HOBO_clean.csv")
colnames(CH) <- c("Point", "Date", "Time", "Temp", "Hum")
CH$Date <- as.Date(CH$Date, format="%m/%d/%y")
CH_AVG_TEMP <- tapply(CH$Temp, CH$Date, mean)
CH_AVG_HUM <- tapply(CH$Hum, CH$Date, mean)
CH_avg <- cbind(CH_AVG_TEMP, CH_AVG_HUM)
head(CH_avg)

#Goodhue
GH <- read.csv("/Users/amandaweaver/Desktop/Columbia University/Diuk-Wasser Lab/thesis/Weather Data (SI, 2018)/HOBO/Goodhue_HOBO_clean.csv")
colnames(GH) <- c("Point", "Date", "Time", "Temp", "Hum")
GH$Date <- as.Date(GH$Date, format="%m/%d/%y")
GH_AVG_TEMP <- tapply(GH$Temp, GH$Date, mean)
GH_AVG_HUM <- tapply(GH$Hum, GH$Date, mean)
GH_avg <- cbind(GH_AVG_TEMP, GH_AVG_HUM)
head(GH_avg)

#King Fisher
KF <- read.csv("/Users/amandaweaver/Desktop/Columbia University/Diuk-Wasser Lab/thesis/Weather Data (SI, 2018)/HOBO/King_Fisher_HOBO_clean.csv")
colnames(KF) <- c("Point", "Date", "Time", "Temp", "Hum")
KF$Date <- as.Date(KF$Date, format="%m/%d/%y")
KF_AVG_TEMP <- tapply(KF$Temp, KF$Date, mean)
KF_AVG_HUM <- tapply(KF$Hum, KF$Date, mean)
KF_avg <- cbind(KF_AVG_TEMP, KF_AVG_HUM)
head(KF_avg)

#Latorette
LT <- read.csv("/Users/amandaweaver/Desktop/Columbia University/Diuk-Wasser Lab/thesis/Weather Data (SI, 2018)/HOBO/Latorette_HOBO_clean.csv")
colnames(LT) <- c("Point", "Date", "Time", "Temp", "Hum")
LT$Date <- as.Date(LT$Date, format="%m/%d/%y")
LT_AVG_TEMP <- tapply(LT$Temp, LT$Date, mean)
LT_AVG_HUM <- tapply(LT$Hum, LT$Date, mean)
LT_avg <- cbind(LT_AVG_TEMP, LT_AVG_HUM, LTdate)
head(LT_avg)
length(LT)
LT_avg <- subset(LT_avg, -c(LTdate))
colnames(LT_avg) <- c("Date", "ignore", "AvgTemp", "AvgHum")
LT_avg$LTdate <- as.Date(LTdate, format="%m/%d/%y")
datetest
class(LTdate)
LTdate <- unique(LT$Date)

#Willowbrook
WB <- read.csv("/Users/amandaweaver/Desktop/Columbia University/Diuk-Wasser Lab/thesis/Weather Data (SI, 2018)/HOBO/Willowbrook_HOBO_clean.csv")
colnames(WB) <- c("Point", "Date", "Time", "Temp", "Hum")
WB$Date <- as.Date(WB$Date, format="%m/%d/%y")
WB_AVG_TEMP <- tapply(WB$Temp, WB$Date, mean)
WB_AVG_HUM <- tapply(WB$Hum, WB$Date, mean)
WB_avg <- cbind(WB$data, WB_AVG_TEMP, WB_AVG_HUM)
colnames(WB_avg) <- c("date", "temp", "humidity")
head(WB_avg)

#making comp avg data frame
daily_mean <- do.call("cbind", list(WB_avg, LT_avg, AW_avg, BH_avg, CL_avg, CH_avg, GH_avg, KF_avg))
date <- seq(as.Date("2018-07-30"), as.Date("2018-10-18"), by="days")
daily_mean <- cbind(date, daily_mean)
date2 <- seq(as.Date("2018-07-30"), as.Date("2018-10-18"), by="days")
daily_mean <- cbind(date2, daily_mean)
head(daily_mean)

#make it a dataframe
class(as.data.frame(daily_mean))
daily_mean <- as.data.frame(daily_mean)

temp_18 <- hobo_comp_18 %>%
  group_by(date, site) %>%
  summarize(mean_daily_temp = mean(temp)) %>%
  mutate(site = as.factor(site))
temp_18$date <- as.Date(temp_18$date)

#making average daily temp plots over the season
library(scales)
avg_temp_18 <- ggplot(data = temp_18, aes(x = date, y = mean_daily_temp, col = site)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Date", y = "Mean Daily Temperature (F)") +
  scale_x_date(date_breaks = "3 weeks", labels = date_format("%m/%d/%Y"))
avg_temp_18


avg_temp_18 <- ggplot() +
  geom_line(data=daily_mean, aes(x=date2, y=WB_AVG_TEMP), color = "red") +
  geom_line(data=daily_mean, aes(x=date2, y=LT_AVG_TEMP), color = "orange") +
  geom_line(data=daily_mean, aes(x=date2, y=AW_AVG_TEMP), color = "yellow") +
  geom_line(data=daily_mean, aes(x=date2, y=BH_AVG_TEMP), color = "green") +
  geom_line(data=daily_mean, aes(x=date2, y=CL_AVG_TEMP), color = "blue") +
  geom_line(data=daily_mean, aes(x=date2, y=CH_AVG_TEMP), color = "purple") +
  geom_line(data=daily_mean, aes(x=date2, y=GH_AVG_TEMP), color = "grey") +
  geom_line(data=daily_mean, aes(x=date2, y=KF_AVG_TEMP)) +
  ylab('Average Daily Temperature - 2018') +
  scale_x_date(name='Date', date_breaks = "2.5 weeks", date_labels = c('August 8', 'August 20', 'September 3', 'September 17', 'October 1', 'October 15')) +
  geom_hline(yintercept = 90, color = "black") +
  geom_hline(yintercept = 77, color = "black")
avg_temp_18


#plotting every temp data point over the season
combined = as.POSIXct(paste(hobo_comp_SI18$date, time_simp), format="%Y-%m-%d %H:%M:%S")
View(combined)
time_simp <- gsub('1899-12-31', '', hobo_comp_SI18$time)
View(time_simp)

hobo_comp_SI18 <- cbind.data.frame(hobo_comp_SI18, combined)

all_temp18 <- ggplot(data = hobo_comp_SI18, aes(x=combined, y = temp, color = site)) +
  geom_line(aes(group = site)) +
  geom_hline(yintercept = c(77, 90), color = "black") +
  ylab("Temperature - 30 min") +
  xlab("Date - 2018") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))
all_temp18
class(combined)

#creating average over staten island column
island_avgt <- rowMeans(as.numeric(SI_temp), na.rm = TRUE)

SI_temp <- cbind.data.frame(date2, WB_AVG_TEMP, AW_AVG_TEMP, BH_AVG_TEMP, CH_AVG_TEMP, CL_AVG_TEMP, GH_AVG_TEMP, KF_AVG_TEMP, LT_AVG_TEMP)
#this didn't work

#making average daily humidity plots over the season
avg_hum_18 <- ggplot() +
  geom_line(data=daily_mean, aes(x=date2, y=WB_AVG_HUM), color = "red") +
  geom_line(data=daily_mean, aes(x=date2, y=LT_AVG_HUM), color = "orange") +
  geom_line(data=daily_mean, aes(x=date2, y=AW_AVG_HUM), color = "yellow") +
  geom_line(data=daily_mean, aes(x=date2, y=BH_AVG_HUM), color = "green") +
  geom_line(data=daily_mean, aes(x=date2, y=CL_AVG_HUM), color = "blue") +
  geom_line(data=daily_mean, aes(x=date2, y=CH_AVG_HUM), color = "purple") +
  geom_line(data=daily_mean, aes(x=date2, y=GH_AVG_HUM), color = "grey") +
  geom_line(data=daily_mean, aes(x=date2, y=KF_AVG_HUM)) +
  ylab('Average Daily Humidity') +
  scale_x_date(name='Date - 2018', date_breaks = "2.5 weeks", date_labels = "%B %e") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=25,face="bold"))
avg_hum_18

#plotting all humidity vals
all_hum18 <- ggplot(data = hobo_comp_SI18, aes(x=combined, y = rel_hum, color = site)) +
  geom_line(aes(group = site)) +
  ylab("Relative Humidity") +
  xlab("Date - 2018") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=25,face="bold"))
all_hum18

#make site a factor variable
hobo_comp_SI18$site <- as.factor(hobo_comp_SI18$site)
class(hobo_comp_SI18$site)

#box plot avg/var temp per site?
temp_site <- ggplot(data = hobo_comp_SI18, aes(x=site, y=temp, fill = site)) +
  geom_boxplot()

temp_site
hum_site

#box plot avg/var humidty per site?
hum_site <- ggplot(data = hobo_comp_SI18, aes(x=site, y=rel_hum, fill = site)) +
  geom_boxplot()

#making a scatter plot of average daily temp vs. # ticks collected 

#creating site subsets for weather data HOBO 2018
BH_hobo <-subset(hobo_comp_SI18, site == "BH")
AW_hobo <-subset(hobo_comp_SI18, site == "AW")
KF_hobo <-subset(hobo_comp_SI18, site == "KF")
CL_hobo <-subset(hobo_comp_SI18, site == "CL")
CH_hobo <-subset(hobo_comp_SI18, site == "CH")
ML_hobo <-subset(hobo_comp_SI18, site == "ML")
JW_hobo <-subset(hobo_comp_SI18, site == "JW")
LT_hobo <-subset(hobo_comp_SI18, site == "LT")

#checking normality of temp and humidity, NOT NORMAL 
install.packages("car")
library("car")
qqPlot(BH$temp)
qqPlot(AW$rel_hum)
shapiro.test(BH$temp)
shapiro.test(AW$temp)
class(as.factor(hobo_comp_SI18$site))
hobo_comp_SI18$site <- as.factor(hobo_comp_SI18$site)

#results HOBO, assuming unpaired data, not normal distribution 
kruskal.test(temp ~ site, data = hobo_comp_SI18) #p-value = 1.05e-12
kruskal.test(rel_hum ~site, data = hobo_comp_SI18) #p-value < 2.2e-16
#differ by site for both temp and humidity

#subsets for ibutton data 2018
BH_ib <-subset(ibutton_comp_SI18, site == "BH")
AW_ib <-subset(ibutton_comp_SI18, site == "AW")
KF_ib <-subset(ibutton_comp_SI18, site == "KF")
CL_ib <-subset(ibutton_comp_SI18, site == "CL")
CH_ib <-subset(ibutton_comp_SI18, site == "CH")
ML_ib <-subset(ibutton_comp_SI18, site == "ML")
JW_ib <-subset(ibutton_comp_SI18, site == "JW")
LT_ib <-subset(ibutton_comp_SI18, site == "LT")

#make site a factor
ibutton_comp_SI18$site <- as.factor(ibutton_comp_SI18$site)

#checking ibutton 2019 for normality, NOT NORMAL
qqPlot(BH_ib$humidity)
shapiro.test(BH_ib$temp)

#results iButton, assuming unpaired data, not normal distribution
kruskal.test(temp ~ site, data = ibutton_comp_SI18) #p-value < 2.2e-16
kruskal.test(humidity ~ site, data = ibutton_comp_SI18) #p-value < 2.2e-16
#differ by site for both temp and humidity

#plotting 2018 IS ticks session by site
overall_IStick <- ggplot(tick.clean2, aes(x=Date, y = IS_Nymphs, color = Site)) + 
  geom_point()+
  geom_line()
overall_IStick

#plotting 2018 AA ticks session by site
overall_AAtick <- ggplot(tick.clean3, aes(x=Date, y = AA_Nymphs, color = Site)) + 
  geom_point()+
  geom_line()
overall_AAtick

tick.clean<- na.omit(subset(tick_comp_SI18, select = c(Date, IS_Nymphs, AA_Nymphs, Site)))
head(tick.clean)
tick.clean$IS_Nymphs <- as.numeric(tick.clean$IS_Nymphs)
tick.clean$AA_Nymphs <- as.numeric(tick.clean$AA_Nymphs)
tick.clean$Site <- as.factor(tick.clean$Site)
class(tick.clean$Date)
#this has all the drags separated right now

tick.clean2 <- aggregate(IS_Nymphs ~ Date + Site, tick.clean, sum)
tick.clean3 <- aggregate(AA_Nymphs ~ Date + Site, tick.clean, sum)
class(tick.clean2$Date)

#plotting as percentage activity over all parks and then combined

#creating new variables
sum(ticksum$IS_Nymphs) #1222
sum(ticksum$AA_Nymphs) #521

percent18AA <- ticksum$AA_Nymphs/521
percent18IS <-ticksum$IS_Nymphs/1222

#aggregating
length(tick.clean2$IS_Nymphs)
length(tick.clean3$AA_Nymphs)
ticksum <- cbind.data.frame(ticksum, percent18IS, percent18AA)
colnames(tick.clean18) <- c("date", "site", "IS_Nymphs", "AA_Nymphs", "Percent_IS", "Percent_AA")
tick.clean18 <- ticksum
head(tick.clean18)

#percent AA plot
percent_AAtick <- ggplot(tick.clean18, aes(x=date, y = Percent_AA, color = site)) + 
  geom_point()+
  geom_line()
percent_AAtick

#percent IS plot
percent_IStick <- ggplot(tick.clean18, aes(x=date, y = Percent_IS, color = site)) + 
  geom_point()+
  geom_line()
percent_IStick

#combined
ISpercent <- ggplot(tick.clean, aes(x=date, y = percent_IS)) +
  geom_point() +
  geom_line()
ISpercent

#aggregating the 2019 data

tick19 <- aggregate(IS_Nymph ~ Date + Site, tick_comp_19, sum)
and <- aggregate(AA_Nymph ~ Date + Site, tick_comp_19, sum)
this <- aggregate(HL_Nymph ~ Date + Site, tick_comp_19, sum)

total <- tick.clean19$IS_Nymphs+ tick.clean19$AA_Nymphs + tick.clean19$HL_Nymphs

tick.clean19 <- cbind.data.frame(tick19, and$AA_Nymph, this$HL_Nymph, percent_IS19, percent_AA19, percent_HL19)
colnames(tick.clean19) <- c("date", "site", "IS_Nymphs", "AA_Nymphs",  "HL_Nymphs", "percent_IS", "percent_AA", "percent_HL")
head(tick.clean19)
length(tick.clean19$date)
tick.clean19$site <- as.factor(tick.clean19$site)
class(tick.clean19$site)

#2019 percent IS plot
percent_IStick19 <- ggplot(tick.clean19, aes(x=date, y = percent_IS, colour = site)) + 
  geom_point()+
  geom_line()
percent_IStick19

#2019 perecent AA plot
percent_AAtick19 <- ggplot(tick.clean19, aes(x=date, y = percent_AA, colour = site)) + 
  geom_point()+
  geom_line()
percent_AAtick19

#2019 percent HL plot
percent_HLtick19 <- ggplot(tick.clean19, aes(x=date, y = percent_HL, colour = site)) + 
  geom_point()+
  geom_line()
percent_HLtick19

#REMAKING PLOTS OF PERCENTAGE

sum(tick.clean19$IS_Nymphs) #668
sum(tick.clean19$AA_Nymphs) #2032
sum(tick.clean19$HL_Nymphs) #1047

percent_AA19 <- tick.clean19$AA_Nymphs/2032
percent_HL19 <- tick.clean19$HL_Nymphs/1047
percent_IS19 <- tick.clean19$IS_Nymphs/668

aggregate(date ~ site, ibutton_comp_SI18, range)
ibutton_comp_SI18$time <- gsub('1899-12-31', '', ibutton_comp_SI18$time)
class(ibutton_comp_SI18$date)
ibutton_comp_SI18$date <- as.Date(ibutton_comp_SI18$date, format="%A, %B %d, %Y")

#figure for remais presentation
head(tick_comp_SI18)
tick_comp_SI18$Site <- as.factor(tick_comp_SI18$Site)
sum(tick_comp_SI18$IS_Nymphs, na.rm = T)
sum(tick_comp_SI18$AA_Nymphs, na.rm = T)
#aggregate
tick18 <- aggregate(IS_Nymphs ~ Date + Site, tick_comp_SI18, sum)
and <- aggregate(AA_Nymphs ~ Date + Site, tick_comp_SI18, sum)
head(tick18)
head(tick.clean18)
ISdense <- tick.clean18$IS_Nymphs/10
AAdense <- tick.clean18$AA_Nymphs/10
tick.clean18 <- cbind.data.frame(tick18, and, ISdense, AAdense)
tick.clean18 <- subset(tick.clean18, select = -c(Date, Site))

densityIS18 <- ggplot(tick.clean18, aes(x=Date, y = ISdense, colour = Site)) + 
  geom_point() +
  ylab("IS Density")
densityIS18

densityAA18 <- ggplot(tick.clean18, aes(x=Date, y = AAdense, colour = Site)) + 
  geom_point()+
  geom_smooth(se = FALSE) +
  ylab("AA Density")
densityAA18

datetest <- as.Date.POSIXlt(LT$date, format="%m/%d/%y")
head(datetest)

#making residuals for 2018 data
is18expmod <- nymphdensity_40m18 %>%
  filter(species == "Ixodes")

# Select an approximate $\theta$, since theta must be lower than min(y), and greater than zero
theta.0 <- min(is18expmod$density40) * 0.5

# Estimate the rest parameters using a linear model
model.0 <- lm(log(density40 - theta.0) ~ session, data=is18expmod)  
alpha.0 <- exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

# Starting parameters
start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
modelis18 <- nls(density40 ~ alpha * exp(beta * session) + theta , data = is18expmod, start = start)
summary(modelis18)

plot(is18expmod$session, is18expmod$density40)
lines(is18expmod$session, predict(modelis18, list(x = is18expmod$session)), col = 'skyblue', lwd = 3)


#same thing but for a couple individual parks
theta.0 <- min(LTis18expmod$is_nymphs) * 0.5
class(theta.0)

# Estimate the rest parameters using a linear model
model.0 <- lm(log(is_nymphs - theta.0) ~ Date, data=LTis18expmod)  
alpha.0 <- exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

# Starting parameters
start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
modelCHis18 <- nls(is_nymph ~ alpha * exp(beta * Date) + theta , data = CHis18expmod, start = start)
summary(modelCHis18)

#just trying a linear model
test <- lm(log(prop_act) ~ Date + Site, data = isnymphs18_mod,)
summary(test)

################################################################################
#individually plotting both average transects for both seasons
#2018 aa nymphs
aanymphs18 <- ggplot(tick18_v2, aes(x = Date, y = mean_transect_aa.nymphs, color = Site)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ylab("AA Nymph Transect - 2018")
aanymphs18

#2019 aa nymphs
aanymphs19 <- ggplot(tick19_v2, aes(x = Date, y = mean_transect_aa.nymphs, color = Site)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ylab("AA Nymph Transect - 2019")
aanymphs19

#both aa nymphs plotted on top of each other NOT FINISHED
ggplot(both_tickv2, aes(x = newdate, y = mean_transect_aa.nymphs)) +
  geom_point(aes(color = year)) +
  geom_smooth(aes(color = year), se = FALSE) +
  ylab("AA Nymph Transect")

#2018 is nymphs
isnymphs18 <- ggplot(tick18_v2, aes(x = Date, y = mean_transect_is.nymphs, color = Site)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ylab("IS Nymph Transect - 2018")
isnymphs18

#2019 is nymphs
isnymphs19 <- ggplot(tick19_v2, aes(x = Date, y = mean_transect_is.nymphs, color = Site)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ylab("IS Nymph Transect - 2019")
isnymphs19

#2018 hl nymphs doesn't exist

#2019 hl nymphs
hlnymphs19 <- ggplot(tick19_v2, aes(x = Date, y = mean_transect_hl.nymphs, color = Site)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ylab("HL Nymph Transect - 2019")
hlnymphs19

########################################################################
#density plots over time

#mean density per 40m
nymphdensity_40m18 <- both_corrected %>%
  group_by(session) %>% 
  filter(Date <= "2018-10-01") %>%
  summarize(aa_nymphs = mean(AA_Nymphs, na.rm = T), is_nymphs = mean(IS_Nymphs, na.rm = T), hl_nymphs = mean(HL_Nymphs, na.rm = T)) %>%
  pivot_longer(cols = ends_with("_nymphs"), values_to = "density40", names_to = "species")
nymphdensity_40m18$session <- as.numeric(nymphdensity_40m18$session)
nymphdensity_40m18$species <- as.factor(nymphdensity_40m18$species)
levels(nymphdensity_40m18$species) <- c("Amblyomma", "Haemaphysalis", "Ixodes")

#keep parks separate
parkdensity_40 <- both_corrected %>%
  group_by(Site, Date) %>%
  filter(Date <= "2018-10-01") %>%
  summarize(aa_nymphs = mean(AA_Nymphs, na.rm = T), is_nymphs = mean(IS_Nymphs, na.rm = T), hl_nymphs = mean(HL_Nymphs, na.rm = T))

LTis18expmod <- parkdensity_40 %>%
  filter(Site == "LT") %>%
  select(Site, Date, is_nymphs)
LTis18expmod$Date <- as.numeric(as.Date(LTis18expmod$Date) - as.Date("1982-1-4"))

ggplot(data = nymphdensity_40m18, aes(x = session, y = density40, col = species)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ exp(-x), se = F) +
  facet_wrap(~species) +
  labs(x = "Session", y = "Nymphal Density 40m") +
  ylim(0,6) +
  scale_x_continuous(breaks = c(1:6))

################################################################################
#looking at lags in the temperaturea and humidity data, seems like a huge amount of    autocorrelation (only done a few so far), displaying two day worth of lags
acf(BH_hobo18$temp, na.action = na.omit, lag.max = 1440)
acf(BH_hobo18$rel_hum, na.action = na.omit, lag.max = 1440)
pacf(BH_hobo18$temp, pl = TRUE, lag.max = 96)
acf(AW_hobo18$temp, na.action = na.omit, lag.max = 1440)
acf(AW_hobo18$rel_hum, na.action = na.omit, lag.max = 96)
acf(KF_hobo18$temp, na.action = na.omit, lag.max = 96)
acf(KF_hobo18$rel_hum, na.action = na.omit, lag.max = 96)
acf(CL_hobo18$temp, na.action = na.omit, lag.max = 96)
acf(CL_hobo18$rel_hum, na.action = na.omit, lag.max = 96)
acf(CH_hobo18$temp, na.action = na.omit, lag.max = 96)
acf(CH_hobo18$rel_hum, na.action = na.omit, lag.max = 96)
acf(WB_hobo18$temp, na.action = na.omit, lag.max = 96)
acf(WB_hobo18$rel_hum, na.action = na.omit, lag.max = 96) 
acf(GH_hobo18$temp, na.action = na.omit, lag.max = 96)
acf(GH_hobo18$rel_hum, na.action = na.omit, lag.max = 96)
acf(LT_hobo18$temp, na.action = na.omit, lag.max = 96)
acf(LT_hobo18$rel_hum, na.action = na.omit, lag.max = 96)




