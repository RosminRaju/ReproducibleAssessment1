setwd("E:/__DScienceJohnHopkins/ReproducibleResearch/Tema")

##Loading and preprocessing the data
#The function "verify_and_install" 
#check if a package is installed and it 
#installs if that is not installed
verify_and_install<-function(name_package)
{
  if(name_package %in% rownames(installed.packages())==FALSE)
  {install.packages(name_package)}
}

verify_and_install("dplyr")    
verify_and_install("ggplot2")
verify_and_install("lattice")
verify_and_install("readr")

#suppressMessages supress the message printed on load library dplyr
suppressMessages(library(dplyr))
library(ggplot2)
library(lattice)
library(readr)

activity <- read_csv("activity.csv",
                     col_types = list(
                       steps=col_integer(),
                       date=col_date(),
                       interval=col_integer()),
                     col_names = TRUE
                     )

##What is mean total number of steps taken per day?

summary(activity)

df <- activity %>%
      group_by(date) %>%
      summarize(steps_per_day = sum(steps, na.rm = any(!is.na(steps))))


df1<-df %>% filter(is.na(steps_per_day)==FALSE)

ggplot(data=df1, aes(steps_per_day)) + 
  geom_histogram(binwidth=2000,
                 col="black", 
                 fill="black", 
                 alpha = .1) + 
  labs(title="Steps per day") +
  labs(x="Steps", y="Frequency") +
  theme_bw()+
  theme(axis.title.y=element_text(vjust=1, size=14), 
        axis.title.x=element_text(vjust=.3, size=14),
        axis.text=element_text(size=14),
        title=element_text(vjust=1.2, size=14)) 

meanValue <- mean(df$steps_per_day, na.rm=TRUE) 
medianValue <- median(df$steps_per_day, na.rm=TRUE)

meanR <- format(meanValue, big.mark=",", scientific=FALSE)
medianR <- format(medianValue, big.mark=",", scientific=FALSE)

  
##What is the average daily activity pattern?

dataSeries <- activity %>%
              group_by(interval) %>%
              summarize(steps_per_day = mean(steps, na.rm = TRUE))


ggplot(data=dataSeries, aes(x=interval, y=steps_per_day)) + 
  geom_line(aes(group=1), color="blue") + 
  theme_bw() + 
  scale_x_discrete(breaks=seq(0, 2355, 205)) + 
  xlab("time interval") + 
  ylab("average number of steps") + 
  labs(title="Average steps by time interval")




position<-which(dataSeries$steps_per_day==max(dataSeries$steps_per_day))
interval<-dataSeries$interval[position]


##Imputing missing values
NANumber<-nrow(activity[complete.cases(activity)==FALSE,])


#imputation
#compute mean for each interval of time and save all in a new dataframe named dtemp
#inner_join between activity and dtemp
#create a new column new_steps
#remove the columns steps and val_new from data.frame
#rename the column new_steps
#save all in dataframe activity_imp

dtemp <- activity %>% 
  group_by(interval) %>%
  summarize(val_new=mean(steps, na.rm=TRUE)) 

#suppressMessages supress the message printed by the execution of inner_join function
suppressMessages(  
  {activity_imp <-activity %>%
    inner_join(dtemp) %>%
    mutate(new_steps=ifelse(is.na(steps), round(val_new),steps))%>%
    select(-c(steps, val_new)) %>%
    rename(steps=new_steps)}
)


##new histogram
#calculates the total number of steps taken per day using 
df_imp <- activity_imp %>%
  group_by(date) %>%
  summarize(steps_per_day = sum(steps))

ggplot(data=df_imp, aes(steps_per_day)) + 
  geom_histogram(binwidth=2000, col="black", fill="black", alpha = .1) + 
  labs(title="Steps per day") +labs(x="Steps", y="Frequency") +
  theme_bw()+
  theme(axis.title.y=element_text(vjust=1, size=14), 
        axis.title.x=element_text(vjust=.3, size=14),
        axis.text=element_text(size=14),
        title=element_text(vjust=1.2, size=14)) 


#Calculate and report the mean and median total number of steps each day.
meanValInp <- mean(df_imp$steps_per_day, na.rm=TRUE)
medianValInp <- median(df_imp$steps_per_day, na.rm=TRUE)
meanRImp<-format(round(meanValInp), big.mark = ",", scientific = FALSE)
medianRImp<-format(round(medianValInp), big.mark = ",", scientific = FALSE)

#compute maximum on the column steps_per_day for both dataframe (df si df_imp)
m1<-max(df$steps_per_day,na.rm=TRUE) 
m2<-max(df_imp$steps_per_day)
m<-format(round(m1), big.mark = ",", scientific = FALSE)
m1==m2

#for each interval [v, v*1000+1] (where v is seq(1,22000,1000)) finds how many days have
#number total of steps in that interval
func1<- function(x) {
  nrow(df %>% filter(steps_per_day<1000+x & steps_per_day>=x))
}

func2<- function(x) {
  nrow(df_imp %>% filter(steps_per_day<1000+x & steps_per_day>=x))
}
v1<-sapply(v<-seq(1,22000,1000), func1)
v2<-sapply(v<-seq(1,22000,1000), func2)

v2-v1
index<-which(v2-v1>0)
i<-format(index,  big.mark = ",", scientific = FALSE)

i1<-format((index-1)*1000+1, big.mark = ",", scientific = FALSE)
i2<-format(index*1000+1, big.mark = ",", scientific = FALSE)

N1<-nrow(df %>% filter(steps_per_day<index*1000+1 & steps_per_day>=(index-1)*1000+1))
N2<-nrow(df_imp %>% filter(steps_per_day<index*1000+1 & steps_per_day>=(index-1)*1000+1))
N2-N1
  
 
#Are there differences in activity patterns between weekdays and weekends?  
activity_f<- activity_imp %>% 
  mutate(day=weekdays(date)) %>%
  mutate(temp1=weekdays(date) %in% c("Sunday","Saturday")) %>%
  mutate(temp2=ifelse(temp1==TRUE, "weekend", "weekday")) %>%
  mutate(WDay=factor(temp2, levels=c("weekend", "weekday"))) %>%
  select(-c(temp1, temp2))

       
                               
dataSeries <- activity_f %>% 
  group_by(interval, WDay) %>%
  summarise(meanDay=mean(steps))  

xyplot(meanDay ~ interval | WDay, dataSeries, 
       type="l", 
       lwd=1, 
       main="Average steps by time interval" ,
       xlab="Time interval", 
       ylab="Average number of steps", 
       layout=c(1,2))



              
   
