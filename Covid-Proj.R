####
# LIBRARIES
library(dplyr)
library(psych)
library(ggplot2)
library(lubridate)
library(cowplot)
library(caTools)

####
# DATASETS
dataset1=read.csv("states.csv")
dataset2=read.csv("state_wise.csv")
dataset3<-read.csv("COVID.csv")
dataset4<-read.csv("StatewiseCovid.csv")
dataset5<-read.csv("COVID19_new_data.csv")

####
# PREPROCESSING DATASET

# GETTING REQUIRED COLUMNS 
dataset1=dataset1[1:5]
dataset2=dataset2[c(-1,-37,-38),1:5]
dataset3=dataset3[c(-9,-8,-12)]
dataset4=dataset4[-4]
dataset5=dataset5[c(-6,-7,-10,-11,-12)]

# EDITING THE DATASET 
names(dataset3)=c("Date","Day","Month","Year","Cases","Deaths","Country","Population","Continent")
dataset4$Date=ymd(dataset4$Date)
dataset3$Date=dmy(dataset3$Date)
dataset1$Date=ymd(dataset1$Date)
dataset2$State[34]="Daman & Diu" # DAMAN AND DIU AND DADRA AND NAGAR HAVELI
dataset2[32,1]="Andaman&Nicobar"
dataset5$age=as.integer(dataset5$age)
dataset5$sex=gsub("Female","female",dataset5$sex)
dataset5$sex=gsub("Male","male",dataset5$sex)
dataset5$sex=gsub("N/A","",dataset5$sex)

# NA VALUES
colSums(is.na(dataset3))
dataset3=na.omit(dataset3)
colSums(is.na(dataset2))
colSums(is.na(dataset1))
colSums(is.na(dataset4))
dataset4=dataset4%>% mutate(Positive= ifelse(is.na(Positive),median(Positive,na.rm = T) , Positive))
colSums(is.na(dataset5))
dataset5=dataset5%>% mutate(age= ifelse(is.na(age),mean(age,na.rm = T), age))
dataset5=na.omit(dataset5)

####
# GENERAL ANALYSIS

# PART OF DATASET
head(dataset1)
head(dataset2)
head(dataset3)
head(dataset4)

# STRUCTURE OF DATASET
str(dataset1)
str(dataset2)
str(dataset3)
str(dataset4)
summary(dataset3)
describe(dataset3)

# ADDITIONAL
sum(dataset3$Cases)
table(dataset5$outcome)#1=discharge  0=death


####
# SUBSETTING 

# COUNTRY WISE CASES
Country_Cases=dataset3 %>% group_by(Country,Population,Continent) %>% summarise(Total_cases = sum(Cases),Total_deaths=sum(Deaths)) %>% arrange(desc(Population))
Country_Cases$Country <- factor(Country_Cases$Country, levels = Country_Cases$Country[order(Country_Cases$Population,decreasing = T)])
head(Country_Cases)

# STATE WISE TESTING 
State_Cases=dataset4 %>% group_by(State) %>% summarise(Total_Tests = sum(TotalSamples),Total_Positives=sum(Positive))
head(State_Cases)

# DATEWISE CASES
Datewise_Cases=dataset4 %>% group_by(Date) %>% summarise(Total_Tests = sum(TotalSamples),Total_Positives=sum(Positive))
head(Datewise_Cases)

# INDIA CASES
India_cases=dataset3[dataset3$Country=="India",]
India_cases=India_cases[-300,]
India_cases$quarter=quarter(India_cases$Month)
head(India_cases)

# CONTINENT WISE CASES
Continent_cases=dataset3 %>% group_by(Date,Continent) %>% summarise(Total_cases = sum(Cases),Total_deaths=sum(Deaths))
head(Continent_cases)

####
# VISUALISATION

# COUNTRY WISE CASES & DEATHS
plot1=ggplot(data = Country_Cases[1:20,])

plot1+geom_point(aes(y=Country,x=Total_cases,col='Total_cases'),alpha=0.7)+geom_point(aes(y=Country,x=Total_deaths,col="Total_death"),alpha=0.5)+geom_vline(xintercept = 5000000,lty=3)+labs(x="Total Cases & Total Deaths",title = "Total Cases of each Country")+scale_colour_manual("Legend",values = c("orange","blue")) +theme_bw(base_family = "Times")

# INDIA CASES & DEATHS DATEWISE
plot2=ggplot(data=India_cases)

p2_1=plot2+geom_col(aes(x=Date,y=Cases),fill ='orange',alpha=.4)+labs(title = "Daily Cases Reported for India",y="Daily cases Reported")+theme_bw()

p2_2=plot2+geom_col(aes(x=Date,y=Deaths),fill ='blue',alpha=.5)+labs(title = "Daily Deaths Reported for India",y="Daily deaths Reported")+theme_bw()

plot_grid(p2_1,p2_2)

# ACTIVE AND RECOVERED FOR VARIOUS STATES
plot3=ggplot(data = dataset2[1:15,])

p3_1=plot3+geom_col(aes(y=Active,x=State),fill="blue",alpha=0.3)+labs(title="Active",x="State")+geom_hline(yintercept = median(dataset2$Active),lty=2)+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

p3_2=plot3+geom_col(aes(y=Recovered,x=State),fill="orange",alpha=0.3)+labs(title="Recovered",x="State")+geom_hline(yintercept = median(dataset2$Recovered),lty=2)+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_grid(p3_1,p3_2)

# DATEWISE TESTING AND POSITIVE CASES
plot4=ggplot(data = Datewise_Cases)

p4_1=plot4+geom_col(aes(x=Date,y=Total_Tests),fill ='red',alpha=.4)+labs(title = "Daily Testing",y="Testing")+theme_bw()

p4_2=plot4+geom_col(aes(x=Date,y=Total_Positives),fill ='green',alpha=.4)+labs(title = "Daily Positive",y="Positives")+theme_bw()

plot_grid(p4_1,p4_2)

#CONTINENT WISE PLOTTING
plot5=ggplot(data = Continent_cases)

plot5+geom_line(aes(x=Date,y=Total_cases,color="Cases"),alpha=0.5,size=1.2) + geom_line(aes(x=Date,y=Total_deaths,color="Death"),alpha=0.5,size=1.2)+theme_bw() +labs(y="Cases & Death",x="Date",title="Daily Trend (Continent-Wise)")+scale_color_manual("Legend",values = c("green","red","orange"))+facet_grid(Continent~.)



####
# Regression Analysis

# 1
set.seed(123)
split=sample.split(Datewise_Cases$Total_Tests,SplitRatio = 0.8)
head(split)

# TRAINING AND TESTING DATA
Training_set= subset(Datewise_Cases,split == TRUE)
Test_set= subset(Datewise_Cases,split == FALSE)

# FEATURE SCALING
Training_set[,2:3]=scale(Training_set[,2:3])
Test_set[,2:3]=scale(Test_set[,2:3])

# MODEL
r1=lm(Total_Positives~.,data = Training_set)
summary(r1)
p1=predict(r1,newdata = Test_set)

# MODEL VISUALIZATION
ggplot()+geom_line(aes(x=Test_set$Date,y=Test_set$Total_Positives,col="Actual_value"))+geom_line(aes(x=Test_set$Date,y=p1,col='Predict_value'))+theme_bw()+labs(x="Date",y="Positive Cases",title = "TESTING SET")+scale_color_manual("Legend",values = c("green","blue"))

# 2

set.seed(234)
split1=sample.split(dataset5$outcome,SplitRatio = 0.75)
head(split1)

# TRAINING AND TESTING DATA
Training_set1= subset(dataset5,split == TRUE)
Test_set1= subset(dataset5,split == FALSE)

# MODEL
r2=lm(outcome~sex+age,data = Training_set1)  
summary(r2)
p2=predict(r2,newdata = Test_set1)

# MODEL VISUALIZATION
r2_1=ggplot()+geom_histogram(aes(x=Test_set1$outcome),fill='green',alpha =0.4,binwidth = 0.2)+theme_bw()+labs(x="Outcome",y="Count",title = "TESTING SET")

r2_2=ggplot()+geom_histogram(aes(x=p2),fill="orange",alpha=0.5,binwidth = 0.1)+theme_bw()+labs(x="Outcome",y="Count",title = "Prediction")

plot_grid(r2_1,r2_2)
