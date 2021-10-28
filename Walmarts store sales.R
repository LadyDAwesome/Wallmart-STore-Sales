library(dplyr)
library(ggplot2)
library(scales)
df <- read.csv("Walmart_Store_sales.csv")
View(df)

df1 <- df

is.null(df1)



#Which store has maximum sales

df[which.max(df$Weekly_Sales), ]$Store

#Which store has maximum standard deviation i.e., the sales vary a lot. 
df_grouped = group_by(df1, Store)
df_group_sum = summarise(df_grouped, mean = mean(Weekly_Sales), sd = sd(Weekly_Sales))
df_group_sum[which.max(df_group_sum$sd), ]
View(df_group_sum)
df_group_sum[which.max(df_group_sum$sd), ]$Store

#Also, find out the coefficient of mean to standard deviation
df_group_sum <- mutate(df_group_sum, Covariance = mean / sd)
df_group_sum

#Which store/s has good quarterly growth rate in Q3’2012

df.2012 <- select(df1, Store, Date, Weekly_Sales)
head(df.2012)


df.2012<- mutate(df.2012, Date_of=as.Date(Date,format="%d-%m-%Y"))
head(df.2012)
str(df.2012)
View(df.2012)

df.2012 <- mutate(df.2012, Quarter_Sale = quarters(Date_of))
head(df.2012)
str(df.2012)


df.2012<- df.2012 %>% filter( df.2012$Date_of > as.Date("01-01-2012",format="%d-%m-%Y"), df.2012$Date_of<as.Date("01-02-2013",format="%d-%m-%Y"))
df.2012.q2<- df.2012 %>% filter(Quarter_Sale == "Q2")
df.2012.q3<- df.2012 %>% filter(Quarter_Sale == "Q3")
df.2012.q2 <- summarize(group_by(df.2012.q2, Store), Q2_sales = sum(Weekly_Sales))
head(df.2012.q2)
df.2012.q3 <- summarize(group_by(df.2012.q3, Store), Q3_sales = sum(Weekly_Sales))
head(df.2012.q3)
df_group_sum <- mutate(df_group_sum,  Q2_Sales = df.2012.q2$Q2_sales )
df_group_sum <- mutate(df_group_sum,  Q3_Sales = df.2012.q3$Q3_sales )

df_group_sum <- mutate(df_group_sum,  Q2_Q3_Growth = ((Q3_Sales - Q2_Sales )/Q2_Sales)*100)

df_group_sum[which.max(df_group_sum$Q2_Q3_Growth), ]
df_group_sum[which.max(df_group_sum$Q2_Q3_Growth), ]$Store

#Some holidays have a negative impact on sales. 
#Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together
df_non_holidays <- df1 %>% filter(df1$Holiday_Flag == 1 )

non_holiday_avg_sales <- mean(df_non_holidays$Weekly_Sales)
df_holidays_profitable <- df1 %>% filter(df1$Holiday_Flag == 1 )

df_holidays_profitable <- df_holidays_profitable %>% filter(df_holidays_profitable$Weekly_Sales > non_holiday_avg_sales)
View(df_holidays_profitable) 


#Provide a monthly and semester view of sales in units and give insights

df1<- mutate(df1, Date_of=as.Date(Date,format="%d-%m-%Y"))
View(df1)
str(df1)

df1 <- mutate(df1,
                 year = as.numeric(format(df1$Date_of, format = "%Y")),
                 month = as.numeric(format(df1$Date_of, format = "%m")),
                 day = as.numeric(format(df1$Date_of, format = "%d")))

df_monthly_sales <- df1 %>% group_by(year, month)%>% summarise(Sales = sum(Weekly_Sales))

#monthly sales
ggplot(df_monthly_sales, aes(x = month, y = Sales/1000000, colour = as.factor(year), group = year)) + scale_colour_manual(values=c("green","red","blue"))+
  geom_line()  +scale_x_continuous(breaks = pretty_breaks())+scale_y_continuous(breaks = pretty_breaks()) 

#Quarterly  and semester sales
df1 <- mutate(df1, Quarter_Sale = quarters(Date_of))
df_semester_sales <- df1 %>% group_by(year, Quarter_Sale)%>% summarise(Sales = sum(Weekly_Sales))
View(df_semester_sales)

ggplot(df_semester_sales, aes(x = year, y = Sales/1000000, colour = as.factor(year), group = Quarter_Sale)) + scale_colour_manual(values=c("green","red","blue"))+
  geom_bar(stat="identity")  +scale_x_continuous(breaks = pretty_breaks())+scale_y_continuous(breaks = pretty_breaks()) 

#Linear Regression – Utilize variables like date and restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order). 

#Hypothesize if CPI, unemployment, and fuel price have any impact on sales.

#CPI and Sales

df %>% ggplot(aes(x = CPI, y = Weekly_Sales)) + geom_point()  + geom_line(aes(y= fitted(lm(Weekly_Sales~CPI, data = df))), colour = "red")

#Uneployment and Sales
df %>% ggplot(aes(x = Unemployment, y = Weekly_Sales)) + geom_point()  + geom_line(aes(y= fitted(lm(Weekly_Sales~Unemployment, data = df))), colour = "red")

#temperature and sales
df %>% ggplot(aes(x = Temperature, y = Weekly_Sales)) + geom_point()  + geom_line(aes(y= fitted(lm(Weekly_Sales~Temperature, data = df))), colour = "red")
