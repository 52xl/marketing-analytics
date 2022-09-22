setwd('C:/Users/landi/OneDrive - Politecnico di Milano/marketing anal/project marketing analytics')
#setwd('C:/Users/lululu/Documents/MA_course_assignment')
library( readxl)#to read excell
library(readr)#to read csv
library(dplyr)#
library(explore) #to explore data
library(lubridate) #to trasform the date
library(ggplot2)#to draw the chart
library(tidyr)
library(plyr)


#importing dataset
product <- read_excel("products_DB.xlsx")
dataset <- read.csv("dataset_marketing_analytics.csv")

#trasformig the data as integer as date 
dataset$ticketdate=ymd_hms(dataset$ticketdate)
dataset$date<- as.Date(dataset$ticketdate) # already got this one from the answers above
dataset$time<- format(as.POSIXct(dataset$ticketdate), format = "%H:%M:%S") 
dataset$ticketdate<-NULL

#two rows have quantity equal to zero, it hase been supposed that it is a transcription error
#so this quantity are set equal to zero
dataset$qty[which(dataset$qty ==0)]<-1
#same records have negative value on the price, they are return product
dataset[which(dataset$totprice <0),]

#aggregating data for RFM
dfrfm<-aggregate(dataset$totprice, by=list(dataset$id_cust,dataset$ticket_id,dataset$date), FUN=sum)
colnames(dfrfm)<-c('id_cust','id_ticket','date','totprice')
dfrfm$id_ticket<-NULL


######################################GENERAL ANALYSIS#########################################
####FOCUS ON STORES
#1 Total sales per store
totsales_store=aggregate(dataset$totprice,by=list(store=dataset$store),sum)
colnames(totsales_store)<-c("store","totsales")
#show the barplot of total sales per store
x11()
ggplot(totsales_store,aes(factor(store),totsales))+geom_bar(stat="identity")+labs(title = "Total sales per store",y='Total sales',x='Store')+ theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)

#Total transaction tims per store
dataset2<-dataset[!duplicated(dataset[,c("store","ticket_id","date")]),]#Removing duplicate transaction counts

tottrs_store=aggregate(dataset2$ticket_id,by=list(store=dataset2$store),length)
colnames(tottrs_store)<-c("store","tottrs")
#View(tottrs_store)
#show the barplot of total number of transactions per store
x11()
ggplot(tottrs_store,aes(factor(store),tottrs))+geom_bar(stat="identity")+labs(title = "Total transaction times per store",y='Total transaction', x='Store')+ theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)

#3 Average sales per ticket per store
avesales_store<-merge(totsales_store,tottrs_store,by="store")
avesales_store$avesales<-avesales_store$totsales/avesales_store$tottrs

#show the barplot of average sales per ticket per store
x11()
ggplot(avesales_store,aes(factor(store),avesales))+geom_bar(stat="identity")+labs(title = "Average sales per ticket per store",y='Ticket cost', x='Store')+ theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)

#4 Time trend of the total transaction times
#time trend in week
tottrs_date=aggregate(dataset2$ticket_id,by=list(store=dataset2$store,date=dataset2$date),length)
colnames(tottrs_date)<-c("store","date","tottrs")

tottrs_date$week<- format(as.POSIXct(tottrs_date$date), format = "%w") 

#cross-check the open time in week of each store
check_week<-tottrs_date[!duplicated(tottrs_date[,c("store","week")]),]

week_store <- ddply(check_week,c("store"),
                    function(check_week)paste(check_week$`week`,
                                              collapse = ","))
colnames(week_store)<-c("store","week")

#calculate the total transactions in a week
tottrs_week=aggregate(tottrs_date$tottrs,by=list(week=(tottrs_date$week)),sum)
colnames(tottrs_week)<-c("week","tottrs")

#calculate the average transactions in a week
d<-c(1,8,8,8,8,8,8)
day<-tottrs_week$week
avetrs<-tottrs_week$tottrs/(d)
avetrs_week<-data.frame(day,avetrs)

#show the lineplot of time trend of the total transaction times
x11()
ggplot(avetrs_week,aes(day,avetrs,group=1))+geom_line(stat="identity",color="black", size=2)+labs(title = "Time trend of the average transaction per day",y='Transaction', x='Day')+ theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)


#time trend in day
tottrs_time=aggregate(dataset2$ticket_id,by=list(store=dataset2$store,date=dataset2$time),length)
colnames(tottrs_time)<-c("store","time","tottrs")
tottrs_time$hour<-lapply(tottrs_time$time,FUN=function(x) hour(hms(x)))#Expressing time in hours
tottrs_time$hour<-as.character(tottrs_time$hour)

#cross-check the open time in day of each store
check_day<-tottrs_time[!duplicated(tottrs_time[,c("store","hour")]),]

day_store <- ddply(check_day,c("store"),
                   function(check_day)paste(check_day$`hour`,
                                            collapse = ","))
colnames(day_store)<-c("store","hour")

#calculate the total transactions in a day
tottrs_hour=aggregate(tottrs_time$tottrs,by=list(hour=(tottrs_time$hour)),sum)

#calculate the average transactions in a day
d<-c(8,8,8,6,2,7,8,8,8,8,3,4,8,8)
hour<-tottrs_hour$hour
avetrs<-tottrs_hour$x/(d)
avetrs_day<-data.frame(hour,avetrs)

#show the lineplot of time trend of the total transaction times
x11()
ggplot(avetrs_day,aes(factor(hour,levels=c('7','8','9','10','11','12','13','14','15','16','17','18','19','20')),avetrs,group=1))+geom_line(stat="identity",color="black", size=2)+labs(title = "time trend of the average transaction per hour",y='Transaction', x='hour')+ theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)



####FOCUS ON CUSTOMERS
#purchase interval
dataset$interval<-time_length(interval(dataset$date,"2021-08-01"),'day')#1th August is the day after the last transaction

customers_interval=aggregate(dataset$interval,by=list(customer=dataset$id_cust),min)

colnames(customers_interval)<-c("customer","interval")
customers_itvcount=aggregate(customers_interval$customer,by=list(interval=customers_interval$interval),length)
colnames(customers_itvcount)<-c("interval","count")

#count1=tapply(customers_itvcount$count,customers_itvcount$interval<="30",sum)
breaks1=c(0,31,60,92)
#count_group<-cut(customers_itvcount$interval,breaks)
count_group1=aggregate(customers_itvcount$count,by=list(count_group=cut(customers_itvcount$interval,breaks1)),sum)

#show the pieplot of distribution of customers' last purchase interval
# Basic piechart

piepercent<- round(100*count_group1$x/sum(count_group1$x), 1)
x11()
pie(count_group1$x, labels = piepercent, main = "Last purchase",col = rainbow(length(count_group1$x)))
legend("topright", c("Last purchase in last 30 days", "Last purchase between 30 days and 60 ago", "Last purchase between 30 days and 60"), cex = 0.8,
         fill = rainbow(length(count_group1$x)))
  
  


#purchase times
customers_times=aggregate(dataset2$ticket_id,by=list(customer=dataset2$id_cust),length)
#View(customers_times)
colnames(customers_times)<-c("customer","times")
breaks=c(0,5,10,20,50,90,Inf)
count_group2=aggregate(customers_times$times,by=list(count_group=cut(customers_times$times,breaks)),length)
#View(count_group2)
#show the pieplot of distribution of customers' purchase times
x11()
percent2<-round(count_group2$x/sum(count_group2$x)*100,1)
label2<-paste(count_group2$count_group," ",percent2,"%",sep="")
pie(count_group2$x, labels=label2, main='distribution of purchase times')


piepercent<- round(100*count_group1$x/sum(count_group1$x), 1)
x11()
pie(count_group1$x, labels = percent2, main = "Last purchase",col = rainbow(length(count_group1$x)))
legend("topright", c("Last purchase in last 30 days", "Last purchase between 30 days and 60 ago", "Last purchase between 30 days and 60"), cex = 0.8,
       fill = rainbow(length(count_group1$x)))



#purchase amount
customers_amount=aggregate(dataset$totprice,by=list(customer=dataset$id_cust),sum)

colnames(customers_amount)<-c("customer","amount")
breaks3=c(0,10,100,1000,5000,10000,20000,Inf)
count_group3=aggregate(customers_amount$amount,by=list(count_group=cut(customers_amount$amount,breaks3)),length)

#show the pieplot of distribution of customers' purchase amount
x11()
percent3<-round(count_group3$x/sum(count_group3$x)*100,1)
label3<-paste(count_group3$count_group," ",percent3,"%",sep="")
pie(count_group3$x, labels=label3, main='distribution of purchase amount')





#RFM
#################################################################################

#RFM
library(rfm)
library(devtools)
library(descriptr)
library(kableExtra)
library(tidyverse)

#two RFM are perfomed, the first one is used to detect outliers between clients
#the second is performed to segment clients.


analysis_date <- lubridate::as_date(max(dfrfm$date)+1)#data from which the RFM analysis is computed
#we consider the day after the last purchase
timediff=max(dfrfm$date)-min(dfrfm$date)
timediff #91 days of difference

#first RFM
rfm_result <- rfm_table_order(dfrfm,id_cust , date, totprice, analysis_date)
rfm_result

#who are the cashier who the real customers? it has been decided that a client can go to
#the supermarket not more then one time per day.
client<-rfm_result$rfm$customer_id[which(rfm_result$rfm$transaction_count<90)]
cashier<-rfm_result$rfm$customer_id[which(rfm_result$rfm$transaction_count>=90)]


#creating the dataset that contains only real customers
final_rfm<-dfrfm[which(dfrfm$id_cust %in% client),]
#churner<-rfm_result$rfm$customer_id[which(rfm_result$rfm$recency_days>=50)]

#final_rfm<-final_rfm[which(final_rfm$id_cust %!in% churner),]
sum(final_rfm$totprice)/sum(dfrfm$totprice)
#the monetary value of the customer is 2% on the total, there is a huge disporoportion of data

# performing RFM analysis 
rfm_<-rfm_table_order(final_rfm,id_cust , date, totprice, analysis_date)

#View(rfm_$rfm)
rfm_
rfm_$threshold
x11()
rfm_heatmap(rfm_)
x11()
rfm_bar_chart(rfm_)
x11()
rfm_histograms(rfm_)
x11()
rfm_order_dist(rfm_)

#scatterplot between 2 variables
#recency vs monetary
x11()
rfm_rm_plot(rfm_)
#frequency vs monetary
x11()
rfm_fm_plot(rfm_)
#recency vs frequency
x11()
rfm_rf_plot(rfm_)


segment_titles<- c("First Grade", "Loyal", "Likely to be Loyal",
                   "New Ones", "Could be Promising", "Require Assistance", "Getting Less Frequent",
                   "Almost Out", "Can't Lose Them", "Don't Show Up at All")
segment_titles1<-c('best', 'middle','almost out')

r_low <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
r_high <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
f_low <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
f_high <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
m_low <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
m_high  <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

divisions<-rfm_segment(rfm_, segment_titles, r_low, r_high, f_low, f_high, m_low, m_high)

library(dplyr) # required for grouping

divisions %>% count(segment) %>% arrange(desc(n)) %>% rename(Segment = segment, Count = n)
x11()
rfm_plot_median_recency(divisions)
x11()
rfm_plot_median_frequency(divisions)
x11()
rfm_plot_median_monetary(divisions)

agg_churner_store$store<-as.factor(agg_churner_store$store)

library(ggplot2)
# hist of segments
x11()
ggplot(data.frame(divisions$segment), aes(x=divisions$segment)) + geom_bar()+labs(title = "Number of customers per each segment",y='Number of customers',x='Segments')+ theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)

 

table(divisions$segment)
48+46/length(divisions$segment)#is the percentage of best customers


name_best<-c("First Grade","Loyal")
id_best<-divisions$id_cust[which(divisions$segment %in% name_best)]
divisions_loyal<-divisions[which(divisions$id_cust %in% id_best),]


sum(divisions_loyal$amount)/sum(divisions$amount)# percentage of amount of
#money the best customer spent
#who are the the chassier

#best customers behaviours
##################################################################################
####best customers' behaviors (except the preferences for purchasing goods, which will be analyze by MBA)
library(plyr)
#cosidering only the first grade customer and the loyal once from the rfm
name_best<-c("First Grade","Loyal")
id_best<-divisions$id_cust[which(divisions$segment %in% name_best)]

#import transaction data of best customers
trs_best<-dataset[which(dataset$id_cust %in% id_best),]
colnames(divisions)[1]<-c("id_cust")

trs_best<-inner_join(trs_best,divisions,"id_cust",all=T)

###When analyzing all the best customers, we consider his transactions in all stores.
#For example, a customer finished shopping at store 517 10 days ago and has never been to store 517 since, but he made a purchase at store 524 yesterday, then we would consider her recent purchase interval to be 1
##average R,F,M
trs_best_id<-trs_best[!duplicated(trs_best[,c("id_cust")]),]#Removing duplicate transaction counts

ave_r<-mean(as.integer(trs_best_id$recency_days))
ave_f<-mean(as.integer(trs_best_id$transaction_count))
ave_m<-mean(as.integer(trs_best_id$amount))
ave_rfm_best<-data.frame(as.integer(ave_r,ave_f,ave_m))


##Distribution of single purchase amount
ticket_amount_best=aggregate(trs_best$totprice,by=list(store=trs_best$store,ticket=trs_best$ticket_id),sum)

breaks=c(0,10,50,100,200,250)
ticket_amount_group<-aggregate(ticket_amount_best$x,by=list(group=cut(ticket_amount_best$x,breaks)),length)

x11()
percent_ta_best<-round(ticket_amount_group$x/sum(ticket_amount_group$x)*100,1)
label2<-paste(ticket_amount_group$group," ",percent_ta_best,"%",sep="")
pie(ticket_amount_group$x, labels=label2, main='Cost per purchase',col=c('blue','green','yellow','red','white'))
legend("bottomleft", c("Between 0 and 10 euros", "Between 10 and 50 euros", "Between 50 and 100 euros","Between 100 and 200 euros","more then 200 euros"), cex = 0.8,
       fill = rainbow(length(count_group1$x)))



##Most frequent shopping time
trs_best_id$hour<-lapply(trs_best_id$time,FUN=function(x) hour(hms(x)))#Expressing time in hours
trs_best_id$hour<-as.character(trs_best_id$hour)

#count the shopping number of each hour
trs_best_hour=aggregate(trs_best_id$ticket_id,by=list(hour=(trs_best_id$hour)),length)
#View(trs_best_hour)
#show the barplot of the shopping number of each hour
x11()
ggplot(trs_best_hour,aes(factor(hour,levels=c('7','8','9','10','11','12','13','14','15','16','17','18','19','20')),x))+geom_bar(stat="identity")+labs(title = "tansaction times per hour")

##Most frequent shopping interval
#count the shopping interval
trs_best_interval=aggregate(trs_best_id$ticket_id,by=list(interval=(trs_best_id$recency_days)),length)

#show the barplot of the shopping number of each hour
x11()
ggplot(trs_best_interval,aes(interval,x))+geom_bar(stat="identity")+labs(title = "Number of transaction per hour",y='Transaction',x='hour') + theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)


##Most frequent shopping store
trs_best_tr<-trs_best[!duplicated(trs_best[,c("store","ticket_id")]),]#Removing duplicate transaction counts

trs_best_store=aggregate(trs_best_tr$ticket_id,by=list(store=(trs_best_tr$store)),length)

#show the barplot of the shopping number of each hour
x11()
ggplot(trs_best_store,aes(factor(store),x))+geom_bar(stat="identity")+labs(title = "Number of transaction per hour",y='Transaction',x='hour') + theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)





#who are the the chassier
##################################################################################
#who are the the chassier
#dataset of the chassier 
df_cashier<-dataset[which(dataset$id_cust %in% cashier),]
monetary_ticket_cashier<-aggregate(df_cashier$totprice, by=list(df_cashier$ticket_id,df_cashier$id_cust,df_cashier$store,df_cashier$date),FUN=sum)
colnames(monetary_ticket_cashier)<-c("ticket_id", "id_cust","store","date",'price' )
#average and standard deviations of the monetary value of ticket of cashier
mean(monetary_ticket_cashier$price)
sd(monetary_ticket_cashier$price)

#dataset of the clients
df_no_chassier<-dataset[which(dataset$id_cust %in% client),]
monetary_ticket_no_casshier<-aggregate(df_no_chassier$totprice, by=list(df_no_chassier$ticket_id,df_no_chassier$id_cust,df_no_chassier$store,df_no_chassier$date),FUN=sum)
colnames(monetary_ticket_no_casshier)<-c("ticket_id", "id_cust","store","date",'price' )
#average and standard deviations of the monetary value of ticket of cashier
mean(monetary_ticket_no_casshier$price)
sd(monetary_ticket_no_casshier$price)

#the two population are not normal so it is not possible to compere variance and mean
shapiro.test((monetary_ticket_no_casshier$price))
shapiro.test(monetary_ticket_casshier$price)

#plot of the distribution of the monetary value of the tickets of the cashiers and of the real client
d<-density(monetary_ticket_no_casshier$price)
d1<-density(monetary_ticket_cashier$price)
x11()
plot(d1, col='red',lwd=4)
lines(d , col='blue',lwd=4)
legend(150, 0.04, legend=c("no chassiers", "chassiers"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

#the graph shows clearly that the distributions spending of the cashiers and
# the distribuonos of the spending of the clients is similar
set.seed(1)
testmean<-data.frame(sample(monetary_ticket_no_casshier$price,500), sample(monetary_ticket_cashier$price,500))
colnames(testmean)<-c('cashiers','clients')
#wilcox.test() 

x11()
boxplot(testmean,main='Boxplot of the average purchase per ticket for cashiers and for clients', ylab='Price')
###########################################################################################################

#merging the dataset
###############################################################################
#merging the dataset
colnames(product)<-c("prod_id",'Description')
total <- merge(dataset,product,by=c("prod_id"), )
##################################################################################

#market basket analysis on the whole dataset


#MBA fidelity customers
##################################################################################

name_best<-c("First Grade","Loyal")
id_best<-divisions$id_cust[which(divisions$segment %in% name_best)]
id_best
#identify the store the trasaction of the best customer
fidel_store<-total[which(total$id_cust %in% id_best),]


library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionDatafidel <- ddply(fidel_store,c("ticket_id" ,"date"),
                              function(df1)paste(df1$Description,
                                                 collapse = ","))
#The R function paste() concatenates vectors to character and separated results using collapse=[any optional charcater string ]. Here ',' is used
#set column InvoiceNo of dataframe transactionData  
transactionDatafidel $ticket_id <- NULL
#set column Date of dataframe transactionData
transactionDatafidel $date <- NULL
#Rename column to items
colnames(transactionDatafidel ) <- c("items")
#Show Dataframe transactionData
transactionDatafidel 

write.csv(transactionDatafidel,"market_basket_transactions_fidel_store.csv", quote = FALSE, row.names = FALSE)
tr_fidel <- read.transactions('market_basket_transactions_fidel_store.csv', format = 'basket', sep=',')


summary(tr_fidel)
inspect(head(tr_fidel))

itemFrequency(tr_fidel)

library(RColorBrewer)

x11()
itemFrequencyPlot(tr_fidel,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")
x11()
itemFrequencyPlot(tr_fidel,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
#return the relative frequency of a name  from the list itemfrequecy
itemFrequency(tr_fidel)[grep('Cookies',itemLabels(tr_fidel))]


rules_fidel <- apriori(tr_fidel, parameter = list(supp=0.02, conf=0.5 ,maxlen=4))
#removing all the rule that are subset of others rules
subset.rules <- which(colSums(is.subset(rules_fidel, rules_fidel)) > 1)

subset_rules_fidel<- rules_fidel[-subset.rules] # remove subset rules.
subset_rules_fidel
inspect(sort(rules_fidel, by='lift', decreasing=TRUE)[1:10])
#View(quality(rules_fidel))
rules_fidel


#MBA  fidelity customers graphs
#################################################################################
#MBA graph
x11()
plot(subset_rules_fidel)

inspect(head(subset_rules_fidel, n = 3, by = "confidence"))

x11()
plot(subset_rules_fidel,method="two-key plot")


top10subRules <- head(rules_fidel, n = 10, by = "lift")
set.seed(1)
plot(top10subRules, method = "graph",  engine = "htmlwidget")
#saveAsGraph(head(top10subRules , n = 1000, by = "lift"), file = "rules.graphml")



#x11()
#plot(rules_fidel, measure = c("support", "lift"), shading = "confidence",interactive=TRUE)

x11()
plot(rules_fidel, method = "two-key plot", pch=20, cex=10)

#x11()#very usefull plot and interactive plot
#sel <- plot(rules_fidel, measure=c("support", "lift"), shading = "confidence", interactive = TRUE)


x11()
plot(rules_fidel, method = "matrix", measure = "lift")

x11()
plot(rules_fidel, method = "matrix", engine = "3d", measure = "lift")
x11()
plot(rules_fidel, method = "grouped")#k default is 20

x11()
plot(rules_fidel, method = "grouped", control = list(k = 50))

#x11()
#sel <- plot(rules_fidel, method = "grouped", interactive = TRUE)


x11()
plot(rules_fidel, method = "graph")

x11()
plot(rules_fidel, method = 'grouped matrix')
#####################################################################################


#analysis of RFM
####################################################################################
#analysis of RFM
library(plyr)
library(dplyr)
name_best<-c("First Grade","Loyal")
id_best<-divisions$id_cust[which(divisions$segment %in% name_best)]
id_best
fidel_store<-dataset[which(dataset$id_cust %in% id_best),]

agg_fidel_store <- ddply(fidel_store,c("id_cust","store"),
                         function(df1)paste(df1$store,
                                            collapse = ","))


#trasforming as a factor 
agg_fidel_store $store<-as.factor(agg_fidel_store $store)

library(ggplot2)
# 624 is the store in which there are more good customer
x11()
ggplot(data.frame(agg_fidel_store $store), aes(x=agg_fidel_store $store)) +geom_bar()+labs(title = "Best customer per store",y='Number of customers',x='Store')+ theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)




#worst customers
churner<-rfm_result$rfm$customer_id[which(rfm_result$rfm$recency_days>=40)]#in order to run this line of code the rfm must be run before
worst_store<-dataset[which(dataset$id_cust %in% churner),]
agg_churner_store <- ddply(worst_store,c("id_cust","store"),
                           function(df1)paste(df1$store,
                                              collapse = ","))
#trasforming as a factor 
agg_churner_store$store<-as.factor(agg_churner_store$store)

library(ggplot2)
x11()
ggplot(data.frame(agg_churner_store$store), aes(x=agg_churner_store$store)) +
  geom_bar()



agg_dataset<- ddply(dataset[which(dataset$id_cust %in% client),],c("id_cust","store"),
                    function(df1)paste(df1$store,
                                       collapse = ","))
#trasforming as a factor 
agg_dataset$store<-as.factor(agg_dataset$store)

library(ggplot2)
# 624 is the store in which there are more good customer
x11()
ggplot(data.frame(agg_dataset$store), aes(x=agg_dataset$store)) +geom_bar()+labs(title = "Best customers that purchase in more then one store",y='NUmber of customer',x='Store')+ theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)




#this are the customers that appears in more then one store
duplicatedcust_<-agg_dataset$id_cust[duplicated(agg_dataset$id_cust)]

df_duplicated<-dataset[which(dataset$id_cust %in% duplicatedcust_),]


agg_duplicated <- ddply(df_duplicated,c("id_cust","store"),
                        function(df1)paste(df1$store,
                                           collapse = ","))
#trasforming as a factor 
agg_duplicated$store<-as.factor(agg_duplicated$store)

library(ggplot2)

x11()
ggplot(data.frame(agg_duplicated $store), aes(x=agg_duplicated $store)) +  geom_bar()+labs(title = "Best customers that purchase in more then one store",y='NUmber of customer',x='Store')+ theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)


duble_customers_df<-divisions[which(divisions$id_cust %in%duplicatedcust_),]
duble_customers_df$segment<-as.factor(duble_customers_df$segment)

library(ggplot2)
# 
x11()
ggplot(data.frame(duble_customers_df$segment), aes(duble_customers_df$segment)) + geom_bar()+labs(title = "Best customers that shop in more then one store",y='NUmber of customer',x='Segment')+ theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent'),
  legend.box.background = element_rect(fill='transparent')
)

#################################################################################




#not double customer
###################################################################################
#not double
'%!in%' <- function(x,y)!('%in%'(x,y))
clientsignle<-client[which(client %!in% duplicatedcust_)]# %!in% means that is not so all the clients that are not duplicates
length(clientsignle)
length(duplicatedcust_)

single_customers_df<-divisions[which(divisions$customer_id %in%clientsignle),]
single_customers_df$segment<-as.factor(single_customers_df$segment)

library(ggplot2)
# 
x11()
ggplot(data.frame(single_customers_df$segment), aes(single_customers_df$segment)) +
  geom_bar()


agg_single<-agg_dataset[which(agg_dataset$id_cust %in% clientsignle),]

#trasforming as a factor 
agg_single$store<-as.factor(agg_single$store)

library(ggplot2)

x11()
ggplot(data.frame(agg_single$store), aes(x=agg_single$store)) +
  geom_bar()





#further analysis on loyal
#####################################################################################
name_best<-c("First Grade","Loyal")
id_best<-divisions$id_cust[which(divisions$segment %in% name_best)]
id_best
#identify the store the trasaction of the best customer
fidel_store<-total[which(total$id_cust %in% id_best),]

avgtick_loyal<-aggregate(fidel_store$qty, by=list(fidel_store$ticket_id,fidel_store$id_cust,fidel_store$store,fidel_store$date),FUN=sum)
colnames(avgtick_loyal)<-c("ticket_id", "id_cust","store","date",'quantity' )

mean(avgtick_loyal$quantity)
median(avgtick_loyal$quantity)
avgtick_loyal$quantity<-as.factor(avgtick_loyal$quantity)

agg_tick_loyal<-aggregate(fidel_store$totprice, by=list(fidel_store$ticket_id,fidel_store$id_cust,fidel_store$store,fidel_store$date),FUN=sum)

colnames(agg_tick_loyal)<-c("ticket_id", "id_cust","store","date",'price' )
#average and standard deviations of the monetary value of ticket of cashier
mean(agg_tick_loyal$price)#mean of cost per ticket
sd(agg_tick_loyal$price)

median(agg_tick_loyal$price)
x11()
pie(agg_tick_loyal$price)


library(ggplot2)
# 
x11()
ggplot(data.frame(avgtick_loyal$quantity), aes(avgtick_loyal$quantity)) +
  geom_bar()
+abline(v=mean(as.integer(  avgtick_loyal$quantity)))
avgtick_loyal$quantity<-as.integer(avgtick_loyal$quantity)
x11()
hist((agg_tick_loyal$price),breaks=15, main='monetary purchase per ticker for loyal customers(red line is the mean)', xlab='Price  per ticket')
abline(v=mean(as.integer(  agg_tick_loyal$price)), col='red')






x11()
hist((avgtick_loyal$quantity),breaks=15, main='Quantity of items purchase per ticker, loyal customers(red line is the median)', xlab='Quantity  per ticket')
abline(v=median(as.integer( avgtick_loyal$quantity)), col='red')




d<-density(agg_tick_loyal$price)
x11()
plot(d, col='red',lwd=4)

