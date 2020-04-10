library(ggplot2)
library(ggpubr)
library(ggalluvial)
library(ggradar)
library(ggunchained)
library(ggcorrplot)
library(tibble)
library(dplyr)
library(randomForest)

#change the dafault colour palette by redefining the ggplot function
ggplot <- function(...) ggplot2::ggplot(...) + scale_fill_manual(values=c("#00bfc4","#f8766d")) + scale_colour_manual(values=c("#00bfc4","#f8766d"))

#read the data
data <- read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')

dim(data)
summary(data)
sapply(data,class)
sapply(data,typeof)


#pre-process
#data <- na.omit(data) #remove null values in total charges column
data[is.na(data$TotalCharges), 'TotalCharges'] = data[is.na(data$TotalCharges), 'MonthlyCharges']
data$SeniorCitizen <- as.factor(ifelse(data$SeniorCitizen==1, 'Yes', 'No'))


#account info
options(scipen=1) #get the values showing on the y-axis in the form of numeric, instead of scientific notation
cdata <- data %>% group_by(Churn) %>% summarise(medianTotal = median(TotalCharges), medianMonthly = median(MonthlyCharges))
p1 <- ggplot(data, aes(x=TotalCharges,fill=Churn))+geom_density(alpha=0.6) + geom_vline(data = cdata, aes(xintercept = medianTotal,col=Churn), size=1, linetype = "dashed")
p2 <- ggplot(data, aes(x=MonthlyCharges,fill=Churn))+geom_density(alpha=0.6)+ geom_vline(data = cdata, aes(xintercept = medianMonthly,col=Churn), size=1, linetype = "dashed")
fig1 <- ggarrange(p1,p2,common.legend = TRUE, legend = 'right', nrow =2, label.y = 2 )  #arrange multiple plots on the same page
fig1

fig2 <- ggplot(data, aes(x=tenure,fill=Churn)) + geom_histogram(position = 'dodge',breaks=seq(1,72,3))+ #set the numerber and boundary of bins
  scale_x_continuous(breaks=seq(0, 72, 3))+
  labs(x = 'Tenure (month)', y = 'Frequency')
fig2

fig3 <- ggplot(data,aes(x=PaymentMethod,fill=Churn)) + geom_bar(position="dodge")+ coord_flip()+ labs(y = 'Number of Customers')
fig3
fig4 <- ggplot(data,aes(Contract))+geom_bar(aes(fill=Churn),position="fill",width=0.8)+coord_flip()+ labs(y = 'Percentage of Customers')
fig4

#demographic info
p3 <- ggplot(data, aes(x=gender,fill=Churn))+ geom_bar(position ='stack')+ labs(y = 'Number of Customers')
p4 <- ggplot(data, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position ='stack')+ labs(y = 'Number of Customers')
p5 <- ggplot(data, aes(x=Partner,fill=Churn))+ geom_bar(position ='stack')+ labs(y = 'Number of Customers')
p6 <- ggplot(data, aes(x=Dependents,fill=Churn))+ geom_bar(position ='stack')+ labs(y = 'Number of Customers')
fig5 <- ggarrange(p3,p4,p5,p6, common.legend = TRUE, legend='right')+ labs(y = 'Number of Customers')
fig5

#service info
#internet service
internetservice <- subset(data,data[10]!='No internet service' & data$Churn=='Yes',select=10:15) #choose customers who subscribed internet service and left, and select internet service attributes 
table_internetservice <- apply(internetservice,2,table) #build a contingency table of the counts at each combination of internet services and whether subscribed
prop_internetservice <- prop.table(table_internetservice,2) #calculate the proportions over the table
prop_internetservice <- rownames_to_column(data.frame(prop_internetservice), 'yon')
fig6 <- ggradar(prop_internetservice, group.colours=c("#f8766d","#00bfc4"), axis.label.size=3, legend.text.size=10, legend.title='Subscription', legend.position = 'right')
fig6

#phone service
fig7 <- ggplot(data, aes(axis1 = PhoneService, axis2 = MultipleLines, axis3 = Churn)) +
  scale_x_discrete(limits = c("PhoneService", "MultipleLines", "Churn"), expand = c(.1, .05)) +
  geom_alluvium(aes(fill = Churn)) +
  geom_stratum() + geom_text(stat = "stratum", infer.label = TRUE) 
fig7

#churn percentage
churnPropTable <-data.frame(t(prop.table(table(data$Churn)))) %>% mutate(label= paste0(Var2,' ', round(Freq *100, 1), "%"), midpoint=cumsum(rev(Freq))- rev(Freq)/2)
fig8 <- ggplot(churnPropTable,aes(x = Var1,y=Freq, fill = Var2))+ geom_bar(width = 0.5,stat = 'identity')+
  coord_polar(theta = "y")+
  labs(x = '', y = '', fill ='Churn')+
  theme_void()+
  geom_text(aes(y = midpoint, label = rev(label)))  #add texts to the plot in appropraite locations
fig8

#internet service and charge
TVCharge <- data %>% filter(data[,14]=='Yes') %>% select(c(19,21)) %>% mutate(service = 'StreamingTV') #choose customers who subscribed streamingTV, select MonthlyCharges and Churn columns
moviesCharge <- data %>% filter(data[,15]=='Yes') %>% select(c(19,21)) %>% mutate(service = 'StreamingMovies')
aggregateCharge <- rbind(TVCharge, moviesCharge)  #aggregate monthly charge and churn information of both two groups
fig9 <- ggplot(aggregateCharge,aes(x=service,y=MonthlyCharges,fill=Churn))+geom_split_violin(trim=FALSE,color="white") 
fig9

#phone service and charge
phoneServiceCharge <- data %>% filter(data[,7]=='Yes') %>% select(c(19,21)) %>% mutate(service = 'PhoneService')
multipleLinesCharge <- data %>% filter(data[,8]=='Yes') %>% select(c(19,21)) %>% mutate(service = 'MultipleLines')
aggregateCharge <- rbind(phoneServiceCharge, multipleLinesCharge)
fig10 <- ggplot(aggregateCharge,aes(x=service,y=MonthlyCharges,fill=Churn))+geom_split_violin(trim=FALSE,color="white") 
fig10

#corr
#corr_data <- select(data, -1) %>% lapply(as.numeric) %>% as.data.frame()  #filter out customerID column and convert factor data to numeric
data <- data.frame(lapply(data, function(x) {gsub("No internet service", "No", x)})) #clean categorical features
data <- data.frame(lapply(data, function(x) {gsub("No phone service", "No", x)}))
data_cat <- data[,-c(1,6,19,20)]
dummy <- data.frame(sapply(data_cat,function(x) data.frame(model.matrix(~x-1,data =data_cat))[,-1])) #dummy encoding
data_cont <- data[,c(6,19,20)] %>% lapply(as.numeric) %>% as.data.frame()
corr_data <- cbind(data_cont,dummy)
corr <- cor(corr_data)  #compute Pearson correlation coefficients
fig11 <- ggcorrplot(corr, tl.cex =7, colors = c("#00bfc4",'white',"#f8766d"),hc.order = TRUE) #reorder using hclust function, and emulate ggplot2 default color palette
fig11

#feature importance 
corr_data$Churn <- as.factor(corr_data$Churn)
forest <- randomForest(Churn~., data = corr_data, mtry=3)
result <- importance(forest,type =2)  #extract variable importance 
result_df <- data.frame('feature' = colnames(corr_data[,-24]), 'importance' = result) 
fig12 <- ggplot(result_df,aes(x=reorder(feature,-MeanDecreaseGini),y=MeanDecreaseGini))+  #reorder the bars in descending order
  geom_bar(stat='identity',fill='#00bfc4')+
  labs(x = '')+
  scale_y_continuous(labels = NULL)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
fig12


# choose the number of variabiles tried at each split 
# err <- as.numeric()
#  for(i in 1:(ncol(corr_data)-1)){ 
#    mtry_test <- randomForest(Churn~., data = corr_data, mtry = i)
#    err <- append(err, mean(mtry_test$err.rate))
#  }
# print(err)
 

#interaction
library(DT)
datatable(data) #interactive table that supports pagination, sorting and search

library(plotly)
ggplotly(p1)  #translate some ggplot graphics above to an interactive version
ggplotly(p2)
ggplotly(fig2)
ggplotly(fig3)
ggplotly(fig4)








