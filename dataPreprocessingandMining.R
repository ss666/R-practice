library(dplyr)
library(ggplot2)

team_raw <- read.csv(file='Team.csv',stringsAsFactors = FALSE)

names(team_raw)
sapply(team_raw,class)
head(team_raw,5)
dim(team_raw)

#DATA PREPROCESSING
team <- team_raw[!duplicated(team_raw[,2:3]),]
#unique players
nrow(team) 
#female players
nrow(team[team$Gender=='Female',])

#the number of missing values for each attribute
sapply(team, function(x) sum(is.na(x)))
#the ratio of misssing values for each attribute
sapply(team, function(x) sum(is.na(x))/length(x))
#Given the ratios of missing values in Muscle Mass, deleting it would be the most appropriate choice
team <- select(team, -Muscle.Mass....)
#Goals.Against has a higer ratio of missing values, take a closer look at teh rate of missing values per position
aggregate(team$Goals.Against,by = list(team$Position),function(x) sum(is.na(x))/length(x)) #repalce missing values with '0'
team[is.na(team$Goals.Against),'Goals.Against'] = 0
#replace missing values of Age with mean age
mean_age <- mean(team$Age, na.rm = TRUE)
team[is.na(team$Age),'Age'] = mean_age
#replace missing values with a suitable value in each case
#take into consideration the players' gender
impute_data = function(df, field, criteria){
  values = aggregate(df[,field],list(df[,criteria]),function(x) median(x, na.rm = TRUE))
  for(v in values[,1]){
    df[is.na(df[,field] & df[,criteria] == v), field] = values[values[,1]==v,2]
  }
  return(df)
}
team <- impute_data(team, 'Height..m.','Gender')
team <- impute_data(team, 'Speed..m.h.','Gender')
team =impute_data(team, "Weight..kg.", "Gender")
#delete instances with Position missing
team <- team[team$Position != '',]
#set values to U for unknown with Pre-selected missing
team[team$Pre.selected == '','Pre.selected'] = 'U'

#eliminate the noise for Average Goals by sampling
#choose the interval and replacement stragegy(value)
library(OneR)
g <- team$Avg.Goals
range(g)
sd(g)
g1 <- bin(g,nbins = 10)
#g2 <- bin(g, method = 'clusters') use clustering to find the intervals
#g3 <- bin(g, method = 'content') divide according to number of samplings so that each bin has the same number of samples

#remove outliers in Height and Speed
#define an outlier if value is more than 3 st. devs from the mean
rm_outliers = function(df, field){
  m = mean(df[,field])
  s = sd(df[,field])
  thrs = 3*s
  out = df[df[,field] <= (m+thrs) & df[,field] >= (m-thrs),]
  return(out)
}
team = rm_outliers(team, 'Height..m.')
team = rm_outliers(team, 'Speed..m.h.')
#see if that was enough
boxplot(team$Height..m.)
boxplot(team$Speed..m.h.)
#call rm_outliers() iteratively to remove remained outerliers
team = rm_outliers(team, 'Height..m.')
boxplot(team$Height..m.)

#include Norm Speed, which normalises the speed between 0 and 1
team$norm.speed = (team$Speed..m.h. - min(team$Speed..m.h.)) / (max(team$Speed..m.h.) - min(team$Speed..m.h.))

#include Body Mass Index, which give more information about the player's physical condition
team$bmi = team$Weight..kg./team$Height..m.^2

#summary
#centrality
summary(team)
#dispersion
apply(team[,c("Age","Height..m.","Weight..kg.","Years.active","Goals.Against", "Avg.Goals","Speed..m.h.","Salary","norm.speed","bmi")], 2, sd)
#iqr
apply(team[,c("Age","Height..m.","Weight..kg.","Years.active","Goals.Against", "Avg.Goals","Speed..m.h.","Salary","norm.speed","bmi")], 2, IQR)

#auxiliary functions to analyse each attribute:
df_sum = function(df, criteria){
 #numerical attributes:
  num = select_if(df, is.numeric)
  num_res = apply(num, 2, FUN = analyse_numerical, criteria)
 #nominal attributes:
  nom = df[,setdiff(names(df), names(num))] #or colnames
  nom_res = apply(nom, 2, FUN = analyse_nominal, criteria)
  return(list(num_res, nom_res))
}

analyse_nominal = function(x, criteria){
  tab = table(criteria,x)
  u = unique(x)
  mode = u[which.max(tabulate(match(x,u)))]
  return(list(frequency = tab, mode = mode))
}

analyse_numerical = function(x,criteria){
  cent = aggregate(x, by=list(criteria),summary)
  disp = aggregate(x, by=list(criteria),sd)
  iqr = aggregate(x, by=list(criteria),IQR)
  return(list(centreility = cent, sd = disp, iqr = iqr))
}
#the analysis per team:
t <- select(team, -c(Player_ID, Name, Surname, Team))
df_sum(t,team$Team)

#DATA MINING
#the overal mean salary and the overal median speed
mean(team$Salary)
median(team$Speed..m.h.)
#the number of different teams with more men than women
ags  <- aggregate(team$Name, by=list(team$Team, team$Gender), NROW)  #length()/sum():arguments: numeric or complex or logical vectors /nrow():arguments: a vector/array/data frame or null
tab =xtabs(x~Group.1+Group.2,ags)
sum(tab[,1]>tab[,2])
#or:ags2 <- team %>% group_by(Team, Gender) %>% summarise(count = n())
#tab2 =xtabs(count~Team+Gender,ags2)
#sum(tab2[,1]>tab2[,2])

#the mean age and salary of male players in Dragon Island
filter(team, Gender == 'Male' & Team == 'Dragon Island') %>% summarise(mean_age = mean(Age), mean_salary = mean(Salary) )

#the median height of female forward players in Bim
result2 <- filter(team, Gender=='Female' & Position == 'Forward') %>% select('Height..m.')
apply(result2, 2, median)

#the home team of the fastet player
fast <- arrange(team,desc(Speed..m.h.))
select(fast[1,],Team)

#the team which spends most on salaries
mostsalaries_team <- group_by(team,Team) %>% summarise(total=sum(Salary))
head(mostsalaries_team[order(-mostsalaries_team$total),],1)

#the team with less defenders
team %>% filter(Position=='Defender') %>% group_by(Team) %>% summarise(count = length(Team)) #or: count=n()

#the team has the biggest difference in salaries
sal = team %>% group_by(Team) %>% summarise(dif.sals = max(Salary) - min(Salary)) %>% arrange(desc(dif.sals))
head(sal,1)

#the number of players that were not selected initially
filter(team, Pre.selected == 'N')  %>% summarise(n())   #or: sum(team$Pre.selected == 'N')

#the frequencey of the positions from the fastest 40 players
ggplot(fast[1:40,]) + geom_histogram(aes(x=Speed..m.h.,fill=Team)) +xlab('Speed (m/h)') + ylab('Frequency') + ggtitle('Histograms from the fastest players by Team')

#the region has more pre-selected players
players <- nrow(filter(team, Pre.selected == 'Y'))
res <- group_by(team,Team) %>% summarise(total = n()) %>% mutate(percentage = round(total/players,3))
ggplot(res,aes(x='',y=percentage,fill=Team)) +geom_bar(width=1, stat= 'identity') + coord_polar('y') + theme_void() + geom_text(aes(label = percentage),position =position_stack(vjust = 0.5))

#a histogram that shows the age distributition from all players at intervals of 1 year, and grouping the data according to Gender
ggplot(team, aes(x = Age, fill = Gender, color = Gender)) +geom_histogram(binwidth = 1, alpha =0.5)+
  scale_x_continuous(breaks = seq(min(team$Age), max(team$Age), 1), name = 'Years')+
  ggtitle('Distributition Ages by Gender')
          
#a histogram that shows the age distributition from all players at intervals of 2 years, and grouping the data according to Team
ggplot(team, aes(x = Age, fill = Team)) + geom_histogram(binwidth = 1, alpha = 1)+
  scale_x_continuous(breaks = seq(min(team$Age), max(team$Age),2))+
  ggtitle('Histogram of Age by Team')+
  theme_minimal()+
  scale_fill_brewer(palette = 'Spectral')
  
#a histogram that shows the salary distribution of players that are Defenders.
b <- seq(min(team[team$Position == 'Defender', 'Salary']), max(team[team$Position == 'Defender','Salary']),1000)
ggplot(team[team$Position == 'Defender',], aes(x = Salary))+
  geom_histogram(breaks = b, colour = 'pink4', fill = 'pink1')+
  ggtitle("Distribution of Defenders' Salaries")

#a scatterplot that shows the relationship between Speed and Height of players from Bim, Dragon Island and Calormen, grouped by Gender. Add a line that fits each group of the data
ggplot(subset(team, Team %in% c('Bism', 'Dragon Island', 'Calormen')), aes(x = Height..m., y = Speed..m.h., color = Gender))+
  geom_point()+
  geom_smooth(method = 'lm')+
  ggtitle('Height vs Speend grouped by Gender and Fitted')

#a scatterplot that shows Weight and Height of players grouped by Team. Separate each scatterplot according to Gender
ggplot(team, aes(x = Weight..kg., y = Height..m., color = Team))+
  geom_point(shape = 2)+
  scale_x_continuous(limits = c(40,90), name = 'Weight(kg)')+
  scale_y_continuous(name = 'Height(m)')+
  facet_grid(vars(Gender))+
  ggtitle("Narnia's National Team information separated by Gender")

#another version with those players who play the Forward position have their speed shown as label
ggplot(team, aes(x = Weight..kg., y = Height..m., color = Team, shape = Gender))+
  geom_point(size = 2, stroke = 1)+ scale_shape_manual(values = c(21,24))+
  geom_text_repel(aes(label = Speed..m.h.), data = subset(team, Position %in% c('Forward')), color = 'grey20')+
  scale_x_continuous(limits = c(40,85), breaks = seq(40,85, by=5),name = 'Weitgh of all players')+
  scale_y_continuous(limits = c(1.49,2.01), breaks = seq(1.49,2.01, by=0.1), name = 'Height of all players')+
  ggtitle("Narnia's National Team Information")

#apply PCA
#attributes have to be numerical values
sub_t = select_if(team,is.numeric)
sub_t = select(sub_t, -c(Player_ID,norm.speed) )
#attributes have to be standardised
stand_t = scale(sub_t)
#apply pca
pca_t = prcomp(stand_t)
summary(pca_t)
#the linear combination of attribute that give PC1 and PC2
pca_t$rotation[,1]
pca_t$rotation[,2]
