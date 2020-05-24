library(ggplot2)
library(ggpubr)
library(dplyr)


#auxiliary functions
maximise_diag <- function(x){
  indices <- perms_index(ncol(x))
  sum_max = 0
  for(i in 1:nrow(indices)){
    m <- x[,indices[i,]]
    sum_diag <- sum(diag(m))
    if(sum_diag > sum_max){
      sum_max = sum_diag
      res <- m
    }
  }  
  return(res)
}

perms_index <- function(n){
  if(n==1){
    return(matrix(1))
  } else {
    sub_p <- perms_index(n-1)
    p <- nrow(sub_p)
    y <- matrix(nrow=n*p, ncol=n)
    for(i in 1:n){
      y[(i-1)*p+1:p,] <- cbind(i,sub_p+(sub_p>=i))
    }
    return(y)
  }
}


data <- read.csv('./cw_dataset.csv/cw_dataset.csv')

#I ANALYSIS AND PRE-PROCESSING
#1 Explore the data
#1.1
str(data)  #compactly display the internal structure
summary(data)
num_data <- select(data, -c(Sample_ID,Class))
num_data %>% apply(2, sd, na.rm = TRUE) #standard deviation
num_data %>% apply(2, var, na.rm = TRUE) #variance

#1.2 histograms for each attribute
g1 <- lapply(2:19, function(i){
  plot <- ggplot(data,aes(data[,i])) + 
    geom_histogram(bins=10) +
    geom_vline(xintercept = mean(data[,i], na.rm = TRUE), color ='red', linetype = 'dashed',show.legend = TRUE) +
    geom_vline(xintercept = median(data[,i], na.rm = TRUE), color ='blue', linetype = 'dashed') +
    xlab(colnames(data)[i])
})
ggarrange(plotlist = g1)
ggarrange(g1[[1]],g1[[13]])

library(e1071)
sapply(data[2:19],skewness,na.rm=TRUE)  #skewness
sapply(data[2:19],kurtosis,na.rm=TRUE)  #kurtosis 


#2 Explore the relationships
#2.1 orientation 4 and orientation 7
cor(data$Orientation..4,data$Orientation..7, use = 'pairwise.complete.obs')
ggplot(data,aes(Orientation..4,Orientation..7))+geom_point()

#2.2 class, orientation 4, orientation 6 and area
ggplot(data, aes(Orientation..4, Orientation..6, Leaf.Area, color = Class)) +
  facet_wrap(~Class)
pCO4 <- ggplot(data,aes(Class,Orientation..4, color = Class))+geom_point()
pCO6 <- ggplot(data,aes(Class,Orientation..4, color = Class))+geom_point()
pCLA <- ggplot(data,aes(Class,Orientation..4, color = Class))+geom_point()
ggarrange(pCO4,pCO6,pCLA, common.legend = TRUE, legend='right')

#scatter matrix:
#pairs(c(data[c('Class','Orientation..4','Orientation..6','Leaf.Area')]),pch = 21, lwd =0.5,bg = c("red", "yellow", "green",'blue','purple')[unclass(data$Class)])
#mtext('type of plant: A:red; B:yellow; C:green; D:blue; E:purple',1, line=3.7,cex=.8)

#2.3 boxplots
g2 <- lapply(2:19, function(i){
  plot <- ggplot(data,aes(x=Class, y=data[,i])) + 
    geom_boxplot() +
    ylab(colnames(data)[i])
})
ggarrange(plotlist = g2)


#3 General Conclusions


#4 Dealing with missing values
#replacement with 0       
data_NAzero <- num_data %>% mutate_all(function(x) ifelse(is.na(x), 0, x)) #also make prepartion for clustering and classification
#replacement with mean  
data_NAmean <- num_data %>% mutate_all(function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)) 
#replacement with median
data_NAmed  <- num_data %>% mutate_all(function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)) 


#5 Attribute transformation
#mean centering
data_meanctr <- scale(num_data, scale=FALSE)

#normalisation
normalisation <- function(x){
  res = (x-min(x))/(max(x)-min(x))
  return(x)
}
data_norm <- apply(num_data,2,normalisation)

#standardisation  
data_std <- scale(num_data)
    

#6 Attribute / instance selection
#6.1 attribute and instance deletion
#instance deletion
NA_instance <- apply(data, 1, function(x) sum(is.na(x)))
table(NA_instance)
data_delNA <- data[rowSums(is.na(data)) < 3,]
#attribution deletion
NA_attribute <- apply(data, 2, function(x) sum(is.na(x)/length(x)))
NA_attribute  
data_delNA <- select(data_delNA, -Leaf.weight)
    
#6.2 uncorrelated attributes and no missing values
subset_data <- select(num_data, -Leaf.weight)
corr <- cor(subset_data, use= 'pairwise.complete.obs')
library(ggcorrplot)
ggcorrplot(corr, lab = TRUE, lab_size = 3, type = 'lower')
data_delcorr <- select(data_delNA, -c(Orientation..4, Orientation..6, Orientation..9)) %>% filter_all(~ !is.na(.))

#6.3 pca    
num_data_delNA <- data_delNA %>%  select(-c(Sample_ID,Class)) 
data_prepca <- num_data_delNA %>%
  mutate_all(function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)) %>%
  scale() %>% as.data.frame()
data_pca <- prcomp(data_prepca, scale = TRUE)
summary(data_pca) # $importance
data_pca8 <- data_pca$x[,1:8]
#data_pca8 <- predict(data_pca8, data_prepca)[,1:8]


#II CLUSTERING
#1
#hierarchical clustering
distEuc <- dist(data_pca8)
hc <- hclust(distEuc) #applies hierarchical clustering
data_delNA$HC5 <- cutree(hc, 5) #stops hierarchy at level 5 and saves it in 
t11 <- table(data_delNA$Class, data_delNA$HC5)  #show clusters class label according to clusters 

#k-means
km5 <- kmeans(data_pca8, 5, iter.max = 100)  #applies k-means with 5 clusters and 100 iterations
data_delNA$KM5 <- km5$cluster #saves clusters in 
t12 <- table(data_delNA$Class, data_delNA$KM5)  #compare results

#pam
library(cluster)
pam5 <- pam(data_pca8, 5)
data_delNA$PAM5 <- pam5$clustering
t13 <- table(data_delNA$Class, data_delNA$PAM5)

#internal metrics
library(fpc)
statisticsHC5 <-  cluster.stats(distEuc,data_delNA$HC5)
statisticsKM5 <- cluster.stats(distEuc,data_delNA$KM5)
statisticsPAM5 <- cluster.stats(distEuc,data_delNA$PAM5)

diameter <- c(mean(statisticsHC5$diameter),mean(statisticsKM5$diameter),mean(statisticsPAM5$diameter))
average.distance <- c(mean(statisticsHC5$average.distance),mean(statisticsKM5$average.distance),mean(statisticsPAM5$average.distance))
separation <- c(mean(statisticsHC5$separation),mean(statisticsKM5$separation),mean(statisticsPAM5$separation))
average.between <- c(statisticsHC5$average.between,statisticsKM5$average.between,statisticsPAM5$average.between)
dfMetrics <- data.frame(diameter=diameter, average.distance=average.distance,
                        separation=separation, average.bewteen=average.between,
                        row.names=c('hierarchical','k-means','PAM'))

#external metrics
#confusion matrix
tc11 <- maximise_diag(t11)
tc12 <- maximise_diag(t12)
tc13 <- maximise_diag(t13)
library(gridExtra)
ggarrange(tableGrob(tc11),tableGrob(tc12),tableGrob(tc13), nrow=1)
#accuracy rate
sum(diag(tc11)) / sum (tc11)
sum(diag(tc12)) / sum (tc12)
sum(diag(tc13)) / sum (tc13)


#2
#hierarcihical clustering
distMan <- dist(data_pca8, method='manhattan')
distMin <- dist(data_pca8, method='minkowski')
dist <- list(distEuc, distMan, distMin)
method <- c('complete','single','average')

HC <- lapply(dist, function(r){
  lapply(method,function(i){
    hc <- hclust(r, method=i)
    cutree(hc, 5)
  })
})
evaluation1 <- sapply(1:3, function(i){
  sapply(1:3,function(j){
    t <- table(data_delNA$Class, HC[[i]][[j]])
    col_max <- apply(t,2,max) #purity
    sum(col_max)/sum(t)
  })
})

df1 <- as.data.frame(evaluation1,row.names = method ) %>% setNames(c('euclidean','manhattan','minkowski'))

#k-means
iter.max <- seq(10,100,10)
algorithm <- c('Hartigan-Wong','Lloyd','MacQueen')
gk <- expand.grid(iter.max,algorithm) #combination

KM <- function(i,r){
  evalKM <- kmeans(data_pca8, 5, iter.max=i, algorithm=r) 
  stat <- cluster.stats(distEuc,evalKM$cluster)
  stat$entropy
}
evaluation2 <-  mapply(KM,gk[[1]],as.character(gk[[2]]))  #grid search

df2 <- cbind(rep(iter.max,3),rep(algorithm,each=10),evaluation2) %>%
  as.data.frame(stringsAsFactors = FALSE) %>% 
  setNames(c('iter.max','algorithm','entropy'))
ggplot(df2,aes(x=as.numeric(iter.max),y=as.numeric(entropy)))+
  geom_line(aes(color=algorithm,group=algorithm))+
  scale_y_continuous(name='entropy',breaks=pretty(range(res222$entropy), 10), labels=pretty(range(res222$entropy), 10))+
  scale_x_continuous(name='maximum number of iteration allowed',breaks=iter.max)

#pam
do.swap=c(TRUE,FALSE)
metric=c("euclidean", "manhattan")
g <- expand.grid(do.swap,metric)

PAM <- function(i,r){
  evalPAM <- pam(data_pca8, 5, metric=r, do.swap=i)
  stat <- cluster.stats(distEuc,evalPAM$cluster)
  stat$dunn
}
evaluation3 <-  mapply(PAM,g[[1]],as.character(g[[2]]))

df3 <- cbind(rep(do.swap,2),rep(metric,each=2),evaluation3) %>%
  as.data.frame() %>% 
  setNames(c('swap','metric','dunn'))
ggplot(df3,aes(x=metric,y=dunn,fill=swap))+geom_bar(stat='identity',position='dodge')


#3
#the reduced data set featureing 10 PCs
data_pca10 <- as.data.frame(data_pca$x[,1:10])
pam31 <- pam(data_pca10, 5)
data_delNA$PAM31 <- pam31$clustering
tc31 <- table(data_delNA$Class, data_delNA$PAM31) %>% maximise_diag

#the dataset after deletion of instances and attributes
data_deleted <- mutate_all(dnum_data_delNA,function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)) 
pam32 <- pam(data_deleted, 5)
data_delNA$PAM32 <- pam32$clustering
tc32 <- table(data_delNA$Class, data_delNA$PAM32) %>% maximise_diag

#the dataset with replacement of NAs
pam33 <- pam(data_NAzero, 5)
pam34 <- pam(data_NAmean, 5)
pam35 <- pam(data_NAmed, 5)
data$PAM33 <- pam33$clustering
data$PAM34 <- pam34$clustering
data$PAM35 <- pam35$clustering
tc33 <- table(data$Class, data$PAM33) %>% maximise_diag
tc34 <- table(data$Class, data$PAM34) %>% maximise_diag
tc35 <- table(data$Class, data$PAM35) %>% maximise_diag

#evaluation of different datasets (true postive rate)
tpr <- c(sum(diag(tc31)) / sum (tc31),sum(diag(tc32)) / sum (tc32),
         sum(diag(tc33)) / sum (tc33),sum(diag(tc34)) / sum (tc34),
         sum(diag(tc35)) / sum (tc35))

dfEval <- data.frame(TPRate= tpr, row.names=c('Reduced PCA','Deleted Dataset','Zero Imputation','Mean Imputation','Median Imputation'))

  
#III CLASSIFCATION (implemented in Weka)
# Classifiers including NaiveBayes, k-NN, decision tree, etc, are used.
# Evaluation metrics, such as precison, recall, F1 score, false positive rate, kappa statistic, MAE, RAE, RMSE

#knitr::kable() is used to in rmd


