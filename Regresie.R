library(tidyverse)
library(modelr)
library(readxl)
library(rsample)
library(rpart)
library(dplyr)
library(rpart.plot)
library(caret)
library(recipes)
library(cluster)
library(ClusterR)
library(factoextra)
library("FactoMineR")
library("dplyr")
library(tree)
library(gridExtra)


InstagramDate<-read_excel("insta_date_2.xlsx")
view(InstagramDate)
attach(InstagramDate)

TiktokDate<-read_excel("tiktok.xlsx")
view(TiktokDate)
attach(TiktokDate)


print("Atribute egale cu 0")
colSums(is.na(InstagramDate))

print("Vizualizare datelor")
names=colnames(InstagramDate)

for(x in 1:8)
{
  print(paste("Vizualizam coloana: ", names[x]))
  print(table(InstagramDate[,x]))
}

summary(InstagramDate)

ggplot(data = InstagramDate , aes(x = Audience_country, y = Followers )) + ggtitle("") + xlab("Audience_country") + ylab("Followers") + geom_boxplot() + geom_smooth()
distinct(InstagramDate, Audience_country)
InstagramDate$Audience_country = as.factor(InstagramDate$Audience_country) 
InstagramDate$Audience_country <- fct_recode(InstagramDate$Audience_country, "Africa" = "Algeria" , "Europe" = "Albania", "South America" =  "Argentina", "South America" = "Brazil", "South America" = "Chile","Asia" = "China", "South America" = "Colombia", "Africa" = "Egypt", "Europe" = "France", "Europe" = "Germany",  "Asia" = "India", "Asia" = "Indonesia", "Asia" = "Iran", "Asia" = "Iraq",  "Europe" = "Italy", "Asia" = "Japan", "North America" = "Mexico", "Africa" = "Morocco", "Africa" = "Nigeria", "Asia" = "Philippines", "Europe" = "Poland",  "Asia" = "Russia",  "Europe" = "Serbia",  "Asia" = "South Korea", "Europe" = "Spain", "Asia" = "Syria",  "Asia" = "Thailand", "Asia" = "Turkey",  "Europe" = "Ukraine", "Asia" = "United Arab Emirates",  "Europe" = "United Kingdom", "North America" = "United States")
InstagramDate$Audience_country
ggplot(data = InstagramDate , aes(x = Audience_country, y = Followers )) + ggtitle("") + xlab("Audience_country") + ylab("Followers") + geom_boxplot() + geom_smooth()

ggplot(data = InstagramDate , aes(x = category_1), y=Engagement_avg) + ggtitle("") + xlab("Category 1") + ylab("Followers") + geom_histogram(stat = "count")
ggplot(data = InstagramDate , aes(x = category_2), y=Engagement_avg) + ggtitle("") + xlab("Category 2") + ylab("Followers") + geom_histogram(stat = "count")


i1 <- ggplot(data = InstagramDate , aes(x = Followers, y = Authentic_engagement )) + ggtitle("") + xlab("Followers") + ylab("Authentic Engagement") + geom_point() + geom_smooth()
i2 <- ggplot(data = InstagramDate , aes(x = Followers, y = Engagement_avg )) + ggtitle("") + xlab("Followers") + ylab("Engagement Average") + geom_point() + geom_smooth()
grid.arrange(i1, i2, ncol=2)
#-----------------------------ORDONARE PCA DUPA INSTAGRAM DATE ----------------------------------


apply(InstagramDate[c('Followers','Engagement_avg')], 2, var) # varianta
apply(InstagramDate[c('Followers','Engagement_avg')], 2, mean) # media

#Followers
scaled_instadate <- apply(InstagramDate[c('Followers','Engagement_avg')], 2, scale)
apply(InstagramDate[c('Followers','Engagement_avg')], 2, var) # varianta
apply(InstagramDate[c('Followers','Engagement_avg')], 2, mean) # media


scaled_instadate <- apply(InstagramDate[c('Followers','Engagement_avg','Authentic_engagement')], 2, scale)
view(scaled_instadate)
apply(scaled_instadate, 2, var) # varianta
apply(scaled_instadate, 2, mean) # media

flw.cov <- cov(scaled_instadate)
flw.cov

set.seed(123)
insta.pca <- prcomp(scaled_instadate, center = TRUE,scale. = TRUE) #principal component analysis
rezultat <- -1*insta.pca$rotation
rezultat
summary(insta.pca) #proportion of the variance. PC1 explica 69% din varianta in modelul


res.pca <- PCA(scaled_instadate,  graph = FALSE)
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))


str(insta.pca)
str(rezultat)
insta.pca$sdev #standard deviation - how spread out the numbers are


biplot(insta.pca, scale = 0)




#----------------------CLUSTER TIKTOK----------------------------------

TiktokDate<-read_excel("tiktok.xlsx")
view(TiktokDate)
attach(TiktokDate)

set.seed(123)
ClusterTiktok <- TiktokDate[,c(3,4,5)]
ClusterTiktok <- scale(ClusterTiktok) 


#ClusterTiktok <- as.data.frame(t(ClusterTiktok))

ClusterTiktok <- na.omit(ClusterTiktok)

kmm = kmeans(ClusterTiktok,2,nstart = 50,iter.max = 15)
kmm

k1 <- kmeans(ClusterTiktok, centers = 3, nstart = 25) #nr de incercari pentru a gasi cea mai mica varianta cluster
print(k1)
#cluster vector 1:k, indicand fiecare cluster pana la ce numar se raspandeste

k1$cluster
k1$centers

aggregate(ClusterTiktok, by=list(cluster=k1$cluster), mean) #medie variabilelor folosita de a vede in ce cluster cad variabilele

dd <- cbind(ClusterTiktok, cluster = k1$cluster)
head(dd) #points/numere de clasificare dupa data reala

view(dd) #analiza datelor. Care instanta in care cluster participe

k1$size #marime de clustere
k1$centers #medie de clustere (centroid)



fviz_cluster(k1, data = ClusterTiktok, labelsize=6) #formularea clusterului
#clustering in 2 grupe
# center = 2  => k=2

#metoda elbow

fviz_nbclust(ClusterTiktok, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(ClusterTiktok, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method") # verifica data-point cu within-cluster-cohersion cu altele data-points


kmm1 = kmeans(ClusterTiktok,3,nstart = 25,iter.max = 15)
fviz_cluster(kmm1, data = ClusterTiktok, labelsize=6)
kmm
kmm1

k2 <- kmeans(ClusterTiktok, centers = 4, nstart = 25)
fviz_cluster(k2, data = ClusterTiktok, labelsize=6)

k2$size

dd1 <- cbind(ClusterTiktok, cluster = k2$cluster)
head(dd1) #points/numere de clasificare dupa data reala

view(dd1) #analiza datelor. Care instanta in care cluster participe

k2$size #marime de clustere
k2$centers #medie de clustere (centroid)

#x numeric matrix, numeric vector
#centers nr. posibile pentru cluster luate dupa x.
#nstart nr. de partii cand center-ul este un numar

ClusterTiktok %>%
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


library(NbClust)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))
nc <- NbClust(ClusterTiktok, min.nc=2, max.nc=10, method="kmeans")

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")
table(nc$Best.n[1,])

#--------------------------/CLUSTER-----------------------------------

#-------------------------REGRESIE INSTAGRAM--------------------------------

InstagramDate<-read_excel("insta_date_2.xlsx")
view(InstagramDate)
attach(InstagramDate)


InstagramDate$Influencer_insta_name = as.factor(InstagramDate$Influencer_insta_name) 
InfluencerNameModel<-lm(InstagramDate$Followers~InstagramDate$Influencer_insta_name,data = InstagramDate)
summary(InfluencerNameModel)

InstagramDate$instagram_name = as.factor(InstagramDate$instagram_name) 
InstaNameModel<-lm(InstagramDate$Followers~InstagramDate$instagram_name,data = InstagramDate)
summary(InstaNameModel)

ggplot(data = InstagramDate , aes(x = InstagramDate$category_1  )) + ggtitle("Histograma Category1") + xlab("InstagramDate$category_1") + ylab("number") + geom_histogram(stat = "count")
ggplot(data = InstagramDate , aes(x = category_1, y = Followers )) + ggtitle("") + xlab("category_1") + ylab("Followers") + geom_boxplot() + geom_smooth()


#Followers ~ Category1
InstagramDate$category_1 = as.factor(InstagramDate$category_1)
Instacategory1Model<-lm(InstagramDate$Followers~InstagramDate$category_1,data = InstagramDate)
summary(Instacategory1Model)
#low f, low R, low p

#Followers ~ Category2
ggplot(data = InstagramDate , aes(x = category_2, y = Followers )) + ggtitle("") + xlab("category_2") + ylab("Followers") + geom_boxplot() + geom_smooth()
InstagramDate$category_2 = as.factor(InstagramDate$category_2) 
Instacategory2Model<-lm(InstagramDate$Followers~InstagramDate$category_2,data = InstagramDate)
summary(Instacategory2Model)
#low f, low r, low p


#Followers ~ Audience Country
InstagramDate$category_2 = as.factor(InstagramDate$Audience_country) 
Instacategory2Model<-lm(InstagramDate$Followers~InstagramDate$Audience_country,data = InstagramDate)
summary(Instacategory2Model)
#low f, r, p


ggplot(data = InstagramDate , aes(x = Audience_country)) + ggtitle("Histograma Audience_country") + xlab("InstagramDate$Audience_country") + ylab("number") + geom_histogram(stat = "count")

#Engagement.avg ~ Followers
ggplot(data = InstagramDate , aes(x = Followers, y = Engagement_avg )) + ggtitle("") + xlab("Authentic_engagement") + ylab("Followers") + geom_point() + geom_smooth()
Model00<-lm(InstagramDate$Engagement_avg~InstagramDate$Followers,data = InstagramDate)
summary(Model00)
#slabo

#Avg engagement ~ Authentic eng
ggplot(data = InstagramDate , aes(x = Authentic_engagement, y = Engagement_avg )) + ggtitle("") + xlab("Authentic Engagement") + ylab("Engagement average") + geom_point() + geom_smooth()
EngagementAvgModel<-lm(InstagramDate$Engagement_avg~InstagramDate$Authentic_engagement,data = InstagramDate)
summary(EngagementAvgModel)

#Avg engagement ~ category1
InstagramDate$category_1 = as.factor(InstagramDate$category_1)
Insta1Model<-lm(InstagramDate$Engagement_avg~InstagramDate$category_1,data = InstagramDate)
summary(Insta1Model)
#low f, low R, low p

#Avg engagement countries
Insta2Model<-lm(InstagramDate$Engagement_avg~InstagramDate$Audience_country,data = InstagramDate)
summary(Insta2Model)
#low f, low R, low p

InstaModelAll<-lm(InstagramDate$Followers~.,data=InstagramDate)
summary(InstaModelAll)

Model1<-lm(InstagramDate$Followers~Influencer_insta_name + instagram_name,data=InstagramDate)
summary(Model1)

Model2<-lm(InstagramDate$Followers~Influencer_insta_name + category_1,data=InstagramDate)
summary(Model2)

Model3<-lm(InstagramDate$Engagement_avg~instagram_name + category_1,data=InstagramDate)
summary(Model3)


#-------------------------/REGRESIE INSTAGRAM----------------------------------

#--------------------------REGRESIE TIKTOK------------------------------------- 

TiktokDate<-read_excel("tiktok.xlsx")
view(TiktokDate)
attach(TiktokDate)


TiktokDate$Tiktoker_name = as.factor(TiktokDate$Tiktoker_name) 
TiktokerModel<-lm(TiktokDate$Subscribers_count~TiktokDate$Tiktoker_name,data = TiktokDate)
summary(TiktokerModel)

TiktokDate$Tiktok_name = as.factor(TiktokDate$Tiktok_name) 
TiktokerModel<-lm(TiktokDate$Subscribers_count~TiktokDate$Tiktok_name,data = TiktokDate)
summary(TiktokerModel)



par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(TiktokDate$Views_avg), main="Density Plot: Medie vizitatori", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(TiktokDate$Views_avg), 2)))  # density plot for 'speed'
polygon(density(TiktokDate$Views_avg), col="red")
plot(density(TiktokDate$Subscribers_count), main="Density Plot: Subscriberi", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(TiktokDate$Subscribers_count), 2)))  # density plot for 'dist'
polygon(density(TiktokDate$Subscribers_count), col="red")
#vizaitatori skewness mean>median
#subscribers skewness mean>median

ggplot(data = TiktokDate , aes(x = Likes_avg, y = Subscribers_count )) + ggtitle("") + xlab("Likes avg.") + ylab("Subscribers") + geom_point() + geom_smooth()
ggplot(data = TiktokDate , aes(x = Views_avg, y = Subscribers_count )) + ggtitle("") + xlab("Likes avg.") + ylab("Subscribers") + geom_point() + geom_smooth()

ggplot(data = TiktokDate , aes(x = Subscribers_count, y = Likes_avg )) + ggtitle("") + xlab("Views") + ylab("Likes") + geom_point() + geom_smooth()
LikesModel1<-lm(TiktokDate$Likes_avg~TiktokDate$Subscribers_count,data = TiktokDate)
summary(LikesModel1)
#F 100, R low.

plot(resid(LikesModel1))


scatter.smooth(x=TiktokDate$Views_avg, y=TiktokDate$Likes_avg, main="Likes ~ Views") 


scatter.smooth(x=TiktokDate$Subscribers_count, y=TiktokDate$Likes_avg, main="Likes ~ Subscribers") 

ggplot(data = TiktokDate , aes(x = Shares_avg, y = Likes_avg )) + ggtitle("") + xlab("Shares Avg") + ylab("Likes") + geom_point() + geom_smooth()
LikesModel2<-lm(TiktokDate$Likes_avg~TiktokDate$Shares_avg,data = TiktokDate)
summary(LikesModel2)
#NE

ggplot(data = TiktokDate , aes(x = Views_avg, y = Likes_avg )) + ggtitle("") + xlab("Shares Avg") + ylab("Likes") + geom_point() + geom_smooth()
LikesModel2<-lm(TiktokDate$Likes_avg~TiktokDate$Shares_avg,data = TiktokDate)
summary(LikesModel3)
#NE
ggplot(data = TiktokDate , aes(x = Views_avg, y = Subscribers_count )) + ggtitle("") + xlab("Shares Avg") + ylab("Likes") + geom_point() + geom_smooth()
ggplot(data = TiktokDate , aes(x = Likes_avg, y = Subscribers_count )) + ggtitle("") + xlab("Shares Avg") + ylab("Likes") + geom_point() + geom_smooth()
ggplot(data = TiktokDate , aes(x = Shares_avg, y = Subscribers_count )) + ggtitle("") + xlab("Shares Avg") + ylab("Likes") + geom_point() + geom_smooth()
ggplot(data = TiktokDate , aes(x = Shares_avg, y = Subscribers_count )) + ggtitle("") + xlab("Shares Avg") + ylab("Likes") + geom_point() + geom_smooth()

ggplot(data = TiktokDate , aes(x = Subscribers_count, y = Views_avg )) + ggtitle("") + xlab("Shares Avg") + ylab("Likes") + geom_point() + geom_smooth()
VModel1<-lm(TiktokDate$Subscribers_count~TiktokDate$Views_avg,data = TiktokDate)
summary(VModel1)

ggplot(data = TiktokDate , aes(x = Likes_avg, y = Views_avg )) + ggtitle("") + xlab("Shares Avg") + ylab("Likes") + geom_point() + geom_smooth()
VModel2<-lm(TiktokDate$Likes_avg~TiktokDate$Shares_avg,data = TiktokDate)
summary(VModel2)

Views~,Subscribers_count+Likes_avg
#NE

cor(TiktokDate$Likes_avg,TiktokDate$Comments_avg)
ggplot(data = TiktokDate , aes(x = Likes_avg, y = Comments_avg )) + ggtitle("") + xlab("Likes") + ylab("Comments") + geom_point() + geom_smooth()
CommentsModel2<-lm(TiktokDate$Comments_avg~TiktokDate$Likes_avg,data = TiktokDate)
summary(CommentsModel2)
#B0=0.0481, B1=0.007817 Y=B0+B1*Likes

ggplot(data = TiktokDate , aes(x = Subscribers_count, y = Comments_avg )) + ggtitle("") + xlab("Subscribers") + ylab("Views avg") + geom_point() + geom_smooth()
CommentsModel3<-lm(TiktokDate$Subscribers_count~TiktokDate$Comments_avg,data = TiktokDate)
summary(CommentsModel3)
#F low, R low

p <-  as.data.frame(TiktokDate$Likes_avg)
prediction <- predict(CommentsModel2, newdata = p)
prediction <- cbind(prediction, TiktokDate$Likes_avg)
prediction <- cbind(prediction, TiktokDate$Comments_avg)
colnames(prediction) <- c('Predictie Comentarii', 'Likeuri', 'Comentarii medie (date)')
view(prediction)

predict(CommentsModel2, data.frame(Likes_avg=300000))

ggplot(data = TiktokDate , aes(x = Likes_avg, y = Subscribers_count )) + ggtitle("") + xlab("Subscribers") + ylab("Comments") + geom_point() + geom_smooth()
LikesModel3<-lm(TiktokDate$Subscribers_count~TiktokDate$Likes_avg,data = TiktokDate)
summary(CommentsModel3)


TiktokModelAll<-lm(TiktokDate$Subscribers_count~.,data=TiktokDate)
summary(TiktokModelAll)

Model1Tt<-lm(TiktokDate$Subscribers_count~Views_avg + Likes_avg,data=TiktokDate)
summary(Model1Tt)
#low F/low R


plot1 <- ggplot(TiktokDate,aes(x = Likes_avg, y =Views_avg )) + geom_point() +geom_smooth(method = "lm")
plot2 <- ggplot(TiktokDate,aes(x = Likes_avg, y =Subscribers_count )) + geom_point() +geom_smooth(method = "lm")
grid.arrange(plot1, plot2, ncol=2)

plot3 <- ggplot(data = TiktokDate , aes(x = Subscribers_count, y = Views_avg )) + ggtitle("") + xlab("Subscribers") + ylab("Views avg") + geom_point() + geom_smooth()
plot4 <- ggplot(data = TiktokDate , aes(x = Likes_avg, y = Views_avg )) + ggtitle("") + xlab("Likes Avg") + ylab("Views") + geom_point() + geom_smooth()
grid.arrange(plot3, plot4, ncol=2)

Model2Tt<-lm(TiktokDate$Likes_avg~Views_avg + Subscribers_count,data=TiktokDate)
summary(Model2Tt)
#dobra

Model3Tt<-lm(TiktokDate$Comments_avg~Views_avg + Likes_avg,data=TiktokDate)
summary(Model3Tt)
#ok

Model1<-lm(TiktokDate$Views_avg~Subscribers_count+Likes_avg,data=TiktokDate)
summary(Model1)




#-----------------------------/TIKTOK-------------------------------------

#------------------------ARBORI DE DECIZIE (INSTA)------------------------

#vizualizam atributele numerice. 
InstaDate = InstagramDate <- select(InstagramDate, category_1:Engagement_avg)
InstaDate %>% 
  select_if(is.numeric) %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill=metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~metric, scales = "free")


set.seed(123)
insta_split <- initial_split(InstaDate, prop = 0.7)
#impartim date 30 cu 70 la suta
insta_train <- training(insta_split)
insta_test <- testing(insta_split)

InstaDate %>%
  ggplot(aes(Followers)) +
  geom_density()
#Cele mai multe Followers medie per persona (acoperire) ar fi unde este pontul



m1 <- rpart(
  formula = Followers ~., #followers against toate atributele
  data=insta_train,
  method="anova" #metoda anova
)
m1
#  1) root 800 1.484870e+18 - SSE 26318380  - media followers
# face arbor dupa category_1, category_2, audience_country, engagement_avg,category_2,authentic_eng
rpart.plot(m1) #afisare grafica a rezultatelor
plotcp(m1) # graf a complexitatii cp=Alpha 
m1$cptable #afiseaza parametrilor alpha


m2 <- rpart(
  formula = Followers ~., #likes against toate atributele
  data=insta_train,
  method="anova", #metoda anova
  control = list(cp=0,xval=8) # se creste arborele pana la obtinerea valorii zero pentru parametrul alpha cross validation 8
)
m2
rpart.plot(m2)
plotcp(m2)
abline(v = 6, lty = "dashed")
#66 splituri
#alpha=8, eroarea nu se duce mai jos,alpha= 0.0022 cea mai mica?

# se obtine un arbore cu parametri minsplit si maxdepth specificati
m3 <- rpart(
  formula = Followers ~ .,
  data = insta_train, 
  method = "anova",
  control = list(minsplit = 11, maxdepth = 13, xval = 10) #istante intrun nod inainte de a taia
)
m3
plotcp(m3) #obtinem date diferite.
#1) root 700 1.315615e+18  26253710 
#minim 5, pana la minim 20. 

#cautam cele mai bune valori pentru parametri minsplit si maxdepth
hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)

#128 observatii hyper-grid
head(hyper_grid)
view(hyper_grid)
models <- list()
for (i in 1:nrow(hyper_grid)) { #pt fiecare linie de hyper_grid
  minsplit <- hyper_grid$minsplit[i] 
  maxdepth <- hyper_grid$maxdepth[i] #le ia toate combinatiile si creaza un model in models[i]
  models[[i]] <- rpart(
    formula = Followers ~. ,
    data = insta_train,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}
#Rezultat: 128 modele pt ca avem 128 observatii in setul asta


view(hyper_grid) #face combinatii intre maxdepth si mindepth ca sa gaseste combinatia optima

#pt fiecare model vedem parametrul CP si erroarea minima
#transmitem un model si vedem care e minimul de error
get_cp <- function(x) {
  min <- which.min(x$cptable[,"xerror"])
  cp <- x$cptable[min, "CP"]
} 

get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
} #

# le adaugam get_cp si get_min_error in hypergrid
mutated_grid <- hyper_grid %>% 
  mutate(
    cp = purrr::map_dbl(models, get_cp), #toate modelele le trecem in functia get_cp
    error = purrr::map_dbl(models, get_min_error) #la fel pt. error
  )   


mutated_grid %>%
  arrange(error) %>% 
  top_n(-5, wt=error) #le atasam in tabelul. Ordonat dupa erroare, cele 5 cu cea mai mica eroare
#REZULTAT: Cea mai buna combinatia este 11 cu 10, cp 0.0100

view(mutated_grid)

optimal_tree <- rpart(
  formula = Followers ~ .,
  data = insta_train,
  method = "anova",
  control = list(minsplit = 11, maxdepth = 10, cp = 0.01000000)
)
optimal_tree
rpart.plot(optimal_tree)


#-----------------------------REZULTATE ARBORE---------------

pred <- predict(optimal_tree, newdata = insta_test)
RMSE(pred = pred, obs = insta_test$Followers) #predictia de erroare medie pe data de train
#Eroare medie pe data de train 36520078
optimal_tree

#----------------------------ARBORI CATEGORICE-----------------

InstagramDate<-read_excel("insta_date_2.xlsx")
view(InstagramDate)
attach(InstagramDate)

Insta1 <- select(InstagramDate, category_1:Engagement_avg)
ggplot(InstagramDate, aes(Engagement_avg)) +geom_density()
ggplot(InstagramDate, aes(Engagement_avg)) +geom_boxplot()
mean(InstagramDate$Engagement_avg)

Insta.cat <- Insta1 %>%
  mutate(Engagement_avg = ifelse(Engagement_avg<=1000000,"Intm","Adc"))
Insta.cat <- Insta.cat %>% mutate(Engagement_avg = as.factor(Engagement_avg))
view(Insta.cat)
table(Insta.cat$Engagement_avg)


set.seed(123)
gram_split <- initial_split(Insta.cat, prop = 0.7, strata = "Engagement_avg")
gram_train <- training(gram_split)
gram_test <- testing(gram_split)

insta_m1 <- rpart(
  formula = Engagement_avg ~., 
  data=gram_train,
  method="class"
)

insta_m1
summary(insta_m1)
rpart.plot(insta_m1)


#-------------------------/ARBORI DE DECIZIE--------------------

