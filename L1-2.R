library(ISLR)
library(ggplot2)
library(cowplot)
library(MASS)
library(caret)
library(corrplot)
# Pirma uzduotis

music <- read.csv(file='music_spotify.csv')
summary(music)
head(music)
attach(music)
# Antra uzduotis

music$target=as.factor(music$target)
p1 <- ggplot(data=music,aes(x=acousticness, fill=target))+ geom_density() + labs(title="Acousticness by target")
p2 <- ggplot(data=music,aes(x=danceability, fill=target))+ geom_density() + labs(title="Danceability by target")
p3 <- ggplot(data=music,aes(x=duration_ms, fill=target))+ geom_density() + labs(title="Duration_ms by target")
p4 <- ggplot(data=music,aes(x=energy, fill=target))+ geom_density() + labs(title="Energy by target")
p5 <- ggplot(data=music,aes(x=instrumentalness, fill=target))+ geom_density() + labs(title="Instrumentalness by target")
plot_grid(p1,p2,p3,p4,p5,ncol=3)

# Trecia uzduotis

# music$loudness=as.factor(music$loudness)
# music$tempo=as.factor(music$tempo)

t1 <- ggplot(data=music,aes(x=liveness, fill=target))+ geom_density() + labs(title="Liveness by target")
t2 <- ggplot(data=music,aes(x=loudness, fill=target))+ geom_density() + labs(title="Loudness by target")
t3 <- ggplot(data=music,aes(x=speechiness, fill=target))+ geom_density() + labs(title="Speechinees by target")
t4 <- ggplot(data=music,aes(x=tempo, fill=target))+ geom_density() + labs(title="Tempo by target")
t5 <- ggplot(data=music,aes(x=valence, fill=target))+ geom_density() + labs(title="Valence by target")
plot_grid(t1, t2, t3, t4, t5, ncol=3)

# Ketvirta uzduotis
b1 <- ggplot(data=music,aes(x=key)) +
  geom_bar(aes(fill=target)) + labs(title="Target vs Key")
b2 <- ggplot(data=music,aes(x=mode)) +
  geom_bar(aes(fill=target)) + labs(title="Target vs mode")
b3 <- ggplot(data=music,aes(x=time_signature)) +
  geom_bar(aes(fill=target)) + labs(title="Target vs Time sig")
plot_grid(b1, b2, b3, ncol=3)

# Penkta uzduotis

# Sesta uzduotis
set.seed(1)
music1 = music[sample(nrow(music)),]
train = (X<=2016*0.75)
train_data=music1[train,]
test_data=music1[!train,]
glm.fits=glm(target~+acousticness+danceability+duration_ms+
               energy+instrumentalness+key+liveness+loudness+
               mode+speechiness+tempo+time_signature+valence,
             family=binomial,data=train_data)

glm.probs=predict(glm.fits,test_data,type="response")
glm.pred=rep("0", 504)
glm.pred[glm.probs > .5]="1"
table(true_classes=test_data$target,predicted=glm.pred)
mean(glm.pred==test_data$target)
mean(glm.pred!=test_data$target)

# Septinata uzduotis

glm.pred=rep("0", 504)
glm.pred[glm.probs > .4]="1"
table(true_classes=test_data$target,predicted=glm.pred)
mean(glm.pred==test_data$target)
mean(glm.pred!=test_data$target)


glm.pred=rep("0", 504)
glm.pred[glm.probs > .3]="1"
table(true_classes=test_data$target,predicted=glm.pred)
mean(glm.pred==test_data$target)
mean(glm.pred!=test_data$target)

# Astunta uzduotis

mydata <- music[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
cormat<-signif(cor(mydata),2)
corrplot(cormat, tl.col = "black", order = "hclust",
         hclust.method = "average", addrect = 4, tl.cex = 0.7)
