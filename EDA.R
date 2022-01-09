if(!require(pacman))install.packages("pacman")
pacman::p_load(
  tidyverse,
  dplyr,
  ggplot, # for graphing 
  caret, # for machine learning
  magnittr,
  pacman,
  GGally,
  knitr,
  parallel, 
  rattel,
  tictoc,
  gridExtra,
  kableExtra,
  readr, 
  purrr,
  randomForest,
  pROC,
  fastDummies, 
  rpart.plot,
  data.table, 
  reshape2,
  graphics,
  corrplot,
  latexpdf,
  ReporteRs,
  tinytex, 
  latexdiffr,
  latex2exp,
  class, #for using knn algorithm
)

temp <- tempfile()
url <- "https://www.kaggle.com/uciml/iris"
download.file(url, temp)
rawdata <- fread("iris.csv", header=TRUE)
unlink(temp)
iris <- rename(rawdata)
rm(rawdata,temp,url)
iris <- iris[,-1]

iris <- iris %>% rename('Petal Length'= PetalLengthCm,
                        'Petal Width' = PetalWidthCm,
                        'Sepal Length'= SepalLengthCm,
                        'Sepal Width' = SepalWidthCm) %>% 
  mutate(Species = fct_recode(Species,'setosa' = 'Iris-setosa',
                              'versicolor' = 'Iris-versicolor',
                              'virginica' = 'Iris-virginica'))
# First rows 
head(iris)

# Last rows 
tail(iris)

# Summary
summary(iris)

# Structure 
str(iris)


# Data analysis
iris %>% gather(attributes, value, 1:4) %>% 
  ggplot(aes(value, fill = attributes)) +
  geom_histogram(bins = 20,colour="black",alpha = 0.5) +
  facet_wrap(. ~ Species) +
  theme_light() +
  theme(legend.title = element_blank())

iris %>% gather(attributes, value, 1:4) %>% 
  ggplot(aes(value, fill = Species)) +
  geom_density(alpha = 0.5)+ 
  facet_wrap(. ~ attributes) +
  ylab("Density")+
  theme_light()+ 
  theme(legend.title =  element_blank())

iris %>% gather(attributes , value, 1:4) %>% 
  ggplot(aes(attributes , value, fill = attributes)) +
  geom_boxplot() + 
  theme_light() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom")
 

# correlation of features
cor <- iris[,1:4] %>% cor() %>% as.data.frame()
print(cor)


ggpairs(cbind(iris, Cluster=as.factor(iris$Species)),
        columns=1:4, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        axisLabels="none", switch="both") +
  theme_light() 


#Data prepration
## data normalized 
iris_scaled <- scale(iris[,1:4])
final_iris <- cbind(iris_scaled,iris[,5])

# split the data to train and test 
set.seed(101)
test_index <- createDataPartition(final_iris$Species,times = 1, p= 0.3, list = FALSE)
train <- final_iris[-test_index,]
test <- final_iris[test_index,]

# using the KNN algorithm for train 
## The Best K value 
fit_knn <- NULL
Accuracy <- NULL

for (i in 1:20){
  fit_knn <- knn(train[,1:4],test[,1:4], train$Species, k =i)
  Accuracy[i] <- mean(fit_knn == test$Species)
}
best_k <- which.max(Accuracy)
best_k


k <- 1:20
Accuracy.df <- data.frame(Accuracy,k)

ggplot(Accuracy.df, aes(k, Accuracy)) + geom_point() + 
     geom_line(lty = "dotted", color  = "red")

# Validating the KNN Algorithm for Test  
Accuracy[best_k]
