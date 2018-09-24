# 1.Dataset Description
## 1.1 Read dataset
library(Matrix)
setwd(dir="/Users/bingranchen/Documents/Macbook13''/UCF/MSDA-Courses/STA 6704 DataMingingII/Project/project-code")
data = read.csv(file="movie_metadata.csv", stringsAsFactors = F)

## 1.2 Structure of data sets
dim(data)
str(data)
head(data)
tail(data)

#Count the number of columns that consists of text data in training dataset
sum(sapply(data[,1:28],typeof) == "character")
#Count the number of columns that consists of numeric data in training dataset
sum(sapply(data[,1:28],typeof) == "integer")


### 2. Preprocessing and Cleaning
ls(data)

### Feature reduction
data2 <- data[ , -which(names(data) %in% c("budget", "aspect_ratio", "actor_1_name", "actor_2_name","actor_3_name", "plot_keywords", "movie_imdb_link", "director_name"))]

### missing values
sum(is.na(data2)) / (nrow(data2) *ncol(data2))
library(colorspace)
library(grid)
library(data.table)
library(VIM)
aggr(data2, numbers=T, prop=T, sortVars=T, labels=names(data2))
aggr(data2, numbers=T, prop=F, sortVars=T, labels=names(data2))

##### Compute missing values in each column
temp_col <- ncol(data2)

for( i in 1:temp_col) {
  rem = sum(is.na(data2[,i])) / 5043 *100
  if(rem > 0) {
    print(i)
    print(rem)
  }
}

data3 <- na.omit(data2)

### Feature constuction
#### "length_of_title"
library(stringr)
data3$movie_title <- str_trim(data3$movie_title)
data3$length_of_title <- str_length(data3$movie_title)

#### "years"(how many years until now)
###bb <- 2018-data3$title_year
###data3$years <- bb

#### " average_actor_likes"
data3$average_actor_likes <- round(rowMeans(data3[c("actor_3_facebook_likes", "actor_1_facebook_likes","actor_2_facebook_likes")]))

##remove excessive variables
data4 <- data3[ , -which(names(data3) %in% c("movie_title", "actor_3_facebook_likes", "actor_1_facebook_likes","actor_2_facebook_likes"))]

#### "genres_new"
data4$temp <- gsub("\\|", " ", data4$genres)
data4$genres_new <- gsub(" .*$", "", data4$temp)

unique(data4$genres_new)
data4[data4$genres_new =="Musical", c("genres", "genres_new")]

data5 <- data4
data6 <- data5[ , -which(names(data5) %in% c("genres", "temp"))]

##################### Export a new movie dataset called "movie_clean"#######################
write.csv(data6, "movie_clean.csv", row.names = F)



### 3. EDA
#### Separate category data and numeric data
cat_var <- names(data6)[which(sapply(data6, is.character))]
numeric_var <- names(data6)[which(sapply(data6, is.numeric))]
data6_cat <- data6[cat_var]
data6_num <- data6[numeric_var]
dim(data6_cat)
dim(data6_num)

#### Bar plot function
library("ggplot2")
library("ggthemes")
blue.bold.italic.text <- element_text(face = "bold.italic", color = "blue")
Hist <- function (data_in, i)
{ 
  data <- data.frame(x=data_in[[i]])
  m <- ggplot(data=data, aes(x=factor(x), fill = "blue")) + stat_count() + xlab(colnames(data_in)[i]) + theme_linedraw() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))+
    theme(axis.title = blue.bold.italic.text) + guides(fill=FALSE)
  return(m)
}

#### Desity plot fuction
library("scales")
library(e1071)
Dens <- function(data_in, i){
  data <- data.frame(x=data_in[[i]])
  m <- ggplot(data= data) + geom_line(aes(x = x, color = "lightblue"), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ', round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_linedraw() + theme(legend.position="none") +theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12))
  return(m)
  
}


#### Scatter plot function
Scat <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], imdb_score = data6$imdb_score)
  m <- ggplot(data = data, aes(x= imdb_score, y=x)) +  geom_jitter()+ geom_point() + xlab(colnames(data_in)[i])+ ylab("imdb_score") + theme_linedraw() + theme(legend.position="none") +theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12))
  return(m)
  
}

#### Function to call Bar plot, Density plot function and Scatter Plot
library("gridExtra")
drawPlots <- function(data_in, fun, ii, ncol=3) 
{
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

#### Histgrams for cateforical features
drawPlots(data6_cat, fun = Hist, ii = 1:1, ncol = 1)
drawPlots(data6_cat, fun = Hist, ii = 2:2, ncol = 1)
drawPlots(data6_cat, fun = Hist, ii = 3:3, ncol = 1)
drawPlots(data6_cat, fun = Hist, ii = 4:5, ncol = 2)

#### Density plots for numeric variables
drawPlots(data6_num, fun = Dens, ii = 1:4, ncol = 2)
drawPlots(data6_num, fun = Dens, ii = 5:8, ncol = 2)
drawPlots(data6_num, fun = Dens, ii = 9:13, ncol = 2)

#### ScatterPlot between imdb_score and other numeric variables
drawPlots(data6_num, fun = Scat, ii = 1:4, ncol = 2) 
drawPlots(data6_num, fun = Scat, ii = 5:8, ncol = 2) 
drawPlots(data6_num, fun = Scat, ii = 9:13, ncol = 2) 

#### Correlation
library(corrplot)
cor(data6_num)
correlations <- cor((data6_num))
#rowName <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
#correlations<- correlations[rowName ,rowName ]
corrplot(correlations, method="number", type="upper")