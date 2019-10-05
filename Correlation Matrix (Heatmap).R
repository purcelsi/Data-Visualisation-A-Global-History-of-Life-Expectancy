#Generating Correlation Matrix to explore relationships between life expectancy and other socioeconomic variables in R studio

#Install and load packages for visualisation
install.packages("ggplot2")
install.packages("reshape2")
library(reshape2)
library(ggplot2)


setwd("C:/Users/Siobhan/Documents/Data Analytics Course (Siobhan)/Semester 2/Data Visualisation/CA2/Tableau/life-expectancy-who")
library(readr)

#Load in the data from kaggle
Life_Expectancy_Data <- read.csv("~/Data Analytics Course (Siobhan)/Semester 2/Data Visualisation/CA2/Tableau/life-expectancy-who/Life Expectancy Data.csv", header = T, na.strings = c(""))

#Exploring the data
View(Life_Expectancy_Data)
str(Life_Expectancy_Data)
head(Life_Expectancy_Data)
names(Life_Expectancy_Data)
summary(Life_Expectancy_Data)
# checking the number of rows with missing values
colSums(is.na(Life_Expectancy_Data))
complete.cases(Life_Expectancy_Data)
sum(is.na(Life_Expectancy_Data))

#Visualiing missing values
install.packages("VIM")
library(VIM)
missingview <- aggr(Life_Expectancy_Data, prop = FALSE, numbers = TRUE)

#Removing mising values (NA values)
Life_Expectancy_Data2 <- na.omit(Life_Expectancy_Data)
nrow(Life_Expectancy_Data2)
View(Life_Expectancy_Data2)
names(Life_Expectancy_Data2)
#########################################################
##                                                     ##
##          Correlation Heatmap                       ##
##                                                     ##
#########################################################

#Correlation Matrix for all socioecnomic factors and life expectancy  

cor_vars_all<-Life_Expectancy_Data2[,c("Life.expectancy","Adult.Mortality","infant.deaths","Alcohol","percentage.expenditure","Hepatitis.B","Measles","BMI", "under.five.deaths",
                                       "Polio","Total.expenditure","Diphtheria", "HIV.AIDS", "GDP", "Population", "thinness..1.19.years", "thinness.5.9.years", "Income.composition.of.resources", "Schooling")]

# Melt the correlation matrix
library(reshape2)
library(ggplot2)
cor_vars_all<-hrdata2[,c("satisfaction_level","evaluation_score","number_project","average_monthly_hours","years_with_company","Work_accident","left","promotion_last_5years")]
cor(cor_vars_all)
trans<-cor(cor_vars_all)
melted_cormat <- melt(trans)

# Heatmap
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Get the lower and upper triangles of the correlation matrix


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

#Store the upper triangle of correlation matrix in object
upper_tri <- get_upper_tri(trans)
upper_tri


# Reconstruct the correlation matrix with just the upper triangle
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Generate New Heatmap 
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

