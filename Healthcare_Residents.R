# loading the two dataset
data1 <- read.csv("Property_Price_Register_Ireland-05-04-2011.csv")
data2 <- read.csv("general_health.csv")

# arranging the attribute 
data2 <-data2[, c(1,3,4,2,5,6,7,8,9,10,11,12,13,14,15,16)]

# Creating a new dataframe where it contains Donegal in County
Donegal_data<- subset(data1, data1$County == "Donegal", select = c(1:8))
#Creating a new dataframe where it contains Dublin in County
Donegal_data1<- subset(data1, data1$County == "Dublin", select = c(1:8))

# Creating dataframe of Donegal
Healthcare <- subset(data2, data2$County == "Donegal", select = c (4:16))
# creating data frame of dublin
Healthcare1 <- subset(data2, data2$County == "Dublin",  select = c(4:16))

# Binding both the dataset
Donegal_Final <- rbind(data6,data7)
str(Donegal_Final)
Healthcare_Final <- rbind(data8,data9)
str(Healthcare_Final)

# taking random samples 
data6 <- Donegal_data[-sample(1:nrow(Donegal_data), 33), ]
# taking random samples
data7 <- Donegal_data1[-sample(1:nrow(Donegal_data1),5001 ), ]

data8 <- Healthcare[-sample(1:nrow(Healthcare),261 ), ]
data9 <- Healthcare1[-sample(1:nrow(Healthcare1),6 ), ]

# Column bind of dataset
FinalDataset <- cbind(Donegal_Final, Healthcare_Final,deparse.level = 1)
FinalDataset$County <- NULL
FinalDataset$Country <- NULL
FinalDataset$Postal.Code <- NULL
FinalDataset$Date.of.Sale..dd.mm.yyyy. <- NULL
Lst_change <- FinalDataset

# Graphical Representation of Missing values
library(mice)
md.pattern(Lst_change)
library(VIM)
missing_values <- aggr(Lst_change, prop = FALSE, numbers = TRUE)

#Reordering the columns 
FinalDataset <-FinalDataset[, c(2,3,4,5,6,7,8,9,10,11,12,1,13,14,15,16,17)]


# Removing the unecesscary charcters
FinalDataset$Price..â... <- gsub("â‚¬", "",FinalDataset$Price..â...)

str(FinalDataset)

# creating a copy
Lst_change <- FinalDataset

# converting to numeric values
FinalDataset$Price..â... <- as.factor(FinalDataset$Price..â...)
FinalDataset$Price..â... <- as.numeric(FinalDataset$Price..â...)

FinalDataset$Address <- as.numeric(FinalDataset$Address)
FinalDataset$County <- as.numeric(FinalDataset$County)
FinalDataset$Not.Full.Market.Price <- as.numeric(FinalDataset$Not.Full.Market.Price)
FinalDataset$VAT.Exclusive <- as.numeric(FinalDataset$VAT.Exclusive)
FinalDataset$Description.of.Property <- as.numeric(FinalDataset$Description.of.Property)
FinalDataset$Very.Good <- as.numeric(FinalDataset$Very.Good)
FinalDataset$Good <- as.numeric(FinalDataset$Good)
FinalDataset$Fair <- as.numeric(FinalDataset$Fair)

FinalDataset$Bad <- as.numeric(FinalDataset$Bad)

FinalDataset$Very.Bad <- as.numeric(FinalDataset$Very.Bad)

FinalDataset$X..Very.Good <- as.numeric(FinalDataset$X..Very.Good)

FinalDataset$X..Good <- as.numeric(FinalDataset$X..Good)
FinalDataset$X..Fair <- as.numeric(FinalDataset$X..Fair)
FinalDataset$X..Bad <- as.numeric(FinalDataset$X..Bad)
FinalDataset$X..Very.Bad <- as.numeric(FinalDataset$X..Very.Bad)
FinalDataset$AIRO_AI_ID <- as.numeric(FinalDataset$AIRO_AI_ID)

Health_Property<- FinalDataset

Health_Property1 <-Health_Property[, c(2:17)]
str(Health_Property1)

# correaltion plot
opar <- par(no.readonly = TRUE)
cr <- cor(Health_Property)
library(corrplot)
corrplot(cr,method = "number")
corrplot(corr = cor(Health_Property1),tl.col = "Black",tl.cex = 0.9)


# PCA 
rm(pca)
pca <- prcomp(Health_Property1, center = TRUE, scale. = TRUE)
pca
summary(pca)

# EigenValues and Variances 

library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values

# Plot PCA and variables
fviz_eig(pca,addlabels = TRUE, ylim = c(0,50))

pca_for_variables <- get_pca_var(pca)
pca_for_variables

library("corrplot")
corrplot(pca_for_variables$cor, is.corr = FALSE)


fviz_pca_var(pca,col.var = "black")


head(pca_for_variables$cos2,10)

fviz_cos2(pca,choice = "var",axes = 1:2)


fviz_pca_var(pca,col.var = "cos2",
             gradient.cols = c("red","Blue","Green"),
             repel = TRUE) 
head(pca_for_variables$contrib,10)


fviz_contrib(pca,choice = "var",axes = 1,top = 20)

fviz_contrib(pca,choice = "var",axes = 2,top = 20)

fviz_contrib(pca,choice = "var",axes = 1:5,top = 10)

fviz_pca_ind(pca,
             axes = c(1,2), col.var ="contrib",
             geom.ind = "point",
             col.ind = Lst_change$VAT.Exclusive,
             palette = c("red","green"),
             addEllipses = TRUE,
             legend.title ="Vat")

fviz_pca_biplot(pca,
                col.ind = Lst_change$County, palette = "jco",
                addEllipses = TRUE, label = "var",
                col.var = "black",repel = TRUE,
                legend.title ="County")

# Plotting Histogram

library(ggplot2)
library(viridis)
str(Health_Property)

hist(FinalDataset$Very.Good, main = "Histogram for ", xlab = "")
hist(FinalDataset$Address, main="Histogram For Address", xlab = "Address",col="green", probability = TRUE)
hist(FinalDataset$County, main="Histogram For county", xlab = "county",col="green", probability = TRUE)
hist(FinalDataset$X..Very.Good, main="Histogram For Good Rating", xlab = "Good",col="green", probability = TRUE)


# Compute descriptive statistics 
install.packages("pastecs")
library(pastecs)
Descriptive <- stat.desc(Health_Property[, -5])
Descriptive

#Power Analysis 
library(pwr)
effect_size <- cohen.ES(test = "r", size = "medium")
effect_size

power_analysis <- pwr.r.test( r = 0.3,
                              sig.level = 0.05, 
                                power = 0.95, 
                            alternative = "two.sided")
power_analysis
plot(power_analysis)

#
normality_test <- shapiro.test(Health_Property$Price..â...)
normality_test
attributes(normality_test)
normality_test$p.value

# Hypothesis Testing 
hypo <- cor.test(Health_Property$County,Health_Property$X..Fair, method = 'pearson', exact = FALSE)
hypo


