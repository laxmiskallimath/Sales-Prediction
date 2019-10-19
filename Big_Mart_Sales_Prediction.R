# Big Mart Sales Prediction

# Set working directory
setwd('E:/analytics vidya')

# Loading Packages
library(data.table)# used for reading and manipulation of data

library(dplyr)# used for data manipulation and joining

library(ggplot2)# used for ploting

library(caret)# used for modeling 

library(corrplot)   # used for making correlation plot

library(xgboost)    # used for building XGBoost model

# install.packages('cowplot')
library(cowplot)    # used for combining multiple plots 

# The Train file contains 11 independent variables and 1 target variable,
# i.e., Item_Outlet_Sales.

# The Test file also contains the same set of independent variables, 
# but there is no target variable because that is what we have to predict.

# The Sample Submissions file contains the format in which we have to
# submit our predictions.

# We will use fread() function of data.table package to read the datasets.

# train_csv = read.csv('Train_UWu5bXk.csv')
# test_csv = read.csv('Test_u94Q5KV.csv')

train = fread("Train_UWu5bXk.csv")

test = fread("Test_u94Q5KV.csv") 

submission = fread("SampleSubmission_TmnO39y.csv")

# Initially we should understand our raw data thoroughly, i.e., 
# we should explore the no. of features/columns and rows, datatype
# of the features, feature names and so on. It helps in working with
# the data in the next stages.

# Let's quicky check the dimensions of our data, i.e., columns and rows.
dim(train)

dim(test)

# train dataset has 8523 rows and 12 features and test has 5681 rows and 11 columns. 
# train has 1 extra column which is the target variable. 
# We will predict this target variable for the test dataset later


# We will take a quick glance over the feature names of train and test datasets.
names(train)

names(test)

# Item_Outlet_Sales is present in train but not in test dataset because 
# this is the target variable that we have to predict.


# In R, we have a pretty handy function called str(). 
# It gives a short summary of all the features present in a dataframe. 
# Let's apply it on train and test data.
str(train)

str(test)

# As we can see, there are 4 numeric and 7 categorical variables.


# Combine Train and Test
# # To explore data in any data science competition, it is advisable to append test data
# to the train data. Combining train and test sets saves a lot of time and effort because
# if we have to make any modification in the data, we would make the change only in the
# combined data and not in train and test data separately.
# Later we can always split the combined data back to train and test.

# # For example, if we wish to multiply Item_Fat_Content variable by 100, 
# we can do it for the train and test data separately or we can do the same
# operation once for the combined dataset.
# # The latter approach is more efficient when there are a lot of changes to be made.
# So, we will go ahead combine both train and test data and will carry out data
# visualization, feature engineering, one-hot encoding, and label encoding. 
# Later we would split this combined data back to train and test datasets.


#  Before that lets create target variable in test set there by we can easily combine 
# both test and train dtaset
test$Item_Outlet_Sales <- NA

#  now combine using rbind function
combi = rbind(train,test)

# Lets check dimensions now
dim(combi)

# Exploratory Data Analysis ---------------

# After understanding the dimensions and properties of data,
# we have to deep dive and explore the data visually. 
# It helps us in understanding the nature of data in terms of
# distribution of the individual variables/features, finding missing values, 
# relationship with other variables and many other things.


# Let's start with univariate EDA. It involves exploring variables individually.
# We will try to visualize the continuous variables using histograms and categorical 
# variables using bar plots.


# Target Variable
# Since our target variable is continuous, 
# we can visualise it by plotting its histogram.

names(train)

# Target Variable
# Since our target variable is continuous, 
# we can visualise it by plotting its histogram.
ggplot(train)+geom_histogram(aes(train$Item_Outlet_Sales),binwidth =100,fill='darkgreen')+xlab('Item_Outlet_sales')

# As you can see, it is a right skewd variable
# and would need some data transformation to treat its skewness.

# Independent Variables (numeric variables)
# Now let's check the numeric independent variables. 
# We'll again use the histograms for visualizations
# because that will help us in visualizing the distribution of the variables.

# Item_weight,Item_visibilty,Item_MRP
p1 = ggplot(combi)+geom_histogram(aes(Item_Weight),binwidth = 0.5,fill='aquamarine4')
p1
p2 = ggplot(combi)+geom_histogram(aes(Item_Visibility),binwidth = 0.005,fill='blue')
p3 = ggplot(combi)+geom_histogram(aes(Item_MRP),binwidth = 1,fill = 'darkgreen')
p3

# Lets use plot_grid() function to combine all these three plots 
library(cowplot)
plot_grid(p1,p2,p3,nrow = 1)

# Observations :
# 1)There seems to be no clear-cut pattern in Item_Weight.
# 2) Item_Visibility is right-skewed and should be transformed to curb its skewness.
# 3) We can clearly see 4 different distributions for Item_MRP. 
# It is an interesting insight.



# Now lets visualise Independent Variables (categorical variables)
# Catgorical Variables : Item_Identifier,Item_Fat_Content,Item_Type, Outlet_Identifier
# Outlet_Size ,Outlet_Location_Type,   Outlet_Type,

# Now we'll try to explore and gain some insights from the categorical variables. 
# A categorical variable or feature can have only a finite set of values. 
unique(combi$Item_Identifier)# It has many set of values eventhough it is categorical variable
# we are not visualizing this variable

# Let's first plot Item_Fat_Content.
# x <- combi %>% group_by(Item_Fat_Content)%>% summarise(Count=n())

ggplot(combi%>%group_by(Item_Fat_Content)%>%summarise(Count = n()))+
  geom_bar(aes(Item_Fat_Content,Count),stat = 'identity',fill='coral1')

# In the figure above, 'LF', 'low fat', and 'Low Fat' are the same 
# category and can be combined into one. Similarly we can be done for
# 'reg' and 'Regular' into one. After making these corrections we'll 
# plot the same figure again.

table(combi$Item_Fat_Content)

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"]= "Low Fat" 

# Lets plot barplot now 
x = combi%>%group_by(Item_Fat_Content)%>%summarise(Count = n())

ggplot(x)+geom_bar(aes(Item_Fat_Content,Count),stat = 'identity',fill='violet')

# Now let's check the other categorical variables.

# plot for Item_Type
table(combi$Item_Type)

p4 = ggplot(combi%>%group_by(Item_Type)%>%summarise(Count=n()))+
     geom_bar(aes(Item_Type,Count),stat = 'identity',fill='coral1')+xlab("")+
     geom_label(aes(Item_Type,Count,label=Count),vjust = 0.5)+
     theme(axis.text.x = element_text(angle = 45,hjust = 1))#to add the text +
     ggtitle('Item_type')

# plot for Outlet_Identifier
p5 = ggplot(combi%>%group_by(Outlet_Identifier)%>%summarise(Count = n()))+
  geom_bar(aes(Outlet_Identifier,Count),stat = 'identity',fill='coral1')+
  geom_label(aes(Outlet_Identifier,Count,label = Count),vjust = 0.5)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))

# plot for Outlet_Size
p6 = ggplot(combi%>% group_by(Outlet_Size)%>%summarise(Count=n()))+
  geom_bar(aes(Outlet_Size,Count),stat = 'identity',fill='coral1')+
  geom_label(aes(Outlet_Size,Count,label=Count),vjust=0.5)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))

# Lets plot these together
second_row = plot_grid(p5,p6,nrow = 1)
plot_grid(p4,second_row,ncol = 1)

# plot_grid(p4,p5,p6,nrow = 2,ncol = 2)

# In Outlet_Size's plot, for 4016 observations, 
# Outlet_Size is blank or missing.We will check for 
# this in the bivariate analysis to substitute the missing 
# values in the Outlet_Size.

# We'll also check the remaining categorical variables.
# plot for Outlet_Establishment_Year
table(combi$Outlet_Establishment_Year)
str(combi$Outlet_Establishment_Year)

p7 = ggplot(combi%>% group_by(Outlet_Establishment_Year)%>%summarise(Count=n()))+
  geom_bar(aes(factor(Outlet_Establishment_Year),Count),stat = 'identity',fill='coral1')+
  geom_label(aes(factor(Outlet_Establishment_Year),Count,label=Count),vjust=0.5)+xlab("Outlet_Establishment_Year")+
  theme(axis.text.x = element_text(size = 8.5))

# plot for Outlet_Type
p8 = ggplot(combi%>%group_by(Outlet_Type)%>%summarise(Count=n()))+
  geom_bar(aes(Outlet_Type,Count),stat='identity',fill='coral1')+
  geom_label(aes(factor(Outlet_Type),Count,label=Count),vjust=0.5)+
  theme(axis.text.x = element_text(size = 8.5))

# ploting both plots together
plot_grid(p7,p8,ncol = 2)

# Observations

# Lesser number of observations in the data for the outlets established
# in the year 1998 as compared to the other years.
# Supermarket Type 1 seems to be the most popular category of Outlet_Type.


# After looking at every feature individually, 
# let's now do some bivariate analysis. Here we'll
# explore the independent variables with respect to the target variable.
# The objective is to discover hidden relationships between the
# independent variable and the target variable and use those findings
# in missing data imputation and feature engineering in the next module.

# We will make use of scatter plots for the continuous or numeric 
# variables and violin plots for the categorical variables.
  
train = combi[1:nrow(train)] # extracting train data from the combined data

# Target Variable vs Independent Numerical Variables
# Let's explore the numerical variables first.

# Item_Weight vs Item_Outlet_Sales 
p9 =ggplot(train)+geom_point(aes(Item_Weight,Item_Outlet_Sales),colour='darkviolet',alpha=0.3)+
  theme(axis.title = element_text(size = 8.5))


# Item_Visibility vs Item_Outlet_Sales 
p10 = ggplot(train)+geom_point(aes(Item_Visibility,Item_Outlet_Sales),colour='darkviolet',alpha=0.3)+
  theme(axis.title = element_text(size=8.5))


# Item_MRP vs Item_Outlet_Sales 
p11 = ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "darkviolet", alpha = 0.3)+  
  theme(axis.title = element_text(size = 8.5))

second_row_2 = plot_grid(p10, p11, ncol = 2)

plot_grid(p9, second_row_2, nrow = 2)

# Observations
# Item_Outlet_Sales is spread well across the entire range of 
# the Item_Weight without any obvious pattern.
# In Item_Visibility vs Item_Outlet_Sales, 
# there is a string of points at Item_Visibility = 0.0 
# which seems strange as item visibility cannot be completely zero. 
# We will take note of this issue and deal with it in the later stages.
# In the third plot of Item_MRP vs Item_Outlet_Sales, we can clearly
# see 4 segments of prices that can be
# used in feature engineering to create a new variable.

# Target Variable vs Independent Categorical Variables
# Now we'll visualise the categorical variables with 
# respect to Item_Outlet_Sales. We will try to check the
# distribution of the target variable across all the categories
# of each of the categorical variable.
# We could have used boxplots here, but instead we'll use the
# violin plots as they show the full distribution of the data. 
# The width of a violin plot at a particular level indicates
# the concentration or density of data at that level.
# The height of a violin tells us about the range of the target variable values.

# # Item_Type vs Item_Outlet_Sales

p12 = ggplot(train)+geom_violin(aes(Item_Type,Item_Outlet_Sales),fill='magenta')+
  theme(axis.text.x = element_text(angle = 45,hjust = 1),
        axis.text = element_text(size=6),axis.title = element_text(size = 8.5))

# Item_Fat_Content vs Item_Outlet_Sales
p13 = ggplot(train)+geom_violin(aes(Item_Fat_Content,Item_Outlet_Sales),fill='magenta')+
  theme(axis.text.x = element_text(angle = 45,hjust = 1),
        axis.text = element_text(size = 8),axis.title = element_text(size = 8.5))

# Outlet_Identifier vs Item_Outlet_Sales
p14 = ggplot(train)+geom_violin(aes(Outlet_Identifier,Item_Outlet_Sales),fill='magenta')+
  theme(axis.text.x = element_text(angle = 45,hjust = 1),axis.text = element_text(size=8),axis.title = element_text(size = 8))
p14

second_row_3 = plot_grid(p13,p14,ncol = 2)
plot_grid(p12,second_row_3,ncol = 1)

# Observations
# Distribution of Item_Outlet_Sales across the categories of Item_Type is 
# not very distinct and same is the case with Item_Fat_Content.
# The distribution for OUT010 and OUT019 categories of Outlet_Identifier 
# are quite similar and very much different from the rest of the categories
# of Outlet_Identifier.

# In the univariate analysis, we came to know about the empty values in 
# Outlet_Size variable. Let's check the distribution of the target 
# variable across Outlet_Size

ggplot(train)+geom_violin(aes(Outlet_Size,Item_Outlet_Sales),fill='magenta')

# The distribution of 'Small' Outlet_Size is almost identical 
# to the distribution of the blank category (first vioin) of 
# Outlet_Size. So, we can substitute the blanks in Outlet_Size with 'Small'.

# Please note that this is not the only way to impute missing values,
# but for the time being we will go ahead and impute the missing values with 'Small'.

# Let's examine the remaining variables.
p15 = ggplot(train)+geom_violin(aes(Outlet_Location_Type,Item_Outlet_Sales),fill='magenta')
p15

p16 = ggplot(train)+geom_violin(aes(Outlet_Type,Item_Outlet_Sales),fill='magenta')
p16

plot_grid(p15,p16,ncol = 1)

# Observations
# Tier 1 and Tier 3 locations of Outlet_Location_Type look similar.
# In the Outlet_Type plot, Grocery Store has most of its data points
# around the lower sales values as compared to the other categories.

# These are the kind of insights that we can extract by visualizing our data.
# Hence, data visualization should be an important part of any kind data analysis.


# Missing Value treatmnet -------------------------------------------------

# Quickly find missing values in a variable.
sum(is.na(combi$Item_Weight))
apply(combi,2,function(x){sum(is.na(x))})

# Imputing Missing Value
# As you can see below, we have missing values in Item_Weight and Item_Outlet_Sales
# Missing data in Item_Outlet_Sales can be ignored since they belong to the test dataset. 
# We'll now impute Item_Weight with mean weight based on the Item_Identifier variable.
missing_index = which(is.na(combi$Item_Weight))

for(i in missing_index){
  item = combi$Item_Identifier[i]
  combi$Item_Weight[i]=mean(combi$Item_Weight[combi$Item_Identifier==item],na.rm = T)
}

# Now let's see if there is still any missing data in Item_Weight

sum(is.na(combi$Item_Weight))

# 0 missing values! 
# It means we have successfully imputed the missing data in the feature.

# Replacing 0's in Item_Visibility variable
# Similarly, zeroes in Item_Visibility variable can be replaced 
# with Item_Identifier wise mean values of Item_Visibility. 
# It can be visualized in the plot below.
length(combi$Item_Visibility[combi$Item_Visibility==0])

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

# Let's replace the zeroes.

zero_index = which(combi$Item_Visibility == 0)
for(i in zero_index){   
  item = combi$Item_Identifier[i]  
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)  
  }


# After the replacement of zeroes, We'll plot the histogram of Item_Visibility again.
# In the histogram, we can see that the issue of zero item visibility has been resolved.

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)


# Feature Engineering -----------------------------------------------------
# Most of the times, the given features in a dataset are not sufficient to
# give satisfactory predictions. In such cases, we have to create new features 
# which might help in improving the model's performance. Let's try to create some 
# new features for our dataset.

# In this section we will create the following new features:
# Item_Type_new: Broader categories for the variable Item_Type.
# Item_category: Categorical variable derived from Item_Identifier.
# Outlet_Years: Years of operation for outlets.
# price_per_unit_wt: Item_MRP/Item_Weight
# Item_MRP_clusters: Binned feature for Item_MRP.

# We can have a look at the Item_Type variable and classify the 
# categories into perishable and non_perishable as per our understanding 
# and make it into a new feature
unique(combi$Item_Type)

length(unique(combi$Item_Type))#16 items

perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables",
               "Meat", "Seafood")

non_perishable = c("Baking Goods", "Canned", "Frozen Foods", 
                   "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")


# Create a new feature 'Item_Type_new'
combi[,Item_Type_new := ifelse(Item_Type %in% perishable,'perishable',
                               ifelse(Item_Type %in% non_perishable,
                                      'non_perishable','not_sure'))]

# Let's compare Item_Type with the first 2 characters of Item_Identifier
# i.e., 'DR', 'FD', and 'NC'. These identifiers most probably stand for 
# drinks, food, and non-consumable.
# head(combi$Item_Type)
# head(combi$Item_Type_new)
# head(combi$Item_Identifier)
# head(substr(combi$Item_Identifier,1,2))

table(combi$Item_Type,substr(combi$Item_Identifier,1,2))

# Based on the above table we can create a new feature. Let's call it Item_category
combi[,Item_category := substr(combi$Item_Identifier,1,2)]

# Alternative method
# combi$Item_category1<- substr(combi$Item_Identifier,1,2)

# We will also change the values of Item_Fat_Content wherever Item_category is 'NC' 
# because non-consumable items cannot have any fat content.

# We will also create a couple of more features - Outlet_Years (years of operation)
# and price_per_unit_wt (price per unit weight).
length(combi$Item_Fat_Content[combi$Item_category =='NC' ])

combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible" 

# years of operation for outlets 
combi[,Outlet_Years:=2013-Outlet_Establishment_Year]

combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year) 

# Price per unit weight 

combi[,price_per_unit_wt:=Item_MRP/Item_Weight]

# Earlier in the Item_MRP vs Item_Outlet_Sales plot,
# we saw Item_MRP was spread across in 4 chunks. 
# Now let's assign a label to each of these chunks and
# use this label as a new variable.

# creating new independent variable - Item_MRP_clusters 

# creating new independent variable - Item_MRP_clusters 
combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st",
                          ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd", 
                          ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]



# Encoding Categorical Variables

# Why encoding categorical variables is essential?

# Most of the machine learning algorithms produce better result with 
# numerical variables only. 
# So, it is essential to treat the categorical variables present in the data.
# One thing that can be done is to completely remove the categorical variables,
# but that would lead to enormous loss of information. 
# Fortunately we have smarter techniques to deal with the categorical variables.

# In this stage, we will convert our categorical variables into numerical ones.
# We will use 2 techniques - Label Encoding and One Hot Encoding.


# 1)Label encoding simply means converting each category in a variable to a number.
# It is more suitable for ordinal variables - categorical variables with some order.

# 2) In One hot encoding, each category of a categorical variable is converted into a 
# new binary column (1/0).


# Label encoding for the categorical variables ----------------------------

# We will label encode Outlet_Size and Outlet_Location_Type as these are ordinal variables.

combi[,Outlet_Size_num := ifelse(Outlet_Size == 'Small',0,
                                 ifelse(Outlet_Size == 'Medium',1,2))]

# combi$Outlet_Size_num_1<- ifelse(combi$Outlet_Size == 'Small',0,
#                                 ifelse(combi$Outlet_Size == 'Medium',1,2))


combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type=='Tier 3',0,
                                          ifelse(Outlet_Location_Type == 'Tier 2',1,2))]


# removing categorical variables after label encoding 
combi[,c('Outlet_Size','Outlet_Location_Type'):=NULL]

# One hot encoding for the categorical variable

ohe = dummyVars('~.',data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")],fullRank = T)

ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")])) 

combi =cbind(combi[,'Item_Identifier'],ohe_df)


# PreProcessing Data ------------------------------------------------------

# What is Data PreProcessing?
# In simple words, pre-processing refers to the transformations applied to your 
# data before feeding it to the algorithm.

# It invloves further cleaning of data, data transformation, 
# data scaling and many more things.


# For our data, we will deal with the skewness and scale the numerical variables
# Skewness in variables is undesirable for predictive modeling.
# Some machine learning methods assume normally distributed data and a skewed
# variable can be transformed by taking its log,square root, or cube root so as
# to make its distribution as close to normal distribution as possible. 


# In our data, variables Item_Visibility and price_per_unit_wt are highly skewed.
# So, we will treat their skewness with the help of log transformation.
combi[,Item_Visibility := log(Item_Visibility + 1)] # log + 1 to avoid division by zero 
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]


# Alterate method 
# combi$Item_Visibility <- log(combi$Item_Visibility+1)


# Scaling numeric predictors
# Let's scale and center the numeric variables to make them have a mean of zero,
# standard deviation of one and scale of 0 to 1.
# Scaling and centering is required for linear regression models.


# index of numeric features
num_vars = which(sapply(combi,is.numeric))
num_vars

# alternate method
# cnames <- select_if(combi,is.numeric)
# num = names(cnames)

num_vars_names = names(num_vars)
num_vars_names

combi_numeric = combi[,setdiff(num_vars_names,"Item_Outlet_Sales"),with=F]

prep_num = preProcess(combi_numeric, method=c("center", "scale"))
prep_num

combi_numeric_norm = predict(prep_num, combi_numeric)

combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables

combi = cbind(combi, combi_numeric_norm)

# Splitting the combined data combi back to train and test set.
train <- combi[1:nrow(train)]
test <- combi[(nrow(train)+1):nrow(combi)]

test[,Item_Outlet_Sales := NULL] # removing Item_Outlet_Sales as it contains only NA for test dataset


# Correlated Variables
# Let's examine the correlated features of train dataset. 
# Correlation varies from -1 to 1.
# negative correlation: < 0 and >= -1
# positive correlation: > 0 and <= 1
# no correlation: 0

# It is not desirable to have correlated features if we are using linear regressions.

cor_train <- cor(train[,-c("Item_Identifier")])

cor_train

corrplot(cor_train,method = 'pie',type = 'lower',tl.cex = 0.9)

# library(corrgram)
# 
# corrgram(cor_train,order = F,upper.panel = panel.pie,
#          text.panel = panel.txt,main="Correlation plot for numeric variables")


# The correlation plot above shows correlation between all 
# the possible pairs of variables in out data. 

# The correlation between any two variables is represented by a pie.
# A blueish pie indicates positive correlation and reddish 
# pie indicates negative correlation. 
# The magnitude of the correlation is denoted by the area covered by the pie.

# Variables price_per_unit_wt and Item_Weight are highly correlated as the
# former one was created from the latter.

# Similarly price_per_unit_wt and Item_MRP are highly correlated for the same reason.


# Model Building ----------------------------------------------------------
# Finally we have arrived at most interesting stage of the whole process 
# - predictive modeling. 
# We will start off with the simpler models and gradually move on to 
# more sophisticated models. 
# We will start with the simpler linear models and then move over to more 
# complex models like RandomForest and XGBoost.

# We will build the following models in the next sections.

# Linear Regression
# Lasso Regression
# Ridge Regression
# RandomForest
# XGBoost

# Evaluation Metrics for Regression
# The process of model building is not complete without evaluation of model's 
# performance. 
# That's why we need an evaluation metric to evaluate our model. 
# Since this is a regression problem, we can evaluate our models 
# using any one of the following evaluation metrics:

# Mean Absolute Error (MAE) is the mean of the absolute value of the errors:
# Mean Squared Error (MSE) is the mean of the squared error
# Root Mean Squared Error (RMSE) is the square root of the mean of the squared errors:


# Linear Regression -------------------------------------------------------

# Linear regression is the simplest and most widely used statistical technique
# # for predictive modeling.
# let's build our linear regression model with all the variables.
# We will use 5-fold cross validationin all the models we are going to build. 
# Basically cross vaidation gives an idea as to how well a model generalizes 
# to unseen data.

# Building Model

linear_reg_model <- lm(Item_Outlet_Sales~.,data = train[,-c("Item_Identifier")])

# Making Predictions on test Data

# preparing dataframe for submission and writing it in a csv file 

submission$Item_Outlet_Sales <- predict(linear_reg_model,test[,-c("Item_Identifier")])

write.csv(submission, "Linear_Reg_submit.csv", row.names = F)

# We have got an RMSE of 1202.33 on the public leaderboard,
# but this score has been calculated by using only the 25% 
# (public) of the test data (see evaluation metric) and we have 
# no idea how this model will perform on the other 75% (private) of the test data. 
# So, there has to be a system in place for us to check generalizability of our model,
# in other words, how consistently our model performs at unseen data or new data.

# To check how robust our model is to unseen data, we'll use Cross Validation.
# It is a technique which involves reserving a particular sample of a dataset 
# on which you do not train the model. 
# Later, you test your model on this sample before finalizing it. 
# Some of the common methods for cross validation are listed below:

# The validation set approach
# k-fold cross validation
# Leave one out cross validation (LOOCV)


# Lasso Regression Model --------------------------------------------------
set.seed(1235)

my_control <- trainControl(method = 'cv',number = 5)

Grid = expand.grid(alpha=1,lambda=seq(0.001,0.1,by=0.0002))

lasso_linear_reg_model = train(x=train[,-c("Item_Identifier", "Item_Outlet_Sales")],
                               y=train$Item_Outlet_Sales,method = 'glmnet',
                               trControl = my_control,tuneGrid = Grid)


# Mean validation score: 1130.02

# Leaderboard score: 1202.26


# Ridge Regression --------------------------------------------------------
set.seed(1236)

my_control <- trainControl(method = 'cv',number = 5)

Grid = expand.grid(alpha = 0,lambda=seq(0.001,0.1,by=0.0002))

ridge_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
                             y = train$Item_Outlet_Sales,                     
                             method='glmnet', trControl= my_control, tuneGrid = Grid)

# Mean validation score: 1135.08

# Leaderboard score: 1219.08


# Random Forest
set.seed(1237)

my_control <- trainControl(method = 'cv',number = 5)# 5-fold CV

tgrid = expand.grid(.mtry = c(3:10),
                    .splitrule = 'variance',
                    .min.node.size = c(10,15,20))

rf_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
               y = train$Item_Outlet_Sales,
               method='ranger',trControl= my_control,
               tuneGrid = tgrid,
               num.trees = 400,importance = "permutation")

# Mean validation score: 1088.05

# Leaderboard score: 1157.25

# Our score on the leaderboard has improved considerably by using RandomForest.
# Now let's visualize the RMSE scores for different tuning parameters.

# Best Model Parameters
plot(rf_mod)

# As per the plot shown above, the best score is 
# achieved at mtry = 5 and min.node.size = 20.


# Variable Importance
# Let's plot feature importance based on the RandomForest model

plot(varImp(rf_mod))

# As expected Item_MRP is the most important variable in predicting the target variable.
# New features created by us, like price_per_unit_wt, Outlet_Years, Item_MRP_Clusters,
# are also among the top most important variables. 
# This is why feature engineering plays such a crucial role in predictive modeling.


# XGBoost -----------------------------------------------------------------
param_list = list(objective = 'reg:linear',beta=0.01,gamma=1,max_depth=6,subsample=0.8,
                  colsample_bytree=0.5)

dtrain = xgb.DMatrix(data = as.matrix(train[,-c("Item_Identifier", "Item_Outlet_Sales")]),
                     label=train$Item_Outlet_Sales)
dtest = xgb.DMatrix(data = as.matrix(test[,-c('Item_Identifier')]))

# Cross Validation
# We are going to use the xgb.cv() function for cross validation. 
# This function comes with the xgboost package itself. 
# Here we are using cross validation for finding the optimal value of nrounds.
set.seed(112)


xgbcv = xgb.cv(params = param_list,data = dtrain, nrounds = 1000,
               nfold = 5,print_every_n = 10,early_stopping_rounds = 30,
               maximize = F)

# Model Training
# As per the verbose above, we got the best
# validation/test score at the 12th iteration. 
# Hence, we will use nrounds = 430 for building the XGBoost model.

xgb_model <- xgb.train(data = dtrain,params = param_list,nrounds = 12)

# Leaderboard score: 1154.70

# This model has even outperformed the RandomForest model.

# Variable Importance
var_imp <- xgb.importance(feature_names = setdiff(names(train),
                    c("Item_Identifier", "Item_Outlet_Sales")),model = xgb_model)

xgb.plot.importance(var_imp)

# Again the features created by us, like price_per_unit_wt,
# Outlet_Years, Item_MRP_Clusters, are among the top most important variables.





























