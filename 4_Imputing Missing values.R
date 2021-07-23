rm(list = ls()) #clear workspace

library(Hmisc)
setwd("/Users/philippbecker/Documents/Uni SoSe 21/Data Science and Marketing Analytics/Project/DSMA") #set working directory
DailyLevel_data <- read.csv("dsma_df_merged.csv",header=TRUE, na.strings="NULL") #read csv file. Be aware of the parameters, such as the string for NA's. Usually either "" or "\\NA".
cum_u_names=DailyLevel_data$cum_u_names
Nobs=length(cum_u_names)

# replacing the cum_u_names with the count of reviewers' genders
if(0){ # takes some time to run, if you prefer not to wait too long and you have not changed the data, you can use "namelist.RData" to load the results.
  #install.packages("gender")
  library(gender)
  #devtools::install_github("ropensci/gender-data-pkg")
  library(genderdata)
  # best is to do gender extractin in parallel
  library(doParallel)
  
  
  gendersplit=function(x){
    a=max.col(gender(unlist(strsplit(x,",")))[,2:3])
    return(c(male=sum(a==1,na.rm=T),female=sum(a==2,na.rm=T)))
  }
  
  cl=makeCluster(detectCores()/2+2)
  registerDoParallel(cl)
  nameslist=NULL
  for(k in 1:20){
    whichrun=floor(Nobs/20*(k-1)+1):floor(Nobs/20*k)
    a=foreach(i=whichrun,.packages=c("gender"),.noexport = c("DailyLevel_data"),.combine=rbind) %dopar%
      {gendersplit(cum_u_names[i])}
    rownames(a)=NULL
    nameslist=rbind(nameslist,a)
    print(k)
  }
  stopImplicitCluster()
  stopCluster(cl)
  save(file="nameslist.RData",list="nameslist")
}else{
  load("nameslist.RData")  
}

DailyLevel_data=cbind(DailyLevel_data,nameslist)  
DailyLevel_data$cum_u_names=NULL

# store the date in date column and remove the old name
DailyLevel_data$date <- as.Date(DailyLevel_data$date_tip) 
DailyLevel_data$date_tip <- NULL

# make a 0/1 variables out of date_ch and remove the old name
DailyLevel_data$ch_in=0
DailyLevel_data$ch_in[!is.na(DailyLevel_data$date_ch)]=1
DailyLevel_data$date_ch=NULL

# make factors out of chr variables
for(j in 1:ncol(DailyLevel_data)){
  if(typeof(DailyLevel_data[,j])=="character")
    DailyLevel_data[,j]=as.factor(DailyLevel_data[,j])
}

# limit the number of categories to Asian, American, Mexican and Others

cat_s=as.character(DailyLevel_data$business_cat)
new_cat_s=c("Others","Asian", "American", "Mexican")

changed=0
for(k in new_cat_s[-1]){
  cat_s[grepl(k,cat_s)]=k
  changed=changed+grepl(k,cat_s)
}
cat_s[changed==0]="Others"
DailyLevel_data$business_cat=as.factor(cat_s)

# some descriptives of the data
describe(DailyLevel_data)

## now dealing with the missings.

DailyLevel_data1 <- subset(DailyLevel_data,select = -c(business_id,date, name)) # removed this because MICE does not like imputing factors with more than 50 levels

library(mice)

#inspect pattern of missings
md.pattern(DailyLevel_data1)

#Below, the predictormatrix is specified.
#It is a square matrix of size ncol(data) containing 0/1 data specifying the set of predictors to be used for each target column. 
#Rows correspond to target variables (i.e. variables to be imputed), in the sequence as they appear in data. 
#A value of '1' means that the column variable is used as a predictor for the target variable (in the rows). 
#The diagonal of predictorMatrix must be zero.
predictorMatrix <- matrix(0,nrow = ncol(DailyLevel_data1), ncol = ncol(DailyLevel_data1)) # Make a matrix of zeros
predictorMatrix[c(1,2,3,5,6,8,9,11,12,13,14,15),c(7,4,10)] <- 1 #variables "startyear","birthyear","gender","segment" can be explained by variables 1:8
#predictorMatrix[1:8,31] <- 1 #variable 31 (daysdiff) can be explained by variables 1:8
#predictorMatrix[14:30,9:12] <- 1 #variables 9:12 can be explained by variables 14:30
#predictorMatrix[14:30,31] <- 1 #variable 31 can be explained by variables 14:30
diag(predictorMatrix) <- 0 #diagonal must be zero

#impute data
DailyLevel_data_imputed <- mice(DailyLevel_data1, predictorMatrix = predictorMatrix, m=5, maxit = 50, seed = 500)
summary(DailyLevel_data_imputed)

#get one of the complete data sets ( 2nd out of 5)
DailyLevel_data_complete_data <- complete(DailyLevel_data_imputed,5)

#the complete data sets can be used to estimate your model of choice
#and the results of all 5 models can be combined as in the earlier example
DailyLevel_data_complete_data$date=DailyLevel_data$date
write.csv(DailyLevel_data_complete_data, file="DailyLevel_data_Imputed.csv")
