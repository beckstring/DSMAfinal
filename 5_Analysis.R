rm(list = ls()) #clear workspace
library(dplyr)
library(doParallel)
library(caret)
library(smotefamily) #Balancing datasets
library(tidyverse) #Basic data manipulation syntax; ggplot for data visualisation
library(mice) #Mice for filling NAs
library(rsample)
library("CustomerScoringMetrics")
library(MLmetrics)
library("kernlab")
library("smotefamily") #Smote Data balancing
library(corrplot) #Correlation Matrices
library(RColorBrewer) #Color palettes for plots

setwd("/Users/philippbecker/Documents/Uni SoSe 21/Data Science and Marketing Analytics/Project/Final Documents/Final Code") #set working directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#source("DailyLevelData_analysis_functions.r")


# ----
# Loading data "do not run", unless you know what you are doing.
if(0){
  #load data
  yelp_data <- read.csv("DailyLevel_data_Imputed.csv",header=TRUE,skipNul = T) #read csv file
  yelp_data$date <- as.Date(yelp_data$date)
  yelp_data$X=NULL
  
  #---- read the temperature data
  wear=extractweather(yelp_data)
  
  # take the averages across stations for each coordinate
  weather=weardailyavg(wear)
  
  
  
  dates=sort(unique(yelp_data$date))
  weatherstations=as.data.frame(t(sapply(weather,function(x){colMeans(x$range)})))
  
  # adding weather data to yelp_data
  if(0){
    stations_by=t(apply(yelp_data[,c("business_lat","business_long")],1,
                        function(x){a=sort((x[1]-weatherstations$rangelat)^2+
                                             (x[2]-weatherstations$rangelong)^2,index.return=T)
                        return(a$ix[1:50])})) # finding the 50 closest stations
    
    # add for example, temperature forecasts to the weather data
    for(i in 1:length(weather)){
      if(nrow(weather[[i]]$data)==0)
        next
      store_weather=weather[[i]]$data
      store_weather$TOBS_1=c(store_weather$TOBS[2:nrow(store_weather)],NA)
      store_weather$TOBS_2=c(store_weather$TOBS[3:nrow(store_weather)],NA,NA)
      store_weather$TOBS_3=c(store_weather$TOBS[4:nrow(store_weather)],NA,NA,NA)
      store_weather$TOBS_4=c(store_weather$TOBS[5:nrow(store_weather)],NA,NA,NA,NA)
      weather[[i]]$data=store_weather
    }
    weatherinf=colnames(store_weather)[-1] # which weather variables are available?
    
    yelp_data_weather=NULL
    for(i in 1:length(weather)){
      k=1 # start with the closest station
      stores_in=stations_by[,k]==i
      if(sum(stores_in)==0)
        next
      store_weather=weather[[i]]$data
      
      temp=yelp_data[stores_in,]
      temp=merge(temp,store_weather,by.x="date",by.y="DATE",all.x=T)
      yelp_data_weather=rbind(yelp_data_weather,temp)
      print(i)
    }
    
    # now deal with the missings, by going to the next possible station
    temp_indx=is.na(yelp_data_weather[,"TOBS"])|is.na(yelp_data_weather[,"PRCP"])
    k_changed=NULL
    for(i in which(temp_indx)){
      temp_date=yelp_data_weather[i,]$date
      for(k in 2:ncol(stations_by)){
        temp=weather[[stations_by[i,k]]]$data
        if(!is.na(as.numeric(temp[temp$DATE==temp_date,"TOBS"]))&!is.na(as.numeric(temp[temp$DATE==temp_date,"PRCP"])))
          break
      }
      k_changed=c(k_changed,k)
      
      yelp_data_weather[i,weatherinf]=temp[temp$DATE==temp_date,-1]
      #print(i)
    }
    
    # add weekends and quarters
    temp=weekdays(yelp_data_weather$date,abbreviate = T)
    yelp_data_weather$WE=temp=="Sa"|temp=="So"
    
    yelp_data_weather$Quarter=as.factor(quarters(yelp_data_weather$date))
    
    #save(file="yelp_data_weather.RData",list=c("yelp_data_weather"))
    #write.csv(yelp_data_weather,file="yelp_data_weather.csv")
    
  }
  
}
# END OF:Loading data "do not run", unless you know what you are doing.

# ----
# Importing and adjusting the yelp-data + weather data
yelp_data_weather=read.csv(file="yelp_data_weather.csv")
#load("yelp_data_weather.RData")

# Load own data
yelp_pb <- read.csv("dsma_df_merged.csv",header=TRUE, na.strings="NULL")

#Load attribute data
attributes <- read.csv("attributes_cleaned_2.csv", na.strings="NA", header=TRUE, sep=";")
summary(attributes)
#Select only attributes with diverse distribution
attributes <- select(attributes, business_id, lot, street, BikeParking, GoodForKids, RestaurantsTakeOut, OutdoorSeating, WiFi, RestaurantsReservations, casual, dinner, lunch)

#Fill missings in na's (could be done at later stage using other variables)
init = mice(attributes, maxit=0)
meth = init$method
predM = init$predictorMatrix
predM[, c("business_id")]=0

MiceImputedData <- mice(attributes, m=2,  method="pmm", predictorMatrix=predM, seed = 500)
summary(MiceImputedData)

attributes<- complete(MiceImputedData,2)
##########

# Create new key for merging
yelp_data_weather <- within(yelp_data_weather,  key <- paste( date, business_lat, business_long, cum_max_us_fans ,sep="_"))

# Do the same for own data
yelp_pb <- within(yelp_pb,  key <- paste( date, business_lat, business_long, cum_max_us_fans ,sep="_"))

#Join attributes using business_id

yelp_pb <- yelp_pb %>%
  inner_join(attributes, by = "business_id")

# Keep only new columnss
yelp_pb <- yelp_pb[,20:41]

# Check whether identifies is unique
uniq <- unique(yelp_data_weather$key)
# Count length of df
length(uniq)

# And for the other df
uniq2 <- unique(yelp_pb$key)
# Count length of df
length(uniq2)

##########


# Merge the two data frames
yelp_merged <- merge(yelp_data_weather, yelp_pb, by='key')

# Drop key column again
yelp_merged$key <- NULL


#Write as csv
write.csv(yelp_merged, "yelp_merged_final.csv", row.names= FALSE)
###########


########### Data Cleaning ##################################################################

yelp_merged <- read.csv("yelp_merged_final.csv")

summary(yelp_merged)

# Convert attributes to the right type
yelp_merged$lot <- as.logical(ifelse(yelp_merged$lot=="True", TRUE,FALSE))
yelp_merged$street <- as.logical(ifelse(yelp_merged$street=="True", TRUE,FALSE))
yelp_merged$BikeParking <- as.logical(ifelse(yelp_merged$BikeParking=="True", TRUE,FALSE))
yelp_merged$GoodForKids <- as.logical(ifelse(yelp_merged$GoodForKids=="True", TRUE,FALSE))
yelp_merged$RestaurantsTakeOut <- as.logical(ifelse(yelp_merged$RestaurantsTakeOut=="True", TRUE,FALSE))
yelp_merged$OutdoorSeating <- as.logical(ifelse(yelp_merged$OutdoorSeating=="True", TRUE,FALSE))
yelp_merged$WiFi <- as.logical(ifelse(yelp_merged$WiFi=="True", TRUE,FALSE))
yelp_merged$RestaurantsReservations <- as.logical(ifelse(yelp_merged$RestaurantsReservations=="True", TRUE,FALSE))
yelp_merged$casual <- as.logical(ifelse(yelp_merged$casual=="True", TRUE,FALSE))
yelp_merged$dinner <- as.logical(ifelse(yelp_merged$dinner=="True", TRUE,FALSE))
yelp_merged$lunch <- as.logical(ifelse(yelp_merged$lunch=="True", TRUE,FALSE))
yelp_merged$business_open <- as.logical(ifelse(yelp_merged$business_open=="True", TRUE,FALSE))


# Check whether conversion worked
library(dplyr)
glimpse(yelp_merged)


# replace missings for weather data
init = mice(yelp_merged, maxit=0)
meth = init$method
predM = init$predictorMatrix
predM[, c("X","name","date","business_park")]=0

MiceImputedData2 <- mice(yelp_merged, m=2,  method="pmm", predictorMatrix=predM, seed = 500)
summary(MiceImputedData)

yelp_merged<- complete(MiceImputedData,2)


##### Create sub data frames ##################################################
summary(yelp_merged)

### Delete unwanted info
yelp_merged$name <- NULL
yelp_merged$X <- NULL
yelp_merged$date <- NULL
yelp_merged$business_park <- NULL # We use the lot attribute
#yelp_merged$business_lat <- NULL
#yelp_merged$business_long <- NULL

physical_attributes <- c("ch_in", "business_price", "business_open", "business_cat", 
                         "lot", "street", "BikeParking", "GoodForKids", 
                         "RestaurantsTakeOut", "OutdoorSeating",
                         "WiFi","RestaurantsReservations", "casual","dinner",
                         "lunch")

user_characteristics <- c("ch_in", "cum_n_tips", "cum_max_friends", 
                          "cum_max_u_elite", "cum_max_us_fans", 
                          "cum_max_us_rev","male","female")

user_content <- c("ch_in","n_photo","twitter_count_pos", "twitter_count_neu",
                  "twitter_count_neg", "yelp_count_pos","yelp_count_neu",
                  "yelp_count_neg", "avg_stars", "onestar_count","fivestar_count")


exogenous_data <- c("ch_in", "business_lat", "business_long", "WE", "Quarter","PRCP", "SNOW","SNWD","TMAX", "TMIN",
                    "TOBS", "TOBS_1", "TOBS_2", "TOBS_3","TOBS_4")

library("dummies")
# Preprocessing
yelp_merged$WE <- as.numeric(yelp_merged$WE)


# Switch check-in and no check-in
yelp_merged$ch_in <- ifelse(yelp_merged$ch_in == 1,0,1)

# Create sub df's
yelp_sub_total <- data.frame(yelp_merged)
#yelp_sub_total <- dummy.data.frame(yelp_sub_total, names = c("business_cat", "Quarter", "business_price") , sep = ".")

yelp_sub_physical <- yelp_merged[physical_attributes]
#yelp_sub_physical <- dummy.data.frame(yelp_sub_physical, names = c("business_cat","business_price") , sep = ".")

# All numeric
yelp_sub_characteristics <- yelp_merged[user_characteristics]
# All numeric
yelp_sub_content <- yelp_merged[user_content]

yelp_sub_exogenous <- yelp_merged[exogenous_data]
#yelp_sub_exogenous <- dummy.data.frame(yelp_sub_exogenous, names = c("Quarter") , sep = ".")

# Create sub df's csv.
write.csv(yelp_sub_total,"yelp_sub_total.csv")
write.csv(yelp_sub_physical,"yelp_sub_physical.csv")
write.csv(yelp_sub_characteristics,"yelp_sub_characteristics.csv")
write.csv(yelp_sub_content,"yelp_sub_content.csv")
write.csv(yelp_sub_exogenous,"yelp_sub_exogenous.csv")

df_names <- c("yelp_sub_physical","yelp_sub_characteristics","yelp_sub_content","yelp_sub_exogenous","yelp_sub_total")
#df_names <- "yelp_sub_total"
##### Create table for seminar paper ##########################################

library(stargazer)

# Physical
stargazer(yelp_sub_physical, title = "Physical attributes", style = "default", decimal.mark = ".", digits = 1, summary = TRUE,
          median = TRUE ,column.sep.width = "5pt", out = "Physical attributes.html")

# UGM - Characteristics
stargazer(yelp_sub_characteristics, title = "UGM - Characteristics", style = "default", decimal.mark = ".", digits = 1, summary = TRUE,
          median = TRUE ,column.sep.width = "5pt", out = "UGM - Characteristics.html")

# UGM - Content
stargazer(yelp_sub_content, title = "UGM - Content", style = "default", decimal.mark = ".", digits = 1, summary = TRUE,
          median = TRUE ,column.sep.width = "5pt", out = "UGM - Content.html")

# Exogenous data
stargazer(yelp_sub_exogenous, title = "Exogenous data", style = "default", decimal.mark = ".", digits = 1, summary = TRUE,
          median = TRUE ,column.sep.width = "5pt", out = "Exogenous data.html")


##### Descriptive Analysis: Correlation Matrices ##########################################
#Correlation analysis

#Characteristics

M <-cor(yelp_sub_characteristics)
corrplot(M, type="upper", order="hclust",tl.col = 'black',
         col=brewer.pal(n=8, name="RdYlBu"))
#Content
M2 <-cor(yelp_sub_content)
corrplot(M2, type="upper", order="hclust",tl.col = 'black',
         col=brewer.pal(n=8, name="RdYlBu"))

#Exogenous
yelp_num_ex <- as.data.frame(sapply(yelp_sub_exogenous, as.numeric ))
M3 <-cor(yelp_num_ex)
corrplot(M3, type="upper", order="hclust",tl.col = 'black',
         col=brewer.pal(n=8, name="RdYlBu"))

#physical
yelp_num_phys <- as.data.frame(sapply(yelp_sub_physical, as.numeric ))
M4 <-cor(yelp_num_phys)
corrplot(M4, type="upper", order="hclust",tl.col = 'black',
         col=brewer.pal(n=8, name="RdYlBu"))
#All data
yelp_num <- as.data.frame(sapply(yelp_merged, as.numeric ))
M5 <-cor(yelp_num)
corrplot(M4, type="upper", order="hclust", tl.col = 'black',
         col=brewer.pal(n=8, name="RdYlBu"))

summary(yelp_num)


####### Data Analysis Preparation ############################################################

# Store performance information in this data frame 
results_df <- setNames(data.frame(matrix(ncol = 10, nrow = 8)), c("dataset", "model_type", "auc", "auc_training", "aucpr", "accuray_max", "f1_max", "sensitivity_max" ,"precision_max", "training_time"))
final_df <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), c("dataset", "model_type", "auc", "auc_training", "aucpr", "accuray_max", "f1_max", "sensitivity_max" ,"precision_max", "training_time"))
comparison_df <- setNames(data.frame(matrix(ncol = 8, nrow = 24)), c( "model_type","measure","yelp_sub_physical","yelp_sub_characteristics","yelp_sub_content","yelp_sub_exogenous","yelp_sub_total", "yelp_tuned"))


summary(yelp_merged)


library(h2o)
#h2o.shutdown()
h2o.init(nthreads = -1)
glimpse(yelp_merged)

for (file in df_names){
  print(file)
}

for (file in df_names){
  print(file)
  results_df$dataset <- file
  
  yelp_file <- read.csv(paste(file, ".csv", sep =""))
  #yelp_file <- read.csv("yelp_sub_total.csv")
  yelp_file$X <- NULL
  library(rsample)      # data splitting 
  # Create training and testing data
  
  
  # Convert ch_in to factor for random forests
  #yelp_file$ch_in <- as.factor(yelp_file$ch_in)
  set.seed(2023)
  yelp_split <- initial_split(yelp_file, prop = .8)
  yelp_train <- training(yelp_split)
  yelp_test  <- testing(yelp_split)
  
  #smote_data <- SMOTE(yelp_train, select(yelp_train, ch_in), K = 5, dup_size = 0)
  #yelp_train <- smote_data$data
  yelp_train$class <- NULL
  write.csv(yelp_train, "yelp_train.csv",row.names= TRUE)
  write.csv(yelp_test, "yelp_test.csv",row.names= TRUE)
  
  # Select predictors for the models
  x <- names(yelp_file)
  x <- x[x!="ch_in"]
  y <- "ch_in"
  
  
  train <- h2o.importFile("yelp_train.csv")
  test <- h2o.importFile("yelp_test.csv")
  
  train$ch_in <- as.factor(train$ch_in)
  test$ch_in <- as.factor(test$ch_in)
  
  ####### Data Analysis ############################################################
  
  ####### 1) Deep Learning ############################################################
  
  ### Update this section
  
  algo_num <- 1
  model_type <- "Deep Learning"
  
  #######################
  
  # Measure time for training
  train_time_start <- proc.time()  # Start time
  
  # str(yelp_dl) #In case you want to learn about the available methods
  
  
  ### Update this section (Change Model)
  
  # Build and train the model:
  yelp_dl <- h2o.deeplearning(x = x,
                              y = y,
                              distribution = "bernoulli",
                              balance_classes = TRUE,
                              seed = 2021,
                              training_frame = train,
                              validation_frame = test)
  
  
  train_time <- proc.time() - train_time_start  # End time 
  train_time_sec <- train_time['elapsed'] # Get time
  
  #Performance on train to check overfitting
  perf_train <- h2o.performance(yelp_dl,train)
  perf_train
  
  # Eval performance:
  perf <- h2o.performance(yelp_dl,test)
  
  # Get Metrics of interest
  # auc aucpr accuray_max f1_max sensitivity_max precision_max
  # https://www.rdocumentation.org/packages/h2o/versions/3.32.1.3/topics/h2o.metric
  
  #Training AUC
  auc_train <- h2o.auc(perf_train)
  
  # AUC
  auc <- h2o.auc(perf)
  
  # AUCPR
  aucpr <- h2o.aucpr(perf)
  
  # Max accuracy
  temp <- h2o.accuracy(perf)
  accuray_max <- max(temp$accuracy)
  
  # Max f1
  temp <- h2o.F1(perf)
  f1_max <- max(temp$f1)
  
  # Something is off with sensitivity metric, pls check!!
  
  # Max sensitivity
  temp <- h2o.sensitivity(perf)
  sensitivity_max <- max(temp$tpr)
  
  # Max precision
  temp <- h2o.precision(perf)
  precision_max <- max(temp$precision)
  
  
  # Store results in results_df
  results_df$model_type[algo_num] <- model_type
  results_df$auc[algo_num] <- auc
  results_df$auc_training[algo_num] <- auc_train
  results_df$aucpr[algo_num] <- aucpr
  results_df$accuray_max[algo_num] <- accuray_max
  results_df$f1_max[algo_num] <- f1_max
  results_df$sensitivity_max[algo_num] <- sensitivity_max
  results_df$precision_max[algo_num] <- precision_max
  results_df$training_time[algo_num] <- train_time_sec
  
  # Get predictions
  mod <- predict(yelp_dl, test)
  mod_df <- as.data.frame(mod)
  # Get tdl
  tdl <- topDecileLift(mod_df$p1, yelp_test$ch_in)
  
  # Get gini
  gini <- Gini(mod_df$p1, (yelp_test$ch_in))
  
  # Store results for model comparison
  comparison_df$model_type[algo_num] <- model_type
  comparison_df$measure[algo_num] <- "AUC"
  comparison_df$measure[algo_num+1] <- "Lift"
  comparison_df$measure[algo_num+2] <- "Gini coefficient"
  comparison_df[,file][algo_num] <- auc
  #comparison_df[,file][algo_num+1] <- lift$lift[1]
  comparison_df[,file][algo_num+1] <- tdl
  comparison_df[,file][algo_num+2] <- gini
  ####### 1) End of Deep Learning ############################################################
  
  ####### 2) Random forests ############################################################
  
  ### Update this section
  
  algo_num <- 2
  model_type <- "Random forests"
  
  #######################
  
  # Measure time for training
  train_time_start <- proc.time()  # Start time
  
  # str(yelp_dl) #In case you want to learn about the available methods
  
  
  ### Update this section (Change Model)
  
  # Build and train the model:
  yelp_dl <- h2o.randomForest(x = x,
                              y = y,
                              #distribution = "bernoulli", -> Deprecated
                              balance_classes = TRUE,
                              seed = 2022,
                              training_frame = train,
                              validation_frame = test,
                              ntrees = 200,
                              max_depth = 10)
  
  
  train_time <- proc.time() - train_time_start  # End time 
  train_time_sec <- train_time['elapsed'] # Get time
  
  #Performance on train to check overfitting
  perf_train <- h2o.performance(yelp_dl,train)
  perf_train
  
  # Eval performance:
  perf <- h2o.performance(yelp_dl,test)
  perf
  
  #Feature importance
  h2o.varimp_plot(yelp_dl)
  
  #Partial dependence
  #yelp_train_h2o <- as.h2o(yelp_train)
  #h2o.partialPlot(yelp_dl, cols = "business_price", data= yelp_train_h2o)
  
  # Get Metrics of interest
  # auc aucpr accuray_max f1_max sensitivity_max precision_max
  # https://www.rdocumentation.org/packages/h2o/versions/3.32.1.3/topics/h2o.metric
  
  #Training AUC
  auc_train <- h2o.auc(perf_train)
  
  # AUC
  auc <- h2o.auc(perf)
  
  # AUCPR
  aucpr <- h2o.aucpr(perf)
  
  # Max accuracy
  temp <- h2o.accuracy(perf)
  accuray_max <- max(temp$accuracy)
  
  # Max f1
  temp <- h2o.F1(perf)
  f1_max <- max(temp$f1)
  
  # Something is off with sensitivity metric, pls check!!
  
  # Max sensitivity
  temp <- h2o.sensitivity(perf)
  sensitivity_max <- max(temp$tpr)
  
  # Max precision
  temp <- h2o.precision(perf)
  precision_max <- max(temp$precision)
  
  
  # Store results in results_df
  results_df$model_type[algo_num] <- model_type
  results_df$auc[algo_num] <- auc
  results_df$auc_training[algo_num] <- auc_train
  results_df$aucpr[algo_num] <- aucpr
  results_df$accuray_max[algo_num] <- accuray_max
  results_df$f1_max[algo_num] <- f1_max
  results_df$sensitivity_max[algo_num] <- sensitivity_max
  results_df$precision_max[algo_num] <- precision_max
  results_df$training_time[algo_num] <- train_time_sec
  
  # Get predictions
  mod <- predict(yelp_dl, test)
  mod_df <- as.data.frame(mod)
  # Get tdl
  tdl <- topDecileLift(mod_df$p1, yelp_test$ch_in)
  
  # Get gini
  gini <- Gini(mod_df$p1, yelp_test$ch_in)
  
  # Store results for model comparison
  algo_num <- (algo_num + ((algo_num - 1) * 2)) #Needed to update the index due to additional entries
  comparison_df$model_type[algo_num] <- model_type
  comparison_df$measure[algo_num] <- "AUC"
  comparison_df$measure[algo_num+1] <- "Lift"
  comparison_df$measure[algo_num+2] <- "Gini coefficient"
  comparison_df[,file][algo_num] <- auc
  comparison_df[,file][algo_num+1] <- tdl
  comparison_df[,file][algo_num+2] <- gini
  
  ####### 2) End of Random forests ############################################################
  
  
  ####### 3) Generalized Linear Model ############################################################
  
  ### Update this section
  
  algo_num <- 3
  model_type <- "Generalized Linear Model"
  
  #######################
  
  # Measure time for training
  train_time_start <- proc.time()  # Start time
  
  # str(yelp_dl) #In case you want to learn about the available methods
  
  
  ### Update this section (Change Model)
  
  # Build and train the model:
  yelp_dl <- h2o.glm(x = x,
                     y = y,
                     balance_classes = TRUE,
                     seed = 2021,
                     training_frame = train,
                     validation_frame = test)
  
  
  train_time <- proc.time() - train_time_start  # End time 
  train_time_sec <- train_time['elapsed'] # Get time
  
  # Eval performance:
  perf <- h2o.performance(yelp_dl,test)
  perf
  
  #Performance on train to check overfitting
  perf_train <- h2o.performance(yelp_dl,train)
  perf_train
  
  # Get Metrics of interest
  # auc aucpr accuray_max f1_max sensitivity_max precision_max
  # https://www.rdocumentation.org/packages/h2o/versions/3.32.1.3/topics/h2o.metric
  
  #Training AUC
  auc_train <- h2o.auc(perf_train)
  
  # AUC
  auc <- h2o.auc(perf)
  
  # AUCPR
  aucpr <- h2o.aucpr(perf)
  
  # Max accuracy
  temp <- h2o.accuracy(perf)
  accuray_max <- max(temp$accuracy)
  
  # Max f1
  temp <- h2o.F1(perf)
  f1_max <- max(temp$f1)
  
  # Something is off with sensitivity metric, pls check!!
  
  # Max sensitivity
  temp <- h2o.sensitivity(perf)
  sensitivity_max <- max(temp$tpr)
  
  # Max precision
  temp <- h2o.precision(perf)
  precision_max <- max(temp$precision)
  
  
  # Store results in results_df
  results_df$model_type[algo_num] <- model_type
  results_df$auc[algo_num] <- auc
  results_df$auc_training[algo_num] <- auc_train
  results_df$aucpr[algo_num] <- aucpr
  results_df$accuray_max[algo_num] <- accuray_max
  results_df$f1_max[algo_num] <- f1_max
  results_df$sensitivity_max[algo_num] <- sensitivity_max
  results_df$precision_max[algo_num] <- precision_max
  results_df$training_time[algo_num] <- train_time_sec
  
  # Get predictions
  mod <- predict(yelp_dl, test)
  mod_df <- as.data.frame(mod)
  # Get tdl
  tdl <- topDecileLift(mod_df$p1, yelp_test$ch_in)
  
  # Get gini
  gini <- Gini(mod_df$p1, yelp_test$ch_in)
  
  # Store results for model comparison
  algo_num <- (algo_num + ((algo_num - 1) * 2)) #Needed to update the index due to additional entries
  comparison_df$model_type[algo_num] <- model_type
  comparison_df$measure[algo_num] <- "AUC"
  comparison_df$measure[algo_num+1] <- "Lift"
  comparison_df$measure[algo_num+2] <- "Gini coefficient"
  comparison_df[,file][algo_num] <- auc
  comparison_df[,file][algo_num+1] <- tdl
  comparison_df[,file][algo_num+2] <- gini
  
  ####### 3) End of Generalized Linear Model ############################################################
  
  
  ####### 4) Gradient Boosting Model ############################################################
  
  ### Update this section
  
  algo_num <- 4
  model_type <- "Gradient Boosting"
  
  #######################
  
  # Measure time for training
  train_time_start <- proc.time()  # Start time
  
  # str(yelp_dl) #In case you want to learn about the available methods
  
  
  ### Update this section (Change Model)
  
  # Build and train the model:
  yelp_dl <- h2o.gbm(x = x,
                     y = y,
                     distribution = "bernoulli",
                     balance_classes = TRUE,
                     seed = 2021,
                     training_frame = train,
                     validation_frame = test)
  
  
  train_time <- proc.time() - train_time_start  # End time 
  train_time_sec <- train_time['elapsed'] # Get time
  
  #Performance on train to check overfitting
  perf_train <- h2o.performance(yelp_dl,train)
  perf_train
  
  # Eval performance:
  perf <- h2o.performance(yelp_dl,test)
  perf
  
  # Get Metrics of interest
  # auc aucpr accuray_max f1_max sensitivity_max precision_max
  # https://www.rdocumentation.org/packages/h2o/versions/3.32.1.3/topics/h2o.metric
  
  #Training AUC
  auc_train <- h2o.auc(perf_train)
  
  # AUC
  auc <- h2o.auc(perf)
  
  # AUCPR
  aucpr <- h2o.aucpr(perf)
  
  # Max accuracy
  temp <- h2o.accuracy(perf)
  accuray_max <- max(temp$accuracy)
  
  # Max f1
  temp <- h2o.F1(perf)
  f1_max <- max(temp$f1)
  
  # Something is off with sensitivity metric, pls check!!
  
  # Max sensitivity
  temp <- h2o.sensitivity(perf)
  sensitivity_max <- max(temp$tpr)
  
  # Max precision
  temp <- h2o.precision(perf)
  precision_max <- max(temp$precision)
  
  
  # Store results in results_df
  results_df$model_type[algo_num] <- model_type
  results_df$auc[algo_num] <- auc
  results_df$auc_training[algo_num] <- auc_train
  results_df$aucpr[algo_num] <- aucpr
  results_df$accuray_max[algo_num] <- accuray_max
  results_df$f1_max[algo_num] <- f1_max
  results_df$sensitivity_max[algo_num] <- sensitivity_max
  results_df$precision_max[algo_num] <- precision_max
  results_df$training_time[algo_num] <- train_time_sec
  
  
  # Get predictions
  mod <- predict(yelp_dl, test)
  mod_df <- as.data.frame(mod)
  # Get tdl
  tdl <- topDecileLift(mod_df$p1, yelp_test$ch_in)
  
  # Get gini
  gini <- Gini(mod_df$p1, yelp_test$ch_in)
  
  # Store results for model comparison
  algo_num <- (algo_num + ((algo_num - 1) * 2)) #Needed to update the index due to additional entries
  comparison_df$model_type[algo_num] <- model_type
  comparison_df$measure[algo_num] <- "AUC"
  comparison_df$measure[algo_num+1] <- "Lift"
  comparison_df$measure[algo_num+2] <- "Gini coefficient"
  comparison_df[,file][algo_num] <- auc
  comparison_df[,file][algo_num+1] <- tdl
  comparison_df[,file][algo_num+2] <- gini
  
  ####### 4) End of Gradient Boosting Model ############################################################
  
  
  ####### 5) Naive Bayes Model ############################################################
  
  ### Update this section
  
  algo_num <- 5
  model_type <- "Naive Bayes"
  
  #######################
  
  # Measure time for training
  train_time_start <- proc.time()  # Start time
  
  # str(yelp_dl) #In case you want to learn about the available methods
  
  
  ### Update this section (Change Model)
  
  # Build and train the model:
  yelp_dl <- h2o.naiveBayes(x = x,
                            y = y,
                            seed =2021,
                            training_frame = train,
                            validation_frame = test)
  
  
  train_time <- proc.time() - train_time_start  # End time 
  train_time_sec <- train_time['elapsed'] # Get time
  
  #Performance on train to check overfitting
  perf_train <- h2o.performance(yelp_dl,train)
  perf_train
  
  # Eval performance:
  perf <- h2o.performance(yelp_dl,test)
  perf
  
  # Get Metrics of interest
  # auc aucpr accuray_max f1_max sensitivity_max precision_max
  # https://www.rdocumentation.org/packages/h2o/versions/3.32.1.3/topics/h2o.metric
  
  #Training AUC
  auc_train <- h2o.auc(perf_train)
  
  # AUC
  auc <- h2o.auc(perf)
  
  # AUCPR
  aucpr <- h2o.aucpr(perf)
  
  # Max accuracy
  temp <- h2o.accuracy(perf)
  accuray_max <- max(temp$accuracy)
  
  # Max f1
  temp <- h2o.F1(perf)
  f1_max <- max(temp$f1)
  
  # Something is off with sensitivity metric, pls check!!
  
  # Max sensitivity
  temp <- h2o.sensitivity(perf)
  sensitivity_max <- max(temp$tpr)
  
  # Max precision
  temp <- h2o.precision(perf)
  precision_max <- max(temp$precision)
  
  
  # Store results in results_df
  results_df$model_type[algo_num] <- model_type
  results_df$auc[algo_num] <- auc
  results_df$auc_training[algo_num] <- auc_train
  results_df$aucpr[algo_num] <- aucpr
  results_df$accuray_max[algo_num] <- accuray_max
  results_df$f1_max[algo_num] <- f1_max
  results_df$sensitivity_max[algo_num] <- sensitivity_max
  results_df$precision_max[algo_num] <- precision_max
  results_df$training_time[algo_num] <- train_time_sec
  
  # Get predictions
  mod <- predict(yelp_dl, test)
  mod_df <- as.data.frame(mod)
  # Get tdl
  tdl <- topDecileLift(mod_df$p1, yelp_test$ch_in)
  
  # Get gini
  gini <- Gini(mod_df$p1, yelp_test$ch_in)
  
  
  # Store results for model comparison
  algo_num <- (algo_num + ((algo_num - 1) * 2)) #Needed to update the index due to additional entries
  comparison_df$model_type[algo_num] <- model_type
  comparison_df$measure[algo_num] <- "AUC"
  comparison_df$measure[algo_num+1] <- "Lift"
  comparison_df$measure[algo_num+2] <- "Gini coefficient"
  comparison_df[,file][algo_num] <- auc
  comparison_df[,file][algo_num+1] <- tdl
  comparison_df[,file][algo_num+2] <- gini
  
  
  ####### 5) End of Naive Bayes Model ############################################################
  
  
  ####### 6) Stacked Ensembles Model ############################################################
  
  ### Update this section
  
  algo_num <- 6
  model_type <- "Stacked Ensembles"
  nfolds <- 5
  #######################
  
  # Measure time for training
  train_time_start <- proc.time()  # Start time
  
  # str(yelp_dl) #In case you want to learn about the available methods
  
  
  ### Update this section (Change Model)
  
  # Build and train the model:
  yelp_gbm <- h2o.gbm(x = x,
                      y = y,
                      distribution = "bernoulli",
                      seed = 2021,
                      training_frame = train,
                      ntrees = 10,
                      max_depth = 3,
                      min_rows = 2,
                      learn_rate = 0.2,
                      nfolds = nfolds,
                      keep_cross_validation_predictions = TRUE)
  
  yelp_rf <- h2o.randomForest(x = x,
                              y = y,
                              seed = 2021,
                              training_frame = train,ntrees = 50,
                              nfolds = nfolds,
                              keep_cross_validation_predictions = TRUE)
  
  ensemble <- h2o.stackedEnsemble(x = x,
                                  y = y,
                                  seed = 2021,
                                  training_frame = train,
                                  base_models = list(yelp_gbm, yelp_rf))
  
  
  train_time <- proc.time() - train_time_start  # End time 
  train_time_sec <- train_time['elapsed'] # Get time
  
  #Performance on train to check overfitting
  perf_train <- h2o.performance(ensemble,train)
  perf_train
  
  # Eval performance:
  perf <- h2o.performance(ensemble,test)
  perf
  
  # Get Metrics of interest
  # auc aucpr accuray_max f1_max sensitivity_max precision_max
  # https://www.rdocumentation.org/packages/h2o/versions/3.32.1.3/topics/h2o.metric
  
  #Training AUC
  auc_train <- h2o.auc(perf_train)
  
  # AUC
  auc <- h2o.auc(perf)
  
  # AUCPR
  aucpr <- h2o.aucpr(perf)
  
  # Max accuracy
  temp <- h2o.accuracy(perf)
  accuray_max <- max(temp$accuracy)
  
  # Max f1
  temp <- h2o.F1(perf)
  f1_max <- max(temp$f1)
  
  # Something is off with sensitivity metric, pls check!!
  
  # Max sensitivity
  temp <- h2o.sensitivity(perf)
  sensitivity_max <- max(temp$tpr)
  
  # Max precision
  temp <- h2o.precision(perf)
  precision_max <- max(temp$precision)
  
  
  # Store results in results_df
  results_df$model_type[algo_num] <- model_type
  results_df$auc[algo_num] <- auc
  results_df$auc_training[algo_num] <- auc_train
  results_df$aucpr[algo_num] <- aucpr
  results_df$accuray_max[algo_num] <- accuray_max
  results_df$f1_max[algo_num] <- f1_max
  results_df$sensitivity_max[algo_num] <- sensitivity_max
  results_df$precision_max[algo_num] <- precision_max
  results_df$training_time[algo_num] <- train_time_sec
  
  # Get predictions
  mod <- predict(yelp_dl, test)
  mod_df <- as.data.frame(mod)
  # Get tdl
  tdl <- topDecileLift(mod_df$p1, yelp_test$ch_in)
  
  # Get gini
  gini <- Gini(mod_df$p1, yelp_test$ch_in)
  
  
  # Store results for model comparison
  algo_num <- (algo_num + ((algo_num - 1) * 2)) #Needed to update the index due to additional entries
  comparison_df$model_type[algo_num] <- model_type
  comparison_df$measure[algo_num] <- "AUC"
  comparison_df$measure[algo_num+1] <- "Lift"
  comparison_df$measure[algo_num+2] <- "Gini coefficient"
  comparison_df[,file][algo_num] <- auc
  comparison_df[,file][algo_num+1] <- tdl
  comparison_df[,file][algo_num+2] <- gini
  
  
  ####### 6) End of Stacked Ensemblel Model ############################################################
  
  
  ####### 7) Decision trees ############################################################
  
  ### Update this section
  
  algo_num <- 7
  model_type <- "Decision trees"
  
  #######################
  
  # Measure time for training
  train_time_start <- proc.time()  # Start time
  
  # str(yelp_dl) #In case you want to learn about the available methods
  
  
  ### Update this section (Change Model)
  
  # Build and train the model:
  yelp_dl <- h2o.randomForest(x = x,
                              y = y,
                              balance_classes = TRUE,
                              seed = 2021,
                              training_frame = train,
                              validation_frame = test,
                              max_depth = 10,
                              ntrees=1,
                              mtries = (ncol(yelp_file)-1),
                              sample_rate = 1)
  
  train_time <- proc.time() - train_time_start  # End time 
  train_time_sec <- train_time['elapsed'] # Get time
  
  #Performance on train to check overfitting
  perf_train <- h2o.performance(yelp_dl,train)
  perf_train
  
  # Eval performance:
  perf <- h2o.performance(yelp_dl,test)
  perf
  
  #Feature importance
  #h2o.varimp_plot(yelp_dl)
  
  # Get Metrics of interest
  # auc aucpr accuray_max f1_max sensitivity_max precision_max
  # https://www.rdocumentation.org/packages/h2o/versions/3.32.1.3/topics/h2o.metric
  
  #Training AUC
  auc_train <- h2o.auc(perf_train)
  
  # AUC
  auc <- h2o.auc(perf)
  
  # AUCPR
  aucpr <- h2o.aucpr(perf)
  
  # Max accuracy
  temp <- h2o.accuracy(perf)
  accuray_max <- max(temp$accuracy)
  
  # Max f1
  temp <- h2o.F1(perf)
  f1_max <- max(temp$f1)
  
  # Something is off with sensitivity metric, pls check!!
  
  # Max sensitivity
  temp <- h2o.sensitivity(perf)
  sensitivity_max <- max(temp$tpr)
  
  # Max precision
  temp <- h2o.precision(perf)
  precision_max <- max(temp$precision)
  
  
  # Store results in results_df
  results_df$model_type[algo_num] <- model_type
  results_df$auc[algo_num] <- auc
  results_df$auc_training[algo_num] <- auc_train
  results_df$aucpr[algo_num] <- aucpr
  results_df$accuray_max[algo_num] <- accuray_max
  results_df$f1_max[algo_num] <- f1_max
  results_df$sensitivity_max[algo_num] <- sensitivity_max
  results_df$precision_max[algo_num] <- precision_max
  results_df$training_time[algo_num] <- train_time_sec
  
  # Get predictions
  mod <- predict(yelp_dl, test)
  mod_df <- as.data.frame(mod)
  # Get tdl
  tdl <- topDecileLift(mod_df$p1, yelp_test$ch_in)
  
  # Get gini
  gini <- Gini(mod_df$p1, yelp_test$ch_in)
  
  
  # Store results for model comparison
  algo_num <- (algo_num + ((algo_num - 1) * 2)) #Needed to update the index due to additional entries
  comparison_df$model_type[algo_num] <- model_type
  comparison_df$measure[algo_num] <- "AUC"
  comparison_df$measure[algo_num+1] <- "Lift"
  comparison_df$measure[algo_num+2] <- "Gini coefficient"
  comparison_df[,file][algo_num] <- auc
  comparison_df[,file][algo_num+1] <- tdl
  comparison_df[,file][algo_num+2] <- gini
  
  ####### 7) End of Decision trees ############################################################
  
  ####### 8) Support Vector Machines ############################################################
  
  ### Update this section
  
  algo_num <- 8
  model_type <- "Support Vector Machines"
  
  yelp_train$ch_in <- make.names(yelp_train$ch_in)
  yelp_test$ch_in <- make.names(yelp_test$ch_in)
  #yelp_test$ch_in <- as.numeric(yelp_test$ch_in)
  train_control <- trainControl(method="none",classProbs=TRUE)
  svm1 <- train(ch_in ~., data = yelp_train, method = "svmRadial", trControl = train_control,  preProcess = c("center","scale"), metric="ROC")
  svm1_pred <- predict(svm1, newdata = yelp_test, type = "prob")
  #svm1_pred <- predict(svm1, newdata = yelp_test, type = "raw")
  library(pROC)
  auc <- roc(response = yelp_test$ch_in, predictor = svm1_pred[, "X1"])
  auc <- auc$auc
  #plot(auc)
  library(CustomerScoringMetrics)
  lift <- topDecileLift(svm1_pred[, "X1"], yelp_test$ch_in)
  lift
  library(MLmetrics)
  yelp_test$ch_in <- as.numeric(as.factor((yelp_test$ch_in)))
  svm1_pred <- as.numeric(predict(svm1, newdata = yelp_test, type = "raw"))
  gini<- Gini(svm1_pred, yelp_test$ch_in)
  
  # Store results in results_df
  results_df$model_type[algo_num] <- model_type
  results_df$auc[algo_num] <- auc
  
  # Store results for model comparison
  algo_num <- (algo_num + ((algo_num - 1) * 2)) #Needed to update the index due to additional entries
  comparison_df$model_type[algo_num] <- model_type
  comparison_df$measure[algo_num] <- "AUC"
  comparison_df$measure[algo_num+1] <- "Lift"
  comparison_df$measure[algo_num+2] <- "Gini coefficient"
  comparison_df[,file][algo_num] <- auc
  comparison_df[,file][algo_num+1] <- lift
  comparison_df[,file][algo_num+2] <- gini
  
  #######################
  
  ####### 8) End of Support Vector Machines ############################################################
  
  
  ####### 9) Storing resutls ############################################################
  
  
  write.csv(results_df, paste(file, "_results.csv", sep =""))
  final_df <- rbind(final_df, results_df) }


write.csv(final_df,"final_results_model_performance.csv")
write.csv(comparison_df, "comparison_auc_gini_lift_df.csv")



#####Evaluation: Feature Importance and partial dependence plots ######


yelp_dl <- h2o.randomForest(x = x,
                            y = y,
                            #distribution = "bernoulli", -> Deprecated
                            balance_classes = TRUE,
                            seed = 2022,
                            training_frame = train,
                            validation_frame = test,
                            ntrees = 200,
                            max_depth = 10)


# Eval performance:
perf <- h2o.performance(yelp_dl,test)
perf

#Feature importance
h2o.varimp_plot(yelp_dl)

#test reverse labels
test_reversed <- test
test_reversed$ch_in <- ifelse(test$ch_in == 1,0,1)

#Partial dependence business long
yelp_train_h2o <- as.h2o(yelp_train)
h2o.partialPlot(yelp_dl, cols = "business_long", data= test_reversed)

#Partial dependence n_photo
yelp_train_h2o <- as.h2o(yelp_train)
h2o.partialPlot(yelp_dl, cols = "n_photo", data= test_reversed)

#Partial dependence cum_n_tips
yelp_train_h2o <- as.h2o(yelp_train)
h2o.partialPlot(yelp_dl, cols = "cum_n_tips", data= test_reversed)




#### End of evaluation

# Exogenous data
library(stargazer)


colnames(comparison_df) <- c("Model Type", "Evaluation Metric","Physical attributes","UGM - Characteristics","UGM - Content", "Exogenous data", "All variables", "Hyperparameter Tuning")

stargazer(comparison_df, title = "Model Comparison", style = "default", decimal.mark = ".", digits = 2, summary = FALSE,
          column.sep.width = "5pt", out = "Model Comparison.html")




library(forcats)
library("scales")


####### 10) Tuning the models ############################################################


####### 10a: Deep learning tuning####

activation_opt <- c("Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout")
params <- sample(seq(50, 1000, 10),size=5)
hidden <-  c(c(params[1],params[1]),c(params[2],params[2]),c(params[3],params[3]),c(params[4],params[4]),c(params[5],params[5]))
hyper_params <- list(activation = activation_opt, hidden = hidden)
search_criteria <- list(strategy = "RandomDiscrete", 
                        max_runtime_secs = 400)

dl_grid2 <- h2o.grid("deeplearning", x = x, y = y,
                     grid_id = "dl_grid",
                     training_frame = train,
                     validation_frame = test,
                     seed = 1,
                     balance_classes= TRUE,
                     hyper_params = hyper_params,
                     parallelism = 0,
                     search_criteria = search_criteria)

dl_gridperf <- h2o.getGrid(grid_id = "dl_grid", 
                           sort_by = "auc", 
                           decreasing = TRUE)

print(dl_gridperf)

best_dl1 <- h2o.getModel(dl_gridperf@model_ids[[1]])

# Get predictions
mod <- predict(best_dl1, test)
mod_df <- as.data.frame(mod)
# Get tdl
tdl <- topDecileLift(mod_df$p1, yelp_test$ch_in)

# Get gini
gini <- Gini(mod_df$p1, yelp_test$ch_in)

#get auc
perf <- h2o.performance(model =best_dl1, newdata=test)
auc <- h2o.auc(perf)


### Update this section
algo_num <- 1
model_type <- "Deep Learning"
algo_num <- (algo_num + ((algo_num - 1) * 2)) #Needed to update the index due to additional entries
comparison_df[,"Hyperparameter Tuning"][algo_num] <- auc
comparison_df[,"Hyperparameter Tuning"][algo_num+1] <- tdl
comparison_df[,"Hyperparameter Tuning"][algo_num+2] <- gini




#10b: Random Forest ##########################################################################################
tuning_df <- setNames(data.frame(matrix(ncol = 5, nrow = 25)), c("ntrees", "max_depth", "auc", "gini", "tdl"))
ntrees <- seq(20, 500, 10)
max_depth <- seq(0, 20, 1)
ntrees <- sample(ntrees, size=5)
max_depth <- sample(max_depth, size=5)

i <- 1
for (depth in max_depth){
  for (ntree in ntrees){
    
    yelp_dl <- h2o.randomForest(x = x,
                                y = y,
                                balance_classes = TRUE,
                                seed = 2021,
                                training_frame = train,
                                validation_frame = test,
                                max_depth = depth,
                                ntrees = ntree)
    
    # Eval performance:
    perf <- h2o.performance(yelp_dl,test)
    auc <- h2o.auc(perf)
    # Get predictions
    mod <- predict(yelp_dl, test)
    mod_df <- as.data.frame(mod)
    # Get tdl
    tdl <- topDecileLift(mod_df$p1, yelp_test$ch_in)
    # Get gini
    gini <- Gini(mod_df$p1, yelp_test$ch_in)
    tuning_df$auc[i] <- auc
    tuning_df$tdl[i] <- tdl
    tuning_df$gini[i] <- gini
    tuning_df$ntrees[i] <- ntree
    tuning_df$max_depth[i] <- depth
    print(i)
    print(auc)
    i <- i + 1
  }}

tuning_df <- tuning_df[order(tuning_df$auc, decreasing=TRUE),]

# Get auc
auc <- tuning_df$auc[1]
# Get tdl
tdl <- tuning_df$tdl[1]
# Get gini
gini <- tuning_df$gini[1]


### Update this section
algo_num <- 2
algo_num <- (algo_num + ((algo_num - 1) * 2)) #Needed to update the index due to additional entries
comparison_df[,"Hyperparameter Tuning"][algo_num] <- auc
comparison_df[,"Hyperparameter Tuning"][algo_num+1] <- tdl
comparison_df[,"Hyperparameter Tuning"][algo_num+2] <- gini



#10d: Gradient Boosting
gbm_params1 <- list(max_depth = sample(seq(1, 15, 1),size=5),
                    col_sample_rate = sample(seq(0, 1, 0.05),size=5))

gbm_grid1 <- h2o.grid("gbm", x = x, y = y,
                      grid_id = "gbm_grid1",
                      training_frame = train,
                      validation_frame = test,
                      ntrees = 100,
                      seed = 1,
                      parallelism = 0, #-> Automatically takes maximum number of parallel models
                      hyper_params = gbm_params1)

gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "auc",
                             decreasing = TRUE)

print(gbm_gridperf1)
best_gbm1 <- h2o.getModel(gbm_gridperf1@model_ids[[1]])

# Get predictions
mod <- predict(best_gbm1, test)
mod_df <- as.data.frame(mod)
# Get tdl
tdl <- topDecileLift(mod_df$p1, yelp_test$ch_in)

# Get gini
gini <- Gini(mod_df$p1, yelp_test$ch_in)

#get auc
perf <- h2o.performance(model =best_gbm1, newdata=test)
auc <- h2o.auc(perf)


### Update this section
algo_num <- 4
algo_num <- (algo_num + ((algo_num - 1) * 2)) #Needed to update the index due to additional entries
comparison_df[,"Hyperparameter Tuning"][algo_num] <- auc
comparison_df[,"Hyperparameter Tuning"][algo_num+1] <- tdl
comparison_df[,"Hyperparameter Tuning"][algo_num+2] <- gini


#10c: Naive Bayes  ####
params_naive <- list(laplace = sample(seq(1, 40, 1),size=5),
                     min_prob = sample(seq(0.001, 0.3, 0.01),size=5) #default=0.001; min prob for observations without data
)
search_criteria <- list(strategy = "RandomDiscrete", 
                        max_models = 10)

dl_grid <- h2o.grid("naivebayes", x = x, y = y,
                    grid_id = "bayes_grid",
                    training_frame = train,
                    validation_frame = test,
                    seed = 1,
                    hyper_params = params_naive,
                    search_criteria = search_criteria,
                    parallelism = 0
)

dl_gridperf <- h2o.getGrid(grid_id = "bayes_grid", 
                           sort_by = "auc", 
                           decreasing = TRUE)
dl_gridperf

best_naive <- h2o.getModel(dl_gridperf@model_ids[[1]])

# Get predictions
mod <- predict(best_naive, test)
mod_df <- as.data.frame(mod)
# Get tdl
tdl <- topDecileLift(mod_df$p1, yelp_test$ch_in)

# Get gini
gini <- Gini(mod_df$p1, yelp_test$ch_in)

#get auc
perf <- h2o.performance(model =best_naive, newdata=test)
auc <- h2o.auc(perf)

### Update this section
algo_num <- 5
algo_num <- (algo_num + ((algo_num - 1) * 2)) #Needed to update the index due to additional entries
comparison_df[,"Hyperparameter Tuning"][algo_num] <- auc
comparison_df[,"Hyperparameter Tuning"][algo_num+1] <- tdl
comparison_df[,"Hyperparameter Tuning"][algo_num+2] <- gini



## Decision tree

tuning_df <- setNames(data.frame(matrix(ncol = 5, nrow = 25)), c("min_split_improvement", "max_depth", "auc", "gini", "tdl"))
min_split_improvement <- sample(seq(10^-10, 10^-3, 10^-8),size=5)
max_depth <- sample(seq(3, 100, 1),size=5)


i <- 1
for (depth in max_depth){
  for (msi in min_split_improvement){
    
    yelp_dl <- h2o.randomForest(x = x,
                                y = y,
                                balance_classes = TRUE,
                                seed = 2021,
                                training_frame = train,
                                validation_frame = test,
                                max_depth = depth,
                                min_split_improvement = msi,
                                ntrees = 1)
    
    # Eval performance:
    perf <- h2o.performance(yelp_dl,test)
    auc <- h2o.auc(perf)
    # Get predictions
    mod <- predict(yelp_dl, test)
    mod_df <- as.data.frame(mod)
    # Get tdl
    tdl <- topDecileLift(mod_df$p1, yelp_test$ch_in)
    # Get gini
    gini <- Gini(mod_df$p1, yelp_test$ch_in)
    tuning_df$auc[i] <- auc
    tuning_df$tdl[i] <- tdl
    tuning_df$gini[i] <- gini
    tuning_df$min_split_improvement[i] <- msi
    tuning_df$max_depth[i] <- depth
    print(i)
    print(auc)
    i <- i + 1
  }}

tuning_df <- tuning_df[order(tuning_df$auc, decreasing=TRUE),]

# Get auc
auc <- tuning_df$auc[1]
# Get tdl
tdl <- tuning_df$tdl[1]
# Get gini
gini <- tuning_df$gini[1]

### Update this section
algo_num <- 7
algo_num <- (algo_num + ((algo_num - 1) * 2)) #Needed to update the index due to additional entries
comparison_df[,"Hyperparameter Tuning"][algo_num] <- auc
comparison_df[,"Hyperparameter Tuning"][algo_num+1] <- tdl
comparison_df[,"Hyperparameter Tuning"][algo_num+2] <- gini


#10e: Support Vector machines
svm_df <- setNames(data.frame(matrix(ncol = 4, nrow = 3)), c("method", "auc", "gini", "tdl"))

yelp_train$ch_in <- make.names(yelp_train$ch_in)
yelp_test$ch_in <- make.names(yelp_test$ch_in)
#yelp_test$ch_in <- as.numeric(yelp_test$ch_in)
train_control <- trainControl(method="none",classProbs=TRUE)

#Try different kernels
svm1 <- train(ch_in ~., data = yelp_train, method = "svmRadial", trControl = train_control,  preProcess = c("center","scale"), metric="ROC")
svm2 <- train(ch_in ~., data = yelp_train, method = "svmLinear", trControl = train_control,  preProcess = c("center","scale"), metric="ROC")
svm3 <- train(ch_in ~., data = yelp_train, method = "svmPoly", trControl = train_control,  preProcess = c("center","scale"), metric="ROC")
svm1_pred <- predict(svm1, newdata = yelp_test, type = "prob")
svm2_pred <- predict(svm2, newdata = yelp_test, type = "prob")
svm3_pred <- predict(svm3, newdata = yelp_test, type = "prob")

#Metrics for model 1
library(pROC)
auc1 <- roc(response = yelp_test$ch_in, predictor = svm1_pred[, "X1"])
auc1 <- auc1$auc

#plot(auc)
library(CustomerScoringMetrics)
lift1 <- topDecileLift(svm1_pred[, "X1"], yelp_test$ch_in)
lift1
library(MLmetrics)
yelp_test$ch_in <- as.numeric(as.factor((yelp_test$ch_in)))
svm1_pred <- as.numeric(predict(svm1, newdata = yelp_test, type = "raw"))
gini1<- Gini(svm1_pred, yelp_test$ch_in)

# Metrics
svm_df$method[1] <- "svmRadial"
svm_df$auc[1] <- auc1
svm_df$tdl[1] <- lift1
svm_df$gini[1] <- gini1

#Metrics for model2
auc2 <- roc(response = yelp_test$ch_in, predictor = svm2_pred[, "X1"])
auc2 <- auc2$auc

lift2 <- topDecileLift(svm2_pred[, "X1"], yelp_test$ch_in)
lift2

svm2_pred <- as.numeric(predict(svm2, newdata = yelp_test, type = "raw"))
gini2<- Gini(svm2_pred, yelp_test$ch_in)

svm_df$method[2] <- "svmLinear"
svm_df$auc[2] <- auc2
svm_df$tdl[2] <- lift2
svm_df$gini[2] <- gini2


#Metrics for model3
auc3 <- roc(response = yelp_test$ch_in, predictor = svm3_pred[, "X1"])
auc3 <- auc3$auc

aulift3 <- topDecileLift(svm3_pred[, "X1"], yelp_test$ch_in)
lift3

svm3_pred <- as.numeric(predict(svm3, newdata = yelp_test, type = "raw"))
gini3<- Gini(svm3_pred, yelp_test$ch_in)

svm_df$method[3] <- "svmPoly"
svm_df$auc[3] <- auc3
svm_df$tdl[3] <- lift3
svm_df$gini[3] <- gini3

svm_df <- svm_df[order(svm_df$auc, decreasing=TRUE),]

# Get auc
auc <- svm_df$auc[1]
# Get tdl
tdl <- svm_df$tdl[1]
# Get gini
gini <- svm_df$gini[1]

### Update this section
algo_num <- 8
algo_num <- (algo_num + ((algo_num - 1) * 2)) #Needed to update the index due to additional entries
comparison_df[,"Hyperparameter Tuning"][algo_num] <- auc
comparison_df[,"Hyperparameter Tuning"][algo_num+1] <- tdl
comparison_df[,"Hyperparameter Tuning"][algo_num+2] <- gini


names(comparison_df)
### Final safe

write.csv(comparison_df, "comparison_auc_gini_lift_df.csv")


# Fix SVM bug
comparison_df[24,"Physical attributes"] <- comparison_df[24,"Physical attributes"] * (-1)
comparison_df[24,"UGM - Characteristics"] <- comparison_df[24,"UGM - Characteristics"] * (-1)
comparison_df[24,"UGM - Content"] <- comparison_df[24,"UGM - Content"] * (-1)
comparison_df[24,"Exogenous data"] <- comparison_df[24,"Exogenous data"] * (-1)
comparison_df[24,"All variables"] <- comparison_df[24,"All variables"] * (-1)
comparison_df[24,"Hyperparameter Tuning"] <- comparison_df[24,"Hyperparameter Tuning"] * (-1)




# Exogenous data
library(stargazer)


colnames(comparison_df) <- c("Model Type", "Evaluation Metric","Physical attributes","UGM - Characteristics","UGM - Content", "Exogenous data", "All variables", "Hyperparameter Tuning")

stargazer(comparison_df, title = "Model Comparison", style = "default", decimal.mark = ".", digits = 2, summary = FALSE,
          column.sep.width = "5pt", out = "Model Comparison.html")

