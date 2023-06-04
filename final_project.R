install.packages('tidyverse')
install.packages('zoo')
library(tidyverse)
library(zoo)
library(ggplot2)
library(reshape2)

#train_cli = read.csv("C:\\Users\\Praty\\Downloads\\train_clinical_data.csv")
#train_pep = read.csv("C:\\Users\\Praty\\Downloads\\protien\\train_proteins.csv")
#train_pro = read.csv("C:\\Users\\Praty\\Downloads\\peptides\\train_peptides.csv")
#supp_data = read.csv("C:\\Users\\Praty\\Downloads\\supplemental_clinical_data.csv")

train_prot = read.csv("/kaggle/input/amp-parkinsons-disease-progression-prediction/train_proteins.csv", na.strings=c("","NA"))
train_pept = read.csv("/kaggle/input/amp-parkinsons-disease-progression-prediction/train_peptides.csv", na.strings=c("","NA"))
train_clin = read.csv("/kaggle/input/amp-parkinsons-disease-progression-prediction/train_clinical_data.csv", na.strings=c("","NA"))
supp_data = read.csv("/kaggle/input/amp-parkinsons-disease-progression-prediction/supplemental_clinical_data.csv", na.strings=c("","NA"))

# displaying the data
head(train_prot)
head(train_pept)
head(train_clin)
head(supp_data)

# 1.1 missing values

# finding if there are null values
colSums(is.na(train_prot))
colSums(is.na(train_pept))
colSums(is.na(train_clin))
colSums(is.na(supp_data))

# # finding the class of the data
sapply(train_prot, class)
sapply(train_pept, class)
sapply(train_clin, class)
sapply(supp_data,  class)

##################################################################
#####*************** DATA ANALYSIS ***********####################
##################################################################
# plotting to check the distribution and relation for null values

col_list = colnames(train_clin)

row_1 = data.frame(name = col_list, value= rep(0,length(col_list)))
row_2 = data.frame(name = col_list, value= rep(0,length(col_list)))
row_3 = data.frame(name = col_list, value= rep(0,length(col_list)))
row_4 = data.frame(name = col_list, value= rep(0,length(col_list)))

for(i in 1: length(train_clin$visit_id))
{
  count = 0
  save_name = list()
  for(j in 1:length(train_clin))
  {
    if(is.na(train_clin[i,j]))
    {
      count  = count + 1
      save_name[count] = col_list[j] 
    }
  }
  #     print(paste("missing values:",count))
  #     print(paste("colname:",save_name))
  
  if(count == 1)
  {
    for(k in 1:length(row_1$name))
    {
      if(row_1$name[k] %in% save_name)
      {
        row_1$value[k]  =  row_1$value[k] + 1
      }
    }
  }
  if(count == 2)
  {
    for(k in 1:length(row_2$name))
    {
      if(row_2$name[k] %in% save_name)
      {
        row_2$value[k]  =  row_2$value[k] + 1
      }
    }
  }
  if(count == 3)
  {
    for(k in 1:length(row_3$name))
    {
      if(row_3$name[k] %in% save_name)
      {
        row_3$value[k]  =  row_3$value[k] + 1
      }
    }   
  }
  if(count == 4)
  {
    for(k in 1:length(row_4$name))
    {
      if(row_4$name[k] %in% save_name)
      {
        row_4$value[k]  =  row_4$value[k] + 1
      }
    }   
  }
}

# Displaying the result:
row_1
row_2
row_3
row_4

# Displaying the Barplot for train_clin_dataset
# Finding the trend for the same.
barplot(row_1$value, 
        border=F, 
        names.arg = row_1$name, 
        las = 2,
        col=c(rgb(0.3,0.1,0.4,0.6) , 
              rgb(0.3,0.5,0.4,0.6) , 
              rgb(0.3,0.9,0.4,0.6) ,  
              rgb(0.3,0.9,0.4,0.6)) , 
        ,main="" )
barplot(row_2$value, 
        border=F, 
        names.arg = row_1$name, 
        las = 2,
        col=c(rgb(0.3,0.1,0.4,0.6) , 
              rgb(0.3,0.5,0.4,0.6) , 
              rgb(0.3,0.9,0.4,0.6) ,  
              rgb(0.3,0.9,0.4,0.6)) , 
        ,main="" )
barplot(row_3$value, 
        border=F, 
        names.arg = row_1$name, 
        las = 2,
        col=c(rgb(0.3,0.1,0.4,0.6) , 
              rgb(0.3,0.5,0.4,0.6) , 
              rgb(0.3,0.9,0.4,0.6) ,  
              rgb(0.3,0.9,0.4,0.6)) , 
        ,main="" )
barplot(row_4$value, 
        border=F, 
        names.arg = row_1$name, 
        las = 2,
        col=c(rgb(0.3,0.1,0.4,0.6) , 
              rgb(0.3,0.5,0.4,0.6) , 
              rgb(0.3,0.9,0.4,0.6) ,  
              rgb(0.3,0.9,0.4,0.6)) , 
        ,main="" )

##########################################################
# Displaying supp clin_dataset
##########################################################

row_1 = data.frame(name = col_list, value= rep(0,length(col_list)))
row_2 = data.frame(name = col_list, value= rep(0,length(col_list)))
row_3 = data.frame(name = col_list, value= rep(0,length(col_list)))
row_4 = data.frame(name = col_list, value= rep(0,length(col_list)))

for(i in 1: length(supp_data$visit_id))
{
  count = 0
  save_name = list()
  for(j in 1:length(supp_data))
  {
    if(is.na(supp_data[i,j]))
    {
      count  = count + 1
      save_name[count] = col_list[j] 
    }
  }
  #     print(paste("missing values:",count))
  #     print(paste("colname:",save_name))
  
  if(count == 1)
  {
    for(k in 1:length(row_1$name))
    {
      if(row_1$name[k] %in% save_name)
      {
        row_1$value[k]  =  row_1$value[k] + 1
      }
    }
  }
  if(count == 2)
  {
    for(k in 1:length(row_2$name))
    {
      if(row_2$name[k] %in% save_name)
      {
        row_2$value[k]  =  row_2$value[k] + 1
      }
    }
  }
  if(count == 3)
  {
    for(k in 1:length(row_3$name))
    {
      if(row_3$name[k] %in% save_name)
      {
        row_3$value[k]  =  row_3$value[k] + 1
      }
    }   
  }
  if(count == 4)
  {
    for(k in 1:length(row_4$name))
    {
      if(row_4$name[k] %in% save_name)
      {
        row_4$value[k]  =  row_4$value[k] + 1
      }
    }   
  }
}
barplot(row_1$value, 
        border=F, 
        names.arg = row_1$name, 
        las = 2,
        col=c(rgb(0.3,0.1,0.4,0.6) , 
              rgb(0.3,0.5,0.4,0.6) , 
              rgb(0.3,0.9,0.4,0.6) ,  
              rgb(0.3,0.9,0.4,0.6)) , 
        ,main="" )
barplot(row_2$value, 
        border=F, 
        names.arg = row_1$name, 
        las = 2,
        col=c(rgb(0.3,0.1,0.4,0.6) , 
              rgb(0.3,0.5,0.4,0.6) , 
              rgb(0.3,0.9,0.4,0.6) ,  
              rgb(0.3,0.9,0.4,0.6)) , 
        ,main="" )
barplot(row_3$value, 
        border=F, 
        names.arg = row_1$name, 
        las = 2,
        col=c(rgb(0.3,0.1,0.4,0.6) , 
              rgb(0.3,0.5,0.4,0.6) , 
              rgb(0.3,0.9,0.4,0.6) ,  
              rgb(0.3,0.9,0.4,0.6)) , 
        ,main="" )
barplot(row_4$value, 
        border=F, 
        names.arg = row_1$name, 
        las = 2,
        col=c(rgb(0.3,0.1,0.4,0.6) , 
              rgb(0.3,0.5,0.4,0.6) , 
              rgb(0.3,0.9,0.4,0.6) ,  
              rgb(0.3,0.9,0.4,0.6)) , 
        ,main="" )

##################################################################
## Relationship between visit_month vs UPDRS
##################################################################

df <- select(train_clin,-1,-2,-8)
cor_mat = melt(round(cor(na.omit(df)),2))


ggplot(data = cor_mat, aes(x=Var1, y=Var2,
                           fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4)

##############################################################
# Protein_dataset vs visit_month
##############################################################

train_proteins_copy <- train_prot
train_proteins_copy$log_NPX <- log(train_proteins_copy$NPX)

ggplot(train_proteins_copy, aes(x = visit_month, y = log_NPX)) +
  geom_boxplot() +
  geom_point(aes(color = "red"), stat = "summary", fun.y = "mean", shape = 16, size = 4) +
  scale_color_manual(values = "red") +
  ggtitle("NPX by Month") +
  xlab("Visit Month") +
  ylab("NPX") +
  theme_bw()

#############################################################
# DATASET PREPARATION 
# WITH median replacement vs omitting values & Model training
# METHOD-1 & 2
#############################################################

# combining the supplemental data with train_clinical_data
df = rbind(train_clin,supp_data)

# 0 6 12 24
df = df[order(df$visit_month, df$patient_id),]
rownames(df) <-NULL
df1 = select(df1,-1,-2,-8)

c0 = df1 %>% group_by(visit_month) %>% summarize(Med1 = median(updrs_1))
c1 = df1 %>% group_by(visit_month) %>% summarize(Med2 = median(updrs_2))
c2 = df1 %>% group_by(visit_month) %>% summarize(Med3 = median(updrs_3))
c3 = df1 %>% group_by(visit_month) %>% summarize(Med4 = median(updrs_4))
df2 = cbind(c0,c1[-1],c2[-1],c3[-1])
df2
for(i in 1:length(df$visit_month))
{
  if((df$visit_month[i] == 0) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[1]
    df$updrs_2[i] = df2$Med2[1]
    df$updrs_3[i] = df2$Med3[1]
    df$updrs_4[i] = df2$Med4[1]
  }
  
  if((df$visit_month[i] == 3) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[2]
    df$updrs_2[i] = df2$Med2[2]
    df$updrs_3[i] = df2$Med3[2]
    df$updrs_4[i] = df2$Med4[2]
  }
  
  if((df$visit_month[i] == 6) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[3]
    df$updrs_2[i] = df2$Med2[3]
    df$updrs_3[i] = df2$Med3[3]
    df$updrs_4[i] = df2$Med4[3]
  }
  if((df$visit_month[i] == 9) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[4]
    df$updrs_2[i] = df2$Med2[4]
    df$updrs_3[i] = df2$Med3[4]
    df$updrs_4[i] = df2$Med4[4]
  }
  
  if((df$visit_month[i] == 12) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[5]
    df$updrs_2[i] = df2$Med2[5]
    df$updrs_3[i] = df2$Med3[5]
    df$updrs_4[i] = df2$Med4[5]
  }
  if((df$visit_month[i] == 18) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[6]
    df$updrs_2[i] = df2$Med2[6]
    df$updrs_3[i] = df2$Med3[6]
    df$updrs_4[i] = df2$Med4[6]
  }
  if((df$visit_month[i] == 24) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[7]
    df$updrs_2[i] = df2$Med2[7]
    df$updrs_3[i] = df2$Med3[7]
    df$updrs_4[i] = df2$Med4[7]
  }
  if((df$visit_month[i] == 30) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[8]
    df$updrs_2[i] = df2$Med2[8]
    df$updrs_3[i] = df2$Med3[8]
    df$updrs_4[i] = df2$Med4[8]
  }
  if((df$visit_month[i] == 36) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[9]
    df$updrs_2[i] = df2$Med2[9]
    df$updrs_3[i] = df2$Med3[9]
    df$updrs_4[i] = df2$Med4[9]
  }
  if((df$visit_month[i] == 42) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[10]
    df$updrs_2[i] = df2$Med2[10]
    df$updrs_3[i] = df2$Med3[10]
    df$updrs_4[i] = df2$Med4[10]
  }
  if((df$visit_month[i] == 48) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[11]
    df$updrs_2[i] = df2$Med2[11]
    df$updrs_3[i] = df2$Med3[11]
    df$updrs_4[i] = df2$Med4[11]
  }
  if((df$visit_month[i] == 54) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[12]
    df$updrs_2[i] = df2$Med2[12]
    df$updrs_3[i] = df2$Med3[12]
    df$updrs_4[i] = df2$Med4[12]
  }
  if((df$visit_month[i] == 60) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[13]
    df$updrs_2[i] = df2$Med2[13]
    df$updrs_3[i] = df2$Med3[13]
    df$updrs_4[i] = df2$Med4[13]
  }
  if((df$visit_month[i] == 72) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[14]
    df$updrs_2[i] = df2$Med2[14]
    df$updrs_3[i] = df2$Med3[14]
    df$updrs_4[i] = df2$Med4[14]
  }
  if((df$visit_month[i] == 84) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[15]
    df$updrs_2[i] = df2$Med2[15]
    df$updrs_3[i] = df2$Med3[15]
    df$updrs_4[i] = df2$Med4[15]
  }
  if((df$visit_month[i] == 96) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[16]
    df$updrs_2[i] = df2$Med2[16]
    df$updrs_3[i] = df2$Med3[16]
    df$updrs_4[i] = df2$Med4[16]
  }
  if((df$visit_month[i] == 108) &
     (is.na(df$updrs_1[i])|
      is.na(df$updrs_2[i])|
      is.na(df$updrs_3[i])|
      is.na(df$updrs_4[i]) ))
  {
    df$updrs_1[i] = df2$Med1[17]
    df$updrs_2[i] = df2$Med2[17]
    df$updrs_3[i] = df2$Med3[17]
    df$updrs_4[i] = df2$Med4[17]
  }
  
}
##########################################################
# Trial run preparing whole data
##########################################################

patients <- list()

for (e in 1:4) {
  for (m in c(0, 6, 12, 24)) {
    train_cli[[paste0('updrs_', e, '_plus_', m, '_months')]] <- 0
  }
}

for (patient in unique(train_cli$patient_id)) {
  temp <- train_cli[train_cli$patient_id == patient, ]
  month_list <- matrix(nrow = nrow(temp), ncol = 4)
  month_windows <- c(0, 6, 12, 24)
  for (i in 1:nrow(temp)) {
    month_list[i, ] <- c(temp$visit_month[i], temp$visit_month[i] + 6,
                         temp$visit_month[i] + 12, temp$visit_month[i] + 24)
  }
  for (month in 1:nrow(month_list)) {
    for (x in 1:4) {
      if (x == 3) {
        arr <- temp[temp$visit_month %in% month_list[month, ], paste0('updrs_', x)]
      } else {
        arr <- temp[temp$visit_month %in% month_list[month, ], paste0('updrs_', x)]
        arr[is.na(arr)] <- 0
      }
      if (length(arr) == 4) {
        for (e in 1:length(arr)) {
          m <- month_list[month, 1]
          temp[temp$visit_month == m, paste0('updrs_', x, '_plus_', month_windows[e], '_months')] <- arr[e]
        }
      } else {
        temp <- temp[!temp$visit_month %in% month_list[month, ], ]
      }
    }
  }
  patients[[patient]] <- temp
}


formatted_clin <- as.data.frame(do.call(rbind, patients))
formatted_clin <- formatted_clin[order(formatted_clin$visit_id),]
formatted_clin <- formatted_clin[, 8:ncol(formatted_clin)]
################################################################
# df data frame with 
################################################################
new_df = na.omit(rbind(train_clin,supp_data))

model_LR <-list()
model_xgb <-list()
model_cat <-list()

model_LR_1 <-list()
model_xgb_1 <-list()
model_cat_1 <-list()
target <- c('updrs_1' , 'updrs_2', 'updrs_3' ,'updrs_4')


for(u in target)
{
  temp <- df[!is.na(df[,u]),]
  if(u == 'updrs_3')
  {
    temp <- temp[temp[,u]!=0,]
  }
  
  X <- temp$visit_month
  y <- temp[,u]
  trained <- lm(y~X)
  model_LR[[u]] <- trained
  
  dtrain <- xgb.DMatrix(data = X, label = y)
  trained_xgb <- xgb.train(params = list(objective = "reg:squarederror"), data = dtrain, nrounds = 50)
  model_xgb[[u]] <- trained_xgb
  
  trained_catboost <- catboost.train(data = catboost.load_pool(data = list(X, y), cat_features = 1), 
                            ntree = 50, loss_function = "RMSE")
  model_cat[[u]] <- trained_catboost
}


for(u in target)
{
  temp <- new_df[!is.na(new_df[,u]),]
  if(u == 'updrs_3')
  {
    temp <- temp[temp[,u]!=0,]
  }
  
  X <- temp$visit_month
  y <- temp[,u]
  trained <- lm(y~X)
  model_LR_1[[u]] <- trained
  
  dtrain <- xgb.DMatrix(data = X, label = y)
  trained_xgb <- xgb.train(params = list(objective = "reg:squarederror"), data = dtrain, nrounds = 50)
  model_xgb_1[[u]] <- trained_xgb
  
  trained_catboost <- catboost.train(data = catboost.load_pool(data = list(X, y), cat_features = 1), 
                                     ntree = 50, loss_function = "RMSE")
  model_cat_1[[u]] <- trained_catboost
}

#######################################################################
# Getting predictions and using only the best model
######################################################################


get_predictions <- function(my_train, model) {
  #install.packages('tidyverse')
  #library(tidyverse)
  # Forecast
  my_train[is.na(my_train)] <- 0
  
  result <- data.frame()
  
  for (u in target) {
    
    # Here is where we will save the final results
    my_train[paste0("result_", u)] <- 0
    
    # Predict    
    X <- my_train$visit_month
    
    # updrs_4 will have only 0's, so we update for others
    if (u != "updrs_4") {
      my_train[paste0("result_", u)] <- predict(model[[u]], data.frame(X))
    }
    
    # Format for final submission
    for (m in c(0, 6, 12, 24)) {
      for (i in 1:nrow(my_train)) {
        temp <- data.frame(
          prediction_id = paste0(my_train$visit_id[i],'_', u, "_plus_", m, "_months"),
          rating = my_train[i, paste0("result_", u)],
          stringsAsFactors = FALSE
        )
        result <- rbind(result, temp)
      }
    }
  }
  
  result <- unique(result[c("prediction_id", "rating")])
  
  return(result)
}