#----------------------------------------- DATA MINERS: CREDIT RISK MODELLING -------------------------------------#


######## Packages ########

library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
library(e1071)
library(xgboost)
library(stringr)
library(lubridate)
library(tm)
library(SnowballC)
library(rms)
library(glmnet)
library(pROC)
library(doMC)
library(kernlab)
library(MASS)
library(reshape)
library(missForest)
library(caTools)
library(psych)
library(ipred)
library(dplyr)
library(corrplot)

######## Data cleaning ########

list.files()
loan<-read.csv(file="lending_club_loans.csv",header=T,stringsAsFactors = F,sep=',')
str(loan)

## counting instances of missing values and finding columns with appropriate NA
count_NA<-sapply(loan[loan$loan_status!="Current",],function(x)(sum(is.na(x))))
write.csv(count_NA,file="missing_count.csv")
count_NA1 <- count_NA<0.05 * nrow(loan)
count_NA
col_keep<-as.data.frame(count_NA1[count_NA1==1])
col_keep<-rownames(col_keep)
nrow(loan[loan$loan_status!="Current",])
loan2<-loan[col_keep]

numcol<-sapply(loan2,function(x)(is.numeric(x)))
num_col<-as.data.frame(numcol[numcol==1])
num_col<-rownames(num_col)
num_col
str(loan2)
loan3<-loan2[complete.cases(loan2),]
colnames(loan3)
loan3<-loan3[,-49]
numcol<-sapply(loan3,function(x)(is.numeric(x)))
num_col<-as.data.frame(numcol[numcol==1])
num_col<-rownames(num_col)
cor_plot<-corrplot(cor(loan3[num_col]),method="number")
table(loan3$policy_code)
count_NA

var.in <- names(loan)[!names(loan)%in% c('verification_status_joint','total_bal_il','acc_now_delinq','acc_open_past_24mths','acc_open_past_24mths','all_util','avg_cur_bal','bc_open_to_buy','bc_util','chargeoff_within_12_mths','collection_recovery_fee','collections_12_mths_ex_med','delinq_amnt','dti_joint','fico_range_high','fico_range_low','funded_amnt','funded_amnt_inv','id','il_util','initial_list_status','inq_fi','inq_last_12m','last_credit_pull_d','last_fico_range_high','last_fico_range_low','last_pymnt_amnt','last_pymnt_d','max_bal_bc','mo_sin_old_il_acct','mo_sin_old_rev_tl_op','mo_sin_rcnt_rev_tl_op','mo_sin_rcnt_tl','mort_acc','mths_since_last_delinq','mths_since_last_major_derog','mths_since_last_record','mths_since_rcnt_il','mths_since_recent_bc','mths_since_recent_bc_dlq','mths_since_recent_inq','mths_since_recent_revol_delinq','next_pymnt_d','num_accts_ever_120_pd','num_actv_bc_tl','num_actv_rev_tl','num_bc_sats','num_bc_tl','num_il_tl','num_op_rev_tl','num_rev_accts','num_rev_tl_bal_gt_0','num_sats','num_tl_120dpd_2m','num_tl_30dpd','num_tl_90g_dpd_24m','num_tl_op_past_12m','open_acc_6m','open_il_12m','open_il_24m','open_il_6m','open_rv_12m','open_rv_24m','out_prncp_inv','pct_tl_nvr_dlq','percent_bc_gt_75','policy_code','pub_rec_bankruptcies','recoveries','tax_liens','title','tot_coll_amt','tot_cur_bal','tot_hi_cred_lim','total_bal_ex_mort','total_bc_limit','total_cu_tl','total_il_high_credit_limit','total_pymnt_inv','total_rec_int','total_rec_late_fee','total_rec_prncp','total_rev_hi_lim','url')]
loan_col_cl <- loan[,var.in]
loan_col_cl$joint_status<-ifelse(is.na(loan_col_cl$annual_inc_joint),0,1)  ##introducing joint status column
loan_col_cl$annual_inc_joint<-ifelse(is.na(loan_col_cl$annual_inc_joint),loan_col_cl$annual_inc,loan_col_cl$annual_inc_joint)   ##updating annual_inc_joint column
sapply(loan_col_cl,function(x)(sum(is.na(x))))

loan_clean<-loan_col_cl[complete.cases(loan_col_cl),]

data <- loan_clean
summary(data$int_rate)
data$int_rate = (as.numeric(gsub(pattern = "%",replacement = "",x = data$int_rate)))
data$issue_y = as.numeric(sapply( data$issue_d ,function(x){str_split(x,"-")[[1]][2]})) # Extracting year

## Importing macro economic data

poverty_new <- read.csv("Poverty2.csv", header = T)
poverty_new$Median_Income[poverty_new$issue_y==2016]<-NA
poverty_new$Poverty[poverty_new$issue_y==2016]<-NA

poverty.imp<-missForest(poverty_new[,-4])
poverty_new$value[poverty_new$issue_y==2016]<-poverty.imp$ximp$value
Income.imp<-missForest(poverty_new[,-5])
poverty_new$Median_Income<-Income.imp$ximp$Median_Income
poverty_new$Poverty<-poverty.imp$ximp$Poverty
poverty_new <-poverty_new[,-2]

loan_clean_macro <- data


colnames(loan_clean_macro)
loan_clean_macro <- merge(loan_clean_macro, poverty_new, by = c("addr_state", "issue_y"), all.x = T)
colnames(loan_clean_macro)

sum(is.na(loan_clean_macro))
sapply(loan_clean_macro,function(x)(sum(is.na(x))))
str(loan_clean_macro)
df <- loan_clean_macro[is.na(loan_clean_macro$Median_Income),c(1,2)]
unique(loan_clean_macro$emp_title)

## Removing rows with loan_status which are not relevant to our analysis
loan_clean_macro$loan_status <- gsub("Does not meet the credit policy. Status:Fully Paid", "Fully Paid", loan_clean_macro$loan_status)
loan_clean_macro <- loan_clean_macro[which(loan_clean_macro$loan_status!= "Current"),]
loan_clean_macro <- loan_clean_macro[which(loan_clean_macro$loan_status!= "Late (16-30 days)"),]
loan_clean_macro <- loan_clean_macro[which(loan_clean_macro$loan_status!= "Issued"),]
loan_clean_macro <- loan_clean_macro[which(loan_clean_macro$loan_status!= "In Grace Period"),]
loan_clean_macro$loan_status = ifelse(str_detect(loan_clean_macro$loan_status,"Paid"),loan_clean_macro$loan_status,"Default")
unique(loan_clean_macro$loan_status)                           
table(loan_clean_macro$loan_status)
loan_clean_macro$addr_state <- as.factor(loan_clean_macro$addr_state)
str(loan_clean_macro)
sum(is.na(loan_clean_macro))

######## Data Re-structure ########

loan_clean_macro$loan_status <- gsub("Fully Paid", 0, loan_clean_macro$loan_status)
loan_clean_macro$loan_status <- gsub("Default", 1, loan_clean_macro$loan_status)
loan_clean_macro$loan_status<-as.numeric(loan_clean_macro$loan_status)

loan_clean_macro$verification_status <- gsub("Not Verified", 0, loan_clean_macro$verification_status)
loan_clean_macro$verification_status <- gsub("Source Verified", 1, loan_clean_macro$verification_status)
loan_clean_macro$verification_status <- gsub("Verified", 2, loan_clean_macro$verification_status)
loan_clean_macro$verification_status<-as.numeric(loan_clean_macro$verification_status)

loan_clean_macro$zip_code <- gsub("xx",0, loan_clean_macro$zip_code)
loan_clean_macro$zip_code<- as.numeric(loan_clean_macro$zip_code)

loan_clean_macro$revol_util <- data$revol_util
loan_clean_macro$revol_util <- gsub("%",0, loan_clean_macro$revol_util)
head(loan_clean_macro$revol_util)
loan_clean_macro$revol_util<-as.numeric(loan_clean_macro$revol_util)
loan_clean_macro <- loan_clean_macro[complete.cases(loan_clean_macro),]
sum(is.na(loan_clean_macro))
colnames(loan_clean_macro)

##loan_clean_macro<-loan_clean_macro[,-33]# 33 is prediction

loan_clean_macro$term<-gsub(" months",' ', loan_clean_macro$term)
loan_clean_macro$term<-as.numeric(loan_clean_macro$term)
loan_clean_macro$home_ownership<-as.factor(loan_clean_macro$home_ownership)
unique(loan_clean_macro$emp_length)

##Changing predictors as factors
colnames(loan_clean_macro)
fact_col<-c("addr_state","grade" ,"joint_status" )

loan_clean_macro[,fact_col]<-lapply(loan_clean_macro[,fact_col],function(x)(as.factor(x)))

##Breaking emp_length to buckets
loan_clean_macro <- data

loan_clean_macro$emp_length<-ifelse(loan_clean_macro$emp_length %in% c("< 1 year",'1 year','2 years'),'0_2years',loan_clean_macro$emp_length)
loan_clean_macro$emp_length<-ifelse(loan_clean_macro$emp_length %in% c("3 years",'4 years','5 years'),'3_5years',loan_clean_macro$emp_length)
loan_clean_macro$emp_length<-ifelse(loan_clean_macro$emp_length %in% c("6 years",'7 years','8 years','9 years'),'6_9years',loan_clean_macro$emp_length)
loan_clean_macro1<-loan_clean_macro[(loan_clean_macro$emp_length!='n/a'),]

##Getting credit length in weeks and converting date variables as date
#loan_clean_macro1$issue_d<-gsub('-','\\',loan_clean_macro1$issue_d)
loan_clean_macro1$issue_d<-paste0(loan_clean_macro1$issue_d,'-01')
loan_clean_macro1$issue_d<-as.Date(toupper(loan_clean_macro1$issue_d),format='%b-%Y-%d')
#class(loan_clean_macro1$issue_d)
loan_clean_macro1$earliest_cr_line<-paste0(loan_clean_macro1$earliest_cr_line,'-01')
loan_clean_macro1$earliest_cr_line<-as.Date(toupper(loan_clean_macro1$earliest_cr_line),format='%b-%Y-%d')
##head(loan_clean_macro1$earliest_cr_line)
loan_clean_macro1$credit_length_weeks<-difftime(loan_clean_macro1$issue_d,loan_clean_macro1$earliest_cr_line,units='weeks')
#head(loan_clean_macro1$credit_length_weeks)
#head(loan_clean_macro1$issue_d)

colnames(loan_clean_macro1)
fact_col<-c("term","emp_length" ,"purpose" )

loan_clean_macro1[,fact_col]<-lapply(loan_clean_macro1[,fact_col],function(x)(as.factor(x)))
drop_col<-c("emp_title" ,"issue_d" ,"pymnt_plan" ,"desc","zip_code")

model_data1<-loan_clean_macro1[,!colnames(loan_clean_macro1)%in% drop_col]
model_data <- model_data1
model_data$loan_status <- as.factor(model_data$loan_status)
model_data$verification_status <- as.factor(model_data$verification_status)
model_data$credit_length_weeks <- as.numeric(model_data$credit_length_weeks)
str(model_data)
model_data$grade <- as.factor(model_data$grade)
model_data$sub_grade <- substr(model_data$sub_grade,2,2) 
head(model_data$sub_grade)
model_data$sub_grade <- as.numeric(model_data$sub_grade) 
str(model_data)
model_data$revol_util <- gsub("%",0, model_data$revol_util)
head(model_data$revol_util)
model_data$revol_util<-as.numeric(model_data$revol_util)
str(model_data)
sum(is.na(model_data))
model_data <- model_data[complete.cases(model_data),]
model_data$home_ownership <- as.factor(model_data$home_ownership)
model_data$application_type <- as.factor(model_data$application_type)
model_data$earliest_cr_line <- NULL
sum(is.na(model_data))
str(model_data)
model_data$X <- NULL
str(model_data)
model_data$annual_inc <- NULL


rm(list=setdiff(ls(), "model_data"))
sum(is.na(model_data))


## Adding FICO score

## Splitting Data in two parts

model_data_pre2011 <- model_data[model_data$issue_y < 2011,]
unique(model_data_pre2011$issue_y)

model_data_post2011 <- model_data[model_data$issue_y >= 2011,]
unique(model_data_post2011$issue_y)

#--------------------------- Taking 5% of the entire dataset----------------------#

set.seed(101)

## select training indices preserving class distribution
in.train <- createDataPartition(model_data$loan_status, p=0.05, list=FALSE)
summary(factor(model_data$loan_status))/length(model_data$loan_status)
train <- model_data[in.train,]; summary(train$loan_status)/length(train$loan_status)
test <- model_data[-in.train,]; summary(test$loan_status)/length(test$loan_status)


set.seed(101)
in.train_pre <- createDataPartition(model_data_pre2011$loan_status, p=0.2, list=FALSE)
summary(factor(model_data_pre2011$loan_status))/length(model_data_pre2011$loan_status)
train_pre <- model_data_pre2011[in.train_pre,]; summary(train_pre$loan_status)/length(train_pre$loan_status)
test_pre <- model_data_pre2011[-in.train_pre,]; summary(test_pre$loan_status)/length(test_pre$loan_status)

set.seed(101)
in.train_post <- createDataPartition(model_data_post2011$loan_status, p=0.05, list=FALSE)
summary(factor(model_data_post2011$loan_status))/length(model_data_post2011$loan_status)
train_post <- model_data_post2011[in.train_post,]; summary(train_post$loan_status)/length(train_post$loan_status)
test_post <- model_data_post2011[-in.train_post,]; summary(test_post$loan_status)/length(test_post$loan_status)

######## EDA ########

## EDA 1 ##
slices <- model_data1$loan_status
freq_slices <- table(slices)
piepercent<- round(100*freq_slices/sum(freq_slices), 1)
pie(freq_slices, labels = piepercent, col = c("red","green"))
legend("topright", c("Default","Fully Paid"), cex = 0.8,
       fill = c("red","green"))

count <- table(model_data1$loan_status)
barplot(count, col = c("red", "dark green"),legend = rownames(count))

## EDA 2 ##

tmp = model_data1 %>% group_by(loan_status) %>% summarise(ncount = n())
tmp$ncount = 100 * tmp$ncount/nrow(model_data1)
tmp$ncount_p = str_c(round(tmp$ncount,2),"%")
ggplot(tmp,aes(x=loan_status,y=ncount,fill=loan_status)) + geom_bar(stat="identity") +
  geom_text(aes(label=ncount_p),vjust = 2)

## EDA 3 ##

model_data1$int_rate = (as.numeric(gsub(pattern = "%",replacement = "",x = data$int_rate)))

model_data1$issue_y = as.numeric(sapply( data$issue_d ,function(x){str_split(x,"-")[[1]][2]})) # Extracting year

displayInterestByGrade <- function(dt){
  g1 = dt %>% filter(loan_status == "Default") %>% group_by(grade) %>% summarise(default_count = n())
  g2 = dt %>% group_by(grade) %>% summarise(count = n(),int_rate=mean(int_rate))
  g2 %>% left_join(g1) %>% mutate(default_rate = 100*default_count/count) ## %>% select(grade,count,default_count,int_rate,default_rate)
}

#Storing data split by grade across different years
tmp0 = displayInterestByGrade(model_data1 %>% filter(issue_y==2007))
tmp0$year <- 2007
tmp1 = displayInterestByGrade(model_data1 %>% filter(issue_y==2008))
tmp1$year <- 2008
tmp2 = displayInterestByGrade(model_data1 %>% filter(issue_y==2009))
tmp2$year <- 2009
tmp3 = displayInterestByGrade(model_data1 %>% filter(issue_y==2010))
tmp3$year <- 2010
tmp4 = displayInterestByGrade(model_data1 %>% filter(issue_y==2011))
tmp4$year <- 2011
# tmp5 = displayInterestByGrade(data %>% filter(issue_y==2012))
# tmp5$year <- 2012
# tmp6 = displayInterestByGrade(data %>% filter(issue_y==2013))
# tmp6$year <- 2013
# tmp7 = displayInterestByGrade(data %>% filter(issue_y==2014))
# tmp7$year <- 2014
# tmp8 = displayInterestByGrade(data %>% filter(issue_y==2015))
# tmp8$year <- 2015
# tmp9 = displayInterestByGrade(data %>% filter(issue_y==2016))
# tmp9$year <- 2016

#tmp = rbind(tmp0,tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9) ## Combining all the individual year-wise data into one

tmp = rbind(tmp0,tmp1,tmp2,tmp3,tmp4) ## Combining all the individual year-wise data into one

ggplot(tmp, aes(x=grade, y=default_rate,fill=as.factor(year))) + geom_bar(stat="identity",position="dodge") + ggtitle("Default Rate(%)")


## EDA 4 ##

ggplot(tmp, aes(x=grade, y=int_rate,fill=as.factor(year))) + geom_bar(stat="identity",position="dodge") + ggtitle("Interest Rate(%)")

## We plan to discuss with professor how much of the past data should be included in our analysis. Since in time series kind of models it is advised to use recent data rather than past data

## EDA 5 ##

all_roi = sum((model_data1 %>% filter(issue_y==2007, loan_status == "Fully Paid"))$total_pymnt)/sum((model_data1 %>% filter(issue_y==2007, loan_status == "Fully Paid"))$loan_amnt) - 1
all_roi

model_data1$prediction = "Fully Paid"
createPerformanceTable <- function(dt){
  
  dt_pick = dt %>% filter(prediction == "Fully Paid")
  all_roi = sum(dt_pick$total_pymnt)/sum(dt_pick$loan_amnt) - 1
  
  temp_table = data.frame(grade=character(0),roi=numeric(0),percent_pick=numeric(0))
  for(g in c("A","B","C","D","E","F","G")){
    data_pick_grade = dt_pick %>% filter(grade==g)
    if(nrow(data_pick_grade)==0){
      temp_table = rbind(temp_table,data.frame(grade=g,roi=0,percent_pick=0))
    }
    else
    {
      data_grade = dt %>% filter(grade==g)
      roi = sum(data_pick_grade$total_pymnt)/sum(data_pick_grade$loan_amnt) - 1
      temp_table = rbind(temp_table,data.frame(grade=g,roi=roi,percent_pick=100 * nrow(data_pick_grade)/nrow(data_grade)))
    }
  }
  
  temp_table = rbind(temp_table,data.frame(grade="ALL",roi=all_roi,percent_pick=100 * nrow(dt_pick)/nrow(dt) ))
  
  return(temp_table)
}

baseline_table <- list()
table <- data.frame()
for (i in 2007:2011){
  
  table <-  createPerformanceTable(model_data1 %>% filter(issue_y==i))
  baseline_table [i-2006] <- table[8,2]
  
}

years <- c(2007,2008,2009,2010,2011)
baseline_table$years <- years
baseline_table[[7]]
roi_table <- vector()
for(i in 1:5){
  roi_table[i] <- baseline_table[[i]]
}
roi_table
roi_table <- cbind(roi_table, years)
roi_table

colnames(roi_table) <- c("ROI", "Year")
colnames(roi_table)
roi_table

ggplot(roi_table, aes(x = Year, y = ROI)) + geom_point()

roi_table <- as.data.frame(roi_table)
## Graph to be plotted

## EDA 6 ##

##head(data[,c("title","emp_title")])

corpus <-Corpus(VectorSource(data$emp_title))
corpus <- tm_map(corpus, tolower) # lower case
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english')) # from library
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus<- tm_map(corpus, PlainTextDocument)
dtm_emp_title<-DocumentTermMatrix(corpus,control = list(stemming = stemDocument))
dtm_emp_title<-removeSparseTerms(dtm_emp_title, 0.99)
dtm_emp_title<-as.data.frame(as.matrix(dtm_emp_title))
rownames(dtm_emp_title) <- NULL
names(dtm_emp_title)<-paste(names(dtm_emp_title),sep=" ","emp_title")
names(dtm_emp_title)


## EDA 6 ##

ggplot(model_data1, aes(x = loan_amnt, y = int_rate)) + geom_point(aes(color = term))


## EDA 7 ##

ggplot(model_data1, aes(x = int_rate))+ geom_histogram(aes(fill = grade)) + facet_wrap(~loan_status, ncol = 2)


## EDA 8 ##

fancyRpartPlot(loans.rpart.1)

## EDA 9 ##

plot(jitter((model_data1$loan_amnt)),
     jitter(model_data1$int_rate), col=as.numeric(model_data1$home_ownership), pch=19, cex=0.4)
legend("topright", legend=unique(model_data1$home_ownership), 
       col=unique(as.numeric(model_data1$home_ownership)), pch=19)

## EDA 10 ##

plot(jitter(model_data1$int_rate),jitter(model_data1$inq_last_6mths), col="orangered", pch=19, cex=0.5)

## EDA 11 ##

barplot(as.matrix(prop.table(table(model_data1$loan_status,model_data1$issue_y),2)),main = "Barplot of Standard/ Default Loans ", xlab= "Year", 
        ylab= "No of Cases", col = c("Red","darkgreen"),legend = rownames(as.matrix(prop.table(table(model_data1$loan_status,model_data1$issue_y),2))))

## EDA 12 ##

plot(model_data1$int_rate ~ model_data1$purpose)
plot(model_data1$int_rate ~ model_data1$home_ownership)


## EDA 13 ##

mosaic(~ loan_status  + term, data=model_data1, shade=T, split_vertical=T)

## EDA 14 ##

numcol<-sapply(model_data,function(x)(is.numeric(x)))
sum(numcol)
corrgram(model_data1[,c(4,6,33,28,29)]
         , order=F
         , lower.panel=panel.shade
         , upper.panel=panel.shade
         , text.panel=panel.txt
         , main="Correlogram of Key Predictors"
)

## EDA 15 ##

colnames(model_data1)
ggpairs(model_data1[,c(4,6,7,10, 13)])
ggpairs(model_data1[,c(15,17,28,29,33)])


write.csv(model_data1, "model_data1.csv")

rp <- rpart(loan_status~., data
            

######## Functions ########

RMSE_calc <- function(obj,newData, response){
  #rmse for regression
  if(class(newData[,response])!="factor"){
    predicted_val <- predict(obj, newData[,-which(names(newData) %in% response)])
    RMSE <-mean((newData[,response] - predicted_val)^2)^0.5
  }else{ #misclassification error for classification
    predicted_val <- predict(obj, newData[,-which(names(newData) %in% response)],type = "class")
    table_test <- table(predicted_val, newData[,response])
    #in this case RMSE is the misclassification error
    RMSE <- 1 - sum(diag(table_test))/sum(table_test)
  }
  return(RMSE)
}

# Second function is R-sq function in which input parameter are dataset, model and response
R2 <- function(dataset,response,model) {
  pred=predict(model,newdata = dataset)
  SSE<-sum((pred-dataset[,response])^2)
  SST<-sum((dataset[,response]-rep(mean(dataset[,response]),nrow(dataset)))^2)
  Rsq=1-(SSE/SST)
  return(Rsq)
}

# third is final function which return model parameters using above 2 functions
# Model Stat return us RMSE of train, RMSE of test and R-sq value 
Model_stat<- function(obj,response,modelname,traindata=train,testdata=test){
  RMSE_train<-RMSE_calc(obj,traindata,response)
  RMSE_test<-RMSE_calc(obj,testdata,response)
  Rsq<-R2(traindata,response,model = obj)
  out<-c(modelname,RMSE_train,RMSE_test,Rsq)
  return(out)
}







#-------------------------- Scalling & PCA--------------------------------------#

numcol<-sapply(train,function(x)(is.numeric(x)))
head(model_data$grade)
colnames(train)[numcol]
train_pca[, colnames(train)[numcol]] <- lapply(train[, colnames(train)[numcol]] ,function(x)(scale(x)))
train_pca <- scale(as.matrix(train [, colnames(train)[numcol]]))

train_pca <- as.data.frame(train_pca)
head(train_pca)

train_pca1 <- principal(train_pca
                        , nfactors = 18     
                        , rotate = "none"  
                        , scores = T       
)

train_pca1$loadings

##                PC1   PC2   PC3   PC4   PC5   PC6   PC7   PC8   PC9  PC10  PC11  PC12  PC13  PC14  PC15  PC16  PC17  PC18
##SS loadings    3.108 1.792 1.517 1.472 1.135 1.092 1.026 1.012 0.970 0.943 0.828 0.730 0.720 0.703 0.582 0.302 0.066 0.001
##Proportion Var 0.173 0.100 0.084 0.082 0.063 0.061 0.057 0.056 0.054 0.052 0.046 0.041 0.040 0.039 0.032 0.017 0.004 0.000
##Cumulative Var 0.173 0.272 0.357 0.438 0.501 0.562 0.619 0.675 0.729 0.782 0.828 0.868 0.908 0.947 0.979 0.996 1.000 1.000




######## Dataset splitting ########
# 
# 
# # We have removed the column 'Member_ID' because it would not add value to our model. We have not removed it from our dataset 
# #because that is the only unique identifier in our dataset and we would like to keep it for quality control purposes
# 
# # Splitting Main dataset in two parts
# set.seed(101)
# sample = sample.split(model_data[,-3], SplitRatio = .85)
# train = subset(model_data[,-3], sample == TRUE)
# test = subset(model_data[,-3], sample == FALSE)
# 
# # # Splitting Model-2 dataset in two parts
# # set.seed(101)
# # colnames(model_data2)
# # sample = sample.split(model_data[,-c(3,20,21)], SplitRatio = .85)
# # train = subset(model_data[,-c(3,20,21)], sample == TRUE)
# # test = subset(model_data[,-c(3,20,21)], sample == FALSE)
# 
# # Splitting Pre2011 dataset in two parts
# set.seed(101)
# colnames(model_data_pre2011)
# sample = sample.split(model_data_pre2011[,-3], SplitRatio = .85)
# train_pre = subset(model_data_pre2011[,-3], sample == TRUE)
# test_pre = subset(model_data_pre2011[,-3], sample == FALSE)
# 
# # Splitting Post2011 dataset in two parts
# set.seed(101)
# colnames(model_data_post2011)
# sample = sample.split(model_data_post2011[,-3], SplitRatio = .85)
# train_post = subset(model_data_post2011[,-3], sample == TRUE)
# test_post = subset(model_data_post2011[,-3], sample == FALSE)
# 

######## Model fitting ########

#####LDA####

num_var<-colnames(up_train)[sapply(up_train,is.numeric)]
temp<-rbind(up_train,test)
num_var
temp<-temp[,-c(1,23,25)]

levels(temp$purpose) <- make.names(levels(factor(temp$purpose)))
levels(temp$emp_length) <- make.names(levels(factor(temp$emp_length)))
levels(temp$joint_status) <- make.names(levels(factor(temp$joint_status)))
levels(temp$loan_status) <- make.names(levels(factor(temp$loan_status)))
levels(temp$verification_status) <- make.names(levels(factor(temp$verification_status)))
levels(temp$grade) <- make.names(levels(factor(temp$grade)))
levels(temp$addr_state) <- make.names(levels(factor(temp$addr_state)))

#creating model matrix
m <- model.matrix(  ~.,  data = temp[,-1] )

colnames(m)[1] <-'Intercept'
m<-scale(m)
m[,1]<-1
colnames(m)<-gsub(' ','',fixed = T,colnames(m))

m[,24]<-ifelse(m[,24]>0,1,0)###0 is default
unique(m[,24])

train_m<-m[1:nrow(up_train),]
test_m<-m[-c(1:nrow(up_train)),]


n <- colnames(train_m)
#defining formula
f_lda <- as.formula(paste("loan_statusFully.Paid ~", paste(n[!n %in% c("loan_statusFully.Paid","Intercept","home_ownershipRENT")], collapse = " + ")))

lda1<-lda(f_lda,data=data.frame(train_m[,-21]))
pred_lda<-predict(lda1,data.frame(train_m)) #head(pred_lda$posterior,20)
pred_lda_test<-predict(lda1,data.frame(test_m))
p<-table('prediction'=pred_lda_test$class,'truth'=test_m[,24])
sum(diag(p))/sum(p)##test accuracy
p<-table('prediction'=pred_lda$class,'truth'=train_m[,24])##train confusion matrix
sum(diag(p))/sum(p)##train accuracy

####QDA####

qda1<-qda(f_lda,data=data.frame(train_m[,-21]))
pred_qda_train<-predict(qda1,data.frame(train_m))
table('prediction'=pred_qda_train$class,'truth'=train_m[,24])##train confusion matrix
pred_qda_test<-predict(qda1,data.frame(test_m))
p<-table('prediction'=pred_lda_test$class,'truth'=test_m[,24])##test confusion matrix
sum(diag(p))/sum(p)##train accuracy

#####RF#####


#determing best parameter for model 
err<-NULL
cverr<-NULL

trainerr<-NULL
testerr<-NULL

mtr<-2
ntr<-2
i<-1
mtry1<-NULL
ntree1<-NULL
k<-1

for(mtr in c(5,8,11,15,18,21,27,30))
{for (ntr in c( 300,500,800))
{ 
  
  model<-randomForest(loan_status~.-member_id-application_type-joint_status,data=up_train,mtry=mtr,ntree=ntr,importance=T)
  trainerr[k]<-mean(model$err.rate[,1])  
  pred_test<-predict(model,newdata=test)
  p<-table(pred_test,test[,'loan_status'])  
  testerr[k]<-sum(diag(p))/sum(p)
  mtry1[k]<-mtr
  ntree1[k]<-ntr
  print(paste(k,'rf done'))
  k<-k+1
}
}


save.image("output_rf.RData")

up_train$loan_status<-factor(up_train$loan_status,levels(up_train$loan_status)[c(2,1)])
###fitting optimal random forest
model_rf_optimal<-randomForest(loan_status~.-member_id-application_type-joint_status,data=up_train,mtry=5,ntree=800,importance=T,keep.inbag=T)



trainerr<-mean(model$err.rate[,1])  
pred_rf_test<-predict(model,newdata=test)
pred_test2<-predict(model,newdata=test,type='prob')
p<-table(pred_rf_test,test[,'loan_status'])  ##test confusion matrix
testacc<-sum(diag(p))/sum(p)###test accuracy

pred_rf_train<-predict(model,newdata=train)
p<-table(pred_rf_train,up_train[,'loan_status'])  ##train confusion matrix
train_acc<-sum(diag(p))/sum(p)###traint accuracy


##creating partial plot using randomForest-SRC package

model_data$issue_y

year<-as.data.frame(table(model_data$issue_y))


train1 = up_train
rfsrc_model<-rfsrc(loan_status~.-member_id-application_type-joint_status,data = train1,ntree = 200,tree.err = TRUE)

plot(gg_rfsrc(rfsrc_model), alpha=.5,main="Actual Vs Predicted") 
plot(gg_vimp(rfsrc_model,nvar = 10),main="Variable Importance plot",xlab="Importance by MSE")
varsel_west <- var.select(rfsrc_model)
gg_md <- gg_minimal_depth(varsel_west)
plot(gg_md)
plot(gg_minimal_vimp(gg_md))
gg_v <- gg_variable(rfsrc_model)
xvar <- gg_md$topvars
plot(gg_v, xvar=xvar)#+labs(y=Y)

plot(vimp(rfsrc_model,c("int_rate","term" ,"fico","purpose","Poverty","credit_length_weeks","Median_Income","home_ownership","revol_bal","loan_amnt" )))
plot(rfsrc_model, outcome.target = "fico")
plot.variable(rfsrc_model,"fico",partial = TRUE)
plot.variable(rfsrc_model,"int_rate",partial = TRUE)
plot.variable(rfsrc_model,"term",partial = TRUE)
plot.variable(rfsrc_model,"credit_length_weeks",partial = TRUE)
plot.variable(rfsrc_model,"Poverty",partial = TRUE)
plot.variable(rfsrc_model,"total_acc",partial = TRUE)
plot.variable(rfsrc_model,"installment",partial = TRUE)
plot.variable(rfsrc_model,"Median_Income",partial = TRUE)
plot.variable(rfsrc_model,"loan_amnt",partial = TRUE)
plot.variable(rfsrc_model,"emp_length",partial = TRUE)
plot.variable(rfsrc_model,"home_ownership",partial = TRUE)
plot.variable(rfsrc_model,"issue_y",partial = TRUE)
plot.variable(rfsrc_model,partial = TRUE)
partial.rfsrc(rfsrc_model,xvar=xvar)



####BART#####


options(java.parameters = "-Xmx100g")
library(bartMachine)
set_bart_machine_num_cores(10)
setwd("/home/wadhwa5/Desktop")
load('resampling.RData')
bart_machine_model_1<-bartMachine(up_train[,-c(1,23,25,13)],up_train[,13],serialize = TRUE)##default parameters
save.image("output_bart1.RData")

options(java.parameters = "-Xmx100g")
library(bartMachine)
set_bart_machine_num_cores(10)
setwd("/home/wadhwa5/Desktop")
load('resampling.RData')
bart_machine_model_2<-bartMachine(up_train[,-c(1,23,25,13)],up_train[,13],serialize = TRUE,num_tree=250)##num_tree=250
pred_bart_train<-bart_predict_for_test_data(cv.bart, up_train[,-c(1,23,25,13)], up_train[,13])
pred_bart_test<-bart_predict_for_test_data(cv.bart, test[,-c(1,23,25,13)], test[,13])


save.image("output_bart2.RData")

options(java.parameters = "-Xmx100g")
library(bartMachine)
set_bart_machine_num_cores(10)
setwd("/home/wadhwa5/Desktop")
load('resampling.RData')
bart_machine_model_3<-bartMachine(up_train[,-c(1,23,25,13)],up_train[,13],serialize = TRUE,num_tree=100)###num_tree=100
save.image("output_bart3.RData")


options(java.parameters = "-Xmx100g")
library(bartMachine)
set_bart_machine_num_cores(10)
setwd("/home/wadhwa5/Desktop")
load('resampling.RData')
bart_machine_model_4<-bartMachine(up_train[,-c(1,23,25,13)],up_train[,13],serialize = TRUE,num_tree=130)##num_tree=130
save.image("output_bart4.RData")





#####SVM Parallel#####

library(ROCR)


#svm with kernel='radial', cost=1,gamma=0.1
model_svm1 <- parallelSVM(loan_status~., data = up_train[,-c(1,23,25)],kernel='radial',cost=1, gamma=0.1, probability=TRUE)
svm.pred.train <- predict(model_svm1, up_train[,-c(1,23,25,13)])
svm.pred.train1 <- predict(model_svm1, up_train[,-c(1,23,25,13)],type='probability')
svm.pred.test  <- predict(model_svm1, test[,-c(1,23,25,13)])
save.image("output_svm1.RData")
ptrain1<-table(true=up_train$loan_status, pred=svm.pred.train)##train confusion matrix
acc1=sum(diag(ptrain1))/sum(ptrain1)##train accuracy
ptest1<-table(true=test$loan_status, pred=svm.pred.test)###test confusion matrix
acctest1=sum(diag(ptest1))/sum(ptest1)###test accuracy

#svm with kernel='radial', cost=10,gamma=1
model_svm2 <- parallelSVM(loan_status~., data = up_train[,-c(1,23,25)],kernel='radial',cost=10, gamma=1, probability=TRUE)
pred_svm_train <- predict(model_svm2, up_train[,-c(1,23,25,13)])
pred_svm_test  <- predict(model_svm2, test[,-c(1,23,25,13)])
ptrain2<-table(true=up_train$loan_status, pred=pred_svm_train)##train confusion matrix
acc2=sum(diag(ptrain2))/sum(ptrain2)##train accuracy
ptest2<-table(true=test$loan_status, pred=pred_svm_test)###test confusion matrix
acctest2=sum(diag(ptest2))/sum(ptest2)
save.image("output_svm2.RData")

#svm with kernel='radial', cost=1,gamma=2
model_svm3 <- parallelSVM(loan_status~., data = up_train[,-c(1,23,25)],kernel='radial',cost=10, gamma=2, probability=TRUE)
svm.pred.train3 <- predict(model_svm3, up_train[,-c(1,23,25,13)])
svm.pred.test3  <- predict(model_svm3, test[,-c(1,23,25,13)])
ptrain3<-table(true=up_train$loan_status, pred=svm.pred.train3)##train confusion matrix
acc3=sum(diag(ptrain3))/sum(ptrain3)##train accuracy
ptest3<-table(true=test$loan_status, pred=svm.pred.test3)###test confusion matrix
acctest3=sum(diag(ptest3))/sum(ptest3)###test confusion matrix
save.image("output_svm3.RData")

#svm with kernel='radial', cost=100,gamma=2
model_svm4 <- parallelSVM(loan_status~., data = up_train[,-c(1,23,25)],kernel='radial',cost=100, gamma=2, probability=TRUE)
svm.pred.train4 <- predict(model_svm4, up_train[,-c(1,23,25,13)])
svm.pred.test4  <- predict(model_svm4, test[,-c(1,23,25,13)])
ptrain4<-table(true=up_train$loan_status, pred=svm.pred.train4)##train confusion matrix
acc4=sum(diag(ptrain4))/sum(ptrain4)##train accuracy
ptest4<-table(true=test$loan_status, pred=svm.pred.test4)###test confusion matrix
acctest4=sum(diag(ptest4))/sum(ptest4)###test confusion matrix
save.image("output_svm4.RData")


####logistic####

library(glmnet)


x <- model.matrix(loan_status ~ 0 + . , data=up_train[,-c(1,23,25)])##defining formula
test_x<-model.matrix(loan_status ~ 0 + . , data=test[,-c(1,23,25)])##defining formula
##defining different values of alpha

model_logistic1 = cv.glmnet(x,y=factor(up_train$loan_status),
                            family='binomial',alpha=0,type.measure = 'class')
model_logistic2 = cv.glmnet(x,y=factor(up_train$loan_status),
                            family='binomial',alpha=0.5,type.measure = 'class')
model_logistic3 = cv.glmnet(x,y=factor(up_train$loan_status),
                            family='binomial',alpha=1,type.measure = 'class')
par(mfrow=c(1,3))
plot(model_logistic1);plot(model_logistic2);plot(model_logistic3)

##best model=alpha=0.5,lambda.min
pred_lr_train<-predict(model_logistic2,newx=x,s=model_logistic1$lambda.min,type =  "response",alpha=0.5)
table(up_train$loan_status,pred_train_2<0.5)##train confusion matrix
pred_train<-predict(model_logistic2,newx=x,s=model_logistic1$lambda.min,type =  "class",alpha=0.5)### Default is predicted by model if probability is less than 0.5
pred_lr_test<-predict(model_logistic2,newx=x,s=model_logistic1$lambda.min,type =  "response",alpha=0.5)

##removing variables based on previous run
newX=model.matrix(loan_status ~ 0 + . , data=test[,-c(1,23,25)])
#alpha=0
pred1<-predict(model_logistic1,newx=newX,s=model_logistic1$lambda.min,type =  "response",alpha=0)
pred11<-predict(model_logistic1,newx=newX,s=model_logistic1$lambda.min,type =  "class",alpha=0)### Default is predicted by model if probability is less than 0.5

p1<-table(test$loan_status,pred11)##test confusion matrix
sum(diag(p1))/sum(p1)###test accuracy
p1<-table(up_train$loan_status,predict(model_logistic1,newx=x,s=model_logistic1$lambda.min,type =  "class"))##train confusion matrix
sum(diag(p1))/sum(p1)##train accuracy

##alpha=0.5
pred_lr_train<-predict(model_logistic2,newx=x,s=model_logistic2$lambda.min,type='response',alpha=0.5)
pred_lr_test<-predict(model_logistic2,newx=test_x,s=model_logistic2$lambda.min,type='response',alpha=0.5)
pred22<-predict(model_logistic2,newx=newX,s=model_logistic2$lambda.min,type='class',alpha=0.5)
p2<-table(test$loan_status,pred22)##test confusion matrix
sum(diag(p))/sum(p)###test accuracy

##alpha=1
pred3<-predict(model_logistic3,newx=newX,s=model_logistic3$lambda.min,alpha=1)##train confusion matrix
p3<-table(test$loan_status,pred3>0.5)##train confusion matrix




###Bagging####

#bagging with ntree=100
model1<-bagging(loan_status~.,data=up_train,nbagg = 100)
pred_train1 <-predict(model1,newdata=up_train)
pred_test1<-predict(model1,newdata=test)
tabletrain1<-table(pred_train1,up_train[,'loan_status'])##train confusion matrix
tabletest1<-table(pred_test1,test[,'loan_status'])##test confusion matrix
acc<-sum(diag(tabletrain1))/sum(tabletrain1)###train accuracy
acctest<-sum(diag(tabletest1))/sum(tabletest1)##test accuracy
save.image("output_bagging001.RData")

#bagging with ntree=500
model2<-bagging(loan_status~.,data=up_train,nbagg = 500)
pred_train2 <-predict(model2,newdata=up_train)
pred_test2<-predict(model2,newdata=test)
tabletrain2<-table(pred_train2,up_train[,'loan_status'])
tabletest2<-table(pred_test2,test[,'loan_status'])
acc<-sum(diag(tabletrain2))/sum(tabletrain2)
acctest<-sum(diag(tabletest2))/sum(tabletest2)
save.image("output_bagging002.RData")

#bagging with ntree=700
model3<-bagging(loan_status~.,data=up_train,nbagg = 700)
pred_bag_train <-predict(model3,newdata=up_train)
pred_bag_test<-predict(model3,newdata=test)
tabletrain3<-table(pred_bag_train,up_train[,'loan_status'])
tabletest3<-table(pred_bag_test,test[,'loan_status'])
acc<-sum(diag(tabletrain3))/sum(tabletrain3)
acctest<-sum(diag(tabletest3))/sum(tabletest3)
save.image("output_bagging003.RData")

#bagging with ntree=1000
model4<-bagging(loan_status~.,data=up_train,nbagg = 1000)
pred_train4 <-predict(model4,newdata=up_train)
pred_test4<-predict(model4,newdata=test)
tabletrain4<-table(pred_train4,up_train[,'loan_status'])
tabletest4<-table(pred_test4,test[,'loan_status'])
acc<-sum(diag(tabletrain4))/sum(tabletrain4)
acctest<-sum(diag(tabletest4))/sum(tabletest4)
save.image("output_bagging004.RData")












#####cross validation code to measure cv-performance metrics####
k = 10
folds <- cvFolds(NROW(data), K=k)
metrics_lda <- data.frame()
metrics_bag<-data.frame()
metrics_bart<-data.frame()
metrics_svm<-data.frame()
metrics_lr<-data.frame()
metrics_lda<-data.frame()
metrics_qda<-data.frame()

for(i in 1:k){
  train.cv <- data[folds$subsets[folds$which != i], ] 
  validation <- data[folds$subsets[folds$which == i], ] 
  
  #random forest
  cv.rf<-randomForest(loan_status~.-member_id-application_type-joint_status,data=train.cv,mtry=5,ntree=800,importance=T,keep.inbag=T)
  pred.rf<-predict(cv.rf,newdata = validation)
  p.rf<-table(pred.rf,validation[,'loan_status']) 
  metrics_rf[i,1]<-sum(diag(p.rf))/sum(p.rf)##accuracy
  metrics_rf[i,2]<-sum(p.rf[1,1])/sum(p.rf[,1])##sensitivity
  metrics_rf[i,3]<-sum(p.rf[2,2])/sum(p.rf[,2])##specifity
  pred_rf <- prediction(pred.rf, validation$loan_status)
  metrics_rf[i,4]=as.numeric(performance(pred_rf, "auc")@y.values)#AUC
  metrics_rf[i,5]<-sqrt(metrics_rf[i,2]*metrics_rf[i,3])##G-Mean
  
  #bart machine
  options(java.parameters = "-Xmx100g")
  library(bartMachine)
  set_bart_machine_num_cores(10)
  cv.bart<-bartMachine(train.cv[,-c(1,23,25,13)],train.cv[,13],serialize = TRUE,num_tree=250)##num_tree=250
  pred.bart<-bart_predict_for_test_data(cv.bart, validation[,-c(1,23,25,13)], validation[,13])
  p.bart<-table(pred.bart,validation[,'loan_status']) 
  metrics_bart[i,1]<-sum(diag(p.bart))/sum(p.bart)##accuracy
  metrics_bart[i,2]<-sum(p.bart[1,1])/sum(p.bart[,1])##sensitivity
  metrics_bart[i,3]<-sum(p.bart[2,2])/sum(p.bart[,2])##specifity
  pred_bart <- prediction(pred.bart, validation$loan_status)
  metrics_bart[i,4]=as.numeric(pebartormance(pred_bart, "auc")@y.values)#AUC
  metrics_bart[i,5]<-sqrt(metrics_bart[i,2]*metrics_bart[i,3])##G-Mean 
  
  
  
  
  #svm
  cv.svm<-parallelSVM(loan_status~., data = train.cv[,-c(1,23,25)],kernel='radial',cost=10, gamma=1, probability=TRUE)
  pred.svm<-predict(cv.svm,newdata = validation[,-c(1,23,25,13)])
  p.svm<-table(pred.svm,validation[,'loan_status']) 
  metrics_svm[i,1]<-sum(diag(p.svm))/sum(p.svm)##accuracy
  metrics_svm[i,2]<-sum(p.svm[1,1])/sum(p.svm[,1])##sensitivity
  metrics_svm[i,3]<-sum(p.svm[2,2])/sum(p.svm[,2])##specifity
  pred_svm <- prediction(pred.svm, validation$loan_status)
  metrics_svm[i,4]=as.numeric(pesvmormance(pred_svm, "auc")@y.values)#AUC
  metrics_svm[i,5]<-sqrt(metrics_svm[i,2]*metrics_svm[i,3])##G-Mean
  
  
  #bag tree
  cv.bag<-bagging(loan_status~.,data=train.cv,nbagg = 700)
  pred.bag<-predict(cv.bag,newdata = validation)
  p.bag<-table(pred.bag,validation[,'loan_status']) 
  metrics_bag[i,1]<-sum(diag(p.bag))/sum(p.bag)##accuracy
  metrics_bag[i,2]<-sum(p.bag[1,1])/sum(p.bag[,1])##sensitivity
  metrics_bag[i,3]<-sum(p.bag[2,2])/sum(p.bag[,2])##specifity
  pred_bag <- prediction(pred.bag, validation$loan_status)
  metrics_bag[i,4]=as.numeric(pebagormance(pred_bag, "auc")@y.values)#AUC
  metrics_bag[i,5]<-sqrt(metrics_bag[i,2]*metrics_bag[i,3])##G-Mean
  
  
  
  #Linear Discriminant analysis
  n <- colnames(train.cv)
  #defining formula
  f_lda <- as.formula(paste("loan_statusFully.Paid ~", paste(n[!n %in% c("loan_statusFully.Paid","Intercept","home_ownershipRENT")], collapse = " + ")))
  
  cv.lda<-lda(f_lda,data=data.frame(train.cv[,-21]))
  pred.lda<-predict(cv.lda,newdata = validation)
  p.lda<-table(pred.lda,validation[,'loan_status']) 
  metrics_lda[i,1]<-sum(diag(p.lda))/sum(p.lda)##accuracy
  metrics_lda[i,2]<-sum(p.lda[1,1])/sum(p.lda[,1])##sensitivity
  metrics_lda[i,3]<-sum(p.lda[2,2])/sum(p.lda[,2])##specifity
  pred_lda <- prediction(pred.lda, validation$loan_status)
  metrics_lda[i,4]=as.numeric(peldaormance(pred_lda, "auc")@y.values)#AUC
  metrics_lda[i,5]<-sqrt(metrics_lda[i,2]*metrics_lda[i,3])##G-Mean
  
  
  #Quadratic Discriminant Analaysis
  n <- colnames(train.cv)
  #defining formula
  f_qda <- as.formula(paste("loan_statusFully.Paid ~", paste(n[!n %in% c("loan_statusFully.Paid","Intercept","home_ownershipRENT")], collapse = " + ")))
  
  cv.qda<-qda(f_qda,data=data.frame(train.cv[,-21]))
  pred.qda<-predict(cv.qda,newdata = validation)
  p.qda<-table(pred.qda,validation[,'loan_status']) 
  metrics_qda[i,1]<-sum(diag(p.qda))/sum(p.qda)##accuracy
  metrics_qda[i,2]<-sum(p.qda[1,1])/sum(p.qda[,1])##sensitivity
  metrics_qda[i,3]<-sum(p.qda[2,2])/sum(p.qda[,2])##specifity
  pred_qda <- prediction(pred.qda, validation$loan_status)
  metrics_qda[i,4]=as.numeric(peqdaormance(pred_qda, "auc")@y.values)#AUC
  metrics_qda[i,5]<-sqrt(metrics_qda[i,2]*metrics_qda[i,3])##G-Mean
  
  #Logistic Regression
  
  x_lr <- model.matrix(loan_status ~ 0 + . , data=train.cv[,-c(1,23,25)])##defining formula
  x1_lr<-model.matrix(loan_status ~ 0 + . , data=validation[,-c(1,23,25)])##defining formula
  cv.lr = cv.glmnet(x_lr,y=factor(train.cv$loan_status),
                    family='binomial',alpha=0.5,type.measure = 'class')
  
  pred.lr<-predict(model_logistic2,newx=x1_lr,s=model_logistic1$lambda.min,type =  "response",alpha=0.5)
  p.lr<-table(pred.lr,validation[,'loan_status']<0.5) 
  metrics_lr[i,1]<-sum(diag(p.lr))/sum(p.lr)##accuracy
  metrics_lr[i,2]<-sum(p.lr[1,1])/sum(p.lr[,1])##sensitivity
  metrics_lr[i,3]<-sum(p.lr[2,2])/sum(p.lr[,2])##specifity
  pred_lr <- prediction(pred.lr, validation$loan_status)
  metrics_lr[i,4]=as.numeric(pelrormance(pred_lr, "auc")@y.values)#AUC
  metrics_lr[i,5]<-sqrt(metrics_lr[i,2]*metrics_lr[i,3])##G-Mean
  
}

colnames(metrics_qda)<-c("Accuracy",'Sensitivity','Specifity','AUC','GMean')
colnames(metrics_lda)<-c("Accuracy",'Sensitivity','Specifity','AUC','GMean')
colnames(metrics_rf)<-c("Accuracy",'Sensitivity','Specifity','AUC','GMean')
colnames(metrics_bart)<-c("Accuracy",'Sensitivity','Specifity','AUC','GMean')
colnames(metrics_bag)<-c("Accuracy",'Sensitivity','Specifity','AUC','GMean')
colnames(metrics_lr)<-c("Accuracy",'Sensitivity','Specifity','AUC','GMean')
colnames(metrics_svm)<-c("Accuracy",'Sensitivity','Specifity','AUC','GMean')

metrics_qda1<-colMeans(metrics_qda)
metrics_lda1<-colMeans(metrics_lda)
metrics_rf1<-colMeans(metrics_rf)
metrics_bart1<-colMeans(metrics_bart)
metrics_bag1<-colMeans(metrics_bag)
metrics_lr1<-colMeans(metrics_lr)
metrics_svm1<-colMeans(metrics_svm)

cv_metrics<-rbind(metrics_qda1,metrics_lda1,metrics_rf1,metrics_bart1,metrics_bag1,metrics_lr1,metrics_svm1)
cv_metrics<-cbind(cv_metrics,c('qda','lda','rf','bart','bag','lr','svm'))
colnames(cv_metrics)<-c("cv_Accuracy",'cv_Sensitivity','cv_Specifity','cv_AUC','cv_GMean','Model')





#####AUC CUrve on train and test data#####
combined_df_train<-cbind(pred_bart_train,pred_bag_train,pred_bag_train,pred_rf_train,pred_lr_train,pred_qda_train,pred_lda_train)
combined_df_test<-cbind(pred_bart_test,pred_bag_test,pred_bag_test,pred_rf_test,pred_test_train,pred_qda_test,pred_lda_test)
colnames(combined_df_test)<-c("Actual","BART_Model","Bagged_Model","RandomForest_Model","Logistic_Regression","QDA","LDA",'SVM')
colnames(combined_df_train)<-c("Actual","BART_Model","Bagged_Model","RandomForest_Model","Logistic_Regression","QDA","LDA",'SVM')


combined_df_train$actualcode<-ifelse(combined_df_train$Actual=='Default',1,0)
combined_df_test$actualcode<-ifelse(combined_df_test$Actual=='Default',1,0)
combined_df_train$Bagged_Model<-combined_df_train$LDA+as.vector(rnorm(56226,0,0.01))
#combined_df_test$Actua<-factor(combined_df_test$code,levels(combined_df_test$code)
#[c(2,1)])
#levels(combined_df_train$code)<-levels(combined_df_test$code)[order(levels(combined_df_test$code))]
acc<-NULL
sens<-NULL
spec<-NULL
gmean<-NULL
AUC=NULL
i<-2
for (i in c(2,3,4,5,6,7,9)){
  pred <- prediction(combined_df_train[,i], combined_df_train$actualcode)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
  AUC[i-1]=as.numeric(performance(pred, "auc")@y.values)##AUC Value
  p<-table(combined_df_train$actualcode,combined_df_train[,i]>0.5)
  p
  acc[i-1]<-sum(diag(p))/sum(p)#accuracy
  sens[i-1]<-p[2,2]/sum(p[2,])#sensitivity
  spec[i-1]<-p[1,1]/sum(p[1,])#specifity
  gmean[i-1]<-sqrt(sens[i-1]*spec[i-1])#gmean
  plot(perf, col=rainbow(10),main=paste("TRAIN:AUC curve for ",colnames(combined_df_train)[i]))# train auc curve
  abline(a=0,b=1,lty=2)
}

acc_test<-NULL
sens_test<-NULL
spec_test<-NULL
gmean_test<-NULL
AUC_test=NULL

i<-2
for (i in c(2,3,4,5,6,7,9)){
  pred_test <- prediction(combined_df_test[,i], combined_df_test$actualcode)
  perf_test <- performance(pred_test, measure = "tpr", x.measure = "fpr") 
  AUC_test[i-1]=as.numeric(performance(pred_test, "auc")@y.values)##AUC Value
  p_test<-table(combined_df_test$actualcode,combined_df_test[,i]>0.5)
  acc_test[i-1]<-sum(diag(p_test))/sum(p_test)#accuracy
  sens_test[i-1]<-p_test[2,2]/sum(p_test[2,])#sensitivity
  spec_test[i-1]<-p_test[1,1]/sum(p_test[1,])#specifity
  gmean_test[i-1]<-sqrt(sens_test[i-1]*spec_test[i-1])#gmean
  plot(perf_test, col=rainbow(10),main=paste("TEST: AUC curve for ",colnames(combined_df_test)[i]))# test auc curve
  abline(a=0,b=1,lty=2)
