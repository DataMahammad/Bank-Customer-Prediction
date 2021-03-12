library(tidyverse)
library(scorecard)
library(skimr)
library(inspectdf)
library(caret)
library(glue)
library(highcharter)
library(h2o)
library(dplyr)

raw <- read_delim("bank-full.csv",delim = ";")
#raw %>% View()

raw %>% glimpse()

raw %>% inspect_na()

raw %>% dim()

raw$y %>% table() %>% prop.table()

#Imbalance problem
#names(raw)[names(raw) == "y"] <- "Class"

#library(imbalance)
#raw %>% imbalanceRatio("Class")

#imbalanceRatio(raw,classAttr = "Class")

#raw <- raw %>% oversample(ratio = 1, method = "PDFOS", filtering = TRUE, iterations = 500, classAttr = "y")
#raw$y %>% table()

raw$y <- raw$y %>% 
  factor(levels = c("yes","no"),
         labels = c(1,0))


#DATA PREPROCESSING

df.num <- raw %>% select_if(is.numeric)
df.chr <- raw %>%  
  mutate_if(is.character,as.factor) %>% 
  select_if(is.factor) %>% select(y,everything())

df.chr %>% glimpse()
#Outliers

df.num %>% skim()
df.num %>% dim()

df.num <- df.num  %>% as.data.frame()

num_vars <- df.num %>% names()


for_vars <- c()
for(i in 1:length(num_vars)){
  Outliers <- boxplot(df.num[num_vars[i]],plot = F)$out
  if(length(Outliers)>0){
    for_vars[i] <- num_vars[i]
  }
}

for_vars <- for_vars %>% as.data.frame() %>% drop_na() %>% pull(.)

df.num <- df.num %>% as.matrix()

boxplot(df.num[,"age"])
boxplot(df.num[,"balance"])
boxplot(df.num[,"duration"])
boxplot(df.num[,"campaign"])
boxplot(df.num[,"pdays"])
boxplot(df.num[,"previous"])


for(i in for_vars){
  Outliers <- boxplot(df.num[,i],plot = F)$out
  mean <- mean(df.num[,i],na.rm = T)
  
  o3 <- ifelse(Outliers>mean,Outliers,NA) %>% as.data.frame() %>% drop_na() %>% pull(.)
  o1 <- ifelse(Outliers>mean,Outliers,NA) %>% as.data.frame() %>% drop_na() %>% pull(.)
  
  val3 <- quantile(df.num[,i],0.75,na.rm = T) + 1.5 * IQR(df.num[,i],na.rm = T)
  val1 <- quantile(df.num[,i],0.25,na.rm = T) - 1.5 * IQR(df.num[,i],na.rm = T)
  
  df.num[which(df.num[,i] %in% o3),i] <- val3
  df.num[which(df.num[,i] %in% o1),i] <- val1
  
}
boxplot(df.num[,"age"])
boxplot(df.num[,"balance"]) #Not all outliers are treated
boxplot(df.num[,"duration"])
boxplot(df.num[,"campaign"])
boxplot(df.num[,"pdays"])#Not all outliers are treated
boxplot(df.num[,"previous"])


#Treating "balance" again 
Outliers <- boxplot(df.num[,"balance"],plot = F)$out
mean <- mean(df.num[,"balance"],na.rm = T)

o3 <- ifelse(Outliers>mean,Outliers,NA) %>% as.data.frame() %>% drop_na() %>% pull(.)
o1 <- ifelse(Outliers>mean,Outliers,NA) %>% as.data.frame() %>% drop_na() %>% pull(.)

val3 <- quantile(df.num[,"balance"],0.75,na.rm = T) + 1.5 * IQR(df.num[,"balance"],na.rm = T)
val1 <- quantile(df.num[,"balance"],0.25,na.rm = T) - 1.5 * IQR(df.num[,"balance"],na.rm = T)

df.num[which(df.num[,"balance"] %in% o3),"balance"] <- val3
df.num[which(df.num[,"balance"] %in% o1),"balance"] <- val1

#Treating "pdays" again
Outliers <- boxplot(df.num[,"pdays"],plot = F)$out
mean <- mean(df.num[,"pdays"],na.rm = T)

o3 <- ifelse(Outliers>mean,Outliers,NA) %>% as.data.frame() %>% drop_na() %>% pull(.)
o1 <- ifelse(Outliers>mean,Outliers,NA) %>% as.data.frame() %>% drop_na() %>% pull(.)

val3 <- quantile(df.num[,"pdays"],0.75,na.rm = T) + 1.5 * IQR(df.num[,"pdays"],na.rm = T)
val1 <- quantile(df.num[,"pdays"],0.25,na.rm = T) - 1.5 * IQR(df.num[,"pdays"],na.rm = T)

df.num[which(df.num[,"pdays"] %in% o3),"pdays"] <- val3
df.num[which(df.num[,"pdays"] %in% o1),"pdays"] <- val1


boxplot(df.num[,"balance"]) #Not all outliers are treated
#Duuno why:/
boxplot(df.num[,"pdays"]) #Outliers are treated

#One Hote Encoding
df.chr %>% glimpse()

ohe <- dummyVars(" ~ .", data = df.chr[-1]) %>% 
  predict(newdata = df.chr[-1]) %>% 
  as.data.frame()

ohe %>% glimpse()


df <- cbind(df.chr[1],ohe,df.num)
df %>% glimpse()


names(df) <- names(df) %>% 
  str_replace_all("\\.","_") %>% 
  str_replace_all("-","_")


names(df) <- names(df) %>% gsub("job_admin_","job_admin",.) ; names(df)

#Standardize variables
#df[,-1] <- df[,-1] %>% scale() %>% as.data.frame()
#DOOOO IIIIIT WITH df.iv !!!!!!!!!!!!!!!!!!
#df %>% skim()

#Filtering with IV values
install.packages("InformationValue")
library(InformationValue)


iv <- df %>% iv(y = 'y') %>% as.tibble() %>% 
      mutate(info_value = round(info_value,3)) %>% 
      arrange(desc(info_value)) ; iv


ivars <- iv %>% filter(info_value>0.02) %>% select(variable) %>% pull(.)


df.iv <- df %>% select(y,ivars)
#Standardize the values
df.iv[,-1] <- df.iv[,-1] %>% scale() %>% as.data.frame()

df.iv %>% skim()
#woe binning
bins <- df.iv %>% woebin("y")

df_list <- df.iv %>% split_df("y",ratio = 0.8,seed = 123)

train_woe <- df_list$train %>% woebin_ply(bins) 
test_woe <- df_list$test %>% woebin_ply(bins)

names <- train_woe %>% names() %>% gsub("_woe","",.) 
names(train_woe) <- names              ; names(test_woe) <- names


#Multicolleniarity

target <- 'y'
variables <- train_woe %>% select(-y) %>% names()

f <- as.formula(paste(target, paste(variables, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = train_woe, family = "binomial")

glm %>% summary()

coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
variables <- variables[!variables %in% coef_na]


f <- as.formula(paste(target, paste(variables, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = train_woe, family = "binomial")

glm %>% summary()

while(glm %>% vif() %>% arrange(desc(gvif)) %>% .[1,2] >= 1.5){
  afterVIF <- glm %>% vif() %>% arrange(desc(gvif)) %>% .[-1,"variable"]
  afterVIF <- afterVIF$variable
  f <- as.formula(paste(target, paste(afterVIF, collapse = " + "), sep = " ~ "))
  glm <- glm(f, data = train_woe, family = "binomial")
}


variables <- glm %>% vif() %>% arrange(desc(gvif))
variables <- variables$variable

#Modelling
library(rJava)

Sys.setenv(JAVA_HOME= "C:\\Program Files\\Java\\jre1.8.0_271")
Sys.getenv("JAVA_HOME")


h2o.init(nthreads = -1, max_mem_size = '2g', ip = "127.0.0.1", port = 54321)

train_h2o <- train_woe %>% select(target,variables) %>% as.h2o()
test_h2o <- test_woe %>% select(target,variables) %>% as.h2o()


model <- h2o.glm(
  x = variables, y = target, family = "binomial", 
  training_frame = train_h2o, validation_frame = test_h2o,
  nfolds = 10, seed = 123, remove_collinear_columns = T,
  balance_classes = T, lambda = 0, compute_p_values = T)


#Filtering p-values
while(model@model$coefficients_table %>%
      as.data.frame() %>%
      select(names,p_value) %>%
      mutate(p_value = round(p_value,3)) %>%
      .[-1,] %>%
      arrange(desc(p_value)) %>%
      .[1,2] >= 0.05){
  model@model$coefficients_table %>%
    as.data.frame() %>%
    select(names,p_value) %>%
    mutate(p_value = round(p_value,3)) %>%
    filter(!is.nan(p_value)) %>%
    .[-1,] %>%
    arrange(desc(p_value)) %>%
    .[1,1] -> v
  variables <- variables[variables!=v]
  
  train_h2o <- train_woe %>% select(target,variables) %>% as.h2o()
  test_h2o <- test_woe %>% select(target,variables) %>% as.h2o()
  
  model <- h2o.glm(
    x = variables, y = target, family = "binomial", 
    training_frame = train_h2o, validation_frame = test_h2o,
    nfolds = 10, seed = 123, remove_collinear_columns = T,
    balance_classes = T, lambda = 0, compute_p_values = T)
}

model@model$coefficients_table %>%
  as.data.frame() %>%
  select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>% 
  arrange(desc(p_value))

model@model$coefficients %>%
  as.data.frame() %>%
  mutate(names = rownames(model@model$coefficients %>% as.data.frame())) %>%
  `colnames<-`(c('coefficients','names')) %>%
  select(names,coefficients)

h2o.varimp(model) %>% as.data.frame() %>% .[.$percentage != 0,] %>%
  select(variable, percentage) %>%
  hchart("pie", hcaes(x = variable, y = percentage)) %>%
  hc_colors(colors = 'orange') %>%
  hc_xAxis(visible=T) %>%
  hc_yAxis(visible=T)

#Evaluation metric

pred <- model %>% predict(newdata = test_h2o) %>% 
  as.data.frame() %>% select(p1,predict)

#The most optimal threshold
model %>% h2o.performance(newdata = test_h2o) %>%
  h2o.find_threshold_by_max_metric('f1')  

pred %>% glimpse()

eva <- perf_eva(
  pred = pred %>% pull(p1),
  label = df_list$test$y %>% as.character() %>% as.numeric(),
  binomial_metric = c("auc","gini"),
  show_plot = "roc");eva


eva$confusion_matrix$dat  #NUUUUULLLLLL
  
  
#Check overfitting
model %>%
  h2o.auc(train = T,
          valid = T,
          xval = T) %>%
  as_tibble() %>%
  round(2) %>%
  mutate(data = c('train','test','cross_val')) %>%
  mutate(gini = 2*value-1) %>%
  select(data,auc=value,gini)








