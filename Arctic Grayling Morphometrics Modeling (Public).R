################################################################################
# Validating Morphometrics as a non-lethal tool to determine Arctic Grayling Sex 
####################### Author: William Samuel #################################
###### Project Collaborators: Lauren Yancy, Elizabeth Hinkle, Jeff Falke #######
################################################################################


#If you only want to use this code to predict fish sex, that begins on line 1309



library(tidyverse)  #general data manipulation
library(ggplot2)    #plotting
library(cowplot)    #for cleaner ggplots
library(ggcorrplot) #to make a correlation plot
library(patchwork)  #Panel plots
library(MuMIn)      #To Dredge the model choices
library(AICcmodavg) #for model statistics
library(rcompanion) #for pseudo R-squared
library(DAAG)       #Evaluating the model



setwd("C:/Users/npwil/OneDrive/Desktop/School/Grad School/Side Quests/Grayling morphometrics project/Arctic-Grayling-Morphometrics-Public")



full.dat <- read.csv("full.dat.final.csv")



# Summary Statistics ------------------------------------------------------
#Number of Females and Males
str(full.dat)
#n=97
sum(full.dat$Sex=='M')
#Males = 45
#Females = 52

mean(full.dat$fork_length) #310
mean(full.dat$fork_measured) #307


ggplot(data=full.dat, aes(x=fork_length))+
  geom_histogram(binwidth = 20, fill = "lightgray", color = "darkgray", )+
  theme_cowplot()+
  labs(#title = "Fork Length of Sample", 
    x = "Fork Length (mm)", y = "Count")+
  theme()
#scale_y_continuous(expand=c(0,0),limits=c(0,20), breaks = c(5, 10, 15, 20))





fl_hist <- ggplot(data=full.dat, aes(x=fork_length, fill = Sex))+
  geom_histogram(binwidth = 20, color = "black", alpha = 0.8)+ #fill = "lightgray",
  theme_cowplot()+
  labs(#title = "Fork Length of Sample", 
    x = "Fork Length (mm)", y = "Count")+
  scale_y_continuous(expand=c(0,0),limits=c(0,12), breaks = c(2,4,6,8,10,12))+
  scale_fill_manual(values = c("darkred", "darkblue"))+
  scale_alpha_manual(values = 0.8)+  
  #geom_vline(xintercept = 245, lwd = 1, linetype = 1)+ #50% maturity size threshold for the Chena River
  #geom_vline(xintercept = 275, lwd = 1, linetype = 2)+ #50% maturity size threshold for the Chatanika River
  geom_vline(xintercept = 260, lwd = 1, linetype = 2)+ #50% of the population are mature at this fork length, averaged across the 2 rivers
  theme(legend.position = "none")+
  facet_wrap(~Sex)
fl_hist

#ggsave(plot= fl_hist,
#       filename = "/Fork Length Histogram.jpeg",
#       dpi = 2000, 
#       height = 4,
#       width = 6,
#       units = "in")


ggplot(data=full.dat, aes(x=fork_length))+
  geom_histogram(aes(y=..density..), binwidth = 20, fill = "lightgray", color = "darkgray")+
  geom_density()+ #need to fix the y-axis if I am going to use this
  theme_cowplot()+
  labs(#title = "Fork Length of Sample", 
    x = "Fork Length (mm)", y = "Count")+
  #scale_y_continuous(expand=c(0,0),limits=c(0,12), breaks = c(2,4,6,8,10,12))+
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank())+
  facet_wrap(~Sex)



m <- full.dat %>% filter(sex_num == 0)

f <- full.dat %>% filter(sex_num == 1)

#Testing if there is a significant difference between the dorsal fin length 
#(standardized by fork length) of males and females, and there is!
t.test(x = m$n, y = f$n, paired = F)

#T = 6.379, df = 66.486, p < 0.00001



#Showing the difference between male and female dorsal length (standardized by fork length)

fl_n <- ggplot(data=full.dat, aes(x=n, fill = Sex))+
  geom_histogram(color = "black", alpha = 0.8)+ #fill = "lightgray",
  theme_cowplot()+
  labs(#title = "Fork Length of Sample", 
    x = "Posterior Dosal Height:Fork Length (mm)", y = "Count")+
  scale_fill_manual(values = c("darkred", "darkblue"))+
  scale_alpha_manual(values = 0.8)+
  theme(legend.position = c(0.9,0.9))
#facet_wrap(~Sex)
fl_n

#ggsave(plot= fl_n,
#       filename = "R_Figures/Posterior Dorsal Height Histogram.jpeg",
#       dpi = 2000, 
#       height = 4,
#       width = 6,
#       units = "in")




fl_n_density <- ggplot(data=full.dat, aes(x=n, fill = Sex))+
  #geom_histogram(color = "black", alpha = 0.8)+ #fill = "lightgray",
  geom_density(alpha = 0.8, lwd = 1)+
  theme_cowplot()+
  labs(#title = "Fork Length of Sample", 
    x = "Posterior Dosal Height:Fork Length (mm)", y = "Count")+
  scale_fill_manual(values = c("darkred", "darkblue"))+
  scale_alpha_manual(values = 0.8)+
  theme(legend.position = c(0.85,0.9))
#facet_wrap(~Sex)
fl_n_density

#ggsave(plot= fl_n_density,
#       filename = "R_Figures/Posterior Dorsal Height Density.jpeg",
#       dpi = 2000, 
#       height = 5,
#       width = 5,
#       units = "in")





# VIF and Colinearity  ----------------------------------------------------


#library(ggcorrplot)
corr <- round(cor(full.dat[c(1:97),c(39:59)]), 1)

ggcorrplot(corr, type = "lower",
           lab = TRUE)
ggcorrplot(corr, type = "lower",
           lab = TRUE, method = "circle")
#Highly correlated (>0.7) 
#K and L (0.8) Pectoral Anal Distance and Preannal Length
#O and S (0.9) Dorsal measurements
#O and Q (0.7) posterior Dorsal height and anal fin height
#H and W (0.7) dorsal length and dorsal to adipose
#R and V (0.7) Pectoral fin length and dorsal end length


#VIF function from Jeff F. 
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  library(fmsb)
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}


preVIF_dat <- full.dat %>% 
  select(c(60:80)) 
str(preVIF_dat)

postVIF_dat <- vif_func(preVIF_dat)
postVIF_dat
#Removed
#predorsal_length_FL_g              VIF = 13.87755     
#preanal_length_FL_k                VIF = 11.00398       







# Constructing the Models --------------------------------------------------


colnames(full.dat)[apply(full.dat, 2, anyNA)]
#NA values in Adipose measurements (not that important) and Anal Fin Base Length, since a tag was covering it. That will prevent that from being included in the dredge model, so maybe just set it to the mean of similar fish???

#Replace NAs with the mean of the parameter
full.dat <- full.dat %>% 
  mutate(s = ifelse(is.na(s), mean(s, na.rm = TRUE), s)) %>% 
  mutate(o = ifelse(is.na(o), mean(o, na.rm = TRUE), o)) %>% 
  mutate(t = ifelse(is.na(t), mean(t, na.rm = TRUE), t)) 



#Final Global Model (without colinear predictors, g and K)
m.global <- glm(sex_num ~ a+b+c+d+e+f+h+i+j+l+m+n+o+p+q+r+s+t+u, data = full.dat, family = "binomial")
summary(m.global)         

nagelkerke(m.global)
accuracy(list(m.global))
#Pseudo.R.squared                   With NA     NA -> mean
#McFadden                            0.794        0.438
#Cox and Snell (ML)                  0.796        0.454
#Nagelkerke (Cragg and Uhler)        0.920        0.606
#Efron.r.squared                     0.782        0.541



options(na.action = "na.fail")
m.global <- dredge(m.global, trace = 2, evaluate = TRUE)
options(na.action = "na.omit")
#Look at the results from this dredge model selection and use it to develop the next model
#This will take a while to run since we have so many parameters. 




#Note that I added Posterior Dorsal Height (N) here, refer to the manuscript
m.dredge <- glm(sex_num ~ a+e+j+q+r+s+u+n, data = full.dat, family = "binomial")
summary(m.dredge)

nagelkerke(m.dredge)
accuracy(list(m.dredge))
#Pseudo.R.squared                   With NA     NA -> mean
#McFadden                            0.        0.355
#Cox and Snell (ML)                  0.        0.388
#Nagelkerke (Cragg and Uhler)        0.        0.518
#Efron.r.squared                     0.        0.483




#Just fins
m.fins.only <- glm(sex_num ~ l+m+n+o+p+q+r+s+t+u, data = full.dat, family = "binomial")
summary(m.fins.only)

nagelkerke(m.fins.only)
accuracy(list(m.fins.only))
#Pseudo.R.squared                   With NA     NA -> mean
#McFadden                            0.664        0.323
#Cox and Snell (ML)                  0.735        0.360
#Nagelkerke (Cragg and Uhler)        0.850        0.480
#Efron.r.squared                     0.586        0.467




#just dorsal fin metrics
m.dorsal.all <- glm(sex_num ~ h+l+m+n#predorsal_length_FL_g+dorsal_to_adipose_FL_v
                    , data = full.dat, family = "binomial")
summary(m.dorsal.all)

nagelkerke(m.dorsal.all)
accuracy(list(m.dorsal.all))
#Pseudo.R.squared                   With NA     NA -> mean
#McFadden                            0.272        0.272
#Cox and Snell (ML)                  0.317        0.314
#Nagelkerke (Cragg and Uhler)        0.419        0.419
#Efron.r.squared                     0.393        0.394



#just dorsal fin LENGTH
m.dorsal.length <- glm(sex_num ~ n #postdorsal_length_FL_h+dorsal_fin_base_length_FL_l+anterior_dorsal_height_FL_m+posterior_dorsal_height_FL_m+predorsal_length_FL_g+dorsal_to_adipose_FL_v
                       , data = full.dat, family = "binomial")
summary(m.dorsal.length)

nagelkerke(m.dorsal.length)
accuracy(list(m.dorsal.length))
#Pseudo.R.squared                   With NA     NA -> mean
#McFadden                            0.195        0.267
#Cox and Snell (ML)                  0.236        0.309
#Nagelkerke (Cragg and Uhler)        0.315        0.412
#Efron.r.squared                     0.286        0.391




#what about just with fish over 250mm?
grayling_dat_250 <- full.dat %>% 
  filter(fork_length >249)
str(grayling_dat_250)
summary(grayling_dat_250$fork_measured)

m.250 <- glm(sex_num ~ a+b+c+d+e+f+h+i+j+l+m+n+o+p+q+r+s+t+u, data = grayling_dat_250, family = "binomial")
summary(m.250)

nagelkerke(m.250)
accuracy(list(m.250))
#Pseudo.R.squared
#McFadden                            1.00
#Cox and Snell (ML)                  0.750
#Nagelkerke (Cragg and Uhler)        1.00
#Efron.r.squared                     1.00 



#what about just with fish over 300mm?
grayling_dat_300 <- full.dat %>% 
  filter(fork_length >299)
str(grayling_dat_300)
summary(grayling_dat_300$fork_measured)

m.300 <- glm(sex_num ~ a+b+c+d+e+f+h+i+j+l+m+n+o+p+q+r+s+t+u, data = grayling_dat_300, family = "binomial")
summary(m.300)

nagelkerke(m.300)
accuracy(list(m.300))
#Pseudo.R.squared
#McFadden                            1.00
#Cox and Snell (ML)                  0.750
#Nagelkerke (Cragg and Uhler)        1.00
#Efron.r.squared                     1.00 






# AIC and Model Selection ------------------------------------------------------
#https://www.statology.org/aic-in-r/


#specify model names
models <- list(m.global, m.dredge, m.fins.only, m.dorsal.all, m.dorsal.length,  m.250, m.300) #m.200, m.350, 
mod.names <- c('m.global', 'm.dredge', 'm.fins.only', 'm.dorsal.all', 'm.dorsal.length', 'm.250', 'm.300')#'m.fins.only','m.200', 'm.350' 'm.dorsal.length')

#calculate AIC of each model
aictab(cand.set = models, modnames = mod.names)
#The difference between m.250 and the global model is most informative, that a 250 mm fork length filter improves the model that much. 



# Model Evaluation --------------------------------------------------------



###Some model checks

#this is a 10 fold cross validation, can help evaluate overfitting
m0.cv <- cv.binary(m.global) # remains at 0.708, potential overfitting
m0.cv <- cv.binary(m.noVIF) # remains at 0.758
m0.cv <- cv.binary(m.dredge) # remains at 0.662, potential minimal overfitting
m0.cv <- cv.binary(m.fins.only) # 
m0.cv <- cv.binary(m.dorsal.only) # remains at 0.763, no indication of overfitting
m0.cv <- cv.binary(m.dorsal.length) # remains at 0.879, no indication of overfitting
m0.cv <- cv.binary(m.250) # remains at 0.788, no indication of overfitting
m0.cv <- cv.binary(m.300) # remains at 0.667, no indication of overfitting


#Assess the predictive accuracy. This basically does the same thing as LOOCV, except not as good....
fitted.results <- predict(m.global, newdata=subset(full.dat, select=c(1:61)), type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != full.dat$sex_num)
1-misClasificError
#Accuracy = 100%


#Plotting/calculating the Area under the curve of the true and false values is a way to evaluate model performance. As a rule of thumb, a model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5.
library(ROCR)
p <- predict(m.global, newdata=subset(full.dat, select=c(1:61)), type="response")
pr <- prediction(p, full.dat$sex_num)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
#= 1



###Estimating Specificity and Sensitivity
pred.m0 <- prediction(fitted(m.250), grayling_dat_250$sex_num)
perf.m0 <- performance(pred.m0, 'tpr','tnr') # true positive/negative rates
perf.m0@alpha.values[[1]][1] <- 1
# specificity
plot(perf.m0@alpha.values[[1]], perf.m0@x.values[[1]], xlab=perf.m0@alpha.name, 
     ylab='Classification rate', type='s')
# sensitivity
lines(perf.m0@alpha.values[[1]], perf.m0@y.values[[1]], type='s', col=2)
legend('right', c('specificity', 'sensitivity'), col=c(1,2),
       lty=c(1,1), lwd=c(1,1), bty='n', cex=.9)

which.min(abs(perf.m0@x.values[[1]]-perf.m0@y.values[[1]])) # Searching for cutoff that maximizes both sensitivity and specificity
perf.m0@alpha.values[[1]][949] # 0.513

dat$pred.m0 <- ifelse(fitted(m0) < 0.5132665, 0, 1)
dat$wrong.m0 <- ifelse(dat$Sex.Bin - dat$pred.m0 == 0, 0, 1)




#estimate overall precision
precision <- function(c) {
  tab1 <- table(fitted(m0)>c, dat$Sex)
  out <- diag(tab1)/apply(tab1, 2, sum)
  names(out) <- c('specificity', 'sensitivity')
  list(tab1, out)
}

precision <- function(c) {
  tab1 <- table(fitted(m.250)>c, grayling_dat_250$sex_num)
  out <- diag(tab1)/apply(tab1, 2, sum)
  names(out) <- c('specificity', 'sensitivity')
  list(tab1, out)
}
precision(perf.m0@alpha.values[[1]][949] - 0.00000001) # spec and sens 0.84

#Cant figure this out... 



#Checking for specification error. Issues with this one too
m1 <- glm(full.dat$sex_num ~ fitted(m.250) + I(fitted(m.250)^2), family = binomial)
#checking for specification error. fitted value should be significant, fitted value ^2 should not be significant. If the fitted value is significant, then we have likely omitted important variables.# from; https://stats.idre.ucla.edu/stata/webbooks/logistic/chapter3/lesson-3-logistic-regression-diagnostics/
summary(m1) # looks good

glm(full.dat$sex_num ~ fitted(m.dredge) + I(fitted(m.dredge)^2), family = binomial) #

glm(full.dat$sex_num ~ fitted(m.noVIF) + I(fitted(m.noVIF)^2), family = binomial) #

glm(full.dat$sex_num ~ fitted(m.reduced) + I(fitted(m.reduced)^2), family = binomial) #

glm(full.dat$sex_num ~ fitted(m.fins.only) + I(fitted(m.fins.only)^2), family = binomial) #

glm(full.dat$sex_num ~ fitted(m.dorsal.all) + I(fitted(m.dorsal.all)^2), family = binomial) #

glm(full.dat$sex_num ~ fitted(m.dorsal.length) + I(fitted(m.dorsal.length)^2), family = binomial) #

glm(full.dat$sex_num ~ fitted(m.250) + I(fitted(m.250)^2), family = binomial) #

glm(full.dat$sex_num ~ fitted(m.300) + I(fitted(m.300)^2), family = binomial) #





# another model fit check
library(performance)

bin_pred <- binned_residuals(m.250)
bin_pred_plot <- ggplot(bin_pred, aes(x = xbar, y = ybar)) +
  geom_point() + 
  geom_line(aes(x = xbar, y = se)) +
  geom_line(aes(x = xbar, y = -se)) +
  xlab("Estimated Probability of Sex") +
  ylab("Average Residual") +
  theme_classic()
bin_MEF <- binned_residuals(m.250, term = "n")
bin_MEF_plot <- ggplot(bin_MEF, aes(x = xbar, y = ybar)) +
  geom_point() + 
  geom_line(aes(x = xbar, y = se)) +
  geom_line(aes(x = xbar, y = -se)) +
  xlab("Posterior Dorsal Height : Fork Length") +
  ylab("Average Residual") +
  theme_classic()
bin_SL.MEF <- binned_residuals(m.250, term = "l")
bin_SL.MEF_plot <- ggplot(bin_SL.MEF, aes(x = xbar/100, y = ybar)) +
  geom_point() + 
  geom_line(aes(y = se)) +
  geom_line(aes(y = -se)) +
  xlab("Dorsal Fin Base Length : Fork Length") +
  ylab("Average Residual") +
  theme_classic()

cowplot::plot_grid(bin_pred_plot, bin_MEF_plot, bin_SL.MEF_plot, labels = "AUTO", ncol = 1)






###The following tests are from: 
#http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/ 


###Linearity assumption
library(broom)

# Predict the probability (p) of diabete positivity
probabilities <- predict(m.global, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

mydata <- full.dat %>%
  dplyr::select(c(62:75, 76:79, 82:83)) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")




#Influential values
plot(m.global, which = 4, id.n = 3)
#Pretty high for #57, also 11 and 56


# Extract model results
model.data <- augment(m.global) %>% 
  mutate(index = 1:n()) 

model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = sex_num), alpha = .5) +
  theme_bw()

model.data %>% 
  filter(abs(.std.resid) > 3)











# This is my Leave One Out Cross Validation -------------------------------


full.dat <- read.csv("full.dat.final.csv")  #97 obs

#Rotate through these two filter to test with diffeent minimum length thresholds
#full.dat <- full.dat %>% filter(fork_length >249) #79 obs
#full.dat <- full.dat %>% filter(fork_length >299)  #66 obs


colnames(full.dat)[apply(full.dat, 2, anyNA)]


#Replace NAs with the mean of the parameter
full.dat <- full.dat %>% 
  mutate(s = ifelse(is.na(s), mean(s, na.rm = TRUE), s)) %>% 
  mutate(o = ifelse(is.na(o), mean(o, na.rm = TRUE), o)) %>% 
  mutate(t = ifelse(is.na(t), mean(t, na.rm = TRUE), t)) 



global_pred <- vector()

for(i in 1:97){
  validate.dat <- full.dat[i,] #Single out one value
  training.dat <- full.dat[-i,] #Use the training data using the data -i
  m.global <- glm(sex_num ~ a+b+c+d+e+f+h+i+j+l+m+n+o+p+q+r+s+t+u, data = full.dat, family = "binomial")
  global_pred[i] <- predict(m.global, newdata = validate.dat, type = "response")
  
}





dredge_pred <- vector()

for(i in 1:97){
  validate.dat <- full.dat[i,] #Single out one value
  training.dat <- full.dat[-i,] #Use the training data using the data -i
  m.dredge <- glm(sex_num ~ a+e+j+q+r+s+u+n, data = training.dat, family = "binomial")
  dredge_pred[i] <- predict(m.dredge, newdata = validate.dat, type = "response")
  
}



fins_only_pred <- vector()

for(i in 1:97){
  validate.dat <- full.dat[i,] #Single out one value
  training.dat <- full.dat[-i,] #Use the training data using the data -i
  um.fins.only <- glm(sex_num ~ l+m+n+o+p+q+r+s+t+u, data = full.dat, family = "binomial")
  fins_only_pred[i] <- predict(m.fins.only, newdata = validate.dat, type = "response")
  
}


dorsal_all_pred <- vector()

for(i in 1:97){
  validate.dat <- full.dat[i,] #Single out one value
  training.dat <- full.dat[-i,] #Use the training data using the data -i
  m.dorsal.all <- glm(sex_num ~ h+l+m+n #g+v
                      , data = full.dat, family = "binomial")
  dorsal_all_pred[i] <- predict(m.dorsal.all, newdata = validate.dat, type = "response")
  
}


dorsal_length_pred <- vector()

for(i in 1:97){
  validate.dat <- full.dat[i,] #Single out one value
  training.dat <- full.dat[-i,] #Use the training data using the data -i
  m.dorsal.length <- glm(sex_num ~ n
                         , data = full.dat, family = "binomial")
  dorsal_length_pred[i] <- predict(m.dorsal.length, newdata = validate.dat, type = "response")
  
}




grayling_dat_250 <- full.dat %>% 
  filter(fork_length >249)

M250_pred <- vector()

for(i in 1:97){
  validate.dat <- full.dat[i,] #Single out one value
  training.dat <- full.dat[-i,] #Use the training data using the data -i
  m.250 <- glm(sex_num ~ a+b+c+d+e+f+h+i+j+l+m+n+o+p+q+r+s+t+u, data = grayling_dat_250, family = "binomial")
  M250_pred[i] <- predict(m.250, newdata = validate.dat, type = "response")
  
}




grayling_dat_300 <- full.dat %>% 
  filter(fork_length >299)


M300_pred <- vector()

for(i in 1:97){
  validate.dat <- full.dat[i,] #Single out one value
  training.dat <- full.dat[-i,] #Use the training data using the data -i
  m.300 <- glm(sex_num ~ a+b+c+d+e+f+h+i+j+l+m+n+o+p+q+r+s+t+u, data = grayling_dat_300, family = "binomial")
  M300_pred[i] <- predict(m.300, newdata = validate.dat, type = "response")
  
}


full.dat <- cbind(full.dat, global_pred,  noVIF_pred, reduced_pred, fins_only_pred, dorsal_all_pred, dorsal_length_pred, M250_pred, M300_pred, dredge_pred) 




#Get missclassifications
full.dat <- full.dat %>% 
  mutate(global_pred = ifelse(global_pred>0.5, 1, 0)) %>% 
  mutate("global_pred_wrong" = ifelse(global_pred == sex_num, 0, 1)) %>% 
  
  mutate(dredge_pred = ifelse(dredge_pred>0.5, 1, 0)) %>% 
  mutate("dredge_pred_wrong" = ifelse(dredge_pred == sex_num, 0, 1)) %>% 
  
  mutate(fins_only_pred = ifelse(fins_only_pred>0.5, 1, 0)) %>% 
  mutate("fins_only_pred_wrong" = ifelse(fins_only_pred == sex_num, 0, 1)) %>% 
  
  mutate(dorsal_all_pred = ifelse(dorsal_all_pred>0.5, 1, 0)) %>% 
  mutate("dorsal_all_pred_wrong" = ifelse(dorsal_all_pred == sex_num, 0, 1)) %>%
  
  mutate(dorsal_length_pred = ifelse(dorsal_length_pred>0.5, 1, 0)) %>% 
  mutate("dorsal_length_pred_wrong" = ifelse(dorsal_length_pred == sex_num, 0, 1)) %>% 
  
  mutate(M250_pred = ifelse(M250_pred>0.5, 1, 0)) %>% 
  mutate("M250_pred_wrong" = ifelse(M250_pred == sex_num, 0, 1)) %>% 
  
  mutate(M300_pred = ifelse(M300_pred>0.5, 1, 0)) %>% 
  mutate("M300_pred_wrong" = ifelse(M300_pred == sex_num, 0, 1)) 





percent_calc <- function(x) {
  
  percent_wrong <- (((sum(x))/(length(x)))*100)
  
  return(percent_wrong)
  
  
} 


#Calculate the misclassification rate
percent_global <- percent_calc(full.dat$global_pred_wrong)

percent_dredge <- percent_calc(full.dat$dredge_pred_wrong)

percent_fins <- percent_calc(full.dat$fins_only_pred_wrong)

percent_dorsal_all <- percent_calc(full.dat$dorsal_all_pred_wrong)

percent_dorsal_length <- percent_calc(full.dat$dorsal_length_pred_wrong)

percent_M250 <- percent_calc(full.dat$M250_pred_wrong)

percent_M300 <- percent_calc(full.dat$M300_pred_wrong)


print(cbind(percent_global, percent_dredge, percent_fins, percent_dorsal_all, percent_dorsal_length, percent_M250, percent_M300))


#Full.dat
#percent_global percent_dredge percent_fins percent_dorsal_all 
15.464          20.61856         16.49485        15.46392           
#percent_dorsal_length  percent_M250     percent_M300
18.5567                      6.19           12.37113


#Fish >250
#percent_global percent_dredge percent_fins percent_dorsal_all 
0                   15.18987      12.65823       15.18987              
#percent_dorsal_length  percent_M250   percent_M300
15.18987                 0              7.594937



#Fish >300
#percent_global percent_dredge  percent_fins  #percent_dorsal_all
0                  13.63636            6.060606          10.60606               
#percent_dorsal_length   percent_M250      percent_M300
    10.60606                    0               0







# Plotting results --------------------------------------------------------




#Signficiant predictors in the single model: 
#c, d, h, l, n, p, q, r, s, u, u

#install.packages("popbio")
library(popbio)
logi.hist.plot(full.dat$posterior_dorsal_height_FL_n, full.dat$sex_num ,boxp=FALSE,type="hist",col="gray", logi.mod = 1)
###I am just going to replicate this ^^ with ggplot



full.dat$fork_length <- as.numeric(full.dat$fork_length)


full.dat <- full.dat %>% 
  mutate("male"=ifelse(sex_num==0, n, NA)) %>% 
  mutate("female"=ifelse(sex_num==1, n, NA))  


p <- ggplot(full.dat) + 
  geom_histogram(aes(x = male, y = stat(count/40)), bins = 15, na.rm = TRUE, fill = "darkblue", color = "black", alpha = 0.85) +
  geom_histogram(aes(x = female, y = -1*stat(count/40)), bins = 15, na.rm = TRUE, position = position_nudge(y = 1), fill = "darkred", color = "black", alpha = 0.75)+
  stat_smooth(aes(x=n, y=sex_num), method=glm, method.args=list(family="binomial"), se=TRUE, color = "darkgreen ", fill = "grey40", size = 2)+
  geom_point(aes(x=n, y=sex_num, size = fork_length, shape = Sex), show.legend = FALSE)+#, color = sex_num) + 
  scale_size_continuous(range = c(1.5, 4))+
  #scale_color_manual(values = c(0 = "darkblue", 1 = "darkred"), )+
  theme(axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        legend.position = "none")+  #This wont work...
  labs(x= "Posterior Dorsal Height (N) : Fork Length Ratio", 
       y = "Probability of Being Female")+
  annotate("text", x = 0.3, y = 0.3, label = "Male", size = 6)+
  annotate("text", x = 0.11, y = 0.76, label = "Female", size = 6)+
  theme_cowplot()
p

ggsave(plot= p,
       filename = "Posterior Dorsal Height model 3.jpeg",
       dpi = 2000, 
       height = 5,
       width = 5.5,
       units = "in")



p_density <- ggplot(full.dat) + 
  geom_histogram(aes(x = male, y = stat(count/40)), bins = 15, na.rm = TRUE, fill = "darkblue", color = "black", alpha = 0.85) +
  geom_histogram(aes(x = female, y = -1*stat(count/40)), bins = 15, na.rm = TRUE, position = position_nudge(y = 1), fill = "darkred", color = "black", alpha = 0.75)+
  geom_density(aes(x = male, y = stat(count/1500)), bins = 15, na.rm = TRUE, fill = "darkblue", color = "black", alpha = 0.75, lwd = 1) +
  geom_density(aes(x = female, y = -1*stat(count/1500)), bins = 15, na.rm = TRUE, position = position_nudge(y = 1), fill = "darkred", color = "black", alpha = 0.75, lwd = 1)+
  stat_smooth(aes(x=n, y=sex_num), method=glm, method.args=list(family="binomial"), se=TRUE, color = "darkgreen ", fill = "grey40", size = 2.5)+
  geom_point(aes(x=n, y=sex_num, size = fork_length, shape = Sex), show.legend = FALSE)+#, color = sex_num) + 
  scale_size_continuous(range = c(1.5, 4))+
  #scale_color_manual(values = c(0 = "darkblue", 1 = "darkred"), )+
  theme(axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        legend.position = "none")+  #This wont work...
  labs(x= "Posterior Dorsal Height (N) : Fork Length Ratio", 
       y = "Probability of Being Female")+
  annotate("text", x = 0.3, y = 0.3, label = "Male", size = 6)+
  annotate("text", x = 0.11, y = 0.76, label = "Female", size = 6)+
  theme_cowplot()
p_density


ggsave(plot= p_density,
       filename = "Posterior Dorsal Height w/ Density.jpeg",
       dpi = 2000, 
       height = 5,
       width = 5.5,
       units = "in")

p+p_density





ggplot(full.dat) +
  geom_density(aes(x = male, y = stat(count/1500)), bins = 15, na.rm = TRUE, color = "black", alpha = 0.75, lwd = 1) +
  geom_density(aes(x = female, y = -1*stat(count/1500)), bins = 15, na.rm = TRUE, position = position_nudge(y = 1), color = "black", alpha = 0.75, lwd = 1)+
  geom_histogram(aes(x = male, y = stat(count/40)), bins = 15, na.rm = TRUE, fill = "darkblue", color = "black", alpha = 0.85) +
  geom_histogram(aes(x = female, y = -1*stat(count/40)), bins = 15, na.rm = TRUE, position = position_nudge(y = 1), fill = "darkred", color = "black", alpha = 0.75)+
  stat_smooth(aes(x=n, y=sex_num), method=glm, method.args=list(family="binomial"), se=TRUE, color = "darkgreen ", fill = "grey40", size = 2.5)+
  geom_point(aes(x=n, y=sex_num, size = fork_length, shape = Sex), show.legend = FALSE)+#, color = sex_num) + 
  scale_size_continuous(range = c(1.5, 4))+
  #scale_color_manual(values = c(0 = "darkblue", 1 = "darkred"), )+
  theme(axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        legend.position = "none")+  #This wont work...
  labs(x= "Posterior Dorsal Height (N) : Fork Length Ratio", 
       y = "Probability of Being Female")+
  annotate("text", x = 0.3, y = 0.3, label = "Male", size = 6)+
  annotate("text", x = 0.11, y = 0.76, label = "Female", size = 6)+
  theme_cowplot()







###




model <- glm(sex_num ~ posterior_dorsal_height_FL_n, family = binomial, data = full.dat)
plot_df <- augment(model, type.predict = "response")
?predict
predict()
m.pred <- cbind(full.dat, "sex_num"=predict(model, newdata = full.dat, type="response", na.action = na.pass))
m.pred$sex_num



p1 <- ggplot(full.dat, aes(x = posterior_dorsal_height_FL_n, y = sex_num, color = Sex)) +
  geom_point(aes(shape = Sex), size = 3) +
  #geom_line(aes(y=model))+
  #geom_line(aes(y = .fitted), color = "blue") +
  stat_smooth(method="glm", color="darkgreen", se=TRUE, 
              method.args = list(family=binomial))+
  scale_color_manual(values = c("M" = "darkblue", "F" = "darkred"), )+
  #geom_smooth(method = lm, se =FALSE) +
  #geom_line()+
  #geom_smooth()+
  theme_classic()+
  theme(axis.title.y = element_blank(), 
        #axis.ticks.y = element_blank(), 
        #axis.text.y = element_blank(),
        legend.position = c(0.90, 0.90))+
  labs(x="Posterior Dorsal Height (N)")
p1

#ggsave(plot= p1,
#       filename = "R_Figures/Posterior Dorsal Height.jpeg",
#       dpi = 500, 
#       height = 4,
#       width = 4,
#       units = "in")



p2 <- ggplot(full.dat, aes(x = dorsal_fin_base_length_FL_l, y = sex_num, color = Sex)) +
  geom_point(aes(shape = Sex), size = 3) +
  #geom_line(aes(y=model))+
  #geom_line(aes(y = .fitted), color = "blue") +
  stat_smooth(method="glm", color="darkgreen", se=TRUE, 
              method.args = list(family=binomial))+
  scale_color_manual(values = c("M" = "darkblue", "F" = "darkred"), )+
  #geom_smooth(method = lm, se =FALSE) +
  #geom_line()+
  #geom_smooth()+
  theme_classic()+
  theme(axis.title.y = element_blank(), 
        #axis.ticks.y = element_blank(), 
        #axis.text.y = element_blank(),
        #legend.position = c(0.12, 0.33),
        legend.position = "none")+
  labs(x="Dorsal Fin Base Length (L)")
p2

ggsave(plot= p2,
       filename = "R_Figures/dorsal fin base length.jpeg",
       dpi = 500, 
       height = 4,
       width = 4,
       units = "in")


p3 <- ggplot(full.dat, aes(x = postorbital_length_FL_c, y = sex_num, color = Sex)) +
  geom_point(aes(shape = Sex), size = 3) +
  #geom_line(aes(y=model))+
  #geom_line(aes(y = .fitted), color = "blue") +
  stat_smooth(method="glm", color="darkgreen", se=TRUE, 
              method.args = list(family=binomial))+
  scale_color_manual(values = c("M" = "darkblue", "F" = "darkred"), )+
  #geom_smooth(method = lm, se =FALSE) +
  #geom_line()+
  #geom_smooth()+
  theme_classic()+
  theme(axis.title.y = element_blank(), 
        #axis.ticks.y = element_blank(), 
        #axis.text.y = element_blank(),
        legend.position = "none")+
  labs(x="Post-Orbital Length (C)")
p3

ggsave(plot= p3,
       filename = "R_Figures/post orbital length.jpeg",
       dpi = 500, 
       height = 4,
       width = 4,
       units = "in")


p4 <- ggplot(full.dat, aes(x = anal_fin_height_FL_p, y = sex_num, color = Sex)) +
  geom_point(aes(shape = Sex), size = 3) +
  #geom_line(aes(y=model))+
  #geom_line(aes(y = .fitted), color = "blue") +
  stat_smooth(method="glm", color="darkgreen", se=TRUE, 
              method.args = list(family=binomial))+
  scale_color_manual(values = c("M" = "darkblue", "F" = "darkred"), )+
  #geom_smooth(method = lm, se =FALSE) +
  #geom_line()+
  #geom_smooth()+
  theme_classic()+
  theme(axis.title.y = element_blank(), 
        #axis.ticks.y = element_blank(), 
        #axis.text.y = element_blank(),
        legend.position = "none")+
  labs(x="Anal Fin Height (P)")
p4

ggsave(plot= p4,
       filename = "R_Figures/anal height.jpeg",
       dpi = 500, 
       height = 4,
       width = 4,
       units = "in")



p5 <- ggplot(full.dat, aes(x = postdorsal_length_FL_h, y = sex_num, color = Sex)) +
  geom_point(aes(shape = Sex), size = 3) +
  #geom_line(aes(y=model))+
  #geom_line(aes(y = .fitted), color = "blue") +
  stat_smooth(method="glm", color="darkgreen", se=TRUE, 
              method.args = list(family=binomial))+
  scale_color_manual(values = c("M" = "darkblue", "F" = "darkred"), )+
  #geom_smooth(method = lm, se =FALSE) +
  #geom_line()+
  #geom_smooth()+
  theme_classic()+
  theme(axis.title.y = element_blank(), 
        #axis.ticks.y = element_blank(), 
        #axis.text.y = element_blank(),
        legend.position = "none")+
  labs(x="Post-Dorsal Length (H)")
p5

ggsave(plot= p5,
       filename = "R_Figures/postdorsal length.jpeg",
       dpi = 500, 
       height = 4,
       width = 4,
       units = "in")







p6 <- ggplot(full.dat, aes(x = pelvic_fin_length_FL_r, y = sex_num, color = Sex)) +
  geom_point(aes(shape = Sex), size = 3) +
  #geom_line(aes(y=model))+
  #geom_line(aes(y = .fitted), color = "blue") +
  stat_smooth(method="glm", color="darkgreen", se=TRUE, 
              method.args = list(family=binomial))+
  scale_color_manual(values = c("M" = "darkblue", "F" = "darkred"), )+
  #geom_smooth(method = lm, se =FALSE) +
  #geom_line()+
  #geom_smooth()+
  theme_classic()+
  theme(axis.title.y = element_blank(), 
        #axis.ticks.y = element_blank(), 
        #axis.text.y = element_blank(),
        legend.position = "none")+
  labs(x="Pelvic Fin Length (R)")
p6

ggsave(plot= p6,
       filename = "R_Figures/Pelvic Fin Length.jpeg",
       dpi = 500, 
       height = 4,
       width = 4,
       units = "in")




p7 <- ggplot(full.dat, aes(x = dorsal_to_adipose_FL_u, y = sex_num, color = Sex)) +
  geom_point(aes(shape = Sex), size = 3) +
  #geom_line(aes(y=model))+
  #geom_line(aes(y = .fitted), color = "blue") +
  stat_smooth(method="glm", color="darkgreen", se=TRUE, 
              method.args = list(family=binomial))+
  scale_color_manual(values = c("M" = "darkblue", "F" = "darkred"), )+
  #geom_smooth(method = lm, se =FALSE) +
  #geom_line()+
  #geom_smooth()+
  theme_classic()+
  theme(axis.title.y = element_blank(), 
        #axis.ticks.y = element_blank(), 
        #axis.text.y = element_blank(),
        legend.position = "none")+
  labs(x="Dorsal To Adipose Length (V)")
p7

ggsave(plot= p7,
       filename = "R_Figures/Dorsal To Adipose Length.jpeg",
       dpi = 500, 
       height = 4,
       width = 4,
       units = "in")



p8 <- ggplot(full.dat, aes(x = pectoral_fin_length_FL_q, y = sex_num, color = Sex)) +
  geom_point(aes(shape = Sex), size = 3) +
  #geom_line(aes(y=model))+
  #geom_line(aes(y = .fitted), color = "blue") +
  stat_smooth(method="glm", color="darkgreen", se=TRUE, 
              method.args = list(family=binomial))+
  scale_color_manual(values = c("M" = "darkblue", "F" = "darkred"), )+
  #geom_smooth(method = lm, se =FALSE) +
  #geom_line()+
  #geom_smooth()+
  theme_classic()+
  theme(axis.title.y = element_blank(), 
        #axis.ticks.y = element_blank(), 
        #axis.text.y = element_blank(),
        legend.position = "none")+
  labs(x="Pectoral Fin Length (Q)")
p8

ggsave(plot= p8,
       filename = "R_Figures/pectoral fin length.jpeg",
       dpi = 500, 
       height = 4,
       width = 4,
       units = "in")


p9 <- ggplot(full.dat, aes(x = head_length_FL_d, y = sex_num, color = Sex)) +
  geom_point(aes(shape = Sex), size = 3) +
  #geom_line(aes(y=model))+
  #geom_line(aes(y = .fitted), color = "blue") +
  stat_smooth(method="glm", color="darkgreen", se=TRUE, 
              method.args = list(family=binomial))+
  scale_color_manual(values = c("M" = "darkblue", "F" = "darkred"), )+
  #geom_smooth(method = lm, se =FALSE) +
  #geom_line()+
  #geom_smooth()+
  theme_classic()+
  theme(axis.title.y = element_blank(), 
        #axis.ticks.y = element_blank(), 
        #axis.text.y = element_blank(),
        legend.position = "none")+
  labs(x="Head Length (D)")
p9

ggsave(plot= p9,
       filename = "R_Figures/head length.jpeg",
       dpi = 500, 
       height = 4,
       width = 4,
       units = "in")



p10 <- ggplot(full.dat, aes(x = pelvic.anal_distance_FL_j, y = sex_num, color = Sex)) +
  geom_point(aes(shape = Sex), size = 3) +
  #geom_line(aes(y=model))+
  #geom_line(aes(y = .fitted), color = "blue") +
  stat_smooth(method="glm", color="darkgreen", se=TRUE, 
              method.args = list(family=binomial))+
  scale_color_manual(values = c("M" = "darkblue", "F" = "darkred"), )+
  #geom_smooth(method = lm, se =FALSE) +
  #geom_line()+
  #geom_smooth()+
  theme_classic()+
  theme(axis.title.y = element_blank(), 
        #axis.ticks.y = element_blank(), 
        #axis.text.y = element_blank(),
        legend.position = "none")+
  labs(x="Pelvic to Anal Fin Distance (J)")
p10



p11 <- ggplot(full.dat, aes(x = horizontal_eye_diameter_FL_a, y = sex_num, color = Sex)) +
  geom_point(aes(shape = Sex), size = 3) +
  #geom_line(aes(y=model))+
  #geom_line(aes(y = .fitted), color = "blue") +
  stat_smooth(method="glm", color="darkgreen", se=TRUE, 
              method.args = list(family=binomial))+
  scale_color_manual(values = c("M" = "darkblue", "F" = "darkred"), )+
  #geom_smooth(method = lm, se =FALSE) +
  #geom_line()+
  #geom_smooth()+
  theme_classic()+
  theme(axis.title.y = element_blank(), 
        #axis.ticks.y = element_blank(), 
        #axis.text.y = element_blank(),
        legend.position = "none")+
  labs(x="Horizontal Eye Diameter (A)")
p11



p12 <- ggplot(full.dat, aes(x = maximum_body_depth_FL_e, y = sex_num, color = Sex)) +
  geom_point(aes(shape = Sex), size = 3) +
  #geom_line(aes(y=model))+
  #geom_line(aes(y = .fitted), color = "blue") +
  stat_smooth(method="glm", color="darkgreen", se=TRUE, 
              method.args = list(family=binomial))+
  scale_color_manual(values = c("M" = "darkblue", "F" = "darkred"), )+
  #geom_smooth(method = lm, se =FALSE) +
  #geom_line()+
  #geom_smooth()+
  theme_classic()+
  theme(axis.title.y = element_blank(), 
        #axis.ticks.y = element_blank(), 
        #axis.text.y = element_blank(),
        legend.position = "none")+
  labs(x="Maximum Body Depth (E)")
p12







#I think this panel is good for now
panel<- p1+p2+p5+p7+p4+p6+p8+p3+p9
panel

ggsave(plot= panel,
       filename = "R_Figures/panel final.jpeg",
       dpi = 1500, 
       height = 8,
       width = 8,
       units = "in")





#Updated panel for poster
panel<- p2+p7+p6+p8+p3+p9
panel

ggsave(plot= panel,
       filename = "R_Figures/panel 2.jpeg",
       dpi = 1500, 
       height = 6,
       width = 9,
       units = "in")








library(plotly)
#https://plotly.com/r/3d-scatter-plots/
  
  
  
  ?plot_ly
#Things to consider adding:
#Scale size by fork length???
#Make points a darker shade as you move backwards so you can see the z axis variation better?  


fig1 <- plot_ly(full.dat, x = ~posterior_dorsal_height_FL_n, 
                y = ~pectoral_fin_length_FL_q, 
                z = ~pelvic_fin_length_FL_r,
                color = ~Sex, colors = c('#BF382A', '#0C4B8E')
                #size = ~fork_length, #This adjusts the size by fork length, but how do I make them bigger overall???
) %>% 
  #layout(list(xaxis = list(zeroline=TRUE, showline = TRUE, linewidth = 10),
  #            yaxis = list(zeroline=TRUE, showline = TRUE),
  #            zaxis = list(zeroline=TRUE, showline = TRUE))) %>% This doesn't seem to do anything...
  #sizes = fork_length) %>% 
  add_markers()  %>% #fig, marker = list(size = 5)) 
  layout(scene = list(xaxis = list(title = 'Posterior Dorsal Height'),
                      yaxis = list(title = 'Pelvic Fin Length'),
                      zaxis = list(title = 'Pectoral Fin Height'))) 

fig1

  
  
  
  
  
  
  
  
  
  

# Use the recommended models to predict Arctic Grayling sex ---------------

### Train the models

#Global 250 model
grayling_dat_250 <- full.dat %>% 
  filter(fork_length >249)
str(grayling_dat_250)
summary(grayling_dat_250$fork_measured)

m.250 <- glm(sex_num ~ a+b+c+d+e+f+h+i+j+l+m+n+o+p+q+r+s+t+u, data = grayling_dat_250, family = "binomial")
summary(m.250)



#just dorsal fin LENGTH (Posterior Dorsal Height)
m.dorsal.length <- glm(sex_num ~ n, data = full.dat, family = "binomial")



###Create your dataset like this (all these measurements are standardized by fork length):
str(full.dat[,c(5,62:83)])
#(you'll want to name each parameter by just the letter (e.g., "a", "b", "c"...),
#I included the full name in the column headings )




#predict with the m.250 model
sex_pred_250 <- predict(m.250, newdata = full.dat, type = "response")
#1 = female, 0 = male

#Here is your prediction!
sex_pred_250

#Join it to the dataset so you can tell which fish is which. That's it!
precition.dat <- cbind(sex_pred_250, full.dat)



#repeat with the dorsal_length model (if desired)
sex_pred_dorsal <- predict(m.dorsal.length, newdata = full.dat, type = "response")
#1 = female, 0 = male

precition.dat <- cbind(sex_pred_dorsal, precition.dat) 

