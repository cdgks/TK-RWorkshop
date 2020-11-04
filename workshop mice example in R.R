##############################################
#	Multiple imputation with MICE example	#
#	Charlie Keown-Stoneman				#
#	2020-11-04						#
##############################################

#Load mice package
library(mice)

#########################################
### Section 1: Prepping data for mice ###
#########################################

load("Workshop_data.RData")


#first, lets have a look at the first few rows of the dataset and a summary to get an idea what we're dealing with
head(eg_data)
summary(eg_data)

#next, make sure categorical variables stored numerically are stored as "factor" variables in R
#for example, the momethnicity variable is stored numerically, but should be stored as a factor
#eg_data$momethnicity <- as.factor(eg_data$momethnicity)

#Next, let's drop the variables we don't need. We won't need instanceid or instancedate
mice_data <- eg_data[,which(!colnames(eg_data)%in%c("instanceid","instancedate","zbmi_group"))]

#check it worked:
head(mice_data)

#####################################################
### Section 2: prediction matrix and running mice ###
#####################################################

#we need to tell mice which variables to use to predict the missing value
#the first step is to create a simple list of ones, indicating we want to use all variables
mice_vars <- rep(1, times=ncol(mice_data))

#however, we don't want to impute "subject" and we don't want "subject" used to impute other variables
#so, we replace the "mice_vars" entry corresponding to "subject" with a 0
mice_vars[which(colnames(mice_data)=="subject")] <- 0

#now we need to turn our list of 0's and 1's into a square matrix ("mice_matrix")
mice_matrix <- mice_vars %*% t(mice_vars)

#lets label the rows and columns of our matrix for our own sake (optional)
colnames(mice_matrix) <- rownames(mice_matrix) <- colnames(mice_data)

#while we want to use all of the variables expect subject for the imputation,
#we don't want them to impute themselves, so the diagonals of the matrix must be 0 
diag(mice_matrix) <- 0

#let's see that the matrix looks okay
mice_matrix

#now we're ready to run the actual imputation.
#Warning: with large datasets this can take awhile;
#However, if it's taking extremely long, it could be a sign there's a problem

sapply(mice_data,class) #checks variable classes

mice_data$CHILDGENDER1 <- as.factor(mice_data$CHILDGENDER1)

#temp <- Sys.time()
imputed_data <- mice(mice_data, print=F, m=10, predictorMatrix = mice_matrix, seed = 268481)
#Sys.time()-temp

#temp <- Sys.time()
#imputed_data <- parlmice(mice_data, print=F, m=50, predictorMatrix = mice_matrix, n.core = 10, cluster.seed = 94351, n.imp.core = 5)
#Sys.time()-temp

plot(imputed_data)

densityplot(imputed_data, ~zbmi)

#complete(imputed_data, action=1)

#################################
### Section 3: fitting models ###
#################################

#You can fit models to the imputed datasets using the with() function
#the syntax is:  with( [imputed data], [model] )

#let's run a linear model and save the results in an object called "model.1"
model.1 <- with(imputed_data,lm(zbmi~ageinmonths+momethnicity+CHILDGENDER1+cmr_zscr_tot_adj))

#now we can get parameter estimates and confidence intervals using the summary() and pool() functions
summary(pool(model.1),conf.int = T)


#checking model assumptions
source("mice_functions.R")

mice_qqnorm(model.1)
mice_res_yhat(model.1)
mice_res_x(model.1,imputed_data,"ageinmonths")
mice_res_x(model.1,imputed_data,"cmr_zscr_tot_adj")


model.1.rcs <- with(imputed_data,lm(zbmi~ageinmonths+momethnicity+CHILDGENDER1+cmr_zscr_tot_adj+rcs(cmr_zscr_tot_adj,5)[,2:4]))
summary(pool(model.1.rcs),conf.int = T)

anova(model.1.rcs,model.1) #default Wald test
anova(model.1.rcs,model.1, method="D3") #likelihood ratio test, often better (particularly for logistic regression, but slower)



#What if we want to test all of the levels of "momethnicity" at once?
model.2 <- with(imputed_data,lm(zbmi~ageinmonths+CHILDGENDER1+cmr_zscr_tot_adj+rcs(cmr_zscr_tot_adj,5)[,2:4]))
summary(pool(model.2),conf.int = T)

anova(model.1.rcs,model.2, method="D3") #likelihood ratio test for maternal ethnicity


#What if we wanted to create zBMI groups after imputation?

temp <- complete(imputed_data, "long", include = TRUE)
temp$zbmi_group <- with(temp, cut(zbmi, breaks=c(-Inf,-2,1,2,Inf), labels=c("Underweight","Normal Weight","Overweight","Obese")))
imputed_data <- as.mids(temp)

#Check that it worked
aggregate(complete(imputed_data, action="long")$zbmi,by=list(complete(imputed_data, action="long")$zbmi_group), FUN=summary)



	  
