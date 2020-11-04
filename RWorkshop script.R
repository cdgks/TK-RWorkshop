####################################
#	R Workshop script			#
#	Dr. Charlie Keown-Stoneman	#
#	2020-10-29				#
####################################

#R is the program that executes your commands
#RStudio is a fancy GUI for R, but R still does everything "under the hood"

###R basics

#Using number signs (a.k.a. octothorpe/pound/hash) you can write comments that R will ignore

#R can be used like a basic calculator
2+2

#R follows BEDMAS
2+2*2
(2+2)*2 #comments can come after code on the same line

a <- 2 #you can use arrows to declare variables and set values etc.
b = 2  #you can also use equal signs
a + b
c = a + b
c

#R can also answer logical questions, such as, is "a" equal to "b"?
a == b
a == c
a < c #is "a" less than "c"?
a > c #is "a" greater than "c"?
a <= b #is "a" less than/or equal to "b"?
#R can also combine TRUE/FALSE results with AND/OR logic, AND is "&", OR is "|"
a == b | a == c
a == b & a == c

#To do the opposite of a logical variable in R, you can use "!", for example,

a == b
!(a == b)
a != b


#Vectorized variables

A <- c(5,7,3,9,4) #c() combines values into a vector
B <- c(1,2,3)

A%in%B

#another difference between == and %in% is how they treat NA
c(1,NA,2,4)==2
c(1,NA,2,4)%in%2


#Some basic statistics
mean(A)
sd(A)
sum(A)
median(A)
summary(A)

#square brackets can be used to specify elements of a vector
A[1]
A[2]

#Example (simulated/fake) TARGet Kids data

eg_data <- read.csv("eg_data.csv")

names(eg_data) #check variable names
class(eg_data) #check class of object
head(eg_data) #look at the first 6 rows
View(eg_data) #open the whole dataset like a spreadsheet

#Square brackets can be used to specify rows/columns
eg_data[1:5,1:4]

#You can use "$" to specify a specific column from a data frame
eg_data$subject

class(eg_data$subject)

is.numeric(eg_data$subject) #check, is this column numeric?

#You can use "?" before a function to learn more about it
?sapply
sapply(eg_data,class) #checks variable classes

class(eg_data$momethnicity)
is.numeric(eg_data$momethnicity)

eg_data$momethnicity <- as.factor(eg_data$momethnicity)

table(eg_data$momethnicity)


eg_data$momethnicity <- factor(eg_data$momethnicity, labels = c("Eurpoean",
												    "East Asian",
												    "South Asian",
												    "Southeast Asian",
												    "Arab",
												    "African",
												    "Latin American",
												    "Mixed Ethnicity",
												    "Other"))

table(eg_data$momethnicity)


#It's important to make sure your factor/character variables have missing stored as "NA" in R

class(eg_data$CHILDGENDER1)
table(eg_data$CHILDGENDER1)

eg_data$CHILDGENDER1[which(is.na(eg_data$CHILDGENDER1))]

eg_data$CHILDGENDER1[which(eg_data$CHILDGENDER1=="")]

eg_data$CHILDGENDER1[which(eg_data$CHILDGENDER1=="")] <- NA


#Alternative Charlie custom "cheat" function
remove_blanks <- function(x){
	for(i in 1:ncol(x)){
		if(class(x[,i])=="character"){
			x[x[,i]%in%"",i] <- NA
		}else if(class(x[,i])=="factor"){
			x[,i] <- as.character(x[,i])
			x[x[,i]%in%"",i] <- NA
			x[,i] <- factor(x[,i])
		}
	}
	return(x)
}

eg_data_alt <- remove_blanks(eg_data)

#creating a categorical variable based on cut-points
eg_data$zbmi_group <- cut(eg_data$zbmi, breaks=c(-Inf,-2,1,2,Inf), labels=c("Underweight","Normal Weight","Overweight","Obese"))

table(eg_data$zbmi_group)

table(eg_data$momethnicity,eg_data$CHILDGENDER1)
with(eg_data,table(momethnicity,CHILDGENDER1,zbmi_group))

with(eg_data,ftable(CHILDGENDER1,momethnicity,zbmi_group))


###R packages
#install.packages("survival")
library(survival)
library(dplyr)

#Using common dplyr commands
underweight <- eg_data %>% filter(zbmi<(-2))
#select(subject,ageinmonths,zbmi)


#some basic plots
plot(eg_data$ageinmonths,eg_data$zbmi)

hist(eg_data$zbmi)
hist(underweight$zbmi)

boxplot(zbmi~CHILDGENDER1, data=eg_data)

pairs(eg_data %>% select(zbmi,ageinmonths,cmr_zscr_tot_adj))

#some fancier looking plots using ggplot2

library(ggplot2)

ggplot(aes(x=ageinmonths, y=zbmi), data=eg_data) +
	geom_point()+
	geom_smooth(method="loess", formula=y~x)

### Merging together different datasets

eg_data2 <- read.csv("eg_data2.csv")

names(eg_data2) #check variable names
head(eg_data2) #look at the first 6 rows

head(eg_data) #look at the first 6 rows of the original dataset

combined_data <- merge(x=eg_data,y=eg_data2, by = c("subject","instanceid"), all.x = TRUE)

head(combined_data)

#Let's try some modelling (on the complete cases)
complete_case_data <- combined_data %>% filter(complete.cases(combined_data))

model1 <- lm(zbmi~ageinmonths+momethnicity+CHILDGENDER1+cmr_zscr_tot_adj, data=complete_case_data)

summary(model1)
plot(model1)

plot(complete_case_data$ageinmonths,resid(model1))
abline(h=0)

plot(complete_case_data$cmr_zscr_tot_adj,resid(model1))
abline(h=0)

#What if we wanted an overall test for momethnicity?
model2 <- lm(zbmi~ageinmonths+CHILDGENDER1+cmr_zscr_tot_adj, data=complete_case_data)

anova(model1,model2)

library(rms)

model1.rcs <- lm(zbmi~ageinmonths+momethnicity+CHILDGENDER1+rcs(cmr_zscr_tot_adj,5), data=complete_case_data)
summary(model1.rcs)
plot(model1.rcs)

plot(complete_case_data$cmr_zscr_tot_adj,resid(model1.rcs))
abline(h=0)

anova(model1.rcs,model1)


#We have repeated measures in this dataset, but what if we only want the baseline (i.e., first) observation per subject?

baseline_data <- combined_data %>% 
	arrange(subject,ageinmonths) %>%
	filter(!duplicated(subject))

nrow(combined_data)
nrow(baseline_data)
length(unique(combined_data$subject))

#Saving our whole workspace (including any datasets we've created)
getwd()
save.image("Workshop_data.RData")


###Other useful functions
#aggregate()
#sapply()

### Example of taking multiple variable inputs and creating an output

eg_data3 <- read.csv("eg_data3.csv")

head(eg_data3)

#one approach is nested ifelse functions:

table(eg_data3$CHILDCARE_ARRANGEMENT)

eg_data3$new_childcare1 <- with(
	eg_data3,ifelse(CHILDCARE_ARRANGEMENT=="Daycare (centre-based or home-based)",
				 ifelse(homebased_daycare_yn=="Yes",
				 	  ifelse(licensed_provider_yn=="Yes",
				 	  	  "Licensed home-based care",
				 	  	  "Unlicensed childcare"),
				 	  ifelse(licensed_provider_yn=="Yes",
				 	  	  "Licensed centre-based childcare",
				 	  	  "Unlicensed childcare")
				 	  ),
				 "Unlicensed childcare")
						 )

table(eg_data3$new_childcare1)

#another option is using a loop, this is less efficient computationally, but can be easier to read/understand later:
#you need to be careful with if statements and missing values though!

eg_data3$new_childcare2 <- NULL
for(i in 1:nrow(eg_data3)){
	if(eg_data3$CHILDCARE_ARRANGEMENT[i]%in%c("Care by a relative","Care in child’s home by a non-relative","Care in someone’s home by a non-relative")){
		eg_data3$new_childcare2[i] <- "Unlicensed childcare"
	}else if(eg_data3$CHILDCARE_ARRANGEMENT[i]%in%"Daycare (centre-based or home-based)"){
		if(eg_data3$licensed_provider_yn[i]%in%"No"){
			eg_data3$new_childcare2[i] <- "Unlicensed childcare"
		}else if(eg_data3$licensed_provider_yn[i]%in%"Yes"){
			if(eg_data3$homebased_daycare_yn[i]%in%"Yes"){
				eg_data3$new_childcare2[i] <- "Licensed home-based care"
			}else if(eg_data3$homebased_daycare_yn[i]%in%"No"){
				eg_data3$new_childcare2[i] <- "Licensed centre-based childcare"
			}
		}	
	}
}

table(eg_data3$new_childcare2)

with(eg_data3,table(new_childcare1,new_childcare2))

#or you could use a series of which statements:

eg_data3$new_childcare3 <- NULL

eg_data3$new_childcare3[which(
	eg_data3$CHILDCARE_ARRANGEMENT%in%c("Care by a relative",
								 "Care in child’s home by a non-relative",
								 "Care in someone’s home by a non-relative")|
		eg_data3$licensed_provider_yn%in%"No")] <- "Unlicensed childcare"

eg_data3$new_childcare3[which(
	eg_data3$CHILDCARE_ARRANGEMENT%in%"Daycare (centre-based or home-based)" &
		eg_data3$homebased_daycare_yn%in%"Yes" &
		eg_data3$licensed_provider_yn%in%"Yes")] <- "Licensed home-based care"

eg_data3$new_childcare3[which(
	eg_data3$CHILDCARE_ARRANGEMENT%in%"Daycare (centre-based or home-based)" &
		eg_data3$homebased_daycare_yn%in%"No" &
		eg_data3$licensed_provider_yn%in%"Yes")] <- "Licensed centre-based childcare"

table(eg_data3$new_childcare3)
with(eg_data3,table(new_childcare1,new_childcare3))

#Lastly, the newer dplyr way,

eg_data3 <- eg_data3 %>% 
	mutate(new_childcare4 = case_when(
		CHILDCARE_ARRANGEMENT%in%c("Care by a relative",
							  "Care in child’s home by a non-relative",
							  "Care in someone’s home by a non-relative")|
			licensed_provider_yn%in%"No" ~ "Unlicensed childcare",
		CHILDCARE_ARRANGEMENT%in%"Daycare (centre-based or home-based)" &
			homebased_daycare_yn%in%"Yes" &
			licensed_provider_yn%in%"Yes" ~ "Licensed home-based care",
		CHILDCARE_ARRANGEMENT%in%"Daycare (centre-based or home-based)" &
			homebased_daycare_yn%in%"No" &
			licensed_provider_yn%in%"Yes" ~ "Licensed centre-based childcare"
		)
	) 

table(eg_data3$new_childcare4)
with(eg_data3,table(new_childcare1,new_childcare4))
