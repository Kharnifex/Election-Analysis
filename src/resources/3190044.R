# load data
elections_df <- haven::read_sav("04_elections2000.sav")

# view the dataframe right after it is imported
sjPlot::view_df(elections_df)

# remove redundant ID column
elections_df = dplyr::select(elections_df, -ID)

# generate table w/ useful info for quantitative variables
df <- data.frame(elections_df)
library(stargazer)
stargazer(df)#generate latex table (values have been copypasted and the ones for income have been rounded up or down to the nearest integer)
# alternatively we can use psych::describe(df) or create a dataframe that contains only continuous numerical variables and run that method on that dataframe

#plot chisq table for dataframe
sjPlot::sjp.chi2(df, title = "Έλεγχος Ανεξαρτησίας του Pearson",
                 axis.labels = attr(df, 'names'))
png(file = "Pearson_Plot.png", bg = "transparent", width = 1080, height = 1080)
sjPlot::sjp.chi2(df, title = "Έλεγχος Ανεξαρτησίας του Pearson",
                 axis.labels = attr(df, 'names'))
dev.off()

# descriptive analysis for categorical variables
elections_df$Gender <- factor(elections_df$Gender, levels=c(1,2), labels=c('male', 'female'))
elections_df$Vote <- factor(elections_df$Vote, levels=c(1,3,5,6,7), labels=c('Al Gore', 'George W. Bush', 'Pat Buchanan', 'Ralph Nader', 'Other (SPECIFY)'))
elections_df$Political_orientation <- factor(elections_df$Political_orientation, levels=c(1,2,3,4,5,6,7), labels=c('Extremely liberal', 'Liberal', 'Slightly liberal', 'Moderate; middle of the road', 'Slightly conservative', 'Conservative', 'Extremely conservative'))
elections_df$Marital_Status <- factor(elections_df$Marital_Status, levels=c(1,2,3,4,5,6), labels=c('Married', 'Widowed', 'Divorced', 'Separated', 'Never married', 'Partnered, not married {VOL}'))

# descriptive analysis for continuous variables
is.numeric(elections_df$Age)
is.numeric(elections_df$Years_of_residence)
is.numeric(elections_df$Income)
library(psych)
describe(elections_df$Age)
describe(elections_df$Years_of_residence)
describe(elections_df$Income)

#checking for outliers in Income variable (outliers existing dont make sense in the context of Age/Years of residence type variables)
png(file = "Income_Boxplot.png", bg = "transparent", width = 1080, height = 1080)
boxplot(elections_df$Income, col="orange",border="brown")
dev.off()
boxplot(elections_df$Income, plot=FALSE)$out #no outliers exist

#normality tests for continuous variables
shapiro.test(elections_df$Age)
shapiro.test(elections_df$Income)
shapiro.test(elections_df$Years_of_residence)
library(nortest)
lillie.test(elections_df$Age)
lillie.test(elections_df$Income)
lillie.test(elections_df$Years_of_residence)

#note for later: none follow normal distribution -> use non parametric tests

#generate density plots for continuous variables
png(file = "Density_Plots.png", bg = "transparent", width = 540, height = 540)
par(mfrow=c(3,1))
plot(density(elections_df$Age), main="Age")
polygon(density(elections_df$Age), col="red")
legend("topright", legend="W = 0.9752 \np-value = 4.87e-12", cex=1.4)
y <- na.omit(elections_df$Years_of_residence)
plot(density(y), main="Years of residence (in current state)")
polygon(density(y), col="blue")
legend("topright", legend="W = 0.83538 \np-value < 2.2e-16", cex=1.4)
z <- na.omit(elections_df$Income)
plot(density(z), main="Annual Income (in USD)")
polygon(density(z), col="green")
legend("topright", legend="W = 0.9177 \np-value < 2.2e-16", cex=1.4)
dev.off()
par(mfrow=c(1,1))



#check linear correlation between all pairs of numeric variables and create scatterplots
library("ggpubr")
#Age~Income
cor.test(elections_df$Age, elections_df$Income, method = 'pearson')
png(file = "Age_Income_Plot.png", bg = "transparent", width = 1080, height = 1080)
plot(elections_df$Age, elections_df$Income, pch = 19, frame = FALSE, xlab="Age of Respondent", ylab="Annual Income (in USD)")
text(x=80,y=120000,labels="pvalue=0.5049 \n r=0.02")
dev.off()

#years of residence~income
cor.test(elections_df$Years_of_residence, elections_df$Income, method = 'pearson')
png(file = "YearsOfResidence_Income_Plot.png", bg = "transparent", width = 1080, height = 1080)
plot(elections_df$Years_of_residence, elections_df$Income, pch = 19, frame = FALSE, xlab="Years of Residence (in current state)", ylab="Annual Income (in USD)")
text(x=60,y=117000,labels="pvalue=0.3514 \n r=-0,03")
dev.off()

#Age~years of residency
cor.test(elections_df$Age, elections_df$Years_of_residence, method = 'pearson')
png(file = "Age_YearsOfResidency_Plot.png", bg = "transparent", width = 1080, height = 1080)
plot(elections_df$Age, elections_df$Years_of_residence, pch = 19, frame = FALSE, xlab="Age of Respondent", ylab="Years of Residence (in current state)")
text(x=80,y=68,labels="pvalue<2.2e-16 \n r=0.574")
dev.off()

#check correlation for each pair of numeric and categorical variables and plot them using boxplots
#again note that none of our numeric variables follow the normal distribution, so we will be using a kruskal test as it is non-parametric
#Income~Gender
kruskal.test(elections_df$Income, elections_df$Gender)
png(file = "Income_Gender_Plot.png", bg = "transparent", width = 400, height = 400)
boxplot(Income~Gender, data=elections_df, col="orange",border="brown", xlab="Gender of Respondent", ylab="Annual Income (in USD)")
legend("topright", legend="H=23.371, df=1 \npvalue=1.336e-06")
dev.off()

#Age~Gender
kruskal.test(elections_df$Age, elections_df$Gender)
png(file = "Age_Gender_Plot.png", bg = "transparent", width = 1080, height = 1080)
boxplot(Age~Gender, data=elections_df, col="orange",border="brown", xlab="Gender of Respondent", ylab="Age of Respondent")
legend("topright", legend="H=0.169, df=1 \npvalue=0.68")
dev.off()

#age~marital status
kruskal.test(elections_df$Age, elections_df$Marital_Status)
png(file = "Age_MaritalStatus_Plot.png", bg = "transparent", width = 1080, height = 1080)
boxplot(Age~Marital_Status, data=elections_df, col="orange",border="brown", xlab="Marital Status", ylab="Age of Respondent")
legend("topright", legend="H=324.91, df=5 \npvalue=0")
dev.off()

#age~vote
kruskal.test(elections_df$Age, elections_df$Vote)
png(file = "Age_Vote_Plot.png", bg = "transparent", width = 1080, height = 1080)
boxplot(Age~Vote, data=elections_df, col="orange",border="brown", xlab="Candidate Voted for", ylab="Age of Respondent")
legend("topright", legend="H=5.2644, df=4 \npvalue=0.2612")
dev.off()

#income~marital status
kruskal.test(elections_df$Income, elections_df$Marital_Status)
png(file = "Income_MaritalStatus_Plot.png", bg = "transparent", width = 400, height = 400)
boxplot(Income~Marital_Status, data=elections_df, col="orange",border="brown", xlab="Marital Status", ylab="Annual Income (in USD)")
legend("top", legend="H=162.67, df=5 \npvalue=0")
dev.off()

#Income~Political orientation
kruskal.test(elections_df$Income, elections_df$Political_orientation)
png(file = "Income_PoliticalOrientation_Plot.png", bg = "transparent", width = 1080, height = 1080)
boxplot(Income~Political_orientation, data=elections_df, col="orange",border="brown", xlab="Marital Status", ylab="Political Orientation")
legend("top", legend="H=21.604, df=6 \npvalue=0.001")
dev.off()

#Income~Vote
kruskal.test(elections_df$Income, elections_df$Vote)
png(file = "Income_Vote_Plot.png", bg = "transparent", width = 400, height = 400)
boxplot(Income~Vote, data=elections_df, col="orange",border="brown", xlab="Marital Status", ylab="Candidate Voted for")
legend("top", legend="H=13.285, df=4 \npvalue=0.01")
dev.off()

#check correlation for pairs of categorical variables using chisquared+fisher tests and plot them using barplots
#Political Orientation~Marital Status
chisq.test(elections_df$Political_orientation, elections_df$Marital_Status)
fisher.test(elections_df$Political_orientation, elections_df$Marital_Status, simulate.p.value=TRUE)
png(file = "PoliticalOrientation_MaritalStatus_Plot.png", bg = "transparent", width = 1080, height = 1080)
barplot(table(elections_df$Political_orientation, elections_df$Marital_Status), ylab="Count" ,legend.text = TRUE, beside = TRUE)
legend("right", legend="X2=52.504, pvalue=0.006 \nFischer's p=0.005497 \n", y.intersp = 0.2)
dev.off()

#Gender~Vote
chisq.test(elections_df$Gender, elections_df$Vote)
fisher.test(elections_df$Gender, elections_df$Vote, simulate.p.value=TRUE)
png(file = "Gender_Vote_Plot.png", bg = "transparent", width = 1080, height = 1080)
barplot(table(elections_df$Gender, elections_df$Vote), ylab="Count" ,legend.text = TRUE, beside = TRUE)
legend("top", legend="X2=17,636, pvalue=0.01 \nFischer's p=0.0009995 \n", y.intersp = 0.2)
dev.off()

#Vote~Political Orientation
chisq.test(elections_df$Vote, elections_df$Political_orientation)
fisher.test(elections_df$Vote, elections_df$Political_orientation, simulate.p.value=TRUE)
png(file = "Vote_PoliticalOrientation_Plot.png", bg = "transparent", width = 1080, height = 1080)
barplot(table(elections_df$Vote, elections_df$Political_orientation), ylab="Count" ,legend.text = TRUE, beside = TRUE)
legend("topleft", legend="X2=279.65, pvalue=0 \nFischer's p=0.0004998 \n", y.intersp = 0.2)
dev.off()

#marital status~vote
chisq.test(elections_df$Marital_Status, elections_df$Vote)
fisher.test(elections_df$Marital_Status, elections_df$Vote, simulate.p.value=TRUE)
png(file = "MaritalStatus_Vote_Plot.png", bg = "transparent", width = 1080, height = 1080)
barplot(table(elections_df$Marital_Status, elections_df$Vote), ylab="Count" ,legend.text = TRUE, beside = TRUE)
legend("right", legend="X2=43.723, pvalue=0.001 \nFischer's p=0.001499 \n", y.intersp = 0.2)
dev.off()

#2o erwthma
#check for correlation between Years_of_residence and vote
#one is numeric, the other catrgorical so we will first be creating an anova model
anova1 <- aov(Years_of_residence~Vote, data=elections_df)
shapiro.test(anova1$residuals)#residuals not normal, we will use a non-parametric test
lillie.test(anova1$residuals)
library(car)
leveneTest(anova1)# p=0.06, homoscedasticity exists (barely)
png(file = "Anova1_Plot.png", bg = "transparent", width = 1080, height = 1080)
qqnorm(anova1$residuals)
qqline(anova1$residuals)
dev.off()
kruskal.test(elections_df$Years_of_residence, elections_df$Vote)# H0 cannot be rejected, p=0.81, differences between medians not statistically significant, variables not dependent
png(file = "YearsOfResidence_Vote_Plot.png", bg = "transparent", width = 1080, height = 1080)
boxplot(Years_of_residence~Vote, data=elections_df, col="orange",border="brown", xlab="Candidate Voted for", ylab="Years of residence (in current state)")
legend("top", legend="H=1.5788, df=4, pvalue=0.8126 \nF statistic=1.338, anova pvalue=0.254")
dev.off()

anova2 <- aov(Years_of_residence~Political_orientation, data=elections_df)
summary(anova2)
shapiro.test(anova2$residuals)#residuals not normal, we will use a non-parametric test
lillie.test(anova2$residuals)
leveneTest(anova2)#homoscedasticity exists, p=0.45
png(file = "Anova2_Plot.png", bg = "transparent", width = 1080, height = 1080)
qqnorm(anova2$residuals)
qqline(anova2$residuals)
dev.off()
kruskal.test(elections_df$Years_of_residence, elections_df$Political_orientation)#H0 rejected, p=0.006, difference in medians statistically significant, variables dependent
png(file = "YearsOfResidence_PoliticalOrientation_Plot.png", bg = "transparent", width = 1080, height = 1080)
boxplot(Years_of_residence~Political_orientation, data=elections_df, col="orange",border="brown", xlab="Political Orientation", ylab="Years of residence (in current state)")
legend("top", legend="H=18.082, df=6, pvalue=0.006 \nF statistic=1.78, anova pvalue=0.1")
dev.off()

#create a dataframe with only the variables that we will use in models and omit null values
library(dplyr)
finalized_df <- select(elections_df, c('Income', 'Gender', 'Vote', 'Marital_Status'))
finalized_df <- na.omit(finalized_df)

#3o erwthma
model1 <- lm(Income~ Gender + Vote + Marital_Status, data=finalized_df)
stargazer(model1)#generate latex table for model 1
shapiro.test(model1$residuals)#residuals not normal
png(file = "Model1_Plot.png", bg = "transparent", width = 1080, height = 1080)#plot residuals and save
qqnorm(model1$residuals)
qqline(model1$residuals)
dev.off()

model2 <- lm(Income~ Gender + Marital_Status, data=finalized_df)#create second model without vote variable
stargazer(model2)#generate latex table for model 2
shapiro.test(model2$residuals)#residuals not normal
png(file = "Model2_Plot.png", bg = "transparent", width = 1080, height = 1080)#plot residuals and save
qqnorm(model2$residuals)
qqline(model2$residuals)
dev.off()

anova(model1, model2)#compare 2 models, difference not statistically significant

aov <- aov(Income~Gender+Marital_Status, data=finalized_df)
anova(aov)
hist(aov$residuals)#plot residuals
shapiro.test(aov$residuals)#residuals not normal

#use stepwise function to determine optimal model
fullModel = lm(Income~ Gender*Vote*Marital_Status, data=finalized_df) 
nullModel = lm(Income ~ 1, data = finalized_df) 
optimal_model = step(
    fullModel,
    scope = list(upper = fullModel, 
                 lower = nullModel), 
    trace = 0,
    k=1) #choose by AIC 
summary(optimal_model)