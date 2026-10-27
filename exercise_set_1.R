### EXERCISE SET 1 - GROUP 4

### ---- EXERCISE 1 ---- 
#a)
dati <- read.table(file.choose(), header=TRUE)
summary(dati)
dati$Flag<-1-dati$Bankrupt #we consider "good" the companies that are not 
#bankrupt
color1<-c("red","blue") #we create a vector of colors that represent bad and good units

par(mfrow=c(1,5)) #in this way we are able to see the boxplot for each variable in one graph

#BOXPLOTS WITHOUT TRANSFORMATION
boxplot(dati$WC.TA~dati$Flag,xlab = "Flag",ylab = "WC.TA",col=color1)
boxplot(dati$RE.TA~dati$Flag,xlab = "Flag",ylab = "RE.TA",col=color1)
boxplot(dati$EBIT.TA~dati$Flag,xlab = "Flag",ylab = "EBIT.TA",col=color1)
boxplot(dati$S.TA~dati$Flag,xlab = "Flag",ylab = "S.TA",col=color1)
boxplot(dati$BVE.BVL~dati$Flag,xlab = "Flag",ylab = "BVE.BVL",col=color1)

#check of the negative variables
min(dati$WC.TA); min(dati$RE.TA); min(dati$EBIT.TA); min(dati$S.TA); min(dati$BVE.BVL)

#creation of the variables for the log-transformation
WC.TA_log<-log(abs(dati$WC.TA+0.1))
RE.TA_log<-log(abs(dati$RE.TA+0.1))
EBIT.TA_log<-log(abs(dati$EBIT.TA+0.1))
S.TA_log<-log(dati$S.TA+0.1)
BVE.BVL_log<-log(dati$BVE.BVL+0.1)
#now we are able to construct the boxplots
boxplot(WC.TA_log~dati$Flag,xlab = "Flag",ylab = "WC.TA_log",col=color1,
        main = "WC.TA log-transformed")
boxplot(RE.TA_log~dati$Flag,xlab = "Flag",ylab = "RE.TA_log",col=color1,
        main = "RE.TA log-transformed")
boxplot(EBIT.TA_log~dati$Flag,xlab = "Flag",ylab = "EBIT.TA_log",col=color1,
        main = "EBIT.TA log-transformed")
boxplot(S.TA_log~dati$Flag,xlab = "Flag",ylab = "S.TA_log",col=color1,
        main = "S.TA log-transformed")
boxplot(BVE.BVL_log~dati$Flag,xlab = "Flag",ylab = "BVE.BVL_log",col=color1,
        main = "BVE.BVL log-transformed")

#HISTOGRAMS WITHOUT TRANSFORMATION

par(mfrow=c(2,1))

#WC.TA
#bad units
hist(dati$WC.TA[dati$Flag==0], xlim=range(min(dati$WC.TA)-1, max(dati$WC.TA)+1),
     xlab = "Bankrupt",main = "Histogram of WC.TA",col = "red")
#good unts
hist(dati$WC.TA[dati$Flag==1], xlim=range(min(dati$WC.TA)-1, max(dati$WC.TA)+1),
     xlab = "No bankrupt",main = "", col = "blue")

#RE.TA
#bad units
hist(dati$RE.TA[dati$Flag==0], xlim=range(min(dati$RE.TA)-1, max(dati$RE.TA)+1),
     xlab = "Bankrupt",main = "Histogram of RE.TA",col = "red")
#good units
hist(dati$RE.TA[dati$Flag==1], xlim=range(min(dati$RE.TA)-1, max(dati$RE.TA)+1),
     xlab = "No bankrupt",main = "",col = "blue")

#EBIT.TA
#bad units
hist(dati$EBIT.TA[dati$Flag==0], xlim=range(min(dati$EBIT.TA)-1,max(dati$EBIT.TA)+1),
     xlab = "Bankrupt",main = "Histogram of EBIT.TA", col = "red")
#good units
hist(dati$EBIT.TA[dati$Flag==1], xlim=range(min(dati$EBIT.TA)-1,max(dati$EBIT.TA)+1),
     xlab = "No bankrupt",main = "", col = "blue")

#S.TA
#bad units
hist(dati$S.TA[dati$Flag==0], xlim=range(min(dati$S.TA)-1, max(dati$S.TA)+1),
     xlab = "Bankrupt",main = "Histogram of S.TA",col = "red")
#good units
hist(dati$S.TA[dati$Flag==1], xlim=range(min(dati$S.TA)-1, max(dati$S.TA)+1),
     xlab = "No bankrupt",main = "",col = "blue")

#BVE.BVL
#bad units
hist(dati$BVE.BVL[dati$Flag==0], xlim=range(min(dati$BVE.BVL)-1, max(dati$BVE.BVL)+1),
     xlab = "Bankrupt",main = "Histogram of BVE.BVL",col = "red")
#good units
hist(dati$BVE.BVL[dati$Flag==1], xlim=range(min(dati$BVE.BVL)-1,max(dati$BVE.BVL)+1),
     xlab = "No bankrupt",main = "",col = "blue")

#HISTOGRAMS WITH THE LOG-TRANSFORMATION

par(mfrow=c(2,1))

#WC.TA_log
#bad units
hist(WC.TA_log[dati$Flag==0], xlim=range(min(WC.TA_log)-1,max(WC.TA_log)+1),
     xlab = "Bankrupt",main = "Histogram of WC.TA log-transformed",col = "red")
#good units
hist(WC.TA_log[dati$Flag==1], xlim=range(min(WC.TA_log)-1, max(WC.TA_log)+1),
     xlab = "No bankrupt",main = "", col = "blue")

#RE.TA_log
#bad units
hist(RE.TA_log[dati$Flag==0], xlim=range(min(RE.TA_log)-1, max(RE.TA_log)+1),
     xlab = "Bankrupt",main = "Histogram of RE.TA log-transformed",col = "red")
#good units
hist(RE.TA_log[dati$Flag==1], xlim=range(min(RE.TA_log)-1, max(RE.TA_log)+1),
     xlab = "No bankrupt",main = "",col = "blue")

#EBIT.TA_log
#bad units
hist(EBIT.TA_log[dati$Flag==0], xlim=range(min(EBIT.TA_log)-1, max(EBIT.TA_log)+1),
     xlab = "Bankrupt",main = "Histogram of EBIT.TA log-transformed", col = "red")
#good units
hist(EBIT.TA_log[dati$Flag==1], xlim=range(min(EBIT.TA_log)-1,max(EBIT.TA_log)+1),
     xlab = "No bankrupt",main = "", col = "blue")

#S.TA_log
#bad units
hist(S.TA_log[dati$Flag==0], xlim=range(min(S.TA_log)-1, max(S.TA_log)+1),
     xlab = "Bankrupt",main = "Histogram of S.TA log-transformed",col = "red")
#good units
hist(S.TA_log[dati$Flag==1], xlim=range(min(S.TA_log)-1, max(S.TA_log)+1),
     xlab = "No bankrupt",main = "",col = "blue")

#BVE.BVL_log
#bad units
hist(BVE.BVL_log[dati$Flag==0], xlim=range(min(BVE.BVL_log)-1,max(BVE.BVL_log)+1),
     xlab = "Bankrupt",main = "Histogram of BVE.BVL log-transformed",col = "red")
#good units
hist(BVE.BVL_log[dati$Flag==1], xlim=range(min(BVE.BVL_log)-1,max(BVE.BVL_log)+1),
     xlab = "No bankrupt",main = "",col = "blue")

#CHECK OF NORMALITY

par(mfrow=c(1,1))

#WC.TA_log
hist(WC.TA_log, xlim=range(min(WC.TA_log)-1, max(WC.TA_log)+1),
     xlab = "WC.TA_log", main = "Histogram of WC.TA_log", col = "darkorange")

#RE.TA_log
hist(RE.TA_log, xlim=range(min(RE.TA_log)-1, max(RE.TA_log)+1),xlab = "RE.TA_log", 
     main = "Histogram of RE.TA_log", col = "khaki1")

#EBIT.TA_log
hist(EBIT.TA_log, xlim=range(min(EBIT.TA_log)-1, max(EBIT.TA_log)+1),
     xlab = "EBIT.TA_log", main = "Histogram of EBIT.TA_log", col = "darkolivegreen1")

#S.TA_log
hist(S.TA_log, xlim=range(min(S.TA_log)-1, max(S.TA_log)+1),xlab = "S.TA_log",
     main = "Histogram of S.TA_log", col = "deepskyblue")

#BVE.BVL_log
hist(BVE.BVL_log, xlim=range(min(BVE.BVL_log)-1, max(BVE.BVL_log)+1),xlab = "BVE.BVL_log",
     main = "Histogram of BVE.BVL_log", col = "plum3")

#b)
par(mfrow=c(1,1))

color<-rep("blue",dim(dati)[1])
color[dati$Flag==0]<-"red"

#WC.TA
plot(dati$WC.TA,dati$RE.TA, col=color,xlab = "WC.TA",ylab = "RE.TA",main = "Scatterplot between WC.TA and RE.TA")
plot(dati$WC.TA,dati$EBIT.TA, col=color,xlab = "WC.TA",ylab = "EBIT.TA",main = "Scatterplot between WC.TA and EBIT.TA")
plot(dati$WC.TA,dati$S.TA, col=color,xlab = "WC.TA",ylab = "S.TA",main = "Scatterplot between WC.TA and S.TA")
plot(dati$WC.TA,dati$BVE.BVL, col=color,xlab = "WC.TA",ylab = "BVE.BVL",main = "Scatterplot between WC.TA and BVE.BVL")

#RE.TA
plot(dati$RE.TA,dati$WC.TA, col=color,xlab = "RE.TA",ylab = "WC.TA",main = "Scatterplot between RE.TA and WC.TA")
plot(dati$RE.TA,dati$EBIT.TA, col=color,xlab = "RE.TA",ylab = "EBIT.TA",main = "Scatterplot between RE.TA and EBIT.TA")
plot(dati$RE.TA,dati$S.TA, col=color,xlab = "RE.TA",ylab = "S.TA",main = "Scatterplot between RE.TA and S.TA")
plot(dati$RE.TA,dati$BVE.BVL, col=color,xlab = "RE.TA",ylab = "BVE.BVL",,main = "Scatterplot between RE.TA and BVE.BVL")

#EBIT.TA
plot(dati$EBIT.TA,dati$WC.TA, col=color,xlab = "EBIT.TA",ylab = "WC.TA",main = "Scatterplot between EBIT.TA and WC.TA")
plot(dati$EBIT.TA,dati$RE.TA, col=color,xlab = "EBIT.TA",ylab = "RE.TA",main = "Scatterplot between EBIT.TA and RE.TA")
plot(dati$EBIT.TA,dati$S.TA, col=color,xlab = "EBIT.TA",ylab = "S.TA",main = "Scatterplot between EBIT.TA and S.TA")
plot(dati$EBIT.TA,dati$BVE.BVL, col=color,xlab = "EBIT.TA",ylab = "BVE.BVL",main = "Scatterplot between EBIT.TA and BVE.BVL")

#S.TA
plot(dati$S.TA,dati$WC.TA, col=color,xlab = "S.TA",ylab = "WC.TA",main = "Scatterplot between S.TA and WC.TA")
plot(dati$S.TA,dati$RE.TA, col=color,xlab = "S.TA",ylab = "RE.TA",main = "Scatterplot between S.TA and RE.TA")
plot(dati$S.TA,dati$EBIT.TA, col=color,xlab = "S.TA",ylab = "EBIT.TA",main = "Scatterplot between S.TA and EBIT.TA")
plot(dati$S.TA,dati$BVE.BVL, col=color,xlab = "S.TA",ylab = "BVE.BVL",main = "Scatterplot between S.TA and BVE.BVL")

#BVE.BVL
plot(dati$BVE.BVL,dati$WC.TA, col=color,xlab = "BVE.BVL",ylab = "WC.TA",main = "Scatterplot between BVE.BVL and WC.TA")
plot(dati$BVE.BVL,dati$RE.TA, col=color,xlab = "BVE.BVL",ylab = "RE.TA",main = "Scatterplot between BVE.BVL and RE.TA")
plot(dati$BVE.BVL,dati$EBIT.TA, col=color,xlab = "BVE.BVL",ylab = "EBIT.TA",main = "Scatterplot between BVE.BVL and EBIT.TA")
plot(dati$BVE.BVL,dati$S.TA, col=color,xlab = "BVE.BVL",ylab = "S.TA",main = "Scatterplot between BVE.BVL and S.TA")

#we change the name of the variables to a better representation on the matrix
WC.TA<-dati$WC.TA
RE.TA<-dati$RE.TA
EBIT.TA<-dati$EBIT.TA
S.TA<-dati$S.TA
BVE.BVL<-dati$BVE.BVL

#scatterplot matrix
pairs(~ WC.TA + RE.TA + EBIT.TA + S.TA + BVE.BVL, 
      data = dati, 
      col = color, 
      main = "Scatterplot matrix of the five covariates",
      cex.labels = 1.5,  
      font.labels = 2)

#scatterplot matrix with the transformation
pairs(~ WC.TA_log + RE.TA_log + EBIT.TA_log + S.TA_log+ BVE.BVL_log, 
      data = dati, 
      col=color,
      main = "Matrix of Scatter Plots of the Five Log-Transformed Covariates",
      cex.labels = 1.5,  
      font.labels = 2)

#c)
X<-BVE.BVL_log
#we create good and bad populations
P0<-X[dati$Flag == 0]  #Bankrupted firms (Flag = 0)
P1<-X[dati$Flag == 1]  #Not bankrupted firms (Flag = 1)

#T TEST under the assumption σ₀ = σ₁
t_test_common_var <- t.test(P0, P1, alternative = "less", var.equal = TRUE)

#T TEST under the assumption σ₀ ≠ σ₁
t_test_diff_var <- t.test(P0, P1, alternative = "less", var.equal = FALSE)

### ---- EXERCISE 2 ----
dati=read.table(file.choose(),header=TRUE,comment.char="#") #file Cartolarization data

a<-table(dati$zres, dati$Flag)

# calculate observed probability of a unit that is in the North to be good
freq_north_good<-a[2,2]/(a[2,2]+a[2,1]) #0.584855

# calculate observed probability of a unit that is in the Center to be good
freq_center_good<-a[1,2]/(a[1,1]+a[1,2]) #0.5504225

# calculate observed probability of a unit that is in the South to be good
freq_south_good<-a[3,2]/(a[3,1]+a[3,2]) #0.5100671

#frequency table to check the results
prop.table(table(dati$zres, dati$Flag), margin = 1)

#good clients of each region over the sum of the good clients
#NORTH
freq_tot_north<-a[2,2]/(a[1,2]+a[2,2]+a[3,2]) #0.3752584
#CENTER
freq_tot_center<-a[1,2]/(a[1,2]+a[2,2]+a[3,2]) #0.3366644
#SOUTH
freq_tot_south<-a[3,2]/(a[1,2]+a[2,2]+a[3,2]) #0.2880772


b<-table(dati$Flag, dati$zres, dati$imm)

b1<-matrix(b[2,,],6,1) #GOOD clients: the first three rows aren't Real Estate 
#Owners, instead the last three rows are Real Estate Owners  
b2<-matrix(b[1,,],6,1) #BAD clients: the first three rows aren't Real Estate 
#Owners, instead the last three rows are Real Estate Owners
B<-cbind(b2,b1) #we create a new table with bad clients in the column 1 and 
#good clients in the column 2; not real estate owner in the first three rows 
#and real estate owner in the last three rows; center in rows 1 and 4, north 
#in rows 2 and 5, south in rows 3 and 6

#relative frequency of clients who are in the ”North” and are ”Real Estate owner”
freq_north_re<-(B[5,1]+B[5,2])/(B[5,1]+B[5,2]+B[2,1]+B[2,2]) #0.09667025

#relative frequency of good clients among the total number of clients who are 
#in the North and are Real Estate Owner
freq_good_north_re<-(B[5,2]/(B[5,1]+B[5,2])) #0.55

#b)
table_2x2 <- matrix(c(sum(B[4:6, 2]), sum(B[1:3, 2]),
                      sum(B[4:6, 1]), sum(B[1:3, 1])),
                    nrow = 2, byrow = TRUE)
#nomi righe e colonne
rownames(table_2x2) <- c("GOOD", "BAD")
colnames(table_2x2) <- c("Imm SI", "Imm NO")
table_2x2

PY<-table_2x2[1,1]/(table_2x2[1,1]+table_2x2[2,1]) #0.5228426

PN<-table_2x2[1,2]/(table_2x2[1,2]+table_2x2[2,2]) #0.5534685

prop.test(c(table_2x2[1,1], table_2x2[1,2]), 
          c((table_2x2[1,1]+table_2x2[2,1]), table_2x2[1,2]+table_2x2[2,2]), 
          alternative = "two.sided",correct = TRUE,
          conf.level = 0.95)

#d)
set.seed(123) 
s <- sample.int(nrow(dati), 500, replace = FALSE) 

validazione <- dati[s, ]  
sviluppo <- dati[-s, ]

#e)
mod<-lm(Flag~lastsal+kept, data=sviluppo) #create the model
summary(mod)
coef(mod) #the score function is s_hat(x)=0.227-0.000023*lastsal+0.6705*kept
score.val <- predict(mod, newdata = validazione)  #score.val is the score function

library(pROC)
par(mfrow=c(1,1))
roc(validazione$Flag, score.val, plot=TRUE)

### ---- EXERCISE 3 ----
#a)
#To solve this exercise, the first step is to write down the data that will be 
#useful for computing the various elements necessary for the decision making 
#approach and customer classification.

# Below, we define the given parameters:
mu1 <- 63      # Mean for class Y = 1
var1 <- 40 # Variance for class Y = 1
mu0 <- 39      # Mean for class Y = 0
var0 <- 30 # Variance for class Y = 0
x <- 52        # Observed value of X
P_Y1 <- 0.75   # Prior probability P(Y = 1)
P_Y0 <- 0.25   # Prior probability P(Y = 0)
cutoff <- 1.2   # Ratio between costs

# a)
#The first point ask us to decide whether to grant or not the new applicant by
#using the criterion that minimises the expected loss.

#To do this we have to follow different steps: 
#The first one is to compute the conditional density functions f1(x) and f0(x)
#given the values of Y and evaluated in x

f1_x <- dnorm(x, mean = mu1, sd = sqrt(var1)) # Likelihood for Y = 1

f0_x <- dnorm(x, mean = mu0, sd = sqrt(var0)) # Likelihood for Y = 0

#The second step is to find the posteriori probabilities  P(Y=1|x) and P(Y=0|x)
#using also the density functions f1(x) and f0(x) computed previously:

P_Y1_x <- (f1_x * P_Y1) / (f1_x * P_Y1 + f0_x * P_Y0) # P(Y=1|x)

P_Y0_x <- (f0_x * P_Y0) / (f1_x * P_Y1 + f0_x * P_Y0) # P(Y=0|x)

#These posterior probabilities are necessary because, in order to define the 
#acceptance region, also called A1, within which the customer is classified as good 
#and therefore eligible to receive the loan, the following rule must be satisfied:
#The ratio of the posterior probabilities P(Y=1∣x)/P(Y=0∣x) must be greater than
#a cut-off score, say c.
#The optimal cut-off score is the ratio between the cost C(0/1)that must be paid 
#if a unit from P1 is assigned to P0(cost of type I error) and C(1/0) viceversa (cost of type II error).

#so if we compute the ratio between posterior probabilities:

post_prob_ratio <- P_Y1_x/P_Y0_x

#We can notice that the ratio of the two posterior probabilities (9.572917) is greater than the
#ratio of the two costs (1.2), therefore we will proceed to accept the customer.

#Decision:
decision <- if(post_prob_ratio > cutoff) "Grant the new applicant" else "Not grant the new applicant"

# b) 
#evaluate the posterior probability P(Y = 1 | x) that such applicant is a good client;

P_Y1_x <- (f1_x * P_Y1) / (f1_x * P_Y1 + f0_x * P_Y0) # P(Y=1|x)

##Probability already calculated In point (a) that consider the flag equal to one (SME is a good firm)

#c) 
#Compute Type I e Type II errors with cut-off at X = 60

cutoffvalue <- 60

# Compute Type I Error (it means we reject a good unit and so we consider a good
#unit like a bad unit
type_I_error <- pnorm(cutoffvalue, mean = mu1, sd = sqrt(var1), lower.tail = TRUE)

# Compute Type II Error( it means we accept a bad unit and so we consider a bad
#unit like good one)
type_II_error <- pnorm(cutoffvalue, mean = mu0, sd = sqrt(var0), lower.tail = FALSE)

#  The ROC is the curve on the cartesian plan that matches the points with x-value
#1−F(s|Y=0) and y-value 1−F(s|Y=1)

#x-value and y-value on the ROC curve
x_roc <- type_II_error #false positive rate (1 - specificity)

y_roc <-1 - type_I_error #true positive rate also known as sensitivity





