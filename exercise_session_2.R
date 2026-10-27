### EXERCISE SESSION 2 - GROUP 4
### Credit Scoring - University of Perugia

library(ggplot2)
library(RColorBrewer)
library(scales)
library(visdat)
library(MASS)
library(pROC)

### ---- EXERCISE 1: logistic regression with two binary predictors ----

# a) Flag = 1 (good client), X1 = marital status (0 never married, 1 otherwise),
#    X2 = home ownership (reference level "N"); find alpha, beta_X1, beta_X2, beta_X1X2

# Theoretical joint distribution
df <- data.frame(
  Flag = c(0, 1, 0, 1, 0, 1, 0, 1),
  X1   = c(0, 0, 1, 1, 0, 0, 1, 1),  # marital status
  X2   = c(0, 0, 0, 0, 1, 1, 1, 1),  # ownership
  Prob = c(0.13, 0.22, 0.21, 0.12, 0.05, 0.08, 0.04, 0.15)
)

# Expand into observational form (probabilities x 1000 -> integer frequencies)
df$Freq <- round(df$Prob * 1000)
df_expanded <- df[rep(1:nrow(df), df$Freq), ]
df_expanded$Flag <- as.factor(df_expanded$Flag)

# Method 1: logistic regression with interaction
model <- glm(Flag ~ X1 * X2, family = binomial, data = df_expanded)
summary(model)

# Method 2: coefficients from the log-odds of each cell
table <- matrix(c(0.13, 0.21, 0.34, 0.05, 0.04, 0.09,
                  0.22, 0.12, 0.34, 0.08, 0.15, 0.23,
                  0.35, 0.33, 0.68, 0.13, 0.19, 0.32), nrow = 3, ncol = 6, byrow = TRUE)
rownames(table) <- c("Flag_0", "Flag_1", "Total")
colnames(table) <- c("MS_0N", "MS_1N", "Total_N", "MS_0Y", "MS_1Y", "Total_Y")
table

p00 <- table[2, 1] / table[3, 1]
p01 <- table[2, 4] / table[3, 4]
p10 <- table[2, 2] / table[3, 2]
p11 <- table[2, 5] / table[3, 5]

log_odds00 <- log(p00 / (1 - p00))  #  0.526
log_odds01 <- log(p01 / (1 - p01))  #  0.470
log_odds10 <- log(p10 / (1 - p10))  # -0.560
log_odds11 <- log(p11 / (1 - p11))  #  1.322

alpha     <- log_odds00                                 #  0.526
beta_X1   <- log_odds10 - alpha                         # -1.086
beta_X2   <- log_odds01 - alpha                         # -0.056
beta_X1X2 <- log_odds11 - alpha - beta_X1 - beta_X2     #  1.937

# b) Log-odds chart by marital status, separately for owners and non-owners
df_log <- data.frame(
  X1       = factor(c(0, 1, 0, 1), labels = c("Never Married", "Married")),
  Owner    = factor(c("N", "N", "Y", "Y"), labels = c("Not Owner", "Owner")),
  log_odds = c(log_odds00, log_odds10, log_odds01, log_odds11)
)
cols <- brewer.pal(n = 3, name = "Dark2")[2:1]

ggplot(df_log, aes(x = X1, y = log_odds, color = Owner, group = Owner)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, shape = 21, stroke = 1.2, aes(fill = Owner)) +
  geom_text(aes(label = sprintf("%.2f", log_odds)), vjust = -1.2, size = 4, show.legend = FALSE) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  labs(title = "Log-odds of Flag = 1 by marital status and ownership",
       x = "Marital status (X1)", y = "Log-odds", color = "Ownership", fill = "Ownership") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "top")
# The lines are not parallel (interaction). The model is saturated: 4 parameters
# for 4 combinations of X1 and X2, so it reproduces the observed probabilities exactly.

# c) Reference level "Y" instead of "N"
df_expanded$X2 <- factor(df_expanded$X2, levels = c(1, 0))
model_newref <- glm(Flag ~ X1 * X2, family = binomial, data = df_expanded)
summary(model_newref)

alpha_Y     <- log_odds01                                     #  0.470
beta_X1_Y   <- log_odds11 - alpha_Y                           #  0.852
beta_X2_Y   <- log_odds00 - alpha_Y                           #  0.056
beta_X1X2_Y <- log_odds10 - alpha_Y - beta_X1_Y - beta_X2_Y   # -1.937

# d) How the table should change for beta_X1X2 to be zero:
#    impose conditional independence of Flag and marital status within each Owner group
marg_probN <- 0.68
marg_probY <- 0.32
marg_probNMS0 <- 0.35; marg_probNMS1 <- 0.33; marg_probNF0 <- 0.34; marg_probNF1 <- 0.34
marg_probYMS0 <- 0.13; marg_probYMS1 <- 0.19; marg_probYF0 <- 0.09; marg_probYF1 <- 0.23

# Conditional probabilities within each Owner group
Pf0N <- (0.13 + 0.21) / marg_probN;  Pf1N <- (0.22 + 0.12) / marg_probN
Pms0N <- (0.13 + 0.22) / marg_probN; Pms1N <- (0.21 + 0.12) / marg_probN
Pf0Y <- (0.05 + 0.04) / marg_probY;  Pf1Y <- (0.08 + 0.15) / marg_probY
Pms0Y <- (0.05 + 0.08) / marg_probY; Pms1Y <- (0.04 + 0.15) / marg_probY

# New joint probabilities under conditional independence
N00 <- marg_probN * Pf0N * Pms0N; N01 <- marg_probN * Pf0N * Pms1N
N10 <- marg_probN * Pf1N * Pms0N; N11 <- marg_probN * Pf1N * Pms1N
Y00 <- marg_probY * Pf0Y * Pms0Y; Y01 <- marg_probY * Pf0Y * Pms1Y
Y10 <- marg_probY * Pf1Y * Pms0Y; Y11 <- marg_probY * Pf1Y * Pms1Y

tablenew <- matrix(c(N00, N01, marg_probNF0, Y00, Y01, marg_probYF0,
                     N10, N11, marg_probNF1, Y10, Y11, marg_probYF1,
                     marg_probNMS0, marg_probNMS1, marg_probN, marg_probYMS0, marg_probYMS1, marg_probY),
                   nrow = 3, ncol = 6, byrow = TRUE)
rownames(tablenew) <- rownames(table)
colnames(tablenew) <- colnames(table)
tablenew

p00new <- tablenew[2, 1] / tablenew[3, 1]
p01new <- tablenew[2, 4] / tablenew[3, 4]
p10new <- tablenew[2, 2] / tablenew[3, 2]
p11new <- tablenew[2, 5] / tablenew[3, 5]

log_odds00new <- log(p00new / (1 - p00new))
log_odds01new <- log(p01new / (1 - p01new))
log_odds10new <- log(p10new / (1 - p10new))
log_odds11new <- log(p11new / (1 - p11new))

beta_X1new   <- log_odds10new - log_odds00new
beta_X2new   <- log_odds01new - log_odds00new
beta_X1X2new <- log_odds11new - log_odds00new - beta_X1new - beta_X2new  # ~ 0


### ---- EXERCISE 2: bankruptcy model for Umbrian firms ----

dati <- read.table(file.choose(), header = TRUE, comment.char = "#")

# a) Check that the flag is well constructed (1 = active, 0 = bankrupt)
table(dati$flag, dati$SG)
dati$Flag <- ifelse(dati$SG == "Ditta in fallim", 0, 1)

var_imp <- c("Flag", "UN06", "ICIP06", "IIB06", "IIF06", "IIL06", "IL06",
             "OF06", "RI06", "RCI06", "ID06", "EBITDAV06")
datin <- dati[, var_imp]

# Missing values: keep complete cases only
vis_miss(datin)
a <- complete.cases(datin)
table(a)
datin_c <- datin[a, ]

# b) Initial model (ICIP06, IL06 and EBITDAV06 also enter in quadratic form)
mod.iniz <- glm(Flag ~ UN06 + ICIP06 + I(ICIP06^2) + IIB06 + IIF06 + IIL06 + IL06 + I(IL06^2) +
                  OF06 + RCI06 + RI06 + ID06 + EBITDAV06 + I(EBITDAV06^2),
                data = datin_c, family = binomial)
summary(mod.iniz)

# Hierarchical reduction: drop ICIP06 and its square; LR test vs Wald test
mod.reduced <- glm(Flag ~ UN06 + IIB06 + IIF06 + IIL06 + IL06 + I(IL06^2) +
                     OF06 + RCI06 + RI06 + ID06 + EBITDAV06 + I(EBITDAV06^2),
                   data = datin_c, family = binomial)
anova(mod.reduced, mod.iniz, test = "Chisq")
summary(mod.iniz)$coefficients["ICIP06", "Pr(>|z|)"]

# c) Backward selection (AIC)
mod.fin <- stepAIC(mod.iniz, direction = "backward", test = "Chisq")
summary(mod.fin)

# d) LR test of the final model against the intercept-only model
mod.null <- glm(Flag ~ 1, family = binomial, data = datin_c)
anova(mod.null, mod.fin, test = "Chisq")
# The null deviance in summary(mod.fin) equals the residual deviance of mod.null,
# so the null model does not need to be estimated separately.

# e) Validation sample
dati_val <- read.csv("umbria_match_stud.csv", sep = ";")
dati_val$Flag <- dati_val$SG == "Ditta attiva"
table(dati_val$Flag, dati_val$SG)

datin_val <- dati_val[, var_imp]
vis_miss(datin_val)
datin_c_val <- datin_val[complete.cases(datin_val), ]

score_pred <- predict(mod.fin, newdata = datin_c_val, type = "response")

# Graphical check of discrimination
boxplot(score_pred ~ datin_c_val$Flag, col = c("lightcoral", "royalblue4"),
        names = c("Bankrupt", "Active"), ylab = "Predicted probability",
        main = "Predicted scores by firm status")
hist(score_pred[datin_c_val$Flag == TRUE], col = "royalblue2",
     main = "Predicted score distribution", xlab = "Score")
hist(score_pred[datin_c_val$Flag == FALSE], col = "lightgoldenrod", add = TRUE)
legend("topleft", legend = c("Active", "Bankrupt"), fill = c("royalblue2", "lightgoldenrod"))

# f) ROC curve and AUC on the validation sample
roc_curve <- roc(datin_c_val$Flag, score_pred)
plot(roc_curve, main = "ROC curve - validation sample", col = "blue4", print.auc = TRUE)
auc(roc_curve)


### ---- EXERCISE 3: retail loan portfolio (securitisation data) ----

dati <- read.table(file.choose(), header = TRUE)  # cartolarization data

# a) Linearity check: logistic regression of Flag on the quartile classes of each
#    continuous variable; coefficients plotted at the class midpoints
#    (red = significant at 5%, black = not significant)
linearity_check <- function(x, name) {
  Flag <- factor(dati$Flag)
  qnt <- quantile(x, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE)
  VAR_Q <- rep(NA, length(x))
  VAR_Q[is.na(x)] <- 5
  VAR_Q[x <= qnt[2]] <- 1
  VAR_Q[x > qnt[2] & x <= qnt[3]] <- 2
  VAR_Q[x > qnt[3] & x <= qnt[4]] <- 3
  VAR_Q[x > qnt[4] & x <= qnt[5]] <- 4
  VARQ <- factor(VAR_Q)
  modl <- glm(Flag ~ VARQ, family = binomial)
  p_value <- summary(modl)$coefficients[, 4]
  color <- ifelse(p_value <= 0.05, "red", "black")
  CV <- (qnt[1:4] + qnt[2:5]) / 2
  plot(CV, modl$coefficients[1:4], type = "b", pch = 19, lty = 1, col = color[1:4],
       xlab = name, ylab = "coeff", main = paste("Linearity check:", name))
}

for (v in c("Age", "lastsal", "lastrat", "avgsal", "recvsaff", "kept", "rpcrate", "prorate")) {
  linearity_check(dati[[v]], v)
}

# b) Train / validation split (4000 / 1276) and univariate screening
dati$prot_bin <- ifelse(dati$prot == "SI", 1, 0)
dati$imm_bin  <- ifelse(dati$imm == "SI", 1, 0)

set.seed(123)
s <- sample.int(nrow(dati), 4000, replace = FALSE)
training   <- dati[s, ]
validation <- dati[-s, ]

for (v in c("Age", "zres", "lastsal", "lastrat", "avgsal", "recvsaff",
            "kept", "prot", "imm", "rpcrate", "prorate")) {
  cat("\n----", v, "----\n")
  print(summary(lm(as.formula(paste("Flag ~", v)), data = training))$coefficients)
}

# c) Large hierarchical model M1 with all second-order interactions involving at least
#    one categorical/binary covariate, followed by backward selection.
#    The model is estimated on the TRAINING sample only, so that the validation
#    sample in point d) is a genuine out-of-sample check.
M1 <- lm(Flag ~ Age + zres + lastsal + lastrat + avgsal + recvsaff + kept +
           prot_bin + imm_bin + rpcrate + prorate +
           Age * zres + Age * prot_bin + Age * imm_bin +
           zres * prot_bin + zres * imm_bin +
           lastsal * zres + lastsal * prot_bin + lastsal * imm_bin +
           lastrat * zres + lastrat * prot_bin + lastrat * imm_bin +
           avgsal * zres + avgsal * prot_bin + avgsal * imm_bin +
           recvsaff * zres + recvsaff * prot_bin + recvsaff * imm_bin +
           kept * zres + kept * prot_bin + kept * imm_bin +
           rpcrate * zres + rpcrate * prot_bin + rpcrate * imm_bin +
           prorate * zres + prorate * prot_bin + prorate * imm_bin +
           prot_bin * imm_bin,
         data = training)

mod.fin3 <- stepAIC(M1, direction = "backward")
summary(mod.fin3)

# d) Misclassification table on the validation sample, with log(C(1|0)/C(0|1)) = 0.4.
#    A client is accepted as good when P(good|x)/P(bad|x) > C(1|0)/C(0|1) = exp(0.4),
#    i.e. when the estimated probability exceeds 1 / (1 + exp(-0.4)) = 0.599.
valida.score   <- predict(mod.fin3, newdata = validation)
cut_off_value  <- 1 / (1 + exp(-0.4))
flag.estimated <- valida.score > cut_off_value

misclassification_table <- table(Actual = validation$Flag, Predicted_good = flag.estimated)
misclassification_table

# Type I error: a good client (Flag = 1) classified as bad
# Type II error: a bad client (Flag = 0) classified as good
type_I_rate  <- misclassification_table["1", "FALSE"] / sum(misclassification_table["1", ])
type_II_rate <- misclassification_table["0", "TRUE"]  / sum(misclassification_table["0", ])
type_I_rate
type_II_rate
