# Credit Scoring in R: Bankruptcy Prediction, Scorecards and Logistic Models

**Group project** – Credit Scoring, University of Perugia (Feb–May 2025)

Two exercise sets covering the full credit-scoring workflow: from exploring financial ratios and testing their discriminatory power, to building, selecting and validating scoring models.

## Exercise Set 1 (`exercise_set_1.R`)

- **Bankruptcy data (Altman ratios):** exploratory analysis of WC/TA, RE/TA, EBIT/TA, S/TA and BVE/BVL for bankrupt vs healthy firms, with log-transformations, boxplots, histograms and scatterplot matrices
- **Discriminatory power:** one-sided t-tests (equal and unequal variances) show that log(BVE/BVL) is significantly lower for bankrupt firms (p < 0.001)
- **Retail loan portfolio (5,276 clients):** good-client rates by region (North 58.5%, Centre 55.0%, South 51.0%) and a two-proportion test on real-estate ownership
- **Linear scorecard:** score built on last salary and repayment behaviour, validated on a 500-client hold-out sample – **AUC = 0.73**
- **Bayes decision rule:** grant/reject decision minimising expected loss with asymmetric misclassification costs, posterior probability of being a good client (0.91), Type I/II errors and ROC coordinates for a given cut-off

## Exercise Session 2 (`exercise_session_2.R`)

- **Logistic regression with interactions:** coefficients of a saturated model with two binary predictors, effect of changing the reference level, and how the table must change for the interaction term to vanish
- **Bankruptcy model for Umbrian firms:** handling missing data (2,186 complete cases out of 2,505), likelihood-ratio vs Wald tests, **backward stepwise selection (AIC)** with quadratic terms
- **Final model:** 5 predictors (including a quadratic EBITDA term); LR test against the null model χ² = 369.6 on 5 df (p < 0.001)
- **Out-of-sample validation** on a separate sample of 185 firms: **AUC = 0.88**
- **Retail portfolio scoring:** linearity checks via quartile-based logistic regressions, univariate screening on a 4,000 / 1,276 train–validation split, a large hierarchical model with second-order interactions reduced by backward selection, and a **cost-sensitive cut-off** (log cost ratio = 0.4) with Type I/II error rates on the validation sample

## Data

The scripts expect the course datasets (bankruptcy data, `cartolarization_data.txt`, Umbrian firms training and validation files), which are not included in this repository.

## Tools

R · pROC · MASS (stepAIC) · visdat · ggplot2
