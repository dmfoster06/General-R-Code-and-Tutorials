# LIBRARIES
library(readxl)
library(openxlsx)
library(dplyr)
library(epiDisplay) #tabpct function
library(sjmisc) #frq function
library(qacBase) #qstats function
library(effectsize) #eta squared
library(emmeans) #multiple comparisons
library(lsr) #cohens d
library(sandwich) #robust standard errors
library(stats) #wilcoxon test
library(car) #Levene Test
library(pairwiseCI) #display pairwise proportions differences
library(ARTool) #aligned rank transform anova
library(pairwiseCI) #present percentage point difference between groups on % favorable
library(remotes) #tidy pairwise z-tests
library(reporttools) #pairwise fisher exact test


# DESCRIPTIVES AND DIAGNOSTICS
## Descriptives
qstats(df, y, group) #Group N, Mean, SD (at least k+1 per group)

## Homogeneity of Variances - ANOVA or t-test
mod <- aov(y ~ group, data = df)

leveneTest(mod) #Levene Test (ns = homogeneous)

tvars <- with(df, tapply(y,group,var, na.rm = TRUE)) #Ratio of largest v smallest group variance (should be no bigger than ~1.5)
max(tvars)/min(tvars)

## Normality of Residuals
qqPlot(mod) #Fitted vs residual plot (should follow diagonal)
hist(mod$residuals) # Histogram (should be approx normal)
shapiro.test(mod$residuals) #Shapiro Wilk test (ns = normal)

# DIFFERENCES BETWEEN 3+ GROUPS - Normal, Homoscedastic or Heteroscedastic
Anova(mod, type=3) #homoscedastic
Anova(mod, type=3, white.adjust="hc0") #heterscedastity-corrected cov matrix / robust SE

eta_squared(mod, partial = FALSE)

qstats(df, y, group) #Group N, Mean, SD 
mod.emmeans <- emmeans(mod, pairwise ~ group, adjust="fdr", infer = c(FALSE, TRUE))
mod.emmeans$contrasts #pairwise contrasts
eff_size(pairs(mod.emmeans), sigma = sigma(mod), edf = df.residual(mod), method = "identity") #cohens d for contrasts
plot(mod.emmeans, comparisons = FALSE) #plotted point estimates and CIs

# DIFFERENCES BETWEEN 3+ GROUPS - One-way with non-normal residuals
kruskal.test(y ~ group, data = df)
dunn_test(df, y ~ group, p.adjust.method = "fdr", detailed = FALSE) #contrasts

# DIFFERENCES BETWEEN 3+ GROUPS - Two-way with non-normal residuals
## Aligned Rank Transform ANOVA
df.clean <- df[!is.na(df$y),] #can only be performed if there are no missing values for the DV
df.clean$FACTOR1 <- as.factor(df.clean$FACTOR1) #factor variables must be classified as factors
df.clean$FACTOR2 <- as.factor(df.clean$FACTOR2) #factor variables must be classified as factors

mod.art <- art(y ~ factor1*factor2,
            data = df.clean)
anova(mod.art)
art.con(mod.art, "factor1:factor2", adjust="fdr")

# DIFFERENCE BETWEEN 2 GROUPS - Normal, homoscedastic or heteroscedastic
t.test(y ~ group, data = df, var.equal=TRUE) #homoscedastic
cohensD(y ~ group, data = df, method ="pooled")

t.test(y ~ group, data = df, var.equal=FALSE) #heteroscedastic
cohensD(y ~ group, data = df, method ="unequal")

# DIFFERENCE BETWEEN 2 GROUPS - Non-normal residuals
wilcox.test(df$y ~ df$group, mu=0,alt="two.sided",conf.int=T,conf.level=0.95,exact=F,correct=F)

# PRESENT DIFFERENCE BETWEEN PAIRWISE FAVORABLE PROPORTIONS (not a test)
df.props <- df %>%
  group_by(group) %>%
  summarise(Favorable = sum(y),
            NotFavorable = sum(1 - y),
            .groups = 'drop')

pairwiseCI(formula = cbind(Favorable, NotFavorable) ~ group,
           data = df.props,
           method = "Prop.diff",
           alternative = "two.sided", 
           conf.level = 0.95) 

# TEST OF DIFFERENCE BETWEEN PAIRWISE FAVORABLE PROPORTIONS (z-test)
## Code only works if you run the section above this one first
prop.test <- pairwiseTest(formula = cbind(Favorable, NotFavorable) ~ group, data = df.props,method="Prop.test")
summary(prop.test, p.adjust.method = "fdr")





