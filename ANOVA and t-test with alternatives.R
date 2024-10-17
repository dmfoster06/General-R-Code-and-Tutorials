# LIBRARIES
library(readxl)
library(openxlsx)
library(dplyr)
library(epiDisplay) #tabpct function
library(sjmisc) #frq function
library(qacBase) #qstats function
library(emmeans) #multiple comparisons
library(lsr) #cohens d
library(sandwich) #robust standard errors
library(stats) #wilcoxon test
library(car) #Levene Test

# DESCRIPTIVES AND DIAGNOSTICS
## Descriptives
tabpct(df$x,df$group,graph=FALSE,percent = "col") #Frequency Table
qstats(df, x, group) #Group N, Mean, SD (at least k+1 per group)

## Homogeneity of Variances - ANOVA or t-test
mod <- aov(x ~ group, data = df)

leveneTest(mod) #Levene Test (ns = homogeneous)

tvars <- with(df, tapply(x,group,var, na.rm = TRUE)) #Ratio of largest v smallest group variance (should be no bigger than ~1.5)
max(tvars)/min(tvars)

## Normality of Residuals
qqPlot(mod) #Fitted vs residual plot (should follow diagonal)
hist(mod$residuals) # Histogram (should be approx normal)
shapiro.test(mod$residuals) #Shapiro Wilk test (ns = normal)

# DIFFERENCES BETWEEN 3+ GROUPS - Normal, Homoscedastic or Heteroscedastic
Anova(mod, type=3) #homoscedastic
Anova(mod, type=3, white.adjust="hc0") #heterscedastity-corrected cov matrix / robust SE

qstats(df, x, group) #Group N, Mean, SD 
mod.emmeans <- emmeans(mod, pairwise ~ group, adjust="fdr", infer = c(FALSE, TRUE))
mod.emmeans$contrasts #pairwise contrasts
eff_size(pairs(mod.emmeans), sigma = sigma(mod), edf = df.residual(mod), method = "identity") #cohens d for contrasts
plot(mod.emmeans, comparisons = FALSE) #plotted point estimates and CIs

# DIFFERENCES BETWEEN 3+ GROUPS - Non-normal residuals
kruskal.test(x ~ group, data = df)
dunn_test(df, x ~ group, p.adjust.method = "fdr", detailed = FALSE) #contrasts

# DIFFERENCE BETWEEN 2 GROUPS - Normal, homoscedastic or heteroscedastic
t.test(x ~ group, data = df, var.equal=TRUE) #homoscedastic
cohensD(x ~ group, data = df, method ="pooled")

t.test(x ~ group, data = df, var.equal=FALSE) #heteroscedastic
cohensD(x ~ group, data = df, method ="unequal")

# DIFFERENCE BETWEEN 2 GROUPS - Non-normal residuals
wilcox.test(df$x ~ df$group, mu=0,alt="two.sided",conf.int=T,conf.level=0.95,exact=F,correct=F)











