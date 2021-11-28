require("here")
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

#pond: the ID of the pond
#success: The number of years in which successful reproduction occurred at the pond
#years: The total number of years that the pond was observed.
#cat.rate: the ratio of successes to total observation years.

#binomial test for this, specifying the number of successes (33) and the total sample size (61), as follows
#Note, we had to first compute the number of successes and the total number of years pooled across all 14 ponds.
n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(n_success, n_years)

#In this scenario, we expect successful reproduction in approximately 5 of every 7 tears.
#In addition, note again that the default test is a two-sided alternative
binom.test(n_success, n_years, p = 5/7) 

#one-sided alternative hypothesis
binom.test(
  n_success,
  n_years,
  p = 5/7,
  alternative='less')

# we need to test whether the sample variances are significantly different
#The simplest test is called Fisher’s F test, based on the F-statistic.
#The F-statistic represents the ratio between two variances.
#It is based on the idea that if the variances of the two samples are the same, then the ratio of the variances will be 1.
#Note that Fisher’s F test for unequal variances assumes that the data are normally distributed.

veg = read.csv(here("data", "vegdata.csv"))
head(veg)

boxplot(pine ~ treatment, data = veg)

var.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

#F-tests Assumes Normality
shapiro.test(veg$pine[veg$treatment=="control"])
shapiro.test(veg$pine[veg$treatment=="clipped"])
#Note, because the Shapiro-Wilk test is a one-sample test, we had to select the records for each treatment and conduct separate tests.\

#Non-parametric Variance Test
#If the results indicate that the data are non-normal, then we should use a non-parametric test of homogeneity of variances, such as the Fligner-Killeen test, as follows
fligner.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

#k-sample problems; i.e., when there are more than two groups.
#the ksample parametric test is called Bartlett’s test, which we can use to test for homogeneity of variances among all four treatment levels as follows:

bartlett.test(pine ~ treatment, data=veg)  
#Note that Bartlett’s test, like Fisher’s F test is highly sensitive to non-normality and the presence of outliers

#The non-parametric alternative test which is largely preferred by many statisticians is called the Fligner-Killeen test. We used it to test two variances above, 
##but it can test n variances as well:
fligner.test(pine ~ treatment, data = veg)

#The Student’s t test is appropriate when the samples are independent, the variances constant, and the errors normally distributed.
t.test(pine~treatment,data=veg,subset=treatment %in% c('control','clipped'), conf.int=TRUE)

#Because we asked for a confidence interval (conf.int=TRUE), the output includes a 95% (by default) confidence interval on the difference between sample means

#Wilcox test
wilcox.test(pine~treatment,data=veg,subset=treatment %in% c('control','clipped'), conf.int=TRUE)

#Tests for paired samples
#First, we need to create separate vectors for the “control” observations and “clipped” observations because,
##the t.test() doesn’t accept formula’s (as above) for the paired option, as follows:
control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']

t.test(control, clipped, paired=TRUE)

wilcox.test(control, clipped, paired=TRUE)

#Marbled Salamander
#dist.class = distance class, based on 100 m intervals;
#disp.rate.ftb = standardized dispersal rate for first-time breeders, which can be interpreted as a relative dispersal probability.
#disp.rate.eb = standardized dispersal rate for experienced breeders, which can be interpreted as a relative dispersal probability

disp = read.csv(here("data", "dispersal.csv"))
disp

plot(disp$disp.rate.ftb, disp$disp.rate.eb)

#we can test the significance of the correlation using the cor.test() function, as follows:
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')
#Note, we needed to specify the use=’complete.obs’ argument to address the missing values for the 700 m distance class 
##(for which there are no ponds in this particular distance interval).

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

#Comparing two distributions
#The Kolmogorov-Smirnov test works on empirical cumulative distribution functions (ecdf).
##Recall that these give the probability that a randomly selected value of X is less than or equal to x.
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)

#Now let’s add the ecdf for the adult dispersal rate, but change the line type (lty) so that we can distinguish it from the ecdf for the juvenile dispersal rate
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)

#Are these two distributions different? We can use the Kolmogorov-Smirnov test (ks.test) to determine if they differ significantly in any aspect
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

#Comparing two or more proportions
##Sex-linked killing

#This is a simple binomial proportions test, which we can easily do in R by specifying two vectors:
##1. the number of mortalities for females and males c(4,16)
##2. the total number of female and male candidates: c(40,250)
prop.test(c(4,16),c(40,250))
#A significant p-value indicates that the proportions are different between samples; i.e., that the proportions observed were unlikely to have been drawn from the same underlying population
#Conversely, an insignificant p-value means that there is insufficient evidence to reject the null hypothesis and we would conclude that the proportions are not statistically different.

#Contingency: Chi-square test
#We would like to know whether the observed counts differ from what we would expect if presence/absence was independent of stand age
owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq.test(owls)

#Fisher’s Exact test
##In this case, an alternative test called Fisher’s exact test is more appropriate
fisher.test(owls)

#Bird habitat data
birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(birds, hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(birdhab$s.edge, birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]

owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq.test(owls)
?chisq.test

# Q1 
chisq.test(br_creeper_table)

#Q3-5
require(palmerpenguins)
fit_fl_sp = 
  lm(
    formula = flipper_length_mm ~ species,
    data = penguins)

#Q3 Show the R-code you can use to create a model fit (call it fit_species) of penguin body mass as predicted by penguin species.
fit_species = 
  lm(
    formula = penguins$body_mass_g ~ penguins$species)

#Q4 Show the R-code you can use to create a model fit (call it fit_sex) of penguin body mass as predicted by sex.
fit_sex = 
  lm(
    formula = penguins$body_mass_g ~ penguins$sex)

#Q5 Show the R-code you can use to create a model fit (call it fit_both) of penguin body mass as predicted by species and sex.
fit_both = 
  lm(
    formula = penguins$body_mass_g ~ penguins$species * penguins$sex)

#Q6 Include a conditional boxplot corresponding to your fit_species model.
install.packages("RColorBrewer")
library(RColorBrewer)

boxplot(formula(fit_species),
        main = "Penguin Body Mass(g) by Species",
        xlab = "Species",
        ylab = "Body Mass in (g)",
        col = "darkorange2")

#Q7:
boxplot(formula(fit_sex),
        main = "Penguin Body Mass(g) by Species",
        xlab = "Sex",
        ylab = "Body Mass in (g)",
        col = "goldenrod2")
# Q8:
boxplot(formula(fit_both),
        main = "Penguin Body Mass(g) by Species and Sex",
        xlab = "",
        ylab = "Body Mass (g)",
        names = c("Female \nAdelie", "Female \nChinstrap", "Female \nGentoo", 
                  "Male \nAdelie", "Male \nChinstrap", "Male \nGentoo"), las = 2,
        col = "darkorange2")

# Q10:
bartlett.test(body_mass_g ~ species, data = penguins)

# Q11:
bartlett.test(body_mass_g ~ sex, data = penguins)

# Q12:

# Q13: What was the p-value from the Bartlett test of homogeneity for observations grouped by both factors?
species_groups = aggregate(
  body_mass_g ~ species,
  data = penguins,
  FUN = c)
str(species_groups)

bartlett.test(species_groups$body_mass_g)

sex_groups = aggregate(
  body_mass_g ~ sex,
  data = penguins,
  FUN = c)
str(sex_groups)

bartlett.test(sex_groups$body_mass_g)

#Explaining the above script
#First, we read in the bird and habitat data and merged them into a single file based on the common fields.
#Then we used the table() function to compute the cross-classified counts.
#What code converted the Brown Creeper counts to presence/absence?
#The next step simply switched the order of the columns so that the present counts were in the first column and the absent counts were in the second column, as this is the expected order in some functions (e.g, prop.test())



fmla = body_mass_g ~ sex * species
fmla = body_mass_g ~ species *sex
bxplt + boxplot(
  fmla,
  data = penguins, plot = FALSE)

str(bxplt)

boxplot
)

bxplt = boxplot(
  body_mass_g ~ sex * species,
  data = penguins, plot = FALSE)

str(bxplt)

bxplt$names
gsub("\\.", "\n", bxplt$names)

)
boxplot(
  body_mass_g ~ sex * species,
  
)
