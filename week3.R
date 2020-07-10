## 1.In a population of interest, a sample of 9 men yielded a sample average brain 
## volume of 1,100cc and a standard deviation of 30cc. What is a 95% Student's 
## T confidence interval for the mean brain volume in this new population?
1100 + c(-1,1) * qt(.975, 8) * 30/sqrt(9)

## 2. A diet pill is given to 9 subjects over six weeks. The average difference 
## in weight (follow up - baseline) is -2 pounds. What would the standard 
## deviation of the difference in weight have to be for the upper endpoint of 
## the 95% T confidence interval to touch 0?
n <- 9
mean <- -2
qt <- qt(.975,8)
s <- 2/qt*sqrt(9)
s

## 4. In a study of emergency room waiting times, investigators consider a new 
## and the standard triage systems. To test the systems, administrators selected 
## 20 nights and randomly assigned the new triage system to be used on 10 nights
## and the standard system on the remaining 10 nights. They calculated the 
## nightly median waiting time (MWT) to see a physician. The average MWT for the
## new system was 3 hours with a variance of 0.60 while the average MWT for the 
## old system was 5 hours with a variance of 0.68. Consider the 95% confidence 
## interval estimate for the differences of the mean MWT associated with the new
## system. Assume a constant variance. What is the interval? Subtract in this
## order (New System - Old System).
munew <- 3
varnew <- 0.6
nnew <- 10
muold <- 5
varold <- 0.68
nold <- 10
df <- nnew+nold-2
sp <- sqrt(((nnew-1)*varnew+(nold-1)*varold)/(nnew+nold-2))
munew - muold +c(-1,1)*qt(0.975, df)*sp*sqrt(1/nnew+1/nold)


## 6. To further test the hospital triage system, administrators selected 200 
## nights and randomly assigned a new triage system to be used on 100 nights 
## and a standard system on the remaining 100 nights. They calculated the 
## nightly median waiting time (MWT) to see a physician. The average MWT for the
## new system was 4 hours with a standard deviation of 0.5 hours while the 
## average MWT for the old system was 6 hours with a standard deviation of 2 
## hours. Consider the hypothesis of a decrease in the mean MWT associated with 
## the new treatment.
meannew <- 4
sdnew <- 0.5
nnew1 <- 100
meanold <- 6
sdold <- 2
nold1 <- 100
df1 <- nnew1 + nold1 -2
sp1 <- sqrt(((nnew1-1)*sdnew^2+(nold1-1)*sdold^2)/(nnew1+nold1-2))
meanold-meannew + c(-1,1)*qt(.975,df1)*sp1*sqrt(1/nnew1+1/nold1)

## 7. Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill
## and a placebo. Subjects’ body mass indices (BMIs) were measured at a baseline
## and again after having received the treatment or placebo for four weeks. The 
## average difference from follow-up to the baseline (followup - baseline) was 
## −3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The 
## corresponding standard deviations of the differences was 1.5 kg/m2 for the 
## treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI
## over the four week period appear to differ between the treated and placebo
## groups? Assuming normality of the underlying data and a common population 
## variance, calculate the relevant *90%* t confidence interval. Subtract in the 
## order of (Treated - Placebo) with the smaller (more negative) number first.
treat <- -3
sdtreat <- 1.5
ntreat <- 9
placebo <- 1
sdplacebo <- 1.8
nplacebo <- 9
df3 <- ntreat+nplacebo-2
sp2 <- sqrt(((ntreat-1)*sdtreat^2+(nplacebo-1)*sdplacebo^2)/(ntreat+nplacebo-2))
treat - placebo + c(-1,1)*qt(.95, df3)*sp2* sqrt(1/ntreat+1/nplacebo)
