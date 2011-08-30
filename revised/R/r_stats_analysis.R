
#An example analysis of a fairly simple dataset.
#The data is from the 2010 CPS March supplement. 
#(Also called the Annual Social and Economic Supplement.)
#The goal is examining the difference in hourly pay
#between males and females. The column A_ERNLWT is 
#survey weights, PTFT is part-time/full-time status, 
#educ is education level, ind is what industry the
#person works in and occ is what occupation they work in. 
#A full description of most of the variables sometimes with 
#slightly different names, can be found at
#http://www.census.gov/apsd/techdoc/cps/cpsmar10.pdf


dat<-read.table("marchCPS_2010.txt", header=T)

dim(dat)


####Actual analysis code

#Remove everyone that doesn't have an houlry wage data

indhw<-which(!is.na(dat$hrwage))
hrdat<-dat[indhw,]

#Remove several people who just didn't fit the models (using the
#standard model checking techniques below) 

hrdat<-hrdat[-c(408,516,853,1198),]

indf<-which(hrdat$sex==2)
indm<-which(hrdat$sex==1)

#Make the factors actually factors
#The columns numbers correspond to GEDIV, educ, marstat, sex, occ,
#ind, race, hispanic, disabled, and PTFT

tmp<-rep("female", length(hrdat$sex))
tmp[hrdat$sex==1]="male"
hrdat$sex <- factor(tmp)

factor_indices <- c(3,5,6,17,18,20,21,23,25)
for(i in factor_indices){
    hrdat[,i] <- factor(hrdat[,i])
}

#With each of these models, typically do some 
#commands to look more at the models, like summary(),
#, anova for the model on its own or betwen two models to see
#how much additional explantory power you get with the added
#variables, and plots to look at residuals, qqplot, and hist of residuals

model1<-lm(log(hrwage)~sex, data = hrdat, weights = A_ERNLWT)

model2<-lm(log(hrwage)~sex+educ+age+PTFT, data=hrdat, weights=A_ERNLWT)

model2.5<-lm(log(hrwage)~sex+educ+age+I(age^2)+PTFT+ind+occ,
                data=hrdat, weights=A_ERNLWT)


pdf('model_diagnostics.pdf', width=16,height=7)
par(mfrow=c(1,3))
hist(model2.5$res, breaks=40, main="Histogram of Residuals", 
        xlab="Residuals", cex.main=2, cex.lab=1.5)
plot(model2.5$fitted, model2.5$res, main="Residual vs. Fitted Values", 
        xlab="Fitted Values", ylab="Residuals",cex.main=2, cex.lab=1.5)
lines(lowess(model2.5$fitted, model2.5$res), col='red', lwd=3)
abline(h=0, col='green', lwd=3)
qqnorm(model2.5$res,cex.main=2, cex.lab=1.5)
qqline(as.vector(model2.5$res), col='red')
dev.off()


indic<-(hrdat$PMHRUSLT>0)


#Used mean replacement for all the people whose hours varied,
#where the mean of 22 was the mean over all part time workers 
#w/o missing data and using their weights
#Done just as a sanity check on the above models

model3<-lm(log(hrwage)~ sex+educ+PTFT+age +I(age^2)+marstat+GEDIV+race+ 
    hispanic+I(PMHRUSLT*indic+22*(1-indic))+disabled, 
    data = hrdat, weights = A_ERNLWT)



#These models bin the works by age group, <=30, 31-40, 41-50, >50.
#This was done to see what the difference betweens males and 
#females was in each bin. This was done as a sanity check for the
#later exploratory analysis that fit lowess curves across age to 
#males and females seperately. I wanted to make sure that the trends 
#observed were real. (Where the gap was smaller for younger workers,
#expanded for middle age workers, and then contracted again.)

model7.1<-lm(log(hrwage)~sex+educ+PTFT+age+I(age^2), 
        data = subset(hrdat, age<=30), weights=A_ERNLWT )

model7.2<-lm(log(hrwage)~sex+educ+PTFT+age+I(age^2), 
        data = subset(hrdat, age>30 & age<=40), weights=A_ERNLWT )

model7.3<-lm(log(hrwage)~sex+educ+PTFT+age+I(age^2), 
        data = subset(hrdat, age>40 & age<=50), weights=A_ERNLWT )

model7.4<-lm(log(hrwage)~sex+educ+PTFT+age+I(age^2), 
        data = subset(hrdat, age>50), weights=A_ERNLWT )


####


#This analysis switches gears and focuses on occupation. 
#The males and females are broken down by occupation and the
#weighted mean of their hourly wage are compared.
#(The weights used are again the survey weights.)
#The matrix wocc stores all of that, and 
#Use xtable to format tables in latex

sum_stat<-function(x){
	tmp1 <- sum(x$A_ERNLWT)
	tmp2 <- weighted.mean(x$hrwage, x$A_ERNLWT)
	c(survey.wt=tmp1, avr.hr.wage=tmp2)
}

split.by <-list(hrdat$sex, hrdat$occ)
wocc<-split(hrdat, split.by)

wocc <-lapply(wocc, sum_stat)
wocc<-do.call(rbind, wocc)

wocc<-split(data.frame(wocc), rep(levels(hrdat$sex), 9))

#Plotting hourly wage versus age and fitting a lowess curve to it. 
#Done sepereately for both males and females. Just to get a feel for 
#what their salary trajectories look like. Though it's not totally
#clear from the picture, the gap is smaller proportionally at younger 
#ages than at older ones. (This can be seen as above by binning the ages,
#or via a separate analysis by fitting an interaction term between
#sex and age.)

plot(lowess(hrdat$age[indm], (hrdat$hrwage)[indm]), col='blue', ylim=c(7,25), 
    xlab="Age", ylab = "Hourly Wage", main="Hourly Wage vs. Age", type='l',
    cex.main=2, cex.lab=1.5, lwd=2)
lines(lowess(hrdat$age[indf], (hrdat$hrwage)[indf]), col='deeppink')
legend('topleft', c("Males", "Females"), cex=1.25, col=c("blue", "deeppink"),
    lty=1, lwd=2)





