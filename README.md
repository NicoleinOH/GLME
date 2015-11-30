# GLME
Code used to run batch GLME models in R

#uploading required libraries 
library(bbmle)   
library(vegan)
library(ggplot2)
library(reshape2)
library(AICcmodavg)
library(MASS)
library(VGAM)
library(nlme)

#If you don't have the package, an error will come up telling you there is no package. You'll need to install that program then load the library.

#uploading file from location
Data2015=read.csv(file.choose())

#checking that everything is okay
head(Data2015)
names(Data2015)
dim(Data2015)


### Globular ###

#Create empty model spaces using code list()
model = list()

#create models using double brackets to fill in the emptpy spaces

model[[1]] = lme(Acari~FaunaRichness, random=~1|Site/Quadrat, data=Data2015, na.action=na.omit)
model[[2]] = lme(Acari~FloraRichness, random=~1|Site/Quadrat, data=Data2015, na.action=na.omit)
model[[3]] = lme(Acari~FaunaRichness+FloraRichness, random=~1|Site/Quadrat, data=Data2015, na.action=na.omit)
model[[4]] = lme(Acari~PC1GC, random=~1|Site/Quadrat, data=Data2015, na.action=na.omit)
model[[5]] = lme(Acari~PC2GC, random=~1|Site/Quadrat, data=Data2015, na.action=na.omit)
model[[6]] = lme(Acari~PC1GC+PC2GC, random=~1|Site/Quadrat, data=Data2015, na.action=na.omit)
model[[7]] = lme(Acari~PC1Dom, random=~1|Site/Quadrat, data=Data2015, na.action=na.omit)
model[[8]] = lme(Acari~PC2Dom, random=~1|Site/Quadrat, data=Data2015, na.action=na.omit)
model[[9]] = lme(Acari~PC1Dom+PC2Dom, random=~1|Site/Quadrat, data=Data2015, na.action=na.omit)
model[[10]] = lme(Acari~PC1Soil, random=~1|Site/Quadrat, data=Data2015, na.action=na.omit)
model[[11]] = lme(Acari~PC2Soil, random=~1|Site/Quadrat, data=Data2015, na.action=na.omit)
model[[12]] = lme(Acari~PC1Soil+PC2Soil, random=~1|Site/Quadrat, data=Data2015, na.action=na.omit)
model[[13]] = lme(Acari~1, random=~1|Site/Quadrat, data=Data2015, na.action=na.omit)

#run AICc creating the names in 'modnames' in the order that models were created
aictab(cand.set = model, second.ord = T, sort = T, modnames = c("Acari_FARichness", "Acari_FLRichness", "Acari_Richness", "Acari_PC1GC", "Acari_PC2_GC", "Acari_PCGC", "Acari_PC1Dom", "Acari_PC2Dom", "Acari_PCDom", "Acari_PC1Soil", "Acari_PC2_Soil", "Acari_PCSoil", "Acari_1"))                                                  
  
##Output From Above##
Model selection based on AICc :

                 K    AICc Delta_AICc AICcWt Cum.Wt   Res.LL
Acari_Richness   6 2273.76       0.00   0.57   0.57 -1130.70
Acari_FARichness 5 2274.45       0.69   0.40   0.98 -1132.09
Acari_PCSoil     6 2281.16       7.40   0.01   0.99 -1134.40
Acari_PCGC       6 2282.34       8.59   0.01   1.00 -1134.99
Acari_PC1Soil    5 2287.70      13.94   0.00   1.00 -1138.72
Acari_PC1GC      5 2288.01      14.26   0.00   1.00 -1138.88
Acari_PC2_GC     5 2288.95      15.19   0.00   1.00 -1139.34
Acari_PC2_Soil   5 2289.66      15.90   0.00   1.00 -1139.70
Acari_PCDom      6 2291.11      17.35   0.00   1.00 -1139.37
Acari_PC2Dom     5 2292.30      18.54   0.00   1.00 -1141.02
Acari_PC1Dom     5 2293.58      19.83   0.00   1.00 -1141.66
Acari_FLRichness 5 2294.54      20.78   0.00   1.00 -1142.14
Acari_1          4 2294.90      21.15   0.00   1.00 -1143.37

Warning message:
In aictab.AIClme(cand.set = model, second.ord = T, sort = T, modnames = c("Acari_FARichness",  :
  
Model selection for fixed effects is only appropriate with method=ML:
REML (default) should only be used to select random effects for a constant set of fixed effects

##Ignore the warning. The best fit model is the one with the lowest AICc score at the top of the table, Acari Richness. Anything with a Delta AICc score within 2 of your best fit model is a competing model. Run your best fit and competing model separately and call summary to get the individual properties of each model. 

AcariRichness = lme(Acari~FaunaRichness, random=~1|Site/Quadrat, data=Data2015, na.action=na.omit)
summary(AcariRichness)

##Output - Best Fit Model##
Linear mixed-effects model fit by REML
 Data: Data2015 
       AIC      BIC    logLik
  2273.394 2294.177 -1130.697

Random effects:
 Formula: ~1 | Site
        (Intercept)
StdDev:    10.50892

 Formula: ~1 | Quadrat %in% Site
        (Intercept) Residual
StdDev:    9.816277 24.91654

Fixed effects: Acari ~ FaunaRichness + FloraRichness 
                   Value Std.Error  DF   t-value p-value
(Intercept)   -15.222675  8.055987 110 -1.889610  0.0614
FaunaRichness   4.101448  0.868627  95  4.721757  0.0000
FloraRichness   0.882285  0.640107  95  1.378340  0.1713
 Correlation: 
              (Intr) FnRchn
FaunaRichness -0.648       
FloraRichness -0706  0.025

Standardized Within-Group Residuals:
        Min          Q1         Med          Q3         Max 
-1.44042728 -0.45348196 -0.18626413  0.09695648  5.14013757 

Number of Observations: 239
Number of Groups: 
             Site Quadrat %in% Site 
               32               142 

##Report the p-value and use the t-value to determine whether to put the p-value in parentheses (if t-value is negative).
Example:

Model Forumula												AICc	Delta AICc	AIC wt	p- value
lme(Acari~FaunaRichness+FloraRichness, random=~1|Site/Quadrat, data=Data2015, na.action=na.omit)	2273.76		0	0.57	
Fauna Richness																<0.001	#This would be bolded
Flora Richness																0.17


