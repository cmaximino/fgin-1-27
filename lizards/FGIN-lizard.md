---
title: "R Notebook for effects of FGIN-1-27 on lizard defensive behavior (LaNeC)"
author:
- Caio Maximino^[Universidade Federal do Sul e Sudeste do Pará]
- Monica Gomes Lima^[Universidade do Estado do Pará]
output:
  github_document 
subtitle: From project "Participation of TSPO on anxiety and fear
tags:
- anxiety
- fear
- wall lizard
- FGIN-1-27
abstract: |
  Data packages for the research project "Participation of TSPO on anxiety and fear". Data are produced by members from Laboratório de Neurociências e Comportamento "Frederico Guilherme Graeff", affiliated to Universidade Federal do Sul e Sudeste do Pará and Universidade do Estado do Pará. The package includes primary data for behavioral experiments on the effects of acute FGIN-1-27 on wall lizard behavior in the defense test battery, as well scripts for statistical analysis of these data.
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the data analysis of the research project "Participation of TSPO on anxiety and fear".

Data packages for the research project "Participation of TSPO on anxiety and fear". Data are produced by members from Laboratório de Neurociências e Comportamento "Frederico Guilherme Graeff", affiliated to Universidade Federal do Sul e Sudeste do Pará and Universidade do Estado do Pará. The package includes primary data for behavioral experiments on the effects of acute FGIN-1-27 on wall lizard behavior in the defense test battery, as well scripts for statistical analysis of these data.

When you execute code within the notebook, the results appear beneath the code. 

* Load needed libraries:
```{r}
if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
}
if(!require(RCurl)){
    install.packages("RCurl")
    library(RCurl)
}
if(!require(plyr)){
    install.packages("plyr")
    library(plyr)
}
```

* Load data
```{r}
x1 <- getURL("https://raw.githubusercontent.com/lanec-unifesspa/fgin-1-27/master/FGIN_lizards.csv")
exp1 <- read.csv(text = x1)
exp1$Dose <- as.factor(exp1$Dose)
exp1$Drug <- as.factor(exp1$Drug)
View(exp1)
```

* Run two-way ANOVA

1) Tonic immobility

```{r}
anova_TI <- aov(TI.Dur ~ Drug * Dose, exp1) ##use aov(object) to test the omnibus hypothesis: Are main effects or interaction effects present in the independent variables?
summary(anova_TI)
TukeyHSD(anova_TI) #Tukey multiple comparisons of means
```

2) Circling

```{r}
anova_Cir <- aov(Circling ~ Drug * Dose, exp1) ##use aov(object) to test the omnibus hypothesis: Are main effects or interaction effects present in the independent variables?
summary(anova_Cir)
TukeyHSD(anova_RA) #Tukey multiple comparisons of means
```

3) Ventilatory frequency

```{r}
anova_Ven <- aov(Ventilatory ~ Drug * Dose, exp1) ##use aov(object) to test the omnibus hypothesis: Are main effects or interaction effects present in the independent variables?
summary(anova_Ven)
TukeyHSD(anova_RA) #Tukey multiple comparisons of means
```

4) Tongue-flicking

```{r}
anova_TF <- aov(TF ~ Drug * Dose, exp1) ##use aov(object) to test the omnibus hypothesis: Are main effects or interaction effects present in the independent variables?
summary(anova_TF)
TukeyHSD(anova_RA) #Tukey multiple comparisons of means
```

5) Thigmotaxis

```{r}
anova_Th <- aov(Thigmotaxis ~ Drug * Dose, exp1) ##use aov(object) to test the omnibus hypothesis: Are main effects or interaction effects present in the independent variables?
TukeyHSD(anova_RA) #Tukey multiple comparisons of means
```

6) Freezing

```{r}
anova_Fr <- aov(Freezing ~ Drug * Dose, exp1) ##use aov(object) to test the omnibus hypothesis: Are main effects or interaction effects present in the independent variables?
summary(anova_Fr)
TukeyHSD(anova_RA) #Tukey multiple comparisons of means
```

7) Locomotor behavior

```{r}
anova_Loc <- aov(Locomotor ~ Drug * Dose, exp1) ##use aov(object) to test the omnibus hypothesis: Are main effects or interaction effects present in the independent variables?
summary(anova_Loc)
TukeyHSD(anova_RA) #Tukey multiple comparisons of means
```

Produce figures on ggplot2
```{r}
plotTI <- ddply(exp1, .(Dose, Drug), summarise, val = mean(TI.Dur))
ggplot(exp1, aes(x = factor(Dose), y = TI.Dur, colour = Drug)) + geom_boxplot(outlier.shape = NA) + geom_point(data = plotTI, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_line(data = plotTI, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_jitter(position = "dodge", alpha = 0.3) + labs(x = "Dose", y = "Tonic immobility duration (s)", color = "Drug") + theme_linedraw()

plotCir <- ddply(exp1, .(Dose, Drug), summarise, val = mean(Circling))
ggplot(exp1, aes(x = factor(Dose), y = Circling, colour = Drug)) + geom_boxplot(outlier.shape = NA) + geom_point(data = plotCir, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_line(data = plotCir, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_jitter(position = "dodge", alpha = 0.3) + labs(x = "Dose", y = "Circling duration (s)", color = "Drug") + theme_linedraw()

plotVen <- ddply(exp1, .(Dose, Drug), summarise, val = mean(Ventilatory))
ggplot(exp1, aes(x = factor(Dose), y = Ventilatory, colour = Drug)) + geom_boxplot(outlier.shape = NA) + geom_point(data = plotVen, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_line(data = plotVen, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_jitter(position = "dodge", alpha = 0.3) + labs(x = "Dose", y = "Ventilatory frequency (N)", color = "Drug") + theme_linedraw()

plotTF <- ddply(exp1, .(Dose, Drug), summarise, val = mean(TF))
ggplot(exp1, aes(x = factor(Dose), y = TF, colour = Drug)) + geom_boxplot(outlier.shape = NA) + geom_point(data = plotTF, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_line(data = plotTF, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_jitter(position = "dodge", alpha = 0.3) + labs(x = "Dose", y = "Tongue-flicking (N)", color = "Drug") + theme_linedraw()

plotTh <- ddply(exp1, .(Dose, Drug), summarise, val = mean(Thigmotaxis))
ggplot(exp1, aes(x = factor(Dose), y = Thigmotaxis, colour = Drug)) + geom_boxplot(outlier.shape = NA) + geom_point(data = plotTh, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_line(data = plotTh, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_jitter(position = "dodge", alpha = 0.3) + labs(x = "Dose", y = "Thigmotaxis (s)", color = "Drug") + theme_linedraw()

plotFr <- ddply(exp1, .(Dose, Drug), summarise, val = mean(Freezing))
ggplot(exp1, aes(x = factor(Dose), y = Freezing, colour = Drug)) + geom_boxplot(outlier.shape = NA) + geom_point(data = plotFr, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_line(data = plotFr, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_jitter(position = "dodge", alpha = 0.3) + labs(x = "Dose", y = "Freezing (s)", color = "Drug") + theme_linedraw()

plotLoc <- ddply(exp1, .(Dose, Drug), summarise, val = mean(Locomotor))
ggplot(exp1, aes(x = factor(Dose), y = Locomotor, colour = Drug)) + geom_boxplot(outlier.shape = NA) + geom_point(data = plotLoc, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_line(data = plotLoc, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_jitter(position = "dodge", alpha = 0.3) + labs(x = "Dose", y = "Total locomotion (N)", color = "Drug") + theme_linedraw()
```
