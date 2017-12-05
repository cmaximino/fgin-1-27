R Notebook for effects of FGIN-1-27 on zebrafish anxiety (LaNeC)
================
Caio Maximino[1]

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the data analysis of the research project "Participation of TSPO on anxiety and fear".

Data packages for the research project "Participation of TSPO on anxiety and fear". Data are produced by members from Laboratório de Neurociências e Comportamento "Frederico Guilherme Graeff", affiliated to Universidade Federal do Sul e Sudeste do Pará and Universidade do Estado do Pará. The package includes primary data for behavioral experiments on the effects of acute FGIN-1-27 on zebrafish anxiety-like behavior in the light/dark test, as well as scripts for statistical analysis of these data.

When you execute code within the notebook, the results appear beneath the code.

-   Load needed libraries:

    ``` {r}
    if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
    }
    if(!require(RCurl)){
    install.packages("RCurl")
    library(RCurl)
    }
    ```

-   Load data

    ``` {r}
    x1 <- getURL("https://raw.githubusercontent.com/lanec-unifesspa/fgin-1-27/master/FGIN_zebrafish.csv")
    exp1 <- read.csv(text = x1)
    exp1$Dose <- as.factor(exp1$Dose)
    exp1$Drug <- as.factor(exp1$Drug)
    View(exp1)
    ```

-   Run two-way ANOVA

1.  Time on white

``` {r}
anova_TW <- aov(TW ~ Drug * Dose, exp1) ##use aov(object) to test the omnibus hypothesis: Are main effects or interaction effects present in the independent variables?
TukeyHSD(anova_TW) #Tukey multiple comparisons of means
```

1.  Risk assessment

``` {r}
anova_RA <- aov(Risk.assessment ~ Drug * Dose, exp1) ##use aov(object) to test the omnibus hypothesis: Are main effects or interaction effects present in the independent variables?
TukeyHSD(anova_RA) #Tukey multiple comparisons of means
```

1.  Thigmotaxis

``` {r}
anova_Th <- aov(Thigmotaxis ~ Drug * Dose, exp1) ##use aov(object) to test the omnibus hypothesis: Are main effects or interaction effects present in the independent variables?
TukeyHSD(anova_RA) #Tukey multiple comparisons of means
```

1.  Freezing

``` {r}
anova_Fr <- aov(Freezing ~ Drug * Dose, exp1) ##use aov(object) to test the omnibus hypothesis: Are main effects or interaction effects present in the independent variables?
TukeyHSD(anova_RA) #Tukey multiple comparisons of means
```

1.  Erratic swimming

``` {r}
anova_ES <- aov(Erratic ~ Drug * Dose, exp1) ##use aov(object) to test the omnibus hypothesis: Are main effects or interaction effects present in the independent variables?
TukeyHSD(anova_RA) #Tukey multiple comparisons of means
```

1.  Transitions to white

``` {r}
anova_Tr <- aov(Crossings ~ Drug * Dose, exp1) ##use aov(object) to test the omnibus hypothesis: Are main effects or interaction effects present in the independent variables?
TukeyHSD(anova_RA) #Tukey multiple comparisons of means
```

Produce figures on ggplot2

``` {r}
plotTW <- ddply(exp1, .(Dose, Drug), summarise, val = mean(TW))
ggplot(exp1, aes(x = factor(Dose), y = TW, colour = Drug)) + geom_boxplot(outlier.shape = NA) + geom_point(data = plotTW, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_line(data = plotTW, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_jitter(position = "dodge", alpha = 0.3) + labs(x = "Dose", y = "Time on white (s)", color = "Drug") + theme_linedraw()

plotRA <- ddply(exp1, .(Dose, Drug), summarise, val = mean(Risk.assessment))
ggplot(exp1, aes(x = factor(Dose), y = Risk.assessment, colour = Drug)) + geom_boxplot(outlier.shape = NA) + geom_point(data = plotRA, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_line(data = plotRA, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_jitter(position = "dodge", alpha = 0.3) + labs(x = "Dose", y = "Risk assessment (N)", color = "Drug") + theme_linedraw()

plotTh <- ddply(exp1, .(Dose, Drug), summarise, val = mean(Thigmotaxis))
ggplot(exp1, aes(x = factor(Dose), y = Thigmotaxis, colour = Drug)) + geom_boxplot(outlier.shape = NA) + geom_point(data = plotTh, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_line(data = plotTh, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_jitter(position = "dodge", alpha = 0.3) + labs(x = "Dose", y = "Thigmotaxis (s)", color = "Drug") + theme_linedraw()

plotFr <- ddply(exp1, .(Dose, Drug), summarise, val = mean(Freezing))
ggplot(exp1, aes(x = factor(Dose), y = Freezing, colour = Drug)) + geom_boxplot(outlier.shape = NA) + geom_point(data = plotFr, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_line(data = plotFr, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_jitter(position = "dodge", alpha = 0.3) + labs(x = "Dose", y = "Freezing (s)", color = "Drug") + theme_linedraw()

plotES <- ddply(exp1, .(Dose, Drug), summarise, val = mean(Erratic))
ggplot(exp1, aes(x = factor(Dose), y = Erratic, colour = Drug)) + geom_boxplot(outlier.shape = NA) + geom_point(data = plotES, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_line(data = plotES, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_jitter(position = "dodge", alpha = 0.3) + labs(x = "Dose", y = "Erratic swimming (N)", color = "Drug") + theme_linedraw()

plotTr <- ddply(exp1, .(Dose, Drug), summarise, val = mean(Crossings))
ggplot(exp1, aes(x = factor(Dose), y = Crossings, colour = Drug)) + geom_boxplot(outlier.shape = NA) + geom_point(data = plotTr, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_line(data = plotTr, aes(y = val, group = Drug), position = position_dodge(width = 0.75)) + geom_jitter(position = "dodge", alpha = 0.3) + labs(x = "Dose", y = "Transitions to white (N)", color = "Drug") + theme_linedraw()
```

[1] Universidade Federal do Sul e Sudeste do Pará

[2] Universidade do Estado do Pará
