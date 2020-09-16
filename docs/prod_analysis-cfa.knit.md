---
title: "Analysis PPM-S"
date: "miércoles 16 septiembre 2020 16:59:24"
css: "../input/css/custom.css"
output:
  bookdown::html_document2:
    number_sections: true
    toc: true
    toc_float:
        collapsed: false
    toc_depth: 2
    code_folding: "hide"
editor_options:
  chunk_output_type: console
---

# Wave 01 {.tabset .tabset-fade .tabset-pills}

On the first wave of the study we conducted the application of the Perceptions and Preferences Meritocracy Scale (PPM-S), in which we decided to test three different modalities for the application of the scale. The design of the study considered a sample of 2200 cases in Chile, using quotas for sex, age and educational level. The most important feature of this study consists in that we randomly assigned the participants to three different groups, in which the subjects complete one of the three combination of the questions. The key feature of these combinations relies on the display order of the questions regarding to each indicator of the scale. On this respect, we have three subsamples that correspond to each application modality that can be analysed separately, but also all together as a full sample.

The scale has eight indicators, divided in four questions for perception and four for preferences, which are also divided in meritocratic and non-meritocratic. The meritocratic indicators refers to topics such as effort and talent, and the non-meritocratic refers to family wealth and networks. The three combinations relies in how these indicators are displayed to the respondent. The first combination corresponds to the group that answered the questions that were sorted by the Perceptions/Preferences dimension. the second combination corresponds to the group in which the questions are displayed following the a topic-sorted way, this means that the effort, talent, rich family and networks questions for perception and preference were displayed together. The last and third group corresponds to a completely randomized display order of the questions.                

The rationale of the design relies on the possibility to test how different display modalities could affect the hypothesized factor structure of the scale. On this regard, we conducted the analysis on two stages. On the first stage we tested the hypothesized factor structure on the three groups corresponding to each application modality and the full sample using a Confirmatory Factor Analysis (CFA). On the second stage, we conducted a Multi-group confirmatory factor analysis (MGCFA) using the groups created by the randomization. Furthermore, we explore the possibility of a second order factor structure, in which we tested two models. The first alternative corresponds to a second-order Confirmatory Factor Analysis assuming the Perception/Preference dimension as a second order factor. On the other hand, we tested a model in which we assume that the second order factor structure follows the Meritocratic/Non-meritocratic constructs.        

---






**Libraries**

```r
library(sjPlot)
library(dplyr)
library(lavaan)
library(semPlot)
library(stargazer)
library(corrplot)
library(psych)
library(knitr)
library(kableExtra)
library(rvest)
library(sjlabelled)
library(ggplot2)
source('production/prod_polychoric.R')
```

**Load data**


```r
load(file = "input/data/proc/dat01.RData")
load(file = "input/data/proc/dat02.RData")
load(file = "input/data/proc/dat03.RData")
load(file = "input/data/proc/dat04.RData")
```


```r
table_format = if(is_html_output()) {
  "html"
} else if(is_latex_output()) {
  "latex"
}
```



---

**Modality**

Each group correspond to the application modality of the scale.

1. **Version 01:** Fixed order by perception / preferences (N=712)

2. **Version 02:** Fixed order, by topic (N=717)

3. **Version 03:** Random order (N=712)

4. **Complete Sample:** Complete sample (N=2141)

## Version 01 (N=712){.tabset}

| Spanish (original)          | English (translated)   |
|-----------------------------|------------------------|
| A. Percepción esfuerzo      | Perception effort      |
| B. Percepción talento       | Perception talent      |
| C. Percepción familia rica  | Perception rich family |
| D. Percepción redes         | Perception networks    |
| E. Preferencia esfuerzo     | Preference effort      |
| F. Preferencia talento      | Preference talent      |
| G. Preferencia familia rica | Preference rich family |
| H. Preferencia redes        | Preference networks    |
Table: Mode 01: Fixed order by perception


<table style="text-align:center"><caption><strong>Descriptive statistics</strong></caption>
<tr><td colspan="9" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Statistic</td><td>N</td><td>Mean</td><td>St. Dev.</td><td>Min</td><td>Pctl(25)</td><td>Median</td><td>Pctl(75)</td><td>Max</td></tr>
<tr><td colspan="9" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">A. pc.effort</td><td>712</td><td>3.16</td><td>1.36</td><td>1</td><td>2</td><td>3</td><td>4</td><td>5</td></tr>
<tr><td style="text-align:left">B. pc.talent</td><td>712</td><td>2.98</td><td>1.12</td><td>1</td><td>2</td><td>3</td><td>4</td><td>5</td></tr>
<tr><td style="text-align:left">C. pc.wpart</td><td>712</td><td>3.60</td><td>1.36</td><td>1</td><td>3</td><td>4</td><td>5</td><td>5</td></tr>
<tr><td style="text-align:left">D. pc.netw</td><td>712</td><td>3.73</td><td>1.27</td><td>1</td><td>3</td><td>4</td><td>5</td><td>5</td></tr>
<tr><td style="text-align:left">E. pf.effort</td><td>712</td><td>3.91</td><td>1.22</td><td>1</td><td>3</td><td>4</td><td>5</td><td>5</td></tr>
<tr><td style="text-align:left">F. pf.talent</td><td>712</td><td>3.27</td><td>1.17</td><td>1</td><td>3</td><td>3</td><td>4</td><td>5</td></tr>
<tr><td style="text-align:left">G. pf.wpart</td><td>712</td><td>2.55</td><td>1.12</td><td>1</td><td>2</td><td>3</td><td>3</td><td>5</td></tr>
<tr><td style="text-align:left">H. pf.netw</td><td>712</td><td>2.33</td><td>1.06</td><td>1</td><td>1</td><td>2</td><td>3</td><td>5</td></tr>
<tr><td colspan="9" style="border-bottom: 1px solid black"></td></tr></table>


```r
cor01<-polychoric(dat01)
rownames(cor01$rho) <- c("A.Perception Effort", "B.Perception Talent","C.Perception Rich parents","D.Perception contacts","E.Preferences Effort","F.Preferences Talent","G.Preferences rich parents", "H.Preferences contacts" )
colnames(cor01$rho) <- c("(A)","(B)","(C)","(D)","(E)","(F)","(G)", "(H)")


corrplot(cor01$rho,method = "color" ,type = "upper", tl.col = "black", addCoef.col = "black",diag = FALSE)
```

<img src="prod_analysis-cfa_files/figure-html/correlacion v01-1.png" width="672" />



```r
model01 <- 'perc_merit=~perc_effort+perc_talent
            perc_nmerit=~perc_wpart+perc_netw
            pref_merit=~pref_effort+pref_talent
            pref_nmerit=~pref_wpart+pref_netw'

fit1_c <- cfa(model = model01,data = dat01,estimator="MLR") # Continuous/ estimator ML Robust

fit1_o <- cfa(model = model01,data = dat01,
            ordered = c("perc_effort","perc_talent",
                        "perc_wpart","perc_netw",
                        "pref_effort","pref_talent",
                        "pref_wpart","pref_netw"))

#---------------------------------------------------#
cnames <- c("Factor","Indicator","Loading (MLR)","Loading (DWLS)")
kable(left_join(x = standardizedsolution(fit1_c) %>% filter(op=="=~") %>% select(lhs,rhs,est.std),y = standardizedsolution(fit1_o) %>% filter(op=="=~") %>% select(lhs,rhs,est.std),c("lhs","rhs")),
      format = "markdown",digits = 2,col.names = cnames, caption = "Factor loadings")
```



|Factor      |Indicator   | Loading (MLR)| Loading (DWLS)|
|:-----------|:-----------|-------------:|--------------:|
|perc_merit  |perc_effort |          0.70|           0.69|
|perc_merit  |perc_talent |          0.72|           0.81|
|perc_nmerit |perc_wpart  |          0.80|           0.85|
|perc_nmerit |perc_netw   |          0.92|           0.94|
|pref_merit  |pref_effort |          0.82|           0.85|
|pref_merit  |pref_talent |          0.58|           0.64|
|pref_nmerit |pref_wpart  |          0.46|           0.55|
|pref_nmerit |pref_netw   |          1.37|           1.26|

> **NOTE**: good fit, heywood case item 8.


```r
labs<- c("A. Who the more they try they manage to get bigger rewards that those who striveless.",
         "B. Who possess more talent they manage to obtain greater rewards than those who possess less talent.",
         "C. Who they have rich parents manage to get out ahead.",
         "D. Who they have good contacts they manage to get out ahead.",
         "E. Who the more they try they should get greater rewards than those who they try less." ,
         "F. Who possess more talent they should get greater rewards than those who possess less talent." ,
         "G. It's fine that those who have rich parents get ahead",
         "H. Is well that those who have good contacts get ahead." )
cnametab<- c("","(A)","(B)","(C)","(D)","(E)","(F)","(G)", "(H)","(1)","(2)","(3)","(4)","(1)","(2)","(3)","(4)")
```









































































