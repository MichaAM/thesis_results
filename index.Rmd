
---
title: "Final project results"
author: "Micha Amsalem"
date: "24 8 2020"
output: html_document
---


### Load R packages 

```{r include=FALSE}
library(sandwich)
library(lmtest)
library(stargazer)
library(estimatr)
library(texreg)
library(tidyverse)
library(plm)
library(lfe)
library(estimatr)
```


## Introduction
The panel data is clustered by 1063 schools. Schools were observed between 2015-2016, before the start of the program and in 2015-2016 after the end of the program. 


Schools were divided into sectors as follows:
```{r}
table(final.df$CODE_MIGZAR)
```
"0" - Jewish sector
"1" _ Arabic sector


Schools were divided by years and sectors as follows:
```{r}
table(final.df$CODE_MIGZAR, final.df$SHNAT_LIMUD)
```

Schools were divided by treatment as follows:
```{r}
table(final.df$treat)
```



```{r}
str(final.df)
```

# Number of schools observed in the 2 periods - Before and after the treatment  
```{r}
length(unique(final.df$CODE_MOSAD))
```

# General DiD regression

## general DiD regression with clustered SE
```{r include=FALSE}
didreg.heb <- lm_robust(heb.zscore ~ treat + after + did,
                      data = final.df, clusters = CODE_MOSAD, se_type = "stata")
didreg.mat <- lm_robust(mat.zscore ~ treat + after + did,
                     data = final.df, clusters = CODE_MOSAD, se_type = "stata")
didreg.eng <- lm_robust(eng.zscore ~ treat + after + did,
                     data = final.df, clusters = CODE_MOSAD, se_type = "stata")
didreg.arb <- lm_robust(arb.zscore ~ treat + after + did,
                     data = final.df, clusters = CODE_MOSAD, se_type = "stata")
```

### Table 1 - General DiD with clustered SE 
```{r}
screenreg(list(didreg.heb, didreg.mat, didreg.eng, didreg.arb), include.ci = F, stars = c(.1, .05, .01), 
          digits = 3, custom.model.names = c("Hebrew", "Math", "English", "Arabic"), custom.coef.map = list("did" = "Diff-in-Diff"))

```

## general DiD regression with clustered SE - with fixed effects

```{r include=FALSE}
didreg.heb.fe <- lm_robust(heb.zscore ~ treat + after + did, fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                        data = final.df, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
didreg.mat.fe <- lm_robust(mat.zscore ~ treat + after + did,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                        data = final.df, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
didreg.eng.fe <- lm_robust(eng.zscore ~ treat + after + did, fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                        data = final.df, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
didreg.arb.fe <- lm_robust(arb.zscore ~ treat + after + did,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                        data = final.df, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
```

### Table 2 - General DiD with clustered SE - with fixed effects 
```{r message=FALSE, warning=FALSE}
screenreg(list(didreg.heb.fe, didreg.mat.fe, didreg.eng.fe, didreg.arb.fe), 
          include.ci = F, stars = c(.1, .05, .01), digits = 3, custom.model.names = c("Hebrew", "Math","English", "Arabic"), custom.coef.map = list("did" = "Diff-in-Diff"))
```

## general DiD regression with clustered SE - with fixed effects & additional variables 

```{r include=FALSE}
didreg.heb.fe.var <- lm_robust(heb.zscore ~  did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR, fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD, data = final.df, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
didreg.mat.fe.var <- lm_robust(mat.zscore ~ did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD, data = final.df, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
didreg.eng.fe.var <- lm_robust(eng.zscore ~  did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR, fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD, data = final.df, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
didreg.arb.fe.var <- lm_robust(arb.zscore ~  did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD, data = final.df, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
```

### Table 3 - General DiD with clustered SE - with fixed effects & additional variables
```{r message=FALSE, warning=FALSE}

screenreg(list(didreg.heb.fe.var, didreg.mat.fe.var, didreg.eng.fe.var, didreg.arb.fe.var), 
          include.ci = F, stars = c(.1, .05, .01), digits = 3, custom.model.names = c("Hebrew", "Math", "English", "Arabic"),
          custom.coef.map = list("did" = "Diff-in-Diff", "CODE_MIN" = "Sex", "SHNOT_LIMUD_AV" = "Father's years of education",
                                 "SHNOT_LIMUD_EM" = "Mother's years of education", "mean_kids_per_class_by_school_year" = "Class size",
                                 "erets_leda_ISR" = "Native indicator"))
```


# Anlalysis by sectors - Jewish("0") vs Arab("1") 

### Obsevations by sectores table 
```{R}
table(final.df$CODE_MIGZAR)
```


### Split the data by sectores
```{R}
final_df_jew <- final.df %>%
  filter(CODE_MIGZAR == "0") 
final_df_arb <- final.df %>%
  filter(CODE_MIGZAR == "1") 
```


## DiD regression with clustered SE - Jewish sector
```{r include=FALSE}
didreg.heb.jew <- lm_robust(heb.zscore ~ treat + after + did,
                        data = final_df_jew, clusters = CODE_MOSAD, se_type = "stata")
didreg.mat.jew <- lm_robust(mat.zscore ~ treat + after + did,
                        data = final_df_jew, clusters = CODE_MOSAD, se_type = "stata")
didreg.eng.jew <- lm_robust(eng.zscore ~ treat + after + did,
                        data = final_df_jew, clusters = CODE_MOSAD, se_type = "stata")
```

### Table 4 - DiD with clustered SE - Jewish sector
```{r}
screenreg(list(didreg.heb.jew, didreg.mat.jew, didreg.eng.jew), include.ci = F, stars = c(.1, .05, .01), 
          digits = 3, custom.model.names = c("Hebrew", "Math", "English"),
          custom.coef.map = list("did" = "Diff-in-Diff"))


```

## DiD regression with clustered SE - Arab sector
```{r include=FALSE}
didreg.arb.arb <- lm_robust(arb.zscore ~ treat + after + did,
                            data = final_df_arb, clusters = CODE_MOSAD, se_type = "stata")
didreg.mat.arb <- lm_robust(mat.zscore ~ treat + after + did,
                            data = final_df_arb, clusters = CODE_MOSAD, se_type = "stata")
didreg.eng.arb <- lm_robust(eng.zscore ~ treat + after + did,
                            data = final_df_arb, clusters = CODE_MOSAD, se_type = "stata")
```

### Table 5 - DiD with clustered SE - Arab sector
```{r}
screenreg(list(didreg.arb.arb, didreg.mat.arb, didreg.eng.arb), include.ci = F, stars = c(.1, .05, .01), 
          digits = 3, custom.model.names = c("Arabic", "Math", "English"),
          custom.coef.map = list("did" = "Diff-in-Diff"))


```

## DiD regression with clustered SE + Fixed effects - Jewish sector
```{r include=FALSE}
didreg.heb.jew.fe <- lm_robust(heb.zscore ~ treat + after + did, fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                            data = final_df_jew, clusters = CODE_MOSAD, se_type = "stata")
didreg.mat.jew.fe <- lm_robust(mat.zscore ~ treat + after + did,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                            data = final_df_jew, clusters = CODE_MOSAD, se_type = "stata")
didreg.eng.jew.fe <- lm_robust(eng.zscore ~ treat + after + did,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                            data = final_df_jew, clusters = CODE_MOSAD, se_type = "stata")
```

### Table 6 - DiD with clustered SE + Fixed effects - Jewish sector
```{r message=FALSE}
screenreg(list(didreg.heb.jew.fe, didreg.mat.jew.fe, didreg.eng.jew.fe), include.ci = F, stars = c(.1, .05, .01), digits = 3, custom.model.names = c("Arabic", "Math", "English"), custom.coef.map = list("did" = "Diff-in-Diff"))


```

## DiD regression with clustered SE + Fixed effects - Arabic sector
```{r include=FALSE}
didreg.arb.arb.fe <- lm_robust(arb.zscore ~ treat + after + did,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                            data = final_df_arb, clusters = CODE_MOSAD, se_type = "stata")
didreg.mat.arb.fe <- lm_robust(mat.zscore ~ treat + after + did,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                            data = final_df_arb, clusters = CODE_MOSAD, se_type = "stata")
didreg.eng.arb.fe <- lm_robust(eng.zscore ~ treat + after + did,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                            data = final_df_arb, clusters = CODE_MOSAD, se_type = "stata")
```

### Table 7 - DiD with clustered SE + Fixed effects - Arabic sector
```{r message=FALSE}
screenreg(list(didreg.arb.arb.fe, didreg.mat.arb.fe, didreg.eng.arb.fe), include.ci = F, stars = c(.1, .05, .01), digits = 3, custom.model.names = c("Arabic", "Math", "English"), custom.coef.map = list("did" = "Diff-in-Diff"))


```



## DiD regression with clustered SE + Fixed effects + additional variables - Jewish sector
```{r include=FALSE}
didreg.heb.fe.var.jew <- lm_robust(heb.zscore ~  did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + 
                                 dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR, fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                               data = final_df_jew, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
didreg.mat.fe.var.jew <- lm_robust(mat.zscore ~ did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + 
                                 dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                               data = final_df_jew, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
didreg.eng.fe.var.jew <- lm_robust(eng.zscore ~  did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + 
                                 dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR, fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                               data = final_df_jew, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)

```

### Table 8 - DiD with clustered SE + Fixed effects + additional variables - Jewish sector
```{r message=FALSE}
screenreg(list(didreg.heb.fe.var.jew, didreg.mat.fe.var.jew, didreg.eng.fe.var.jew), 
          include.ci = F, stars = c(.1, .05, .01), digits = 3, custom.model.names = c("Hebrew", "Math", "English"),
          custom.coef.map = list("did" = "Diff-in-Diff", "CODE_MIN" = "Sex", "SHNOT_LIMUD_AV" = "Father's years of education",
                                 "SHNOT_LIMUD_EM" = "Mother's years of education", "mean_kids_per_class_by_school_year" = "Class size",
                                 "erets_leda_ISR" = "Native indicator"))
```


## DiD regression with clustered SE + Fixed effects + additional variables - Arabic sector
```{r include=FALSE}
didreg.arb.fe.var.arb <- lm_robust(arb.zscore ~  did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + 
                                     dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR, fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                                   data = final_df_arb, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
didreg.mat.fe.var.arb <- lm_robust(mat.zscore ~ did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + 
                                     dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                                   data = final_df_arb, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
didreg.eng.fe.var.arb <- lm_robust(eng.zscore ~  did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + 
                                     dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR, fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                                   data = final_df_arb, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)

```

### Table 9 - DiD with clustered SE + Fixed effects + additional variables - Arabic sector
```{r message=FALSE}
screenreg(list(didreg.arb.fe.var.arb, didreg.mat.fe.var.arb, didreg.eng.fe.var.arb), 
          include.ci = F, stars = c(.1, .05, .01), digits = 3, custom.model.names = c("Arabic", "Math", "English"),
          custom.coef.map = list("did" = "Diff-in-Diff", "CODE_MIN" = "Sex", "SHNOT_LIMUD_AV" = "Father's years of education",
                                 "SHNOT_LIMUD_EM" = "Mother's years of education", "mean_kids_per_class_by_school_year" = "Class size",
                                 "erets_leda_ISR" = "Native indicator"))
```


# DiD regression by regions - periphery vs rest
Because almost all observation in the north and south regions took part in the program, I will check only the difference in the 
other regions. 



### Split the data by regions periphery ("0") and rest ("1") 
```{r message=FALSE}
final.df$CODE_MACHOZ_GEOGRAFI <- ifelse(final.df$CODE_MACHOZ_GEOGRAFI == "2" | final.df$CODE_MACHOZ_GEOGRAFI == "6", "0", "1")

final.df_periphery <- final.df %>%
  filter(CODE_MACHOZ_GEOGRAFI == "0")
final.df_rest <- final.df %>%
  filter(CODE_MACHOZ_GEOGRAFI == "1")
```


### Split final.df_rest by sector
```{r message=FALSE}
final.df_rest.jew <- final.df_rest %>%
  filter(CODE_MIGZAR == "0")
final.df_rest.arb <- final.df_rest %>%
  filter(CODE_MIGZAR == "1")
```


### Table of observations per region 
```{r message=FALSE}
region_tbl <- table(final.df$CODE_MACHOZ_GEOGRAFI)
names(region_tbl)[1]<-paste("Periphery")
names(region_tbl)[2]<-paste("Rest")
region_tbl

```

### Table of observations per region per sector 
```{r message=FALSE}
region_sectore_table <- table(final.df$CODE_MACHOZ_GEOGRAFI, final.df$CODE_MIGZAR)
rownames(region_sectore_table) <- c("Periphery", "Rest" )
colnames(region_sectore_table) <- c("Jewish", "Arab" )
region_sectore_table
```

# Simple DiD regression on rest regions by sectores 

### DiD regression with clustered SE - rest regions - Jewish sector only
```{r include=FALSE}
didreg.heb.jew.rest <- lm_robust(heb.zscore ~ treat + after + did,
                        data = final.df_rest.jew, clusters = CODE_MOSAD, se_type = "stata")
didreg.mat.jew.rest <- lm_robust(mat.zscore ~ treat + after + did,
                        data = final.df_rest.jew, clusters = CODE_MOSAD, se_type = "stata")
didreg.eng.jew.rest <- lm_robust(eng.zscore ~ treat + after + did,
                        data = final.df_rest.jew, clusters = CODE_MOSAD, se_type = "stata")
```

### Table 10 - DiD with clustered SE - Jewish sector
```{r}
screenreg(list(didreg.heb.jew.rest, didreg.mat.jew.rest, didreg.eng.jew.rest), include.ci = F, stars = c(.1, .05, .01), 
          digits = 3, custom.model.names = c("Hebrew", "Math", "English"),
          custom.coef.map = list("did" = "Diff-in-Diff"))


```

## DiD regression with clustered SE - rest regions - Arab sector only
```{r include=FALSE}
didreg.arb.arb.rest <- lm_robust(arb.zscore ~ treat + after + did,
                        data = final.df_rest.arb, clusters = CODE_MOSAD, se_type = "stata")
didreg.mat.jew.rest <- lm_robust(mat.zscore ~ treat + after + did,
                        data = final.df_rest.arb, clusters = CODE_MOSAD, se_type = "stata")
didreg.eng.jew.rest <- lm_robust(eng.zscore ~ treat + after + did,
                        data = final.df_rest.arb, clusters = CODE_MOSAD, se_type = "stata")
```

### Table 11 - DiD with clustered SE - Arab sector
```{r}
screenreg(list(didreg.heb.jew.rest, didreg.mat.jew.rest, didreg.eng.jew.rest), include.ci = F, stars = c(.1, .05, .01), 
          digits = 3, custom.model.names = c("Arabic", "Math", "English"),
          custom.coef.map = list("did" = "Diff-in-Diff"))


```


# DiD regression with clustered SE + Fixed effects

## DiD regression with clustered SE + Fixed effects - Jewish sector
```{r include=FALSE}
didreg.heb.rest.jew.fe <- lm_robust(heb.zscore ~ treat + after + did, fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                            data = final.df_rest.jew, clusters = CODE_MOSAD, se_type = "stata")
didreg.mat.rest.jew.fe <- lm_robust(mat.zscore ~ treat + after + did,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                            data = final.df_rest.jew, clusters = CODE_MOSAD, se_type = "stata")
didreg.eng.rest.jew.fe <- lm_robust(eng.zscore ~ treat + after + did,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                            data = final.df_rest.jew, clusters = CODE_MOSAD, se_type = "stata")
```

### Table 12 - DiD with clustered SE + Fixed effects - Jewish sector
```{r message=FALSE}
screenreg(list(didreg.heb.rest.jew.fe, didreg.mat.rest.jew.fe, didreg.eng.rest.jew.fe), include.ci = F, stars = c(.1, .05, .01), digits = 3, custom.model.names = c("Hebrew", "Math", "English"), custom.coef.map = list("did" = "Diff-in-Diff"))


```

## DiD regression with clustered SE + Fixed effects - Arab sector
```{r include=FALSE}
didreg.arb.rest.arb.fe <- lm_robust(arb.zscore ~ treat + after + did, fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                            data = final.df_rest.arb, clusters = CODE_MOSAD, se_type = "stata")
didreg.mat.rest.arb.fe <- lm_robust(mat.zscore ~ treat + after + did,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                            data = final.df_rest.arb, clusters = CODE_MOSAD, se_type = "stata")
didreg.eng.rest.arb.fe <- lm_robust(eng.zscore ~ treat + after + did,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                            data = final.df_rest.arb, clusters = CODE_MOSAD, se_type = "stata")
```

### Table 13 - DiD with clustered SE + Fixed effects - Arab sector
```{r message=FALSE}
screenreg(list(didreg.arb.rest.arb.fe, didreg.mat.rest.arb.fe, didreg.eng.rest.arb.fe), include.ci = F, stars = c(.1, .05, .01), digits = 3, custom.model.names = c("Arabic", "Math", "English"), custom.coef.map = list("did" = "Diff-in-Diff"))


```

# DiD regression with clustered SE + Fixed effects + additional variables 


## DiD regression with clustered SE + Fixed effects + additional variables - Jewish sector
```{r include=FALSE}
didreg.heb.fe.var.jew.rest <- lm_robust(heb.zscore ~  did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + 
                                 dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR, fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                               data = final.df_rest.jew, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
didreg.mat.fe.var.jew.rest <- lm_robust(mat.zscore ~ did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + 
                                 dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                               data = final.df_rest.jew, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
didreg.eng.fe.var.jew.rest <- lm_robust(eng.zscore ~  did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + 
                                 dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR, fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                               data = final.df_rest.jew, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)

```

### Table 14 - DiD with clustered SE + Fixed effects + additional variables - Jewish sector
```{r message=FALSE}
screenreg(list(didreg.heb.fe.var.jew.rest, didreg.mat.fe.var.jew.rest, didreg.eng.fe.var.jew.rest), 
          include.ci = F, stars = c(.1, .05, .01), digits = 3, custom.model.names = c("Hebrew", "Math", "English"),
          custom.coef.map = list("did" = "Diff-in-Diff", "CODE_MIN" = "Sex", "SHNOT_LIMUD_AV" = "Father's years of education",
                                 "SHNOT_LIMUD_EM" = "Mother's years of education", "mean_kids_per_class_by_school_year" = "Class size",
                                 "erets_leda_ISR" = "Native indicator"))
```

## DiD regression with clustered SE + Fixed effects + additional variables - Arab sector
```{r include=FALSE}
didreg.arb.fe.var.arb.rest <- lm_robust(arb.zscore ~  did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + 
                                 dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR, fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                               data = final.df_rest.arb, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
didreg.mat.fe.var.arb.rest <- lm_robust(mat.zscore ~ did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + 
                                 dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR,fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                               data = final.df_rest.arb, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)
didreg.eng.fe.var.arb.rest <- lm_robust(eng.zscore ~  did + CODE_MIN + SHNOT_LIMUD_AV + SHNOT_LIMUD_EM + dummy_av_88 + 
                                 dummy_em_88 + mean_kids_per_class_by_school_year + erets_leda_ISR, fixed_effects = ~CODE_MOSAD+SHNAT_LIMUD,
                               data = final.df_rest.arb, clusters = CODE_MOSAD, se_type = "stata", try_cholesky = TRUE)

```

### Table 15 - DiD with clustered SE + Fixed effects + additional variables - Arab sector
```{r message=FALSE}
screenreg(list(didreg.arb.fe.var.arb.rest, didreg.mat.fe.var.arb.rest, didreg.eng.fe.var.arb.rest), 
          include.ci = F, stars = c(.1, .05, .01), digits = 3, custom.model.names = c("Arabic", "Math", "English"),
          custom.coef.map = list("did" = "Diff-in-Diff", "CODE_MIN" = "Sex", "SHNOT_LIMUD_AV" = "Father's years of education",
                                 "SHNOT_LIMUD_EM" = "Mother's years of education", "mean_kids_per_class_by_school_year" = "Class size",
                                 "erets_leda_ISR" = "Native indicator"))
```



