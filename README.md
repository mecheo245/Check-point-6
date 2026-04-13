---
name: "Joel Adam Thuo"
title: "Check point 6"
output: html_document
date: "2026-04-09"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Load Libraries

```{r}
#install.packages("lme4")
library(lme4)
library(ggplot2)
library(readr)


```

#Load data
```{r setup, include=FALSE}

knitr::opts_knit$set(root.dir = "/Users/joeladamthuo/Desktop/R/Check point 6/data")

brfss <- read_csv("BRFSS_data.csv")

head(brfss)
```


#Define the model

```{r}

brfss$`Smoke everyday` <- as.numeric(gsub("[^0-9.]", "", brfss$`Smoke everyday`))


brfss_clean <- brfss[!is.na(brfss$`Smoke everyday`), ]

summary(brfss$`Smoke everyday`)


model <- lmer(`Smoke everyday` ~ Year + (1 | State), data = brfss_clean)
summary(model)

```

#Visualization
##Plot 1
```{r}
plot1 <- ggplot(brfss_clean, aes(x = Year, y = `Smoke everyday`, group = State)) +
  geom_line(alpha = 0.2) +
  geom_smooth(aes(group = 1), method = "lm", linewidth = 1.2) +
  theme_minimal() +
  labs(
    title = "Daily Smoking Prevalence by State Over Time",
    x = "Year",
    y = "Smoking Prevalence (%)"
  )

plot1

ggsave("figures/plot1_smoking_trend_states.png", plot = plot1, width = 8, height = 5)
```

**The plot shows a clear decline in daily smoking prevalence over time across states. While there is variability between states, the overall downward trend indicates a consistent national decrease, with year being a strong predictor of smoking rates.**


##Plot 2
```{r}

plot2 <- ggplot(brfss_clean, aes(x = Year, y = `Smoke everyday`)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", linewidth = 1.2) +
  theme_minimal() +
  labs(
    title = "Overall Trend in Daily Smoking Prevalence",
    x = "Year",
    y = "Smoking Prevalence (%)"
  )

plot2

ggsave("figures/plot2_overall_trend.png", plot = plot2, width = 8, height = 5)

```

**The plot shows a clear downward trend in daily smoking prevalence over time, with the regression line indicating a steady decrease. Despite variability across state-year observations, the overall pattern suggests that smoking rates have consistently declined over the years.**

#Statistical Analysis

```{r}
model <- lmer(`Smoke everyday` ~ Year + (1 | State), data = brfss)
summary(model)
```


#AI Disclosure
**This document was partially done using ChatGPT. Iused it as a teaching assistant to help me understand how to code some things**
