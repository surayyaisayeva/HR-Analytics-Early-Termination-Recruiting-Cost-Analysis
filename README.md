# HR Analytics: Early Termination & Recruiting Cost Analysis

R project on first-90-day employee attrition and recruiting channel cost-effectiveness, built after an 8-day R programming course. Data: rhuebner "Human Resources Data Set" on Kaggle (core_dataset.csv, recruiting_costs.csv, salary_grid.csv).

## Question

Which departments and recruiting sources have the highest early-termination risk, and how does that relate to cost per hire?

## What's in it

Cleaning and joining three separate tables, tenure/attrition calculations, a reusable function applied across grouping variables with purrr::map, four statistical tests (t-test, chi-square, Spearman correlation, ANOVA), and three ggplot2 charts.

## Findings

- Early termination rate (first 90 days): ...
- Highest-risk department: ...
- Highest-risk recruiting source: ...
- Cost per hire vs. early termination rate: ...

## Running it

Put the three CSVs in the project folder, open hr_attrition_analysis.R in RStudio, run top to bottom.

## Tools

R, tidyverse, broom
