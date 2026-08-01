# HR Analytics: Early Termination & Recruiting Cost Analysis

R project on first-90-day employee attrition and recruiting channel cost-effectiveness, built after an 8-day R programming course. Data: rhuebner "Human Resources Data Set" on Kaggle (core_dataset.csv, recruiting_costs.csv, salary_grid.csv).

## Question

Which departments and recruiting sources have the highest early-termination risk, and how does that relate to cost per hire?

## What's in it

Cleaning and joining three separate tables, tenure/attrition calculations, a reusable function applied across grouping variables with purrr::map, four statistical tests (t-test, chi-square, Spearman correlation, ANOVA), and three ggplot2 charts.

## Findings

- Early termination rate (first 90 days): **13.7%** of employees who left the company did so within their first 90 days.
- Highest-risk department: **Production**, at **5.8%** (12 of 208 employees), followed by **IT/IS** at **4.9%** (2 of 41). The other four departments (Admin Offices, Executive Office, Sales, Software Engineering) had no early terminations in this dataset.
- Highest-risk recruiting sources: **Internet Search** (16.7%) and **Word of Mouth** (15.4%) had the highest early-departure rates among sources with 5+ hires — though Internet Search is based on just 6 hires, so treat that number cautiously.
- Cost per hire vs. early termination rate: Spearman's rho = 0.09 (p = 0.69) — no statistically significant relationship. Expensive recruiting channels weren't associated with higher or lower early-termination risk in this dataset.


## Running it

Put the three CSVs in the project folder, open hr_attrition_analysis.R in RStudio, run top to bottom.

## Tools

R, tidyverse, broom
