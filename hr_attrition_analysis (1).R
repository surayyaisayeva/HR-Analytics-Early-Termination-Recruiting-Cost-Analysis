library(tidyverse)
library(broom)

# install.packages(c("tidyverse", "broom")) if needed

core       <- read.csv("core_dataset.csv")
recruiting <- read.csv("recruiting_costs.csv")
salary     <- read.csv("salary_grid.csv")

glimpse(core)
glimpse(recruiting)
glimpse(salary)

colSums(is.na(core))
sum(duplicated(core))
sum(duplicated(recruiting))
sum(duplicated(salary))

# ---- salary_grid ----
# "Min" is a leftover label row here, not a real salary value
salary_clean <- salary[!is.na(salary$Salary) & salary$Salary != "" & salary$Salary != "Min",
                        c("Salary", "Hourly")]
salary_clean$Salary <- as.numeric(salary_clean$Salary)
salary_clean$Hourly <- as.numeric(salary_clean$Hourly)
salary_clean <- salary_clean[!duplicated(salary_clean), ]

# ---- core_dataset ----
core_clean <- core[rowSums(is.na(core)) < ncol(core), ]

core_clean$Department <- trimws(core_clean$Department)  # a few had trailing spaces

core_clean$DOB <- as.Date(core_clean$DOB, format = "%m/%d/%Y")
core_clean$Date.of.Hire <- as.Date(core_clean$Date.of.Hire, format = "%m/%d/%Y")
core_clean$Date.of.Termination[core_clean$Date.of.Termination == ""] <- NA
core_clean$Date.of.Termination <- as.Date(core_clean$Date.of.Termination, format = "%m/%d/%Y")

core_clean <- core_clean[!apply(core_clean, 1, function(x) all(is.na(x) | x == "")), ]

# checking Employment.Status lines up with Date.of.Termination
core_clean[!is.na(core_clean$Date.of.Termination) &
             !core_clean$Employment.Status %in% c("Terminated for Cause", "Voluntarily Terminated"),
           c("Employee.Name", "Employment.Status", "Date.of.Termination", "Reason.For.Term")]

core_clean[is.na(core_clean$Date.of.Termination) &
             core_clean$Employment.Status %in% c("Terminated for Cause", "Voluntarily Terminated"),
           c("Employee.Name", "Employment.Status", "Date.of.Termination", "Reason.For.Term")]

# ---- tenure + early termination flag ----
core_clean$Tenure_Days <- as.numeric(core_clean$Date.of.Termination - core_clean$Date.of.Hire)
core_clean$Early_Termination <- !is.na(core_clean$Tenure_Days) & core_clean$Tenure_Days <= 90

summary(core_clean$Tenure_Days)
table(core_clean$Early_Termination)

terminated <- core_clean[core_clean$Employment.Status %in%
                            c("Terminated for Cause", "Voluntarily Terminated"), ]

early_termination_rate <- mean(terminated$Early_Termination) * 100
early_termination_rate

early_90 <- terminated[terminated$Early_Termination, ]

table(early_90$Reason.For.Term)
table(early_90$Department, early_90$Reason.For.Term)

# same logic, reusable for any grouping column
attrition_summary <- function(data, group_var) {
  data |>
    filter(!is.na(.data[[group_var]]), .data[[group_var]] != "") |>
    group_by(.data[[group_var]]) |>
    summarise(
      Hires = n(),
      Early_Terminations = sum(Early_Termination, na.rm = TRUE),
      Early_Termination_Rate = Early_Terminations / Hires * 100,
      .groups = "drop"
    )
}

attrition_by <- map(c("Department", "Employee.Source", "Position"),
                     ~ attrition_summary(core_clean, .x))
names(attrition_by) <- c("Department", "Employee.Source", "Position")

department_90_rate <- attrition_by[["Department"]]
department_90_rate

# ---- department x source ----
department_source_analysis <- core_clean |>
  group_by(Department, Employee.Source) |>
  summarise(
    Hires = n(),
    Early_Terminations = sum(Early_Termination, na.rm = TRUE),
    Early_Termination_Rate = Early_Terminations / Hires * 100,
    .groups = "drop"
  )

department_source_filtered <- department_source_analysis |>
  filter(Department != "", Employee.Source != "", Hires >= 5)  # skip tiny-sample combos

department_source_filtered |>
  arrange(desc(Early_Termination_Rate)) |>
  slice_head(n = 10) |>
  select(Department, Employee.Source, Hires, Early_Terminations, Early_Termination_Rate)

# ---- recruiting cost ----
recruiting$Calculated_Total <- rowSums(recruiting[, 2:13])
recruiting[recruiting$Total != recruiting$Calculated_Total,
           c("Employment.Source", "Total", "Calculated_Total")]

recruiting_cost <- recruiting[order(-recruiting$Calculated_Total),
                               c("Employment.Source", "Calculated_Total")]
recruiting_cost

hire_count <- table(core_clean$Employee.Source)
hire_df <- data.frame(Employment.Source = names(hire_count), Hires = as.numeric(hire_count))

recruiting_analysis <- merge(recruiting[, c("Employment.Source", "Calculated_Total")],
                              hire_df, by = "Employment.Source")
recruiting_analysis$Cost_Per_Hire <- recruiting_analysis$Calculated_Total / recruiting_analysis$Hires
recruiting_analysis <- recruiting_analysis[order(-recruiting_analysis$Cost_Per_Hire), ]

early_source_count <- table(early_90$Employee.Source)
early_source_df <- data.frame(Employment.Source = names(early_source_count),
                               Early_Terminations = as.numeric(early_source_count))
recruiting_analysis <- merge(recruiting_analysis, early_source_df,
                              by = "Employment.Source", all.x = TRUE)
recruiting_analysis$Early_Terminations[is.na(recruiting_analysis$Early_Terminations)] <- 0
recruiting_analysis$Early_Termination_Rate <-
  recruiting_analysis$Early_Terminations / recruiting_analysis$Hires * 100
recruiting_analysis

# ---- stats ----
chisq.test(table(core_clean$Department, core_clean$Early_Termination))

t.test(Pay.Rate ~ Early_Termination, data = core_clean)

anova_model <- aov(Pay.Rate ~ Department, data = core_clean)
broom::tidy(anova_model)

cor.test(recruiting_analysis$Cost_Per_Hire, recruiting_analysis$Early_Termination_Rate,
         method = "spearman")

# ---- chart 1: department ----
ggplot(department_90_rate, aes(x = Department, y = Early_Termination_Rate, fill = Department)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Early_Termination_Rate, 2), "%")), vjust = -0.5) +
  labs(
    title = "First-90-day early termination rate by department",
    x = "Department",
    y = "Early Termination Rate (%)"
  ) +
  scale_fill_manual(values = c(
    "Production" = "#E76F51",
    "IT/IS" = "#457B9D",
    "Admin Offices" = "#A8DADC",
    "Executive Office" = "#A8DADC",
    "Sales" = "#A8DADC",
    "Software Engineering" = "#A8DADC"
  )) +
  theme_minimal() +
  theme(legend.position = "none")

# ---- chart 2: recruiting channel ----
paid_sources_filtered <- recruiting_analysis |>
  filter(Cost_Per_Hire > 0, Hires >= 5)

ggplot(paid_sources_filtered,
       aes(x = reorder(Employment.Source, Cost_Per_Hire), y = Cost_Per_Hire)) +
  geom_col(aes(fill = Early_Termination_Rate)) +
  geom_text(aes(label = paste0(round(Early_Termination_Rate, 1), "% | ", Hires, " hires")),
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Cost per hire by recruiting channel",
    subtitle = "Channels with at least 5 hires | color = early termination rate",
    x = "Recruitment Channel", y = "Cost per Hire", fill = "Early Termination Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# ---- chart 3: riskiest combos ----
department_source_cost <- department_source_filtered |>
  left_join(paid_sources_filtered |> select(Employment.Source, Cost_Per_Hire),
            by = c("Employee.Source" = "Employment.Source"))

top_10_risky <- department_source_cost |>
  filter(!is.na(Cost_Per_Hire)) |>
  arrange(desc(Early_Termination_Rate)) |>
  slice_head(n = 10) |>
  mutate(
    Channel = paste(Department, Employee.Source, sep = " | "),
    Channel = reorder(Channel, Early_Termination_Rate)
  )

ggplot(top_10_risky, aes(x = Early_Termination_Rate, y = Channel)) +
  geom_col(aes(fill = Cost_Per_Hire), width = 0.7) +
  geom_text(aes(label = paste0(round(Early_Termination_Rate, 1), "%  |  $",
                                round(Cost_Per_Hire, 0), "  |  ", Hires, " hires")),
            hjust = -0.05, size = 3.5) +
  labs(
    title = "Recruiting channels with the highest early termination risk",
    subtitle = "% = left within 90 days | $ = cost per hire | hires = number recruited",
    x = "Early Termination Rate (%)", y = "Department | Recruitment Channel",
    fill = "Cost per Hire"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 100, 10, 10)
  )
top_10_risky
