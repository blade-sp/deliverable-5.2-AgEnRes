# Script for data analysis, figures, and tables replication
#
# Author: Riccardo Spada
#
# Contact: riccardo.spada@wur.nl

# set wd if needed

# clean environment
rm(list = ls())

# load libraries
requiredPackages <- c(
  "tidyverse",
  "readxl",
  "networkD3",
  "ggbreak",
  "stringr",
  "patchwork",
  "RColorBrewer",
  "pagedown",
  "grid",
  "writexl"
)

for (p in requiredPackages) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
  rm(p)
}

### import data ###############################################################

df <- read_excel(
  "1_Data/Data_SLR_BehavPriceRisk.xlsx",
  sheet = "data",
  skip = 2
)

### global variables and functions ############################################

farm_type <- c("crop", "FV", "livestock", "fish", "other")

study_type <- c("ex_ante", "ex_post")

study_design <- c(
  "theoretical_model",
  "optimization",
  "econometric_method",
  "experiment"
)

model <- c(
  "max_EU",
  "M_V",
  "max_profits",
  "stochastic_dominance",
  "prospect_theory",
  "bayesian",
  "agency_theory",
  "VaR"
)

tools <- c(
  "forward",
  "futures",
  "options",
  "price_insurance",
  "swap",
  "multiple_tools"
)

behav_pref <- c(
  "risk_preferences",
  "uncertainty_aversion",
  "variation_aversion",
  "reference_dependence",
  "loss_aversion",
  "probability_weighting",
  "time_preference",
  "price_expectation_theor"
)

psyc_factors <- c(
  "risk_attitudes",
  "risk_perceptions",
  "price_yield_expectation",
  "alternative_perceptions",
  "network_influence",
  "subjective_norms",
  "management_attitudes",
  "innovativeness",
  "perceived_usefulness",
  "tools_knowledge"
)

oper <- c(
  "predicted",
  "self_reported_intention",
  "self_reported_adoption",
  "actual_adoption"
)

risk_type <- c("input_prices", "output_prices", "not_specified")

direction <- c("positive", "negative", "no_direction")

significance <- c("significant", "non_significant")

other_strategies <- c(
  "cash_marketing",
  "storage",
  "diversification",
  "government_programs",
  "non_price_insurance",
  "other_risk_sharing_strategies",
  "leverage",
  "nothing"
)


# function that transforms data from horizontal to vertical
pivot_columns <- function(
  data,
  cols_to_pivot,
  names_col,
  values_col = "value"
) {
  data %>%
    pivot_longer(
      cols = all_of(cols_to_pivot),
      names_to = names_col,
      values_to = values_col
    ) %>%
    filter(!!sym(values_col) == TRUE) %>%
    select(-all_of(values_col))
}

# function that creates a summary table for each behavioral preference
create_behavior_table <- function(
  data,
  behavior_col,
  behav_pref,
  farm_type,
  tools
) {
  data %>%
    filter(rowSums(across(all_of(behav_pref), ~ . != 0)) > 0) %>%
    select(c(
      "authors",
      "year",
      "study_loc",
      all_of(farm_type),
      "farm_type_notes",
      all_of(behavior_col),
      all_of(tools),
      all_of(model),
      all_of(direction)
    )) %>%
    filter(!!sym(behavior_col) != 0) %>%
    pivot_columns(cols_to_pivot = farm_type, names_col = "farm_type") %>%
    pivot_columns(cols_to_pivot = tools, names_col = "tool_considered") %>%
    pivot_columns(cols_to_pivot = model, names_col = "model") %>%
    pivot_columns(cols_to_pivot = direction, names_col = "sign") %>%
    select(
      "authors",
      "year",
      "study_loc",
      "farm_type",
      "farm_type_notes",
      "tool_considered",
      "model",
      "sign"
    ) %>%
    arrange(tool_considered)
}

# function that creates a summary table for each psychological factor
create_psych_table <- function(
  data,
  psych_col,
  psyc_factors,
  farm_type,
  tools
) {
  data %>%
    filter(rowSums(across(all_of(psyc_factors), ~ . != 0)) > 0) %>%
    select(c(
      "authors",
      "year",
      "study_loc",
      all_of(farm_type),
      "farm_type_notes",
      all_of(psych_col),
      all_of(tools),
      all_of(oper),
      all_of(direction),
      all_of(significance)
    )) %>%
    filter(!!sym(psych_col) != 0) %>%
    pivot_columns(cols_to_pivot = farm_type, names_col = "farm_type") %>%
    pivot_columns(cols_to_pivot = tools, names_col = "tool_considered") %>%
    pivot_columns(cols_to_pivot = oper, names_col = "operationalization") %>%
    pivot_columns(cols_to_pivot = direction, names_col = "sign") %>%
    pivot_columns(cols_to_pivot = significance, names_col = "significance") %>%
    select(
      "authors",
      "year",
      "study_loc",
      "farm_type",
      "farm_type_notes",
      "tool_considered",
      "operationalization",
      "sign",
      "significance"
    ) %>%
    arrange(tool_considered)
}

### data cleaning #############################################################

df <- df %>%
  mutate(across(everything(), ~ ifelse(. == "x", 1, .))) %>%
  mutate(across(
    all_of(c(
      farm_type,
      study_type,
      study_design,
      model,
      tools,
      behav_pref,
      psyc_factors,
      oper,
      risk_type,
      direction,
      significance,
      other_strategies
    )),
    ~ ifelse(is.na(.), 0, .)
  ))


### Analysis ##################################################################

### Geographical distribution #################################################

# Study location table
location <- df %>%
  distinct(authors, .keep_all = TRUE) %>%
  group_by(study_loc) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(study_loc = if_else(study_loc == "US", "USA", study_loc))

location


### Share of tools ############################################################

tool_share_df <- df %>%
  summarise(across(all_of(tools), ~ mean(.x) * 100)) %>% # Calculate percentages
  pivot_longer(
    cols = everything(), # Convert to long format
    names_to = "Tool",
    values_to = "Percentage"
  ) %>%
  mutate(Percentage = round(Percentage, 2)) %>%
  arrange(desc(Percentage))

tool_share_df


### Farm type #################################################################

farm_type_df <- df %>%
  summarise(across(all_of(farm_type), ~ mean(.x) * 100)) %>% # Calculate percentages
  pivot_longer(
    cols = everything(), # Convert to long format
    names_to = "Farm_Type",
    values_to = "Percentage"
  ) %>%
  mutate(Percentage = round(Percentage, 2)) %>%
  arrange(desc(Percentage))

farm_type_df

### Type of price risk ########################################################

risk_type_df <- df %>%
  distinct(authors, .keep_all = TRUE) %>%
  summarise(across(all_of(risk_type), ~ mean(.x) * 100)) %>% # Calculate percentages
  pivot_longer(
    cols = everything(), # Convert to long format
    names_to = "Risk_type",
    values_to = "count"
  ) %>%
  arrange(desc(count))

risk_type_df

### Distribution of studies by year ###########################################

df_year <- df %>%
  distinct(authors, .keep_all = TRUE) %>%
  mutate(contains_psych_factor = if_any(all_of(psyc_factors), ~ . == 1)) %>%
  group_by(year, contains_psych_factor) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(
    year = full_seq(df$year, 1),
    contains_psych_factor,
    fill = list(count = 0)
  )

# plot the distribution
year_distribution_plot <- ggplot(
  df_year,
  aes(x = year, y = count, fill = contains_psych_factor)
) +
  geom_col(position = "stack") +
  labs(x = "", y = "Number of papers") +
  scale_fill_manual(
    values = c("#5788C4", "#D79E42"),
    name = "Type of study",
    labels = c("Behavioral preference", "Psychological factor")
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 2)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.key.size = unit(0.6, "cm"), # Size of legend keys
    legend.text = element_text(size = 9), # Size of legend text
    legend.title = element_text(size = 10, face = "bold"),
    legend.position = c(0.88, 0.8),
    legend.box.background = element_rect(fill = "white"),
  )

year_distribution_plot


### Study design ##############################################################

design_proportion <- df %>%
  distinct(authors, .keep_all = TRUE) %>%
  summarise(across(all_of(study_design), ~ mean(.x) * 100)) %>% # Calculate percentages
  pivot_longer(cols = everything(), names_to = "Method", values_to = "Count")

design_proportion


### Quality assessment ########################################################

quality_df <- df %>%
  distinct(authors, .keep_all = TRUE) %>%
  mutate(Total = round(Total, 1)) %>%
  group_by(Total) %>%
  summarise(count = n())

quality_plot <- ggplot(quality_df) +
  geom_col(aes(x = Total, y = count), fill = "#5788C4") +
  labs(x = "", y = "Frequency") +
  theme_minimal()

quality_plot

### behavioral preference and psychological factors comparison ################

behavioral_obs_count <- df %>%
  select(all_of(behav_pref)) %>%
  summarise(total = sum(across(everything(), ~ . == 1)))

psychological_obs_count <- df %>%
  select(all_of(psyc_factors)) %>%
  summarise(total = sum(across(everything(), ~ . == 1)))

behavioral_obs_count + psychological_obs_count


### theoretical models ########################################################

### visualize which theoretical model is used for each financial tool

model_df <- df %>%
  filter(rowSums(across(all_of(model), ~ !is.na(.))) > 0) %>%
  select(all_of(c(model, tools)))

# make combinatio of all variables pair
pairs <- combn(names(model_df), 2, simplify = FALSE)

# count each combination occurrence
pair_counts <- lapply(pairs, function(vars) {
  tibble(
    pairs = paste(vars, collapse = " & "), # Pair name
    count = sum(model_df[[vars[1]]] == 1 & model_df[[vars[2]]] == 1) # Count rows where both are TRUE
  )
}) %>%
  bind_rows() %>% # Combine results into a single table
  filter(count != 0) %>% # remove each combination that does not occour
  separate(pairs, into = c("model", "tool"), sep = " & ") %>%
  mutate(
    model = case_when(
      model == "max_EU" ~ "Expected Utility maximisation",
      model == "M_V" ~ "Mean-Variance",
      model == "max_profits" ~ "Profit maximisation",
      model == "stochastic_dominance" ~ "Stochastic dominance",
      model == "prospect_theory" ~ "Prospect theory",
      model == "bayesian" ~ "Bayesian approach",
      model == "agency_theory" ~ "Agency theory",
      model == "VaR" ~ "Value at risk",
      TRUE ~ model
    )
  ) %>%
  mutate(tool = str_to_title(tool)) %>%
  mutate(
    tool = case_when(
      tool == 'Multiple_tools' ~ 'Multiple tools',
      tool == 'Price_insurance' ~ 'Price insurance',
      TRUE ~ tool
    )
  )

nodes <- data.frame(
  name = c(as.character(pair_counts$model), as.character(pair_counts$tool))
) %>%
  unique()

### Make a Sankey diagram  ####################################################

model_df2 <- df %>%
  filter(rowSums(across(all_of(model), ~ !is.na(.))) > 0) %>%
  select(all_of(c(behav_pref, model)))

# make combinatio of all variables pair
pairs2 <- combn(names(model_df2), 2, simplify = FALSE)

pair_counts2 <- lapply(pairs2, function(vars) {
  tibble(
    pairs = paste(vars, collapse = " & "), # Pair name
    count = sum(model_df2[[vars[1]]] == 1 & model_df2[[vars[2]]] == 1)
  )
}) %>%
  bind_rows() %>%
  filter(count > 0) %>%
  separate(pairs, into = c("preference", "model"), sep = " & ") %>%
  mutate(
    model = case_when(
      model == "max_EU" ~ "Expected Utility maximisation",
      model == "M_V" ~ "Mean-Variance",
      model == "max_profits" ~ "Profit maximisation",
      model == "stochastic_dominance" ~ "Stochastic dominance",
      model == "prospect_theory" ~ "Prospect theory",
      model == "bayesian" ~ "Bayesian approach",
      model == "agency_theory" ~ "Agency theory",
      model == "VaR" ~ "Value at risk",
      TRUE ~ model
    )
  ) %>%
  mutate(
    preference = case_when(
      preference == 'risk_preferences' ~ 'Risk preferences',
      preference == 'time_preference' ~ 'Time preferences',
      preference == 'uncertainty_aversion' ~ 'Uncertainty aversion',
      preference == 'variation_aversion' ~ 'Variation aversion',
      preference == 'reference_dependence' ~ 'Reference dependence',
      preference == 'price_expectation_theor' ~ 'Price expectations',
      preference == 'loss_aversion' ~ 'Loss aversion',
      preference == 'probability_weighting' ~ 'Probability weighting',
      TRUE ~ preference
    )
  )

# make a sankey diagram

nodes2 <- data.frame(
  name = c(
    as.character(pair_counts$model),
    as.character(pair_counts$tool),
    as.character(pair_counts2$preference)
  )
) %>%
  unique()


pair_counts$IDmodel <- match(pair_counts$model, nodes$name) - 1
pair_counts$IDtool <- match(pair_counts$tool, nodes$name) - 1
pair_counts2$IDmodel <- match(pair_counts2$model, nodes$name) - 1
pair_counts2$IDpreference <- match(pair_counts2$preference, nodes2$name) - 1

# Combine all links
all_links <- bind_rows(
  pair_counts %>% select(Source = IDmodel, Target = IDtool, Value = count),
  pair_counts2 %>%
    select(Source = IDpreference, Target = IDmodel, Value = count)
)


flow_diagram2 <- sankeyNetwork(
  Links = all_links,
  Nodes = nodes2,
  Source = "Source",
  Target = "Target",
  Value = "Value",
  NodeID = "name",
  units = "studies",
  sinksRight = TRUE,
  fontSize = 14,
  nodeWidth = 30,
  fontFamily = "Arial"
)

flow_diagram2


### behavioral preferences ####################################################

behav_pref_df <- df %>%
  filter(rowSums(across(all_of(behav_pref), ~ . != 0)) > 0) %>%
  select(all_of(c(behav_pref, tools)))

# make combinatio of all variables pair
behav_pairs <- combn(names(behav_pref_df), 2, simplify = FALSE)

# count each combination occurrence
behav_pair_counts <- lapply(behav_pairs, function(vars) {
  tibble(
    behav_pairs = paste(vars, collapse = " & "), # Pair name
    obs = sum(behav_pref_df[[vars[1]]] == 1 & behav_pref_df[[vars[2]]] == 1) # Count rows where both are TRUE
  )
}) %>%
  bind_rows() %>% # Combine results into a single table
  filter(obs != 0) %>% # remove each combination that does not occour
  separate(behav_pairs, into = c("factor", "tool"), sep = " & ") %>%
  bind_rows(tibble(
    factor = "total_behav_preference",
    tool = "any",
    obs = as.integer(behavioral_obs_count)
  )) %>%
  mutate(
    factor = case_when(
      factor == "risk_preferences" ~ "Risk preferences",
      factor == "uncertainty_aversion" ~ "Uncertainty aversion",
      factor == "variation_aversion" ~ "Variation aversion",
      factor == "reference_dependence" ~ "Reference dependence",
      factor == "prospect_theory" ~ "Prospect theory",
      factor == "loss_aversion" ~ "Loss aversion",
      factor == "probability_weighting" ~ "Probability weighting",
      factor == "time_preference" ~ "Time preference",
      factor == "price_expectation_theor" ~ "Price expectations",
      factor == "total_behav_preference" ~ "Total",
      TRUE ~ factor
    )
  ) %>%
  mutate(tool = str_to_title(tool)) %>%
  mutate(
    tool = case_when(
      tool == 'Multiple_tools' ~ 'Multiple tools',
      tool == 'Price_insurance' ~ 'Price insurance',
      TRUE ~ tool
    )
  ) %>%
  group_by(factor) %>%
  mutate(cumsum_obs = cumsum(obs))

print(behav_pair_counts)


### psychological factors #####################################################

### overview

psyc_factor_df <- df %>%
  filter(rowSums(across(all_of(psyc_factors), ~ . != 0)) > 0) %>%
  select(all_of(c(psyc_factors, tools)))

# make combinatio of all variables pair
psych_pairs <- combn(names(psyc_factor_df), 2, simplify = FALSE)

# count each combination occurrence
psych_pair_counts <- lapply(psych_pairs, function(vars) {
  tibble(
    psych_pairs = paste(vars, collapse = " & "), # Pair name
    obs = sum(psyc_factor_df[[vars[1]]] == 1 & psyc_factor_df[[vars[2]]] == 1) # Count rows where both are TRUE
  )
}) %>%
  bind_rows() %>% # Combine results into a single table
  filter(obs != 0) %>% # remove each combination that does not occour
  separate(psych_pairs, into = c("factor", "tool"), sep = " & ") %>%
  bind_rows(tibble(
    factor = "total_psyc_factors",
    tool = "any",
    obs = as.integer(psychological_obs_count)
  )) %>%
  mutate(
    factor = case_when(
      factor == "risk_attitudes" ~ "Risk attitudes",
      factor == "risk_perceptions" ~ "Risk perceptions",
      factor == "price_yield_expectation" ~ "Price and yield expectation",
      factor == "alternative_perceptions" ~ "Alternative tools perceptions",
      factor == "network_influence" ~ "Network influence",
      factor == "subjective_norms" ~ "Subjective norms",
      factor == "management_attitudes" ~ "Management attitudes",
      factor == "innovativeness" ~ "Innovativeness",
      factor == "perceived_usefulness" ~ "Perceived usefulness",
      factor == "tools_knowledge" ~ "Tools knowledge",
      factor == "total_psyc_factors" ~ "Total",
      TRUE ~ factor
    )
  ) %>%
  mutate(tool = str_to_title(tool)) %>%
  mutate(
    tool = case_when(
      tool == 'Multiple_tools' ~ 'Multiple tools',
      tool == 'Price_insurance' ~ 'Price insurance',
      TRUE ~ tool
    )
  ) %>%
  group_by(factor) %>%
  mutate(cumsum_obs = cumsum(obs))

psych_pair_counts


### Overview plot #############################################################

# Calculate the total sum for each factor to display as a label
behav_pair_counts_total <- behav_pair_counts %>%
  group_by(factor) %>%
  summarise(total = sum(obs))

psych_pair_counts_total <- psych_pair_counts %>%
  group_by(factor) %>%
  summarise(total = sum(obs))

# behavioral preference bar chart
behav_plot <- ggplot(
  behav_pair_counts,
  aes(x = fct_reorder(factor, cumsum_obs), y = obs, fill = tool)
) +
  geom_bar(position = "stack", stat = "identity", width = 0.5) +
  labs(title = "Behavioral preferences", x = "", y = "Number of observations") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Paired", name = "Financial tool") +
  geom_text(
    data = behav_pair_counts_total,
    aes(x = factor, y = total, label = total),
    inherit.aes = FALSE, # Prevent geom_text from using aesthetics in ggplot()
    position = position_nudge(y = 2.5), # Slightly nudge the label upward
    size = 3
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.key.size = unit(0.6, "cm"), # Size of legend keys
    legend.text = element_text(size = 9), # Size of legend text
    legend.title = element_text(size = 10, face = "bold"),
    legend.position = c(0.8, 0.5),
    legend.box.background = element_rect(fill = "white"),
    axis.text.y = element_text(size = 10)
  )

behav_plot

# psychological factor bar chart
psych_plot <- ggplot(
  psych_pair_counts,
  aes(x = fct_reorder(factor, cumsum_obs), y = obs, fill = tool)
) +
  geom_bar(position = "stack", stat = "identity", width = 0.5) +
  labs(title = "Psychological factors", x = "", y = "Number of observations") +
  theme_minimal() +
  coord_flip() +
  scale_y_break(c(60, 150), scales = "fixed", space = 0.2) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 160)) +
  scale_fill_brewer(palette = "Paired") +
  geom_text(
    data = psych_pair_counts_total,
    aes(x = factor, y = total, label = total),
    inherit.aes = FALSE, # Prevent geom_text from using aesthetics in ggplot()
    position = position_nudge(y = 2.5), # Slightly nudge the label upward
    size = 3
  ) +
  expand_limits(y = 170) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x.top = element_blank(), # Removes text on the right y-axis
    axis.ticks.x.top = element_blank(),
    legend.position = "none", # Size and style of legend title)
    axis.text.y = element_text(size = 10)
  )

psych_plot

# combine the two plot
overview_plot <- behav_plot +
  psych_plot +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

overview_plot


### behavioral preferences table ####################################################

# Generate a table for each risk preference by calling the function
risk_preferences_table <- create_behavior_table(
  df,
  "risk_preferences",
  behav_pref,
  farm_type,
  tools
)

loss_aversion_table <- create_behavior_table(
  df,
  "loss_aversion",
  behav_pref,
  farm_type,
  tools
)

probability_weighting_table <- create_behavior_table(
  df,
  "probability_weighting",
  behav_pref,
  farm_type,
  tools
)

uncertainty_aversion_table <- create_behavior_table(
  df,
  "uncertainty_aversion",
  behav_pref,
  farm_type,
  tools
)

variation_aversion_table <- create_behavior_table(
  df,
  "variation_aversion",
  behav_pref,
  farm_type,
  tools
)

time_preference_behav <- create_behavior_table(
  df,
  "time_preference",
  behav_pref,
  farm_type,
  tools
)

reference_dependence_behav <- create_behavior_table(
  df,
  "reference_dependence",
  behav_pref,
  farm_type,
  tools
)

price_exp_behav <- create_behavior_table(
  df,
  "price_expectation_theor",
  behav_pref,
  farm_type,
  tools
)

# Combine tables
behavioral_preference_table <- bind_rows(
  risk_preferences_table %>% mutate(Preference = "risk_preferences"),
  loss_aversion_table %>% mutate(Preference = "loss_aversion"),
  probability_weighting_table %>% mutate(Preference = "probability_weighting"),
  uncertainty_aversion_table %>% mutate(Preference = "uncertainty_aversion"),
  variation_aversion_table %>% mutate(Preference = "variation_aversion"),
  time_preference_behav %>% mutate(Preference = "time_preference"),
  reference_dependence_behav %>% mutate(Preference = "reference_dependence"),
  price_exp_behav %>% mutate(Preference = "price_expectation")
) %>%
  mutate(
    farm_type_notes = ifelse(
      is.na(farm_type_notes),
      "not specified",
      farm_type_notes
    ),
    study_loc = ifelse(is.na(study_loc), "not specified", study_loc),
    model = case_when(
      model == "M_V" ~ "Mean-Variance",
      model == "max_EU" ~ "EU max",
      model == "stochastic_dominance" ~ "Stochastic Dominance",
      model == "max_profits" ~ "Profit max",
      model == "agency_theory" ~ "Agency Theory",
      model == "bayesian" ~ "Bayesian approach",
      model == "prospect_theory" ~ "Prospect theory",
      model == "VaR" ~ "Value at Risk"
    ),
    sign = case_when(
      sign == "positive" ~ "+",
      sign == "negative" ~ "-",
      sign == "no_direction" ~ "0"
    )
  ) %>%
  mutate(authors = paste0(authors, " (", year, ")")) %>%
  select(c(
    "Preference",
    "tool_considered",
    "authors",
    "study_loc",
    "farm_type",
    "farm_type_notes",
    "model",
    "sign"
  )) %>%
  arrange(Preference, tool_considered, model) %>%
  rename(
    Tool = tool_considered,
    Study = authors,
    Country = study_loc,
    "Farm type" = farm_type,
    "Product" = farm_type_notes,
    Model = model,
    Direction = sign
  )

behavioral_preference_table

### behavioral preferences direction plot #####################################

behav_data <- behavioral_preference_table %>%
  mutate(
    Direction = case_when(
      Direction == "+" ~ 1,
      Direction == "-" ~ -1,
      Direction == "0" ~ 0
    ),
    Preference = case_when(
      Preference == "risk_preferences" ~ "Risk preferences",
      Preference == "uncertainty_aversion" ~ "Uncertainty aversion",
      Preference == "variation_aversion" ~ "Variation aversion",
      Preference == "reference_dependence" ~ "Reference dependence",
      Preference == "loss_aversion" ~ "Loss aversion",
      Preference == "probability_weighting" ~ "Probability weighting",
      Preference == "time_preference" ~ "Time preference",
      Preference == "price_expectation" ~ "Price expectations"
    )
  )

summary_behav_data <- behav_data %>%
  group_by(Preference) %>%
  summarise(
    Positive = sum(Direction == 1),
    Negative = sum(Direction == -1),
    No_direction = sum(Direction == 0),
    Total = Positive + Negative + No_direction
  )

behav_pref_absolute <- ggplot(data = behav_data) +
  geom_col(
    aes(x = Preference, y = Direction, fill = Direction > 0),
    width = 0.5
  ) +
  scale_x_discrete(
    limits = c(
      "Price expectations",
      "Reference dependence",
      "Loss aversion",
      "Probability weighting",
      "Variation aversion",
      "Uncertainty aversion",
      "Time preference",
      "Risk preferences"
    ),
    expand = expansion(mult = c(0.15, 0.15))
  ) +
  scale_fill_manual(values = c("#79AAD9", "#5788C4"), guide = "none") +
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "solid") +
  geom_label(
    data = summary_behav_data,
    aes(x = Preference, y = 0, label = Positive + Negative),
    color = "black",
    size = 3
  ) +
  geom_label(
    aes(x = 8.7, y = 0, label = "Significant"),
    color = "black",
    size = 3
  ) +
  geom_text(
    data = summary_behav_data,
    aes(x = Preference, y = -12.5, label = Total),
    hjust = 0,
    size = 3,
    color = "black"
  ) +
  geom_text(
    data = summary_behav_data,
    aes(x = Preference, y = -10.3, label = paste0("(", No_direction, ")")),
    hjust = 0,
    size = 3,
    color = "black"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0)),
    breaks = seq(-8, 30, by = 2),
    minor_breaks = seq(-7, 29, by = 2)
  ) +
  coord_flip() +
  labs(x = "", y = "\nNumber of observations") +
  theme_minimal() +
  expand_limits(y = c(NA, max(abs(behav_data$Direction)) + 2)) +
  annotate(
    "text",
    x = 0.2,
    y = -2.5,
    label = "Negative \neffect",
    color = "black",
    size = 3
  ) +
  annotate(
    "text",
    x = 0.2,
    y = 2.5,
    label = "Positive \neffect",
    color = "black",
    size = 3
  ) +
  annotate(
    "text",
    x = 8.7,
    y = -13,
    label = "Total",
    hjust = 0,
    size = 3,
    color = "black"
  ) +
  annotate(
    "text",
    x = 8.7,
    y = -11.2,
    label = "(No effect)",
    hjust = 0,
    size = 3,
    color = "black"
  ) +
  theme(
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )

behav_pref_absolute


# Calculate percentages and counts
behav_data_pct <- behav_data %>%
  group_by(Preference) %>%
  mutate(Total = n()) %>%
  group_by(Preference, Direction) %>%
  summarise(
    Count = n(),
    Total = first(Total),
    Percentage = (n() / first(Total)) * 100,
    .groups = 'drop'
  ) %>%
  mutate(
    Direction_cat = case_when(
      Direction > 0 ~ "Positive",
      Direction < 0 ~ "Negative",
      TRUE ~ "Insignificant"
    )
  ) %>%
  mutate(
    Direction_cat = factor(
      Direction_cat,
      levels = c("Positive", "Insignificant", "Negative")
    )
  )

# Create the plot
behav_pref_percentage <- ggplot(
  data = behav_data_pct,
  aes(x = Preference, y = Count, fill = Direction_cat)
) +
  geom_col(width = 0.6) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    breaks = seq(0, 60, by = 5)
  ) +
  scale_x_discrete(
    limits = rev(c(
      "Risk preferences",
      "Time preference",
      "Reference dependence",
      "Loss aversion",
      "Price expectations",
      "Probability weighting",
      "Variation aversion",
      "Uncertainty aversion"
    ))
  ) +
  scale_fill_manual(
    values = c(
      "Positive" = "#ce9d41ff",
      "Insignificant" = "gray70",
      "Negative" = "#482f57ff"
    ),
    name = NULL
  ) +
  geom_text(
    data = behav_data_pct %>% distinct(Preference, Total),
    aes(x = Preference, y = Total, label = paste0("n = ", Total)),
    inherit.aes = FALSE,
    hjust = -0.4,
    size = 3.5,
    fontface = "bold"
  ) +
  labs(x = "", y = "\nNumber of observations") +
  theme_classic() +
  theme(
    legend.position = c(0.85, 0.25),
    legend.background = element_rect(
      fill = "white",
      color = "black",
      linewidth = 0.5
    ),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )

behav_pref_percentage


### Dispositional factors table ###############################################

risk_attitude_table <- create_psych_table(
  df,
  "risk_attitudes",
  psyc_factors,
  farm_type,
  tools
)

management_attitudes_table <- create_psych_table(
  df,
  "management_attitudes",
  psyc_factors,
  farm_type,
  tools
)

innovativeness_table <- create_psych_table(
  df,
  "innovativeness",
  psyc_factors,
  farm_type,
  tools
)

dispositional_factor_table <- bind_rows(
  risk_attitude_table %>% mutate(Factor = "Risk attitudes"),
  management_attitudes_table %>% mutate(Factor = "Management attitudes"),
  innovativeness_table %>% mutate(Factor = "Innovativeness")
) %>%
  mutate(
    Direction = case_when(
      sign == "positive" & significance == "significant" ~ "+",
      sign == "negative" & significance == "significant" ~ "-",
      significance == "non_significant" ~ "0"
    ),
    Product = ifelse(is.na(farm_type_notes), "not specified", farm_type_notes),
    Country = ifelse(is.na(study_loc), "not specified", study_loc),
    Operationalization = case_when(
      operationalization ==
        "self_reported_intention" ~ "Self reported intention",
      operationalization == "self_reported_adoption" ~ "Self reported adoption",
      operationalization == "actual_adoption" ~ "Actual adoption"
    ),
    Study = paste0(authors, " (", year, ")")
  ) %>%
  select(c(
    "Factor",
    "tool_considered",
    "Study",
    "Country",
    "farm_type",
    "Product",
    "Operationalization",
    "Direction"
  )) %>%
  rename(Tool = tool_considered, "Farm type" = farm_type)

dispositional_factor_table


### social factor table #######################################################

network_influence_table <- create_psych_table(
  df,
  'network_influence',
  psyc_factors,
  farm_type,
  tools
)

subjective_norms_table <- create_psych_table(
  df,
  "subjective_norms",
  psyc_factors,
  farm_type,
  tools
)

social_factor_table <- bind_rows(
  network_influence_table %>% mutate(Factor = "Network influence"),
  subjective_norms_table %>% mutate(Factor = "Subjective norms")
) %>%
  mutate(
    Direction = case_when(
      sign == "positive" & significance == "significant" ~ "+",
      sign == "negative" & significance == "significant" ~ "-",
      significance == "non_significant" ~ "0"
    ),
    Product = ifelse(is.na(farm_type_notes), "not specified", farm_type_notes),
    Country = ifelse(is.na(study_loc), "not specified", study_loc),
    Operationalization = case_when(
      operationalization ==
        "self_reported_intention" ~ "Self reported intention",
      operationalization == "self_reported_adoption" ~ "Self reported adoption",
      operationalization == "actual_adoption" ~ "Actual adoption"
    ),
    Study = paste0(authors, " (", year, ")")
  ) %>%
  select(c(
    "Factor",
    "tool_considered",
    "Study",
    "Country",
    "farm_type",
    "Product",
    "Operationalization",
    "Direction"
  )) %>%
  rename(Tool = tool_considered, "Farm type" = farm_type)

social_factor_table


### Cognitive factor table ####################################################

risk_perception_table <- create_psych_table(
  df,
  "risk_perceptions",
  psyc_factors,
  farm_type,
  tools
)

price_yield_expectations_psych <- create_psych_table(
  df,
  "price_yield_expectation",
  psyc_factors,
  farm_type,
  tools
)

alternative_perception_table <- create_psych_table(
  df,
  "alternative_perceptions",
  psyc_factors,
  farm_type,
  tools
)

perceived_usefulness_table <- create_psych_table(
  df,
  "perceived_usefulness",
  psyc_factors,
  farm_type,
  tools
)

tools_knowledge_table <- create_psych_table(
  df,
  "tools_knowledge",
  psyc_factors,
  farm_type,
  tools
)


cognitive_factor_table <- bind_rows(
  risk_perception_table %>% mutate(Factor = "Risk perceptions"),
  price_yield_expectations_psych %>%
    mutate(Factor = "Price and yield expectations"),
  alternative_perception_table %>%
    mutate(Factor = "Alternative tools perceptions"),
  perceived_usefulness_table %>% mutate(Factor = "Perceived usefulness"),
  tools_knowledge_table %>% mutate(Factor = "Tools knowledge")
) %>%
  mutate(
    Direction = case_when(
      sign == "positive" & significance == "significant" ~ "+",
      sign == "negative" & significance == "significant" ~ "-",
      significance == "non_significant" ~ "0"
    ),
    Product = ifelse(is.na(farm_type_notes), "not specified", farm_type_notes),
    Country = ifelse(is.na(study_loc), "not specified", study_loc),
    Operationalization = case_when(
      operationalization ==
        "self_reported_intention" ~ "Self reported intention",
      operationalization == "self_reported_adoption" ~ "Self reported adoption",
      operationalization == "actual_adoption" ~ "Actual adoption"
    ),
    Study = paste0(authors, " (", year, ")")
  ) %>%
  select(c(
    "Factor",
    "tool_considered",
    "Study",
    "Country",
    "farm_type",
    "Product",
    "Operationalization",
    "Direction"
  )) %>%
  rename(Tool = tool_considered, "Farm type" = farm_type)

cognitive_factor_table

### share of statistically significant observations ###########################

psych_data <- bind_rows(
  dispositional_factor_table,
  social_factor_table,
  cognitive_factor_table
) %>%
  mutate(
    Direction = case_when(
      Direction == "+" ~ 1,
      Direction == "-" ~ -1,
      Direction == "0" ~ 0
    )
  )

psych_percentage <- psych_data %>%
  group_by(Factor) %>%
  summarise(
    tot_stat_sign = sum(Direction != 0),
    count = n(),
    share_stat_sign = tot_stat_sign / sum(count)
  ) %>%
  mutate(
    share_tot = count / sum(count),
    share_stat_sign_test = share_stat_sign * share_tot
  )

psych_factor_share <- ggplot(data = psych_percentage) +
  geom_col(
    aes(x = Factor, y = count, fill = "Total Observations"),
    alpha = 0.7,
    width = 0.5
  ) + # First bar color
  geom_col(
    aes(x = Factor, y = tot_stat_sign, fill = "Statistically Significant"),
    alpha = 0.7,
    width = 0.5
  ) + # Second bar color
  scale_x_discrete(
    limits = c(
      "Price and yield expectations",
      "Risk perceptions",
      "Perceived usefulness",
      "Alternative tools perceptions",
      "Tools knowledge",
      "Subjective norms",
      "Network influence",
      "Management attitudes",
      "Innovativeness",
      "Risk attitudes"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Total Observations" = "lightgrey",
      "Statistically Significant" = "#5788C4"
    )
  ) + # Custom colors
  labs(fill = "Observations", x = "", y = "Number of observations") +
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.2),
    legend.box.background = element_rect(fill = "white"),
    axis.text.y = element_text(size = 10)
  ) +
  coord_flip() +
  geom_vline(
    xintercept = 5.5,
    color = "black",
    size = 0.3,
    linetype = "solid"
  ) +
  annotate(
    "text",
    x = 4.5,
    y = 40,
    label = "Cognitive",
    size = 3,
    color = "black"
  ) +
  geom_vline(
    xintercept = 7.5,
    color = "black",
    size = 0.3,
    linetype = "solid"
  ) +
  annotate(
    "text",
    x = 6.5,
    y = 40,
    label = "Social",
    size = 3,
    color = "black"
  ) +
  geom_vline(
    xintercept = 10.5,
    color = "black",
    size = 0.3,
    linetype = "solid"
  ) +
  annotate(
    "text",
    x = 9,
    y = 40,
    label = "Dispositional",
    size = 3,
    color = "black"
  )


psych_factor_share


### absolute value of positive/negative observations ##########################

summary_psych_data <- psych_data %>%
  group_by(Factor) %>%
  summarise(
    Positive = sum(Direction == 1),
    Negative = sum(Direction == -1),
    Insignificant = sum(Direction == 0),
    Total = Positive + Negative + Insignificant
  )

# operationalization fill
psych_factor_absolute <- ggplot(data = psych_data) +
  geom_col(
    aes(
      x = Factor,
      y = Direction,
      fill = interaction(Operationalization, Direction)
    ),
    width = 0.5
  ) +
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "solid") +
  labs(x = "", y = "\nNumber of statistically significant observations") +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "Self reported intention.1" = "#D79E42",
      "Self reported intention.-1" = "#E7B769",
      "Self reported adoption.1" = "#5788C4",
      "Self reported adoption.-1" = "#79AAD9",
      "Actual adoption.1" = "#5A9C87",
      "Actual adoption.-1" = "#72B7A0"
    ),
    name = "Operationalization",
    breaks = c(
      "Self reported intention.1",
      "Self reported adoption.1",
      "Actual adoption.1"
    ),
    labels = c(
      "Self reported intention to adopt",
      "Self reported adoption",
      "Actual adoption"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    breaks = seq(-4, 20, by = 2),
    minor_breaks = seq(-5, 19, by = 2)
  ) +
  scale_x_discrete(
    limits = c(
      "Price and yield expectations",
      "Risk perceptions",
      "Perceived usefulness",
      "Alternative tools perceptions",
      "Tools knowledge",
      "Subjective norms",
      "Network influence",
      "Management attitudes",
      "Innovativeness",
      "Risk attitudes"
    ),
    expand = expansion(mult = c(0.15, 0.15))
  ) +
  coord_flip() +
  geom_label(
    data = summary_psych_data,
    aes(x = Factor, y = 0, label = Positive + Negative),
    color = "black",
    size = 3
  ) +
  geom_label(
    aes(x = 10.8, y = 0, label = "Significant"),
    color = "black",
    size = 3
  ) +
  geom_text(
    data = summary_psych_data,
    aes(x = Factor, y = -8.1, label = Total),
    hjust = 0,
    size = 3,
    color = "black"
  ) +
  geom_text(
    data = summary_psych_data,
    aes(x = Factor, y = -6.5, label = paste0("(", Insignificant, ")")),
    hjust = 0,
    size = 3,
    color = "black"
  ) +
  annotate(
    "text",
    x = 0.1,
    y = -2,
    label = "Negative \neffect",
    color = "black",
    size = 3
  ) +
  annotate(
    "text",
    x = 0.1,
    y = 2,
    label = "Positive \neffect",
    color = "black",
    size = 3
  ) +
  theme(
    legend.position = c(0.8, 0.2),
    legend.box.background = element_rect(fill = "white")
  ) +
  geom_vline(
    xintercept = 5.5,
    color = "black",
    size = 0.3,
    linetype = "solid"
  ) +
  annotate(
    "text",
    x = 4,
    y = 16,
    label = "Cognitive",
    size = 3,
    color = "black"
  ) +
  geom_vline(
    xintercept = 7.5,
    color = "black",
    size = 0.3,
    linetype = "solid"
  ) +
  annotate("text", x = 6, y = 16, label = "Social", size = 3, color = "black") +
  annotate(
    "text",
    x = 9,
    y = 16,
    label = "Dispositional",
    size = 3,
    color = "black"
  ) +
  annotate(
    "text",
    x = 10.8,
    y = -8.4,
    label = "Total",
    hjust = 0,
    size = 3,
    color = "black"
  ) +
  annotate(
    "text",
    x = 10.8,
    y = -7.2,
    label = "(Insignificant)",
    hjust = 0,
    size = 3,
    color = "black"
  ) +
  theme(
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )

psych_factor_absolute
