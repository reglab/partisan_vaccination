# all of the functions associated with partisan_difference_regression and partisan_difference_policy

##### Loading data #######
process_data <- function(filename, time_var, 
                         filter_yr, filter_outcome_time, filter_engage_time,
                         remove_after_4 = FALSE){
  data = read_csv(paste0('/share/pi/deho-pi/AFC/mortonc/processed/',filename))
  
  data = data %>%
    filter(years_to_engage < filter_engage_time)
  
  data$time_to_event = data[time_var]
  
  data = data %>%
    filter(time_to_event >= 0)
  
  data$indic = (data$time_to_event < filter_outcome_time)
  data$birth_year = as.numeric(substr(data$dob, 1, 4))
  data = data %>%
    filter(birth_year >= filter_yr)
  
  if(remove_after_4){
    data = data %>%
      filter(time_to_event < 3+11/12 | time_to_event == 100) # remove cases where the time to event is after 3 yrs 11 months
  }
  # add states as regions (from https://github.com/cphalpert/census-regions/blob/master/us%20census%20bureau%20regions%20and%20divisions.csv)
  url = "https://raw.githubusercontent.com/cphalpert/census-regions/7bdc6aa1cb0892361e90ce9ad54983041c2ad015/us%20census%20bureau%20regions%20and%20divisions.csv"
  regions = read_csv(url)
  regions$state = regions$`State Code`
  
  data = merge(regions,  data)
  data$region = data$Region
  
  data <- data %>%
    filter(party != "Bipartisan")
  
  # Remove unknowns from the data
  data <- data %>%
    filter(gender != "Other/Unknown" &
             urban_rural != "Unknown")
  
  # indic is true if vaccinated by age 4.
  data$gender = as.factor(data$gender)
  data$region = as.factor(data$region)
  data$race = as.factor(data$race)
  data$hispanic = as.factor(data$hispanic)
  data$practiceid = as.factor(data$practiceid)
  data$party = as.factor(data$party)
  data$policy = as.factor(data$policy)
  data$urban_rural = as.factor(data$urban_rural)
  
  # work in percentage points
  data$D <- as.numeric(data$party == "Democratic")
  data$R <- as.numeric(data$party == "Republican")
  
  data$year <- as.factor(data$birth_year)
  data$year_num <- as.numeric(as.character(data$year))
  
  return(data)
}
####### Plotting disparities and histograms by year #########
plot_year_disparity_party <- function(data, 
                                      outcome,
                                      party_baseline = "D", 
                                      title = "Partisan Refusal Rate by Year (Democrat-Republican)", 
                                      ylab = "Estimated Gap in Refusal Rate\n(Proportion)",
                                      line_color = "darkslategray4"){
  # Fit model
  model_by_year <- feols(as.formula(paste0(outcome, " ~ ", party_baseline, ":year | year")), data = data, vcov = "hetero")
  
  # Extract model estimates
  gap_df <- coefplot(model_by_year, vcov = "hetero", plot = FALSE)$prms %>%
    mutate(year = as.numeric(gsub(paste0(party_baseline, ":year"), "", estimate_names)),
           year_label = year)
  
  # Print p-values by year
  model_summary <- summary(model_by_year)
  coef_df <- as.data.frame(model_summary$coeftable)
  print(coef_df)
  
  # Calculate x breaks every 5 years
  x_range <- range(c(gap_df$year_label), na.rm = TRUE)
  x_breaks <- seq(from = floor(x_range[1] / 5) * 5, to = ceiling(x_range[2] / 5) * 5, by = 5)
  
  # Main plot (estimates)
  main_plot <- ggplot(gap_df, aes(x = year_label, y = estimate)) +
    geom_line(color = line_color) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = line_color, alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(breaks = x_breaks) +
    labs(
      title = title,
      x = NULL,
      y = ylab
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_line(color = "grey80")
    )+
    coord_cartesian(xlim=c(min(data$birth_year), max(data$birth_year)))
  return (main_plot)
}


plot_year_disparity_hist <- function(data, 
                                     outcome,
                                     party_baseline = "D", 
                                     hist_title = "Number of Observations per Year",
                                     hist_color = "steelblue",
                                     labFalse = "False",
                                     labTrue = "True",
                                     legend = TRUE) {
  
  
  # Histogram counts
  count_df <- data %>%
    group_by(year_num) %>%
    summarise(
      indic_true = ifelse(outcome == 'indic', sum(!!rlang::sym(outcome), na.rm = TRUE), 0)/100,
      total = n(),
      .groups = 'drop'
    ) %>%
    mutate(indic_false = total - indic_true,
           year_label = year_num)
  
  # Reshape for stacked bars
  bar_df <- count_df %>%
    select(year_label, indic_true, indic_false) %>%
    pivot_longer(cols = c("indic_false", "indic_true"),
                 names_to = "indic_type",
                 values_to = "count") %>%
    mutate(indic_type = factor(
      indic_type,
      levels = c("indic_false", "indic_true"),
      labels = c("Indic = FALSE", "Indic = TRUE")
    ))
  
  # Calculate x breaks every 5 years
  x_range <- range(c(count_df$year_label), na.rm = TRUE)
  x_breaks <- seq(from = floor(x_range[1] / 5) * 5, to = ceiling(x_range[2] / 5) * 5, by = 5)
  
  # Legend labels
  legend_labels <- c("Indic = FALSE" = labFalse, "Indic = TRUE" = labTrue)
  
  # Histogram (stacked bars)
  hist_plot <- ggplot(bar_df, aes(x = year_label, y = count, fill = indic_type)) +
    geom_col(position = "stack",show.legend=legend) +
    scale_fill_manual(
      values = c(
        "Indic = TRUE" = hist_color, 
        "Indic = FALSE" = "grey"
      ),
      labels = legend_labels,
      name = NULL
    ) +
    scale_x_continuous(breaks = x_breaks) +
    labs(
      title = hist_title,
      x = "Birth Year",
      y = "Count"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(size = 10),
      axis.text.x = element_text(hjust = .5),
      panel.grid.major.x = element_line(color = "grey80"),
      #legend.position = c(0.1, 0.75),  
      #legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
      #legend.key.size = unit(0.4, "cm"),
      #legend.text = element_text(size = 8)
      legend.position = "top",
      legend.justification = "right",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.margin = margin(t = -10, r = 0, b = 0, l = 0),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.key.size = unit(0.4, "cm"),
      legend.text = element_text(size = 8),
      
      # Expand right-side margin so there's space
      plot.margin = margin(t = 10, r = 40, b = 5, l = 5)
    )
  return (hist_plot)
}

plot_year_disparity <- function(data, 
                                outcome,
                                party_baseline = "D", 
                                title = "Partisan Refusal Rate by Year (Democrat-Republican)", 
                                hist_title = "Number of Observations per Year",
                                ylab = "Estimated Gap in Refusal Rate",
                                line_color = "darkslategray4",
                                hist_color = "darkslategray4",
                                labFalse = "False",
                                labTrue = "True",
                                part = '',
                                legend = TRUE) {
  main_plot = plot_year_disparity_party(data, outcome, party_baseline, title, ylab, line_color)
  hist_plot = plot_year_disparity_hist(data, outcome, party_baseline, hist_title, hist_color, labFalse, labTrue, legend)
  
  # Combine using patchwork
  combined_plot <- main_plot / hist_plot + 
    plot_layout(heights = c(4, 0.6)) + 
    labs(tag = part) +
    theme(plot.tag = element_text())
  
  return(combined_plot)
}

####### Interacting party and year #######
add_space_before_positive <- function(value) {
  # Regular expression to check if the number is positive (ignores the stars)
  if (grepl("^[0-9]", toString(value))) {
    return(paste0("\\phantom{-}", toString(value)))
  } else {
    return(value)  # Keep negative values as they are
  }
}

format_for_model <- function(data, outcome, ref_year){
  # Set reference levels
  data$gender <- factor(data$gender, levels = c("M", "F"))
  data$race <- factor(data$race, levels = c(
    "white", "american indian or alaska native", "asian",
    "black or african american", "multiple races",
    "native hawaiian or other pacific islander", "unknown"
  ))
  data$hispanic <- factor(data$hispanic, levels = c("not hispanic or latino", "hispanic or latino", "unknown"))
  data$region <- factor(data$region, levels = c("Midwest", "Northeast", "South", "West"))
  data$urban_rural <- factor(data$urban_rural, levels = c("Urban", "Rural"))
  data$State <- factor(data$State)
  data$year_num <- data$year_num - ref_year
  data$practiceid <- factor(data$practiceid)
  return(data)
}

add_stars_to_odds_ratios <- function(df) {
  df$stars <- cut(as.numeric(df$p.value),
                  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                  labels = c("***", "**", "*", ".", ""))
  df$estimate_star <- paste0('\\makecell[l]{', 
                             mapply(add_space_before_positive, as.numeric(formatC(df$estimate, digits = 2))), 
                             df$stars,
                             "\\\\ (",mapply(formatC, df$std.error, format = "e", digits = 2),")}")
  
  #df$estimate_star <- paste0(round(df$estimate, 3), df$stars, "\n(", round(df$std.error, 3), ")")
  df <- df[, c("term", "estimate_star")]
  names(df) <- c("Variable", "Value")
  return(df)
}

add_std_err <- function(df) {
  df$stars <- cut(as.numeric(df$p.value),
                  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                  labels = c("***", "**", "*", ".", ""))
  df$estimate_stderr <- paste0(mapply(add_space_before_positive, formatC(df$estimate, format = "f", digits = 3, flag = "0")),
                               " (",mapply(formatC, -df$std.error*1.96+df$estimate, format = "f", digits = 2),',',
                               mapply(formatC, df$std.error*1.96+df$estimate, format = "f", digits = 2),')')
  #df$estimate_star <- paste0('\\makecell[l]{', 
   #                          mapply(add_space_before_positive, as.numeric(formatC(df$estimate, digits = 2))), 
   #                          df$stars,
   #                          "\\\\ (",mapply(formatC, df$std.error, format = "e", digits = 2),")}")
  df$p_val <- ifelse(df$p.value < .001, "<0.001", formatC(df$p.value, digits = 2))
  #df$estimate_star <- paste0(round(df$estimate, 3), df$stars, "\n(", round(df$std.error, 3), ")")
  df <- df[, c("term", "estimate_stderr", "p_val")]
  names(df) <- c("Variable", "Value", "P-value")
  return(df)
}

format_table_econometric <- function(tab, state_FE, x){
  #Apply and append stars to the odds ratio column
  tab_all <- merge(
    add_stars_to_odds_ratios(as.data.frame(tab$minimal)),
    merge(
      add_stars_to_odds_ratios(as.data.frame(tab$medium)),
      merge(add_stars_to_odds_ratios(as.data.frame(tab$maximal)),
            merge(add_stars_to_odds_ratios(as.data.frame(tab$state)),
                  add_stars_to_odds_ratios(as.data.frame(tab$prac)), by="Variable", all = TRUE), 
            by="Variable", all = TRUE),
      by = "Variable", all = TRUE
    ),
    by = "Variable", all = TRUE
  )
  names(tab_all) <- c("Variable", "1", "2", "3", "4", "5")
  
  lastrow = tab_all[nrow(tab_all),]
  
  #if(state_FE){
  #  tab_all[4:(nrow(tab_all)),] = tab_all[3:(nrow(tab_all)-1),]
  #  tab_all[3,] = lastrow
  #} else{
    tab_all[5:(nrow(tab_all)),] = tab_all[4:(nrow(tab_all)-1),]
    tab_all[4,] = lastrow
    tab_all = tab_all[2:(nrow(tab_all)),] # remove intercept
  #}
  
  if (x) {
    policy_row <- c('Policy Controls', 'X', 'X', 'X', 'X', 'X')
    regional_row <- c('Regional Controls', '', 'X', 'X', 'X', 'X')
    personal_row <- c('Individual Controls', '', '', 'X', 'X', 'X')
    state_row <- c('State FE', '', '', '', 'X', 'X')
    prac_row <- c('Practice FE', '', '', '', '','X')
    
    tab_all <- tab_all[1:3, ]
    tab_all <- rbind(tab_all, policy_row, regional_row, personal_row, state_row, prac_row)
  }
  return(tab_all)
}

format_table_jama <- function(tab, state_FE, x, party_reference = 'D'){
  #Apply and append stars to the odds ratio column
  tab_all <- merge(
    add_std_err(as.data.frame(tab$minimal)),
    merge(
      add_std_err(as.data.frame(tab$medium)),
      merge(add_std_err(as.data.frame(tab$maximal)),
            merge(add_std_err(as.data.frame(tab$state)),
                  add_std_err(as.data.frame(tab$prac)), by="Variable", all = TRUE), 
            by="Variable", all = TRUE),
      by = "Variable", all = TRUE
    ),
    by = "Variable", all = TRUE
  )
  tab_all <- tab_all[tab_all['Variable'] == paste0(party_reference,':year_num'),]
  
  tab_all <- tab_all %>%
    mutate(across(-Variable, as.character))
  
  long_df <- tab_all %>%
    pivot_longer(
      cols = -Variable,
      names_to = "Measurement",
      values_to = "Val"
    ) %>%
    mutate(Type = if_else(grepl("^P", Measurement), "p value", "Value"),
           Group = stringr::str_extract(Measurement, "(\\.x\\.1|\\.y\\.1|\\.x|\\.y|\\.1)$"),
           Group = if_else(is.na(Group), "0", Group))
  
  final_df <- long_df %>%
    select(Variable, Group, Type, Val) %>%
    pivot_wider(names_from = Type, values_from = Val) %>%
    select(-Group)  # Remove Group if you don't need it
  
  final_df$Variable <- c("Policy controls", "+Regional controls", "+Individual controls", "+State Fixed Effects", "+Practice Fixed Effects")

  return(final_df)
}

interaction_model <- function(data, ref_year, outcome, party_reference = 'D', state_FE = FALSE, x = FALSE){
  data = format_for_model(data, 'indic', ref_year)
  
  control_terms <- list(
    minimal = "",
    medium = "+ region + urban_rural + share_republican",
    maximal = "+ region + urban_rural + share_republican + gender + race + hispanic",
    state = "+ region + urban_rural + share_republican + gender + race + hispanic",
    prac = "+ region + urban_rural + share_republican + gender + race + hispanic"
  )
  
  tab <- list()
  #means <- list()
  ns <- list()
  
  for (term in names(control_terms)) {
    base_formula <- paste0(outcome, " ~ ", party_reference, " * year_num + policy ", control_terms[[term]])
    if(term == 'prac'){
      full_formula <-  as.formula(paste0(base_formula, "| practiceid + State"))
    } else if (term == 'state'){
      full_formula <-  as.formula(paste0(base_formula, "| State"))
    } else{
      full_formula <- as.formula(base_formula)
    }

    model <- feols(full_formula, data = data)
    
    # Store tidy summary of fixed effects
    #tab[[term]] <- broom::tidy(model)
    tab[[term]] <- broom::tidy(model)
    
    #tab[[term]] <- broom.mixed::tidy(model, effects = "fixed")
    #means[[term]] <- mean(data[outcome])
    ns[[term]] <- nrow(data)
  }
  
  # Extract p-values
  extract_p <- function(df, term_label) {
    row <- df[df$term == term_label, ]
    if (nrow(row) == 0) return(NA)
    return(row$p.value)
  }
  
  term_name <- paste0("", party_reference, ":year_num")
  
  p_val_min <- extract_p(tab$minimal, term_name)
  p_val_med <- extract_p(tab$medium, term_name)
  p_val_max <- extract_p(tab$maximal, term_name)
  p_val_state <- extract_p(tab$state, term_name)
  p_val_prac <- extract_p(tab$prac, term_name)
  
  print(c(p_val_min, p_val_med, p_val_max, p_val_state, p_val_prac))
 
  #tab_all = format_table_econometric(tab, state_FE, x = TRUE)
  tab_all = format_table_jama(tab, state_FE, x = TRUE, party_reference = party_reference)
  
  
  return(tab_all)
}

rename_lookup <- c(
  "(Intercept)" = "Intercept",
  "D" = "Democratic Household",
  "R" = "Republican Household",
  "year_num" = "Birth Year",
  "policy1" = "Religious Exemptions",
  "policy2" = "Personal and Religious Exemptions",
  "regionNortheast" = "Northeast",
  "regionSouth" = "South",
  "regionWest" = "West",
  "urban_ruralRural" = "Rural",
  "share_republican" = "County Share Republican",
  "genderF" = "Female",
  "raceamerican indian or alaska native" = "American Indian/Alaska Native",
  "raceasian" = "Asian",
  "raceblack or african american" = "Black/African American",
  "racemultiple races" = "Multiple Races",
  "racenative hawaiian or other pacific islander" = "Hawaiian/Pacific Islander",
  "raceunknown" = "Unknown",
  "hispanichispanic or latino" = "Hispanic/Latino",
  "hispanicunknown" = "Unknown",
  "R:year_num" = "Republican Household:Birth Year",
  "D:year_num" = "Democratic Household:Birth Year",
  'Policy Controls' = 'Policy Controls',
  'Regional Controls' = 'Regional Controls',
  'Individual Controls' = 'Individual Controls',
  'State FE' = 'State FE'
)


######## POLICY ANALYSIS (OLD) #######
# Tests if gaps within each policy type are significantly different from zero
interaction_model_policy <- function(data, party_reference, ref_year){
  # Set reference levels manually
  data$gender <- factor(data$gender, levels = c("M", "F"))
  data$race <- factor(data$race, levels = c("white", "american indian or alaska native", "asian", "black or african american", "multiple races", "native hawaiian or other pacific islander", "unknown"))
  data$hispanic <- factor(data$hispanic, levels = c("not hispanic or latino", "hispanic or latino", "unknown"))          # No = reference
  data$region <- factor(data$region, levels = c("Midwest", "Northeast", "South", "West"))  # Northeast = reference
  data$urban_rural <- factor(data$urban_rural, levels = c("Urban", "Rural"))  # Northeast = reference
  data$State <- factor(data$State)
  data$year_num = data$year_num-ref_year
  
  control_terms <- list(
    minimal = "",
    medium = "+ region + urban_rural + share_republican",
    maximal = "+ region + urban_rural + share_republican + gender + race + hispanic "
  )
  
  tab_med <- list()
  means_med <- list()
  ns_med <- list()
  
  tab_nonmed <- list()
  means_nonmed <- list()
  ns_nonmed <- list()
  
  data_med = data %>%
    filter(policy == 0)
  data_nonmed = data %>%
    filter(policy != 0)
  
  helper_run_models <- function(full_formula, data_med, data_nonmed){
    model_med <- feols(full_formula, data = data_med, vcov = "hetero")
    model_nonmed <- feols(full_formula, data = data_nonmed, vcov = "hetero")
    tab_med <- summary(model_med)$coeftable
    tab_nonmed <- summary(model_nonmed)$coeftable
    
    #means_med[[term]] <- mean(data_med$indic)
    #means_nonmed[[term]] <- mean(data_nonmed$indic)
    #ns_med[[term]] <- nrow(data_med)
    #ns_nonmed[[term]] <- nrow(data_nonmed)
    #return(tab_nonmed)
    #return(as.data.frame(add_stars_to_odds_ratios(as.data.frame(tab_med))))
    return(list(as.data.frame(add_stars_to_odds_ratios(as.data.frame(tab_med))), 
                as.data.frame(add_stars_to_odds_ratios(as.data.frame(tab_nonmed)))))
  }
  
  # start with no controls, no FE
  full_formula <- as.formula(paste("indic ~ ",party_reference," * year_num"))
  mod_1 = helper_run_models(full_formula, data_med, data_nonmed)
  
  # then no controls, FE
  full_formula <- as.formula(paste("indic ~ ",party_reference," * year_num", "|State"))
  mod_2 = helper_run_models(full_formula, data_med, data_nonmed)
  
  # then FE and controls
  full_formula <- as.formula(paste("indic ~ ",party_reference," * year_num", control_terms['maximal'], "|State"))
  mod_3 = helper_run_models(full_formula, data_med, data_nonmed)
  
  tab_med = merge(mod_1[[1]], merge(mod_2[[1]], mod_3[[1]], by = "Variable", all = TRUE), by = "Variable", all = TRUE)
  tab_nonmed = merge(mod_1[[2]], merge(mod_2[[2]], mod_3[[2]], by = "Variable", all = TRUE), by = "Variable", all = TRUE)
  tab_all = merge(tab_med, tab_nonmed, by = "Variable", all = TRUE)
  
  tab_all = tab_all[match(tab_all$Variable, tab_all$Variable),]
  
  rownames(tab_all) = 1:nrow(tab_all)
  
  lastrow = tab_all[nrow(tab_all),]
  tab_all[4:(nrow(tab_all)),] = tab_all[3:(nrow(tab_all)-1),]
  tab_all[3,] = lastrow
  
  colnames(tab_all) = c("Variable", "1", "2", "3", "4", "5", "6")
  
  # replace rows with Xs
  control_row     <- c('Regional/Individual Controls', '', '', 'X', '', '', 'X')
  fe_row     <- c('State FE', '', 'X', 'X', '', 'X', 'X')
  tab_all = tab_all[2:4,] # get only party, year_num, and party:year_num
  tab_all <- rbind(
    tab_all,
    control_row,
    fe_row
  )
  
  return(tab_all)
}

##### POLICY ANALYSIS (NEW) #######
# tests if gaps from policy type to policy type are significantly different from one another
compare_policies <- function(data, term){
  control_terms <- list(
    Minimal = c(),
    Medium = c("region", "urban_rural" ,"share_republican"),
    Maximal = c("region", "urban_rural", "share_republican", "gender", "race", "hispanic")
  )
  data$policy <- ifelse(data$policy == 0, 0, 1)
  data$policy = as.factor(data$policy)
  
  #data$policy <- ifelse(data$policy == 0, "Medical", "Nonmedical")
  full_formula <- as.formula(paste0("indic ~ D * policy + D * birth_year ", 
                                    ifelse(length(control_terms[[term]])==0, 
                                           "", 
                                           paste("+", paste(control_terms[[term]], collapse = " + "))) ,
                                    "|State"))
  
  model <- feols(full_formula, data = data, vcov = "hetero")
  
  pred_data <- expand.grid(
    D = c(0, 1),
    policy = unique(data$policy),
    birth_year = mean(data$birth_year, na.rm = TRUE)
  )
  
  # Add other control terms (use mean or mode)
  for (var in c('State', names(data)[names(data) %in% control_terms[[term]]])) {
    if (is.numeric(data[[var]])) {
      pred_data[[var]] <- mean(data[[var]], na.rm = TRUE)
    } else {
      pred_data[[var]] <- data[[var]][1]
    }
  }
  
  pred_data$pred <- predict(model, newdata = pred_data)
  coefs <- coef(model)
  vcv   <- vcov(model)
  birth_mean = mean(data$birth_year, na.rm = TRUE)
  
  make_row <- function(D_val, policy_val) {
    data.frame(
      D = D_val,
      policy = factor(policy_val, levels = c(0, 1)),
      birth_year = birth_mean
    )
  }
  
  row1 <- model.matrix(~ D * policy + D * birth_year, data = make_row(0, 1))
  row2 <- model.matrix(~ D * policy + D * birth_year, data = make_row(1, 1))
  row3 <- model.matrix(~ D * policy + D * birth_year, data = make_row(0, 0))
  row4 <- model.matrix(~ D * policy + D * birth_year, data = make_row(1, 0))
  
  pred1 <- pred_data$pred[pred_data$D == 0 & pred_data$policy == 1]
  pred2 <- pred_data$pred[pred_data$D == 1 & pred_data$policy == 1]
  pred3 <- pred_data$pred[pred_data$D == 0 & pred_data$policy == 0]
  pred4 <- pred_data$pred[pred_data$D == 1 & pred_data$policy == 0]
  
  gap_0 <- pred2 - pred1  # Dem - Rep in Nonmedical
  gap_1 <- pred4 - pred3  # Dem - Rep in Medical
  diff_gap <- gap_1 - gap_0
  
  C <- row4 - row3 - row2 + row1  # This is the difference vector
  C <- as.numeric(C)  # Make sure it's numeric
  
  names(C) <- colnames(row1)
  C_full <- rep(0, length(coefs))
  names(C_full) <- names(coefs)
  C_full[names(C)] <- C  # Insert only values for matched terms
  C_full <- C_full[colnames(vcv)]
  se <- sqrt(t(C_full) %*% vcv %*% C_full)
  z_stat <- diff_gap / se
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  

  return(c(paste0(formatC(diff_gap, format = "f", digits = 2), 
    paste0(" (", 
           formatC(diff_gap-1.96*se, format = "f", digits = 2), ", ",
           formatC(diff_gap+1.96*se, format = "f", digits = 2), ")")),
    ifelse(p_value<.001, "<0.001", formatC(p_value, format = "f", digits = 2))))
  # cat(paste("Gap: ", diff_gap, 
  #           "\nSE: ", se, 
  #           "\nLower Bound: ", diff_gap - 1.96*se, 
  #           "\nUpper Bound: ", diff_gap + 1.96*se, 
  #           "\nP-value: ", p_value))
  # stars = ifelse(p_value < .001, "***", 
  #                ifelse(p_value < .01, "**", 
  #                       ifelse(p_value < .05, "*", "")))
  # return(paste0(round(diff_gap, 3), stars, "\n(", round(se, 3), ")"))
}