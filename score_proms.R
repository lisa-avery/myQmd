# CanRehab Project 3: PROM Scoring ----
# Each function takes the full data frame and returns it with score columns appended

# Helper: prorate a score if no more than `max_missing` items are missing ----
# Returns NA otherwise. Used by SDI, GAD-7, PHQ-9.
prorate_score <- function(data, items, max_missing = 2) {
  temp <- data[, items, drop = FALSE]
  n_items <- length(items)
  n_answered <- rowSums(!is.na(temp))
  raw_sum <- rowSums(temp, na.rm = TRUE)
  ifelse(
    (n_items - n_answered) <= max_missing & n_answered > 0,
    raw_sum * (n_items / n_answered),
    NA_real_
  )
}


# WHO-DAS 2.0 (12-item) ----
# Items scored 0-4 (None=0 to Extreme/Cannot do=4)
# Simple sum range: 0-48
score_whodas <- function(df) {
  whodas_items <- c(
    "standing",
    "taking_care",
    "learning",
    "community",
    "emotional",
    "concentrating",
    "walking",
    "washing",
    "dressing",
    "people",
    "friendship",
    "work"
  )

  whodas_num <- df |>
    select(all_of(whodas_items)) |>
    mutate(rw = 1:n()) |>
    pivot_longer(!rw) |>
    mutate(
      score = recode_values(
        value,
        "None" ~ 0,
        "Mild" ~ 1,
        "Moderate" ~ 2,
        "Severe" ~ 3,
        "Extreme/ Cannot do" ~ 4
      )
    ) |>
    select(!value) |>
    pivot_wider(values_from = score) |>
    arrange(rw)

  # Sum score (0-48) ----
  df$whodas_total <- rowSums(whodas_num[, whodas_items], na.rm = TRUE)
  df$whodas_n_missing <- rowSums(is.na(whodas_num[, whodas_items]))
  df$whodas_total <- 12 / (12 - df$whodas_n_missing) * df$whodas_total
  # If more than one item is missing, set total to NA ----
  df$whodas_total[which(df$whodas_n_missing > 1)] <- NA

  cbind(df$whodas_total, rowSums(whodas_num[, whodas_items], na.rm = TRUE))
  attr(df$whodas_total, "label") <- "WHODAS SF Total Score"
  attr(df$whodas_total, "miss_handling") <-
    "Score set to NA if more than 1 item is missing; otherwise prorated sum score"
  df
}


# SF-36 Physical Function (10-item) ----
# Items scored 1-3 (limited a lot=1 to not limited=3), transformed to 0-100
score_sf36_pf <- function(df) {
  sf36_pfi_items <- c(
    "vigorous_activity",
    "moderate_activity",
    "grocery",
    "several_flights_stairs",
    "one_flight_stairs",
    "bending",
    "walking_more_one",
    "walking_several",
    "walking_one",
    "bathing"
  )

  outRangeNA <- function(x, Min = 1L, Max) {
    replace(x, x < Min | x > Max, NA)
  }

  X <- df |>
    mutate(rw = 1:n()) |>
    select(rw, all_of(sf36_pfi_items)) |>
    pivot_longer(cols = !rw) |>
    mutate(
      score = case_when(
        value == "No, not limited at all" ~ 3,
        value == "Yes, limited a little" ~ 2,
        value == "Yes, limited a lot" ~ 1,
        TRUE ~ NA
      )
    ) |>
    select(!value) |>
    pivot_wider(names_from = name, values_from = score) |>
    arrange(rw)

  X[, sf36_pfi_items] <- lapply(X[, sf36_pfi_items], outRangeNA, Max = 3L)
  X$PFNUM <- apply(X[, sf36_pfi_items], 1, function(x) sum(!is.na(x)))
  X$PFMEAN <- apply(X[, sf36_pfi_items], 1, mean, na.rm = TRUE)
  X[, sf36_pfi_items] <- lapply(
    X[, sf36_pfi_items],
    function(x, y) ifelse(!is.na(x), x, y),
    y = X$PFMEAN
  )
  X$RAWPF <- ifelse(
    X$PFNUM >= 5,
    apply(X[, sf36_pfi_items], 1, sum, na.rm = TRUE),
    NA
  )
  df$sf36_PF <- with(X, ((RAWPF - 10) / (30 - 10)) * 100)
  df$sf36_pf_n_missing <- rowSums(is.na(df[, sf36_pfi_items]))

  attr(df$sf36_PF, "label") <- "SF36 Physical Functioning"
  attr(df$sf36_PF, "miss_handling") <-
    "Missing items imputed with person mean; score set to NA if fewer than 5 of 10 items answered; final score rescaled to 0-100"
  df
}


# SDI (21-item Social Difficulties Inventory) ----
# Items scored 0-3 (No difficulty=0, A little=1, Quite a bit=2, Very much=3)
# Three subscales + 5 standalone items
# Prorated if no more than 2 items missing
score_sdi <- function(df) {
  # Everyday Living subscale ----
  everyday <- c(
    "independence",
    "domestic_chores",
    "personal_care",
    "dependants",
    "support_for_family",
    "mobility"
  )

  # Money Matters subscale ----
  money <- c(
    "benefits",
    "financial_difficulties",
    "financial_services",
    "work_concerns"
  )

  # Self and Others subscale ----
  self_others <- c(
    "communication",
    "communication_with_others",
    "sexual_concerns",
    "family_planning",
    "body_image",
    "isolation"
  )

  # Additional standalone items ----
  standalone <- c(
    "future_planning",
    "living_environment",
    "rectreation_activities",
    "travel_or_holiday",
    "everyday_life"
  )

  all_sdi <- c(everyday, money, self_others, standalone)
  sdi_16_items <- c(everyday, money, self_others)

  # Convert the scores to numeric ----
  sdi_numeric <- df |>
    select(all_of(all_sdi)) |>
    mutate(rw = 1:n()) |>
    pivot_longer(!rw) |>
    mutate(
      num_value = case_when(
        value == "No difficulty" ~ 0,
        value == "A little" ~ 1,
        value == "Quite a bit" ~ 2,
        value == "Very much" ~ 3,
        TRUE ~ NA
      )
    ) |>
    select(!value) |>
    pivot_wider(values_from = num_value) |>
    arrange(rw)

  # Prorated subscale and total scores ----
  df$sdi_everyday <- prorate_score(sdi_numeric, everyday, max_missing = 1)
  df$sdi_money <- prorate_score(sdi_numeric, money, max_missing = 1)
  df$sdi_self_others <- prorate_score(sdi_numeric, self_others, max_missing = 1)
  df$sdi_total <- prorate_score(sdi_numeric, all_sdi)
  df$sdi_16 <- prorate_score(sdi_numeric, sdi_16_items)

  df$sdi_n_missing <- rowSums(is.na(sdi_numeric[, all_sdi]))

  attr(df$sdi_everyday, "label") <- "SDI Everyday"
  attr(df$sdi_money, "label") <- "SDI Money"
  attr(df$sdi_self_others, "label") <- "SDI Self & Others"
  attr(df$sdi_total, "label") <- "SDI Total"
  attr(df$sdi_16, "label") <- "SDI 16"

  miss_txt <- "Prorated (mean of non-missing * n_items) if no more than 1 item missing, else NA"
  attr(df$sdi_everyday, "miss_handling") <- miss_txt
  attr(df$sdi_money, "miss_handling") <- miss_txt
  attr(df$sdi_self_others, "miss_handling") <- miss_txt
  attr(
    df$sdi_total,
    "miss_handling"
  ) <- "Prorated (mean of non-missing * n_items) if no more than 2 item missing, else NA"
  attr(
    df$sdi_16,
    "miss_handling"
  ) <- "Prorated (mean of non-missing * n_items) if no more than 2 item missing, else NA"

  df
}


# GAD-7 (Generalized Anxiety Disorder) ----
# Items scored 0-3 (not at all=0, several days=1, more than half=2, nearly every day=3)
# Total range: 0-21; prorated if no more than 2 items missing
score_gad7 <- function(df) {
  gad7_items <- c(
    "nervous_edge",
    "worrying",
    "worrying_toomuch",
    "trouble_relaxing",
    "restless",
    "irritable",
    "afraid"
  )

  # Convert the scores to numeric ----
  gad_numeric <- df |>
    select(all_of(gad7_items)) |>
    mutate(rw = 1:n()) |>
    pivot_longer(!rw) |>
    mutate(
      num_value = as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", value))
    ) |>
    select(!value) |>
    pivot_wider(values_from = num_value) |>
    arrange(rw)

  # Prorated total score (0-21) ----
  df$gad7_total <- prorate_score(gad_numeric, gad7_items)

  # Severity category ----
  df$gad7_severity <- cut(
    df$gad7_total,
    breaks = c(-Inf, 4, 9, 14, 21),
    labels = c("minimal", "mild", "moderate", "severe"),
    right = TRUE
  )

  # # Clinical threshold (>=10) ----
  # df$gad7_clinical <- as.integer(df$gad7_total >= 10)

  # Difficulty item stored separately ----
  df$gad7_difficulty <- df$anxiety_difficulty

  df$gad7_n_missing <- rowSums(is.na(gad_numeric[, gad7_items]))

  attr(df$gad7_total, "label") <- "GAD-7 Total Score"
  attr(df$gad7_severity, "label") <- "GAD-7 Severity"
  # attr(df$gad7_clinical, "label") <- "GAD-7 Clinical Threshold"
  attr(df$gad7_difficulty, "label") <- "GAD-7 Difficulty"

  attr(df$gad7_total, "miss_handling") <-
    "Prorated (mean of non-missing * 7) if no more than 2 items missing, else NA"
  attr(df$gad7_severity, "miss_handling") <-
    "Derived from gad7_total; NA if gad7_total is NA"
  # attr(df$gad7_clinical, "miss_handling") <-
  #   "Derived from gad7_total; NA if gad7_total is NA"
  attr(df$gad7_difficulty, "miss_handling") <-
    "Single-item score; NA if the source item is missing"

  df
}


# PHQ-9 (Patient Health Questionnaire) ----
# Items scored 0-3 (not at all=0, several days=1, more than half=2, nearly every day=3)
# Total range: 0-27; prorated if no more than 2 items missing
score_phq9 <- function(df) {
  phq9_items <- c(
    "interest",
    "depression_or_hopelessness",
    "trouble_with_sleep",
    "tiredness_energy",
    "appetite_overeating",
    "self_esteem",
    "concentration",
    "speech",
    "self_harm_suicidal"
  )

  # Convert the scores to numeric ----
  phq_numeric <- df |>
    select(all_of(phq9_items)) |>
    mutate(rw = 1:n()) |>
    pivot_longer(!rw) |>
    mutate(
      num_value = as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", value))
    ) |>
    select(!value) |>
    pivot_wider(values_from = num_value) |>
    arrange(rw)

  # Prorated total score (0-27) ----
  df$phq9_total <- prorate_score(phq_numeric, phq9_items)

  # Severity category ----
  df$phq9_severity <- cut(
    df$phq9_total,
    breaks = c(-Inf, 4, 9, 14, 19, 27),
    labels = c(
      "none-minimal",
      "mild",
      "moderate",
      "moderately severe",
      "severe"
    ),
    right = TRUE
  )

  # Clinical threshold (>=10) ----
  df$phq9_clinical <- as.integer(df$phq9_total >= 10)

  df$phq9_n_missing <- rowSums(is.na(phq_numeric[, phq9_items]))

  attr(df$phq9_total, "label") <- "PHQ-9 Total Score"
  attr(df$phq9_severity, "label") <- "PHQ-9 Severity"
  attr(df$phq9_clinical, "label") <- "PHQ-9 Clinical Threshold"

  attr(df$phq9_total, "miss_handling") <-
    "Prorated (mean of non-missing * 9) if no more than 2 items missing, else NA"
  attr(df$phq9_severity, "miss_handling") <-
    "Derived from phq9_total; NA if phq9_total is NA"
  attr(df$phq9_clinical, "miss_handling") <-
    "Derived from phq9_total; NA if phq9_total is NA"

  df
}


# EuroQoL 5D-5L ----
# 5 dimensions scored 1-5 (no problems=1 to extreme problems=5)
score_eq5d <- function(df) {
  eq5d_dims <- tribble(
    ~eq5d_variable       , ~eq5d_desc                ,
    "eq5d_mb_5l_can_eng" , "EQ5D_Mobility"           ,
    "eq5d_sc_5l_can_eng" , "EQ5D_Self_Care"          ,
    "eq5d_ua_5l_can_eng" , "EQ5D_Usual_Activities"   ,
    "eq5d_pd_5l_can_eng" , "EQ5D_Pain_Discomfort"    ,
    "eq5d_ad_5l_can_eng" , "EQ5D_Anxiety_Depression" ,
  )

  # Recode EQ-5D-5L text responses to factors based on numeric scores ----
  eq5d_scores <- df |>
    select(all_of(eq5d_dims$eq5d_variable)) |>
    pivot_longer(everything()) |>
    distinct() |>
    mutate(
      score = case_when(
        str_detect(value, "no problems|not anxious|no pain") ~ 1L,
        str_detect(value, "slight") ~ 2L,
        str_detect(value, "moderate") ~ 3L,
        str_detect(value, "severe") ~ 4L,
        str_detect(value, "extreme|unable") ~ 5L,
        TRUE ~ NA_integer_
      )
    ) |>
    filter(!is.na(score)) |>
    (\(x) split(x, x$name))()

  # Convert to level for easier reporting ----
  for (i in 1:nrow(eq5d_dims)) {
    v <- eq5d_dims$eq5d_variable[i]
    df[[eq5d_dims$eq5d_desc[i]]] <- df |>
      transmute(rw = 1:n(), value = !!sym(v)) |>
      left_join(eq5d_scores[[v]]) |>
      arrange(rw) |>
      mutate(level = if_else(is.na(score), NA, paste("Level", score))) |>
      pull(level)

    attr(df[[eq5d_dims$eq5d_desc[i]]], "miss_handling") <-
      "Single-item recoded; NA if source item is missing or not recognised"
  }

  df$eq5d_n_missing <- rowSums(is.na(df[, eq5d_dims$eq5d_variable]))

  df
}


# FACIT-Pal ----
# Items scored 0-4; negatively worded items reverse scored
# Subscales prorated if more than 50% of items answered
score_facitpal <- function(df) {
  pwb_items <- c(
    "energy",
    "nausea",
    "physical_condition",
    "pain",
    "side_effects",
    "ill",
    "time_in_bed"
  )
  pwb_reverse <- pwb_items

  swb_items <- c(
    "close_to_friend",
    "emotional_support",
    "friend_support",
    "accepted",
    "satisfied",
    "close_to_partner",
    "sex_life"
  )
  swb_reverse <- character(0)

  ewb_items <- c(
    "sad",
    "coping",
    "hope",
    "nervous",
    "worry_dying",
    "worry_worse"
  )
  ewb_reverse <- c("sad", "hope", "nervous", "worry_dying", "worry_worse")

  fwb_items <- c(
    "able_to_work",
    "work_is_fulfilling",
    "enjoy_life",
    "accepted_illness",
    "sleeping_well",
    "enjoy_things",
    "content_quality"
  )
  fwb_reverse <- character(0)

  pals_items <- c(
    "maintain_friends",
    "family_responsibility",
    "family_appreciation",
    "family_burden",
    "short_of_breath",
    "constipated",
    "losing_weight",
    "vomiting",
    "swelling_body",
    "mouth_and_throat_dryness",
    "independent_body",
    "useful",
    "day",
    "peace",
    "hopeful",
    "decision_making",
    "thinking",
    "reconcile_others",
    "discuss_people"
  )
  pals_reverse <- c(
    "family_burden",
    "short_of_breath",
    "constipated",
    "losing_weight",
    "vomiting",
    "swelling_body",
    "mouth_and_throat_dryness"
  )

  facit_items <- c(pwb_items, swb_items, ewb_items, fwb_items, pals_items)
  facit_numeric <- df |>
    select(all_of(facit_items)) |>
    mutate(rw = 1:n()) |>
    pivot_longer(!rw) |>
    mutate(
      score = recode_values(
        value,
        "Not at all" ~ 0,
        "A little bit" ~ 1,
        "Some-what" ~ 2,
        "Somewhat" ~ 2,
        "Quite a bit" ~ 3,
        "Very much" ~ 4,
        "Very Much" ~ 4,
      )
    ) |>
    arrange(rw) |>
    select(!value) |>
    pivot_wider(values_from = score) |>
    arrange(rw)

  # Helper: reverse score and compute prorated subscale (>50% rule) ----
  score_subscale <- function(df, items, rev_items) {
    temp <- df[, items]
    for (v in rev_items) {
      temp[[v]] <- 4 - temp[[v]]
    }
    n_items <- length(items)
    n_answered <- rowSums(!is.na(temp))
    raw_sum <- rowSums(temp, na.rm = TRUE)
    ifelse(
      n_answered >= ceiling(n_items / 2),
      raw_sum * (n_items / n_answered),
      NA_real_
    )
  }

  df$facit_pwb <- score_subscale(facit_numeric, pwb_items, pwb_reverse)
  df$facit_swb <- score_subscale(facit_numeric, swb_items, swb_reverse)
  df$facit_ewb <- score_subscale(facit_numeric, ewb_items, ewb_reverse)
  df$facit_fwb <- score_subscale(facit_numeric, fwb_items, fwb_reverse)
  df$facit_pals <- score_subscale(facit_numeric, pals_items, pals_reverse)

  df$facit_factg_total <- df$facit_pwb +
    df$facit_swb +
    df$facit_ewb +
    df$facit_fwb
  df$facitpal_total <- df$facit_factg_total + df$facit_pals
  df$facitpal_toi <- df$facit_pwb + df$facit_fwb + df$facit_pals

  attr(df$facit_pwb, "label") <- "FACIT Physical Well-Being"
  attr(df$facit_swb, "label") <- "FACIT Social/Family Well-Being"
  attr(df$facit_ewb, "label") <- "FACIT Emotional Well-Being"
  attr(df$facit_fwb, "label") <- "FACIT Functional Well-Being"
  attr(df$facit_pals, "label") <- "FACIT Palliative Care"
  attr(df$facit_factg_total, "label") <- "FACIT FACT-G Total Score"
  attr(df$facitpal_total, "label") <- "FACIT-Pal Total Score"
  attr(df$facitpal_toi, "label") <- "FACIT-Pal Trial Outcome Index"

  subscale_miss <-
    "Reverse-scored where applicable; prorated if more than 50% of subscale items answered, else NA"
  attr(df$facit_pwb, "miss_handling") <- subscale_miss
  attr(df$facit_swb, "miss_handling") <- subscale_miss
  attr(df$facit_ewb, "miss_handling") <- subscale_miss
  attr(df$facit_fwb, "miss_handling") <- subscale_miss
  attr(df$facit_pals, "miss_handling") <- subscale_miss

  attr(df$facit_factg_total, "miss_handling") <-
    "Sum of PWB + SWB + EWB + FWB; NA if any component subscale is NA"
  attr(df$facitpal_total, "miss_handling") <-
    "FACT-G total + Palliative subscale; NA if any component is NA"
  attr(df$facitpal_toi, "miss_handling") <-
    "Sum of PWB + FWB + Palliative subscale; NA if any component is NA"

  df
}


# ESAS (Edmonton Symptom Assessment System, 9 items) ----
# Items scored 0-10 (0=none, 10=worst possible)
score_esas <- function(df) {
  esas_items <- c(
    "level_of_pain",
    "tiredness",
    "drowsiness",
    "presence_of_nausea",
    "appetite",
    "shortness_of_breath",
    "depression",
    "anxiety",
    "wellbeing"
  )

  esas_items |>
    purrr::walk(\(item) {
      new_item <- paste0("esas_", item)
      label <- paste("ESAS", str_to_title(str_replace_all(item, "_", " ")))
      df[[new_item]] <<- as.numeric(substr(df[[item]], 1, 1))
      attr(df[[new_item]], "label") <<- label
      attr(df[[new_item]], "miss_handling") <<-
        "Single-item score (0-10); NA if the source item is missing"
    })
  df$esas_n_missing <- rowSums(is.na(df[, esas_items]))

  df
}


# GODIN (Modified Leisure Score Index) ----
# Frequency x duration for mild, moderate, strenuous, and resistance activity
score_godin <- function(df) {
  df$godin_strenuous_min <- df$average_frequency_a * df$average_duration_a
  df$godin_moderate_min <- df$average_frequency_b * df$average_duration_b
  df$godin_mild_min <- df$average_frequency_c * df$average_duration_c
  df$godin_resistance_days <- df$average_frequency_d

  df$godin_mvpa_min <- df$godin_strenuous_min + df$godin_moderate_min

  df$godin_lsi <- (9 * df$average_frequency_a) +
    (5 * df$average_frequency_b) +
    (3 * df$average_frequency_c)

  df$godic_scale_score <- cut(
    df$godin_lsi,
    breaks = c(-Inf, 14, 24, Inf),
    labels = c(
      "Insufficiently Active/Sedentary",
      "Moderately Active",
      "Active"
    ),
    right = FALSE
  )

  attr(df$godin_strenuous_min, "label") <- "Godin Strenuous Minutes Per Week"
  attr(df$godin_moderate_min, "label") <- "Godin Moderate Minutes Per Week"
  attr(df$godin_mild_min, "label") <- "Godin Mild Minutes Per Week"
  attr(
    df$godin_resistance_days,
    "label"
  ) <- "Godin Resistance Training Days Per Week"
  attr(df$godin_mvpa_min, "label") <-
    "Godin Moderate-to-Vigorous Physical Activity Minutes Per Week"
  attr(df$godin_lsi, "label") <- "Godin Leisure Score Index"
  attr(df$godic_scale_score, "label") <- "Godin Activity Classification"

  single_miss <- "Computed as frequency * duration; NA if either source input is missing"
  combo_miss <- "Computed from multiple frequency/duration inputs; NA if any required input is missing"

  attr(df$godin_strenuous_min, "miss_handling") <- single_miss
  attr(df$godin_moderate_min, "miss_handling") <- single_miss
  attr(df$godin_mild_min, "miss_handling") <- single_miss
  attr(df$godin_resistance_days, "miss_handling") <-
    "Taken directly from average_frequency_d; NA if source is missing"
  attr(df$godin_mvpa_min, "miss_handling") <- combo_miss
  attr(df$godin_lsi, "miss_handling") <-
    "MET-weighted sum of strenuous/moderate/mild frequencies; NA if any frequency is missing"
  attr(df$godic_scale_score, "miss_handling") <-
    "Derived from godin_lsi; NA if godin_lsi is NA"

  df
}
