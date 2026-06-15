# ============================================================================
# UHN De-Identification Tool — Core Functions
# Based on UHN's De-Identification and Anonymized Data Standard (Oct 2023)
# ============================================================================

# Load Required Packages ----
suppressPackageStartupMessages({
  library(openxlsx)
  library(readxl)
  library(readr)
})


# ============================================================================
# SECTION 0: File I/O
# ============================================================================

# Read Input File (CSV or Excel) ----
read_input_file <- function(filepath) {
  ext <- tolower(tools::file_ext(filepath))
  if (ext == "csv") {
    readr::read_csv(filepath, show_col_types = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(filepath)
  } else {
    stop("Unsupported file type: ", ext, ". Use .csv, .xlsx, or .xls")
  }
}


# ============================================================================
# SECTION 1: Column Profiling Engine (Two-Pass: Name + Content)
# ============================================================================

# --- 1A: Variable Name Tokenizer -------------------------------------------

# Tokenize Column Name ----
# Splits a variable name into constituent words for keyword matching.
# Handles: underscores, dots, camelCase, digit-letter boundaries, hyphens.
# Examples:
#   "pt_age_at_dx"     -> c("pt", "age", "at", "dx")
#   "dateOfBirth"       -> c("date", "of", "birth")
#   "v12_fname"         -> c("v12", "fname")
#   "DischargeDate"     -> c("discharge", "date")
#   "admit.dt"          -> c("admit", "dt")
tokenize_colname <- function(colname) {
  s <- colname

  # Insert separator before uppercase letters in camelCase
  s <- gsub("([a-z])([A-Z])", "\\1_\\2", s)

  # Insert separator at digit-letter boundaries
  s <- gsub("([0-9])([A-Za-z])", "\\1_\\2", s)
  s <- gsub("([A-Za-z])([0-9])", "\\1_\\2", s)

  # Split on underscores, dots, hyphens, spaces
  tokens <- unlist(strsplit(s, "[_. -]+"))
  tokens <- tokens[nchar(tokens) > 0]
  tolower(tokens)
}


# --- 1B: Name-Based Classification ----------------------------------------

# Keyword Map ----
# Each entry: element name -> list of keyword vectors, ordered by specificity.
# Multi-word patterns (compound keywords) are checked first.
# Then single-token keywords are checked.
get_name_keyword_map <- function() {
  list(

    # ----- Direct identifiers -----

    "Full Name" = list(
      compound = c("full_name", "fullname", "patient_name", "pt_name",
                   "patname", "client_name"),
      tokens   = c("fullname", "patname")
    ),
    "First Name" = list(
      compound = c("first_name", "given_name", "forename"),
      tokens   = c("fname", "forename", "prenom", "given")
    ),
    "Last Name" = list(
      compound = c("last_name", "family_name"),
      tokens   = c("lname", "surname", "family")
    ),
    "Middle Name" = list(
      compound = c("middle_name", "mid_name"),
      tokens   = c("mname", "middle")
    ),
    "MRN / Patient ID" = list(
      compound = c("medical_record", "pat_id", "patient_id", "chart_no",
                   "chart_num", "encounter_id", "enc_id", "enc_no",
                   "visit_id", "case_id", "case_no", "account_no",
                   "acct_no"),
      tokens   = c("mrn", "ptid", "encounter", "chart", "acct")
    ),
    "Health Card Number" = list(
      compound = c("health_card", "health_ins", "insurance_no",
                   "insurer_id", "member_id", "policy_no"),
      tokens   = c("hcn", "ohip", "hin")
    ),
    "SIN/SSN" = list(
      compound = c("social_ins", "social_sec", "tax_id", "national_id"),
      tokens   = c("sin", "ssn")
    ),
    "Driver's License Number" = list(
      compound = c("drivers_lic", "driver_lic", "dl_num", "dl_no"),
      tokens   = c("licence", "license")
    ),
    "Email Address" = list(
      compound = c("email_addr", "e_mail"),
      tokens   = c("email")
    ),
    "Phone Number" = list(
      compound = c("phone_num", "phone_home", "phone_work", "phone_cell",
                   "contact_num", "tel_no", "cell_num", "fax_no",
                   "pager_no"),
      tokens   = c("phone", "telephone", "mobile", "cell", "fax",
                    "pager", "tel")
    ),
    "Street Address" = list(
      compound = c("street_addr", "addr_line", "home_address",
                   "mailing_addr", "postal_addr"),
      tokens   = c("address", "addr", "street", "suite", "residence")
    ),
    "URL" = list(
      compound = c("web_address", "homepage"),
      tokens   = c("url", "website")
    ),
    "IP Address" = list(
      compound = c("ip_addr", "ip_address", "ipv4_addr"),
      tokens   = c("ipv4", "ipv6")
    ),

    # ----- Quasi-identifiers: Dates -----

    "Date of Birth" = list(
      compound = c("date_of_birth", "birth_date", "birthdate", "birth_dt"),
      tokens   = c("dob", "bdate", "birthdt", "born")
    ),
    "Date of Death" = list(
      compound = c("date_of_death", "death_date", "deathdate", "death_dt",
                   "deceased_date"),
      tokens   = c("dod", "deceased", "died", "expiry")
    ),
    "Admission Date" = list(
      compound = c("admit_date", "adm_date", "admit_dt", "adm_dt",
                   "date_admit", "date_admitted"),
      tokens   = c("admitted")
    ),
    "Discharge Date" = list(
      compound = c("discharge_date", "disch_date", "disch_dt", "dc_date",
                   "date_disch", "date_dc", "sep_date"),
      tokens   = c("discharge", "disch", "separated")
    ),
    "Procedure Date" = list(
      compound = c("surgery_date", "surg_date", "proc_date", "proc_dt",
                   "or_date", "op_date", "procedure_date", "oper_date"),
      tokens   = character(0)
    ),
    "Date (Other)" = list(
      compound = c("referral_date", "dx_date", "dx_dt", "enrol_date",
                   "registration_date", "consult_date", "follow_up",
                   "visit_date", "appt_date", "appointment_date",
                   "collected_date", "received_date", "reported_date",
                   "ordered_date", "signed_date", "specimen_date",
                   "onset_date", "diag_date"),
      tokens   = c("date", "dt", "dte", "datetime", "timestamp")
    ),

    # ----- Quasi-identifiers: Categorical (recode) -----

    "Gender" = list(
      compound = c("sex_at_birth", "birth_sex", "assigned_sex",
                   "gender_identity"),
      tokens   = c("sex", "gender")
    ),
    "Ethnicity" = list(
      compound = c("ethnic_group", "racial_cat"),
      tokens   = c("ethnicity", "ethnic", "race", "racial", "ancestry",
                    "cultural")
    ),
    "Nationality" = list(
      compound = c("country_of_origin", "country_birth", "country_of_birth"),
      tokens   = c("nationality", "citizenship", "cob")
    ),
    "Marital Status" = list(
      compound = c("marital_status", "civil_status"),
      tokens   = c("marital", "married")
    ),
    "Occupation" = list(
      compound = c("job_title", "employment_status"),
      tokens   = c("occupation", "profession", "vocation", "employ")
    ),
    "Organization" = list(
      compound = c("company_name", "employer_name", "school_name"),
      tokens   = c("employer", "company", "workplace", "school")
    ),
    "Physician Name" = list(
      compound = c("referring_dr", "attending_phys", "surgeon_name",
                   "provider_id", "provider_name", "consultant_name",
                   "staff_name"),
      tokens   = c("physician", "doctor", "surgeon", "practitioner",
                    "attending", "referring", "consultant", "clinician",
                    "specialist", "provider", "npi", "msp")
    ),
    "Initials" = list(
      compound = c("pt_initials", "patient_initials"),
      tokens   = c("initials", "initial")
    ),
    "Family Member Names" = list(
      compound = c("next_of_kin", "emergency_contact", "nok_name",
                   "guardian_name", "caregiver_name"),
      tokens   = c("kin", "nok", "guardian", "caregiver")
    ),
    "Phone Extension" = list(
      compound = c("phone_ext"),
      tokens   = c("ext", "extension")
    ),

    # ----- Quasi-identifiers: Continuous (treat) -----

    "Age" = list(
      compound = c("age_at", "ageat", "age_dx", "age_admit", "age_enrol",
                   "gest_age", "gestational_age", "years_old"),
      tokens   = c("age")
    ),
    "Year of Birth" = list(
      compound = c("year_of_birth", "birth_year", "year_born"),
      tokens   = c("yob")
    ),
    "Height" = list(
      compound = c("height_cm", "height_in", "ht_cm", "ht_in"),
      tokens   = c("height", "stature")
    ),
    "Weight" = list(
      compound = c("weight_kg", "weight_lbs", "wt_kg", "wt_lbs",
                   "body_weight"),
      tokens   = c("weight", "mass")
    ),
    "Postal Code" = list(
      compound = c("postal_code", "post_code", "zip_code"),
      tokens   = c("postal", "postcode", "zip", "fsa")
    ),
    "City" = list(
      compound = c("city_of_residence"),
      tokens   = c("city", "town", "municipality")
    ),
    "Location (Vague)" = list(
      compound = c("area_of_residence"),
      tokens   = c("neighbourhood", "neighborhood", "intersection",
                    "district")
    ),
    "Medical Organization" = list(
      compound = c("treating_site", "facility_name", "location_of_care"),
      tokens   = c("hospital", "clinic", "centre", "center", "campus",
                    "facility")
    ),

    # ----- Non-identifiers -----

    "Diagnosis" = list(
      compound = c("dx_icd", "icd_code", "primary_dx", "secondary_dx"),
      tokens   = c("diagnosis", "diag", "dx", "icd", "icd9", "icd10",
                    "comorbid", "condition")
    ),
    "Procedure Code" = list(
      compound = c("cci_code", "cpt_code", "proc_code",
                   "intervention_code", "ohip_code"),
      tokens   = c("cci", "ccp", "cpt")
    ),
    "Medication" = list(
      compound = c("drug_name", "atc_code", "din_code", "ndc_code"),
      tokens   = c("medication", "drug", "pharma", "rx", "prescription",
                    "therapeutic", "din", "atc", "ndc", "regimen")
    ),
    "Lab / Test Code" = list(
      compound = c("loinc_code", "lab_test", "test_name", "lab_result",
                   "lab_code"),
      tokens   = c("loinc", "assay", "analyte", "specimen")
    ),
    "Treatment" = list(
      compound = c("therapy_type", "chemo_regimen"),
      tokens   = c("therapy", "chemo", "radiation", "xrt", "modality")
    ),
    "Adverse Event" = list(
      compound = c("adverse_event", "side_effect", "ae_grade"),
      tokens   = c("adverse", "complication", "reaction")
    ),
    "Province" = list(
      compound = character(0),
      tokens   = c("province", "prov", "territory", "state")
    ),
    "Country" = list(
      compound = character(0),
      tokens   = c("country", "nation")
    )
  )
}


# Classify by Variable Name ----
# Returns a list: element (character), confidence ("high", "medium", "none")
classify_by_name <- function(colname) {
  tokens <- tokenize_colname(colname)
  col_joined <- paste(tokens, collapse = "_")
  kw_map <- get_name_keyword_map()

  # Pass 1: Compound keyword match (most specific, check first)
  for (element in names(kw_map)) {
    compounds <- kw_map[[element]]$compound
    for (cmp in compounds) {
      if (grepl(cmp, col_joined, fixed = TRUE)) {
        return(list(element = element, confidence = "high",
                    matched_on = paste0("compound: ", cmp)))
      }
    }
  }

  # Pass 2: Single-token match — pick longest keyword if multiple match
  matches <- list()
  for (element in names(kw_map)) {
    tk_keywords <- kw_map[[element]]$tokens
    for (kw in tk_keywords) {
      if (kw %in% tokens) {
        matches[[length(matches) + 1]] <- list(
          element = element, keyword = kw,
          specificity = nchar(kw)
        )
      }
    }
  }

  if (length(matches) > 0) {
    specs <- sapply(matches, function(m) m$specificity)
    best <- matches[[which.max(specs)]]
    return(list(element = best$element, confidence = "high",
                matched_on = paste0("token: ", best$keyword)))
  }

  # Pass 3: Generic "name" token
  if ("name" %in% tokens || "nm" %in% tokens) {
    return(list(element = "Possible Name", confidence = "high",
                matched_on = "token: name/nm (generic)"))
  }

  list(element = "Unknown", confidence = "none", matched_on = "")
}


# --- 1C: Content-Based Classification -------------------------------------

# Detect Dates (multiple strategies) ----
detect_date_content <- function(x) {
  x_nona <- x[!is.na(x)]
  if (length(x_nona) == 0) return(list(is_date = FALSE))

  # Already Date or POSIXct
  if (inherits(x_nona, "Date") || inherits(x_nona, "POSIXct")) {
    return(list(is_date = TRUE, method = "R Date/POSIXct class"))
  }

  # Try parsing character values
  if (is.character(x_nona) || is.factor(x_nona)) {
    x_char <- as.character(x_nona)
    formats <- c(
      "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d",
      "%d-%b-%Y", "%b %d, %Y", "%B %d, %Y", "%Y%m%d",
      "%d-%m-%Y", "%m-%d-%Y", "%d.%m.%Y", "%Y.%m.%d",
      "%d %b %Y", "%d %B %Y", "%Y-%m-%d %H:%M:%S",
      "%m/%d/%Y %H:%M", "%d/%m/%Y %H:%M"
    )
    for (fmt in formats) {
      parsed <- suppressWarnings(as.Date(x_char, format = fmt))
      pct <- mean(!is.na(parsed))
      if (pct > 0.5) {
        return(list(is_date = TRUE, method = paste0("parsed: ", fmt),
                    pct_parsed = pct))
      }
    }
  }

  # Check for Excel serial date numbers (numeric 25000–55000 ≈ 1968–2050)
  if (is.numeric(x_nona)) {
    x_int <- x_nona[x_nona == floor(x_nona)]
    if (length(x_int) > 0.8 * length(x_nona)) {
      in_range <- x_int >= 25000 & x_int <= 55000
      if (mean(in_range) > 0.8) {
        return(list(is_date = TRUE, method = "Excel serial date (numeric 25000–55000)"))
      }
    }
  }

  list(is_date = FALSE)
}


# Detect Email ----
detect_email_content <- function(x) {
  x <- as.character(x[!is.na(x)])
  if (length(x) == 0) return(FALSE)
  mean(grepl("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", x)) > 0.5
}


# Detect URL ----
detect_url_content <- function(x) {
  x <- as.character(x[!is.na(x)])
  if (length(x) == 0) return(FALSE)
  mean(grepl("(https?://|www\\.)[A-Za-z0-9./-]+", x)) > 0.5
}


# Detect IP Address ----
detect_ip_content <- function(x) {
  x <- trimws(as.character(x[!is.na(x)]))
  if (length(x) == 0) return(FALSE)
  mean(grepl("^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$", x)) > 0.5
}


# Detect Canadian Postal Code ----
detect_postal_content <- function(x) {
  x <- trimws(as.character(x[!is.na(x)]))
  if (length(x) == 0) return(FALSE)
  mean(grepl("^[A-Za-z]\\d[A-Za-z]\\s?\\d[A-Za-z]\\d$", x)) > 0.5
}


# Detect SIN/SSN (9 digits) ----
detect_sin_ssn_content <- function(x) {
  x <- gsub("[- ]", "", as.character(x[!is.na(x)]))
  if (length(x) == 0) return(FALSE)
  mean(grepl("^\\d{9}$", trimws(x))) > 0.5
}


# Detect Ontario Health Card Number (10 digits + optional version) ----
detect_hcn_content <- function(x) {
  x <- gsub("[- ]", "", as.character(x[!is.na(x)]))
  if (length(x) == 0) return(FALSE)
  mean(grepl("^\\d{10}[A-Za-z]{0,2}$", trimws(x))) > 0.5
}


# Detect Phone Number ----
detect_phone_content <- function(x) {
  x <- as.character(x[!is.na(x)])
  if (length(x) == 0) return(FALSE)
  x_clean <- gsub("[^0-9+(). -]", "", x)
  mean(grepl("^[+]?[0-9(). -]{7,15}$", trimws(x_clean))) > 0.5
}


# Detect ICD-9/10 Code ----
detect_icd_content <- function(x) {
  x <- trimws(as.character(x[!is.na(x)]))
  if (length(x) == 0) return(FALSE)
  # ICD-10: letter + 2-3 digits + optional dot + digits (e.g. E11.9, I25.10)
  # ICD-9: 3-5 digits with optional dot (e.g. 250.00, 401)
  icd10 <- grepl("^[A-Za-z]\\d{2,3}(\\.\\d{1,4})?$", x)
  icd9  <- grepl("^\\d{3}(\\.\\d{1,2})?$", x)
  mean(icd10 | icd9) > 0.5
}


# Detect Unique Numeric ID (MRN-like) ----
detect_unique_id_content <- function(x) {
  x <- trimws(as.character(x[!is.na(x)]))
  if (length(x) < 3) return(FALSE)
  all_digits <- all(grepl("^\\d+$", x))
  if (!all_digits) return(FALSE)
  lengths <- nchar(x)
  consistent_length <- (max(lengths) - min(lengths)) <= 1
  all_unique <- length(unique(x)) / length(x) > 0.95
  avg_len <- mean(lengths)
  consistent_length && all_unique && avg_len >= 5 && avg_len <= 12
}


# Detect Sex/Gender Values ----
detect_sex_content <- function(x) {
  x <- toupper(trimws(as.character(x[!is.na(x)])))
  if (length(x) == 0) return(FALSE)
  n_unique <- length(unique(x))
  if (n_unique > 6 || n_unique < 2) return(FALSE)
  known <- c("M", "F", "MALE", "FEMALE", "OTHER", "NON-BINARY",
             "NONBINARY", "UNKNOWN", "X", "INTERSEX",
             "TRANSGENDER", "0", "1", "2", "U",
             "MAN", "WOMAN", "PREFER NOT TO SAY", "NOT SPECIFIED")
  mean(x %in% known) > 0.8
}


# Detect Province/Territory Values ----
detect_province_content <- function(x) {
  x <- toupper(trimws(as.character(x[!is.na(x)])))
  if (length(x) == 0) return(FALSE)
  codes <- c("ON", "QC", "BC", "AB", "MB", "SK", "NS", "NB", "NL",
             "PE", "NT", "YT", "NU")
  full <- c("ONTARIO", "QUEBEC", "BRITISH COLUMBIA", "ALBERTA",
            "MANITOBA", "SASKATCHEWAN", "NOVA SCOTIA", "NEW BRUNSWICK",
            "NEWFOUNDLAND", "PRINCE EDWARD ISLAND", "NORTHWEST TERRITORIES",
            "YUKON", "NUNAVUT")
  mean(x %in% c(codes, full)) > 0.8
}


# Detect Marital Status Values ----
detect_marital_content <- function(x) {
  x <- toupper(trimws(as.character(x[!is.na(x)])))
  if (length(x) == 0) return(FALSE)
  n_unique <- length(unique(x))
  if (n_unique > 10 || n_unique < 2) return(FALSE)
  known <- c("SINGLE", "MARRIED", "DIVORCED", "WIDOWED", "SEPARATED",
             "COMMON-LAW", "COMMON LAW", "PARTNER", "UNKNOWN",
             "S", "M", "D", "W", "CL", "P", "1", "2", "3", "4", "5")
  mean(x %in% known) > 0.6
}


# Detect Address-Like Strings ----
detect_address_content <- function(x) {
  x <- as.character(x[!is.na(x)])
  if (length(x) == 0) return(FALSE)
  street_words <- "\\b(st|ave|avenue|rd|road|dr|drive|blvd|boulevard|crescent|cres|way|lane|ln|court|ct|place|pl|hwy|highway|trail|terrace|circle|pkwy|parkway)\\b"
  has_number_and_street <- grepl("\\d", x) & grepl(street_words, x, ignore.case = TRUE)
  mean(has_number_and_street) > 0.3
}


# Detect Name-Like Strings ----
detect_name_content <- function(x) {
  x <- trimws(as.character(x[!is.na(x)]))
  if (length(x) < 3) return(FALSE)
  alpha_only <- grepl("^[A-Za-z' -]{2,40}$", x)
  title_case <- grepl("^[A-Z][a-z]", x)
  uniqueness <- length(unique(x)) / length(x)
  # Names are high-uniqueness, mostly alphabetic, mostly title case
  mean(alpha_only) > 0.8 && mean(title_case) > 0.5 && uniqueness > 0.5
}


# Detect Free Text / Clinical Notes ----
detect_free_text_content <- function(x) {
  x <- as.character(x[!is.na(x)])
  if (length(x) < 3) return(FALSE)
  median_len <- median(nchar(x))
  has_spaces <- mean(grepl(" ", x)) > 0.5
  high_unique <- length(unique(x)) / length(x) > 0.8
  median_len > 50 && has_spaces && high_unique
}


# Compute Numeric Profile ----
# Returns a summary of the numeric characteristics of a column.
compute_numeric_profile <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  x_num <- x_num[!is.na(x_num)]

  if (length(x_num) < 3) {
    return(list(is_numeric = FALSE))
  }

  n <- length(x_num)
  all_integer <- all(x_num == floor(x_num))
  n_unique <- length(unique(x_num))

  list(
    is_numeric   = TRUE,
    n            = n,
    n_unique     = n_unique,
    uniqueness   = n_unique / n,
    all_integer  = all_integer,
    min_val      = min(x_num),
    max_val      = max(x_num),
    median_val   = median(x_num),
    mean_val     = mean(x_num),
    sd_val       = sd(x_num),
    # Specific range checks
    plausible_age    = all_integer && min(x_num) >= 0 && max(x_num) <= 120 &&
                       sd(x_num) > 3 && n_unique > 5,
    plausible_height_cm = min(x_num) > 30 && max(x_num) < 250 &&
                          median(x_num) > 100 && median(x_num) < 210 &&
                          sd(x_num) > 3 && sd(x_num) < 40,
    plausible_weight_kg = min(x_num) > 1 && max(x_num) < 400 &&
                          median(x_num) > 20 && median(x_num) < 200 &&
                          sd(x_num) > 5,
    plausible_year   = all_integer && min(x_num) >= 1900 && max(x_num) <= 2030 &&
                       n_unique > 3,
    low_cardinality  = all_integer && n_unique <= 10,
    excel_serial_date = all_integer && min(x_num) >= 25000 && max(x_num) <= 55000 &&
                        mean(x_num >= 25000 & x_num <= 55000) > 0.8
  )
}


# Compute General Column Profile ----
compute_column_profile <- function(x) {
  n <- length(x)
  n_na <- sum(is.na(x))
  x_nona <- x[!is.na(x)]

  list(
    n           = n,
    n_na        = n_na,
    pct_na      = round(n_na / n * 100, 1),
    n_unique    = length(unique(x_nona)),
    uniqueness  = if (length(x_nona) > 0) length(unique(x_nona)) / length(x_nona) else 0,
    r_class     = paste(class(x), collapse = ", "),
    is_character = is.character(x) || is.factor(x),
    is_numeric  = is.numeric(x) || is.integer(x),
    is_date     = inherits(x, "Date") || inherits(x, "POSIXct")
  )
}


# Classify by Content ----
# Returns STRUCTURAL descriptions only. Does not speculate on meaning.
# Answers: "what IS this data?" not "what does it MEAN?"
# Returns: list(element, confidence, reason)
classify_by_content <- function(x, colname = "") {
  prof <- compute_column_profile(x)

  # All NA — nothing to detect
  if (prof$n_unique == 0) {
    return(list(element = "Empty Column", confidence = "high",
                reason = "All values are NA"))
  }

  # --- Date detection (any data type, checked first) ---

  date_check <- detect_date_content(x)
  if (date_check$is_date) {
    return(list(element = "Date", confidence = "high",
                reason = paste("Date detected:", date_check$method)))
  }

  # --- Character column detectors ---

  if (prof$is_character || is.factor(x)) {

    # High-confidence format patterns
    if (detect_email_content(x))
      return(list(element = "Email Address", confidence = "high",
                  reason = "Email regex match"))
    if (detect_url_content(x))
      return(list(element = "URL", confidence = "high",
                  reason = "URL pattern match"))
    if (detect_ip_content(x))
      return(list(element = "IP Address", confidence = "high",
                  reason = "IPv4 pattern match"))
    if (detect_postal_content(x))
      return(list(element = "Postal Code", confidence = "high",
                  reason = "Canadian postal code pattern (A1A 1A1)"))
    if (detect_hcn_content(x))
      return(list(element = "Health Card Number", confidence = "high",
                  reason = "10-digit + version code pattern"))
    if (detect_sin_ssn_content(x))
      return(list(element = "SIN/SSN", confidence = "high",
                  reason = "9-digit pattern"))
    if (detect_icd_content(x))
      return(list(element = "Diagnosis", confidence = "high",
                  reason = "ICD-9/10 code pattern"))

    # Known value-set matching (string labels only)
    if (detect_sex_content(x))
      return(list(element = "Gender", confidence = "high",
                  reason = "Values match known sex/gender labels"))
    if (detect_province_content(x))
      return(list(element = "Province", confidence = "high",
                  reason = "Values match Canadian province codes/names"))
    if (detect_marital_content(x))
      return(list(element = "Marital Status", confidence = "medium",
                  reason = "Values match known marital status labels"))

    # Structural string patterns
    if (detect_phone_content(x))
      return(list(element = "Phone Number", confidence = "medium",
                  reason = "Digit string with separators, length 7-15"))
    if (detect_address_content(x))
      return(list(element = "Street Address", confidence = "medium",
                  reason = "Contains digits + street-type words"))
    if (detect_free_text_content(x))
      return(list(element = "Free Text / Notes", confidence = "medium",
                  reason = paste0("Median length ",
                                  round(median(nchar(as.character(x[!is.na(x)]))), 0),
                                  " chars, high uniqueness — may contain PHI")))
    if (detect_name_content(x))
      return(list(element = "Possible Name", confidence = "medium",
                  reason = paste0("Title case, alphabetic, uniqueness=",
                                  round(prof$uniqueness, 2))))

    # Low-cardinality string — descriptive, not a semantic label
    if (prof$n_unique <= 20 && prof$uniqueness < 0.1) {
      vals <- head(sort(unique(as.character(x[!is.na(x)]))), 8)
      return(list(element = "Low-Cardinality String", confidence = "medium",
                  reason = paste0(prof$n_unique, " unique values: ",
                                  paste(vals, collapse = ", "))))
    }

    # High-cardinality string — could be names, IDs, or other
    if (prof$uniqueness > 0.8) {
      return(list(element = "High-Uniqueness String", confidence = "low",
                  reason = paste0("Uniqueness=", round(prof$uniqueness, 2),
                                  ", ", prof$n_unique, " unique values")))
    }

    # Moderate-cardinality string
    return(list(element = "Moderate-Cardinality String", confidence = "low",
                reason = paste0(prof$n_unique, " unique values, uniqueness=",
                                round(prof$uniqueness, 2))))
  }

  # --- Numeric column detectors ---

  if (prof$is_numeric || is.numeric(x)) {
    np <- compute_numeric_profile(x)

    if (np$is_numeric) {
      # Excel serial dates
      if (np$excel_serial_date) {
        return(list(element = "Date", confidence = "medium",
                    reason = "Numeric values in Excel serial date range (25000-55000)"))
      }

      # Unique numeric ID (structural: all unique, consistent digit length)
      if (detect_unique_id_content(x)) {
        return(list(element = "Unique Numeric ID", confidence = "medium",
                    reason = paste0("All unique, all digits, consistent length (",
                                   round(mean(nchar(trimws(as.character(x[!is.na(x)])))), 0),
                                   " chars)")))
      }

      # Low-cardinality integer — descriptive fact, no semantic claim
      if (np$low_cardinality) {
        vals <- sort(unique(suppressWarnings(as.numeric(x[!is.na(x)]))))
        return(list(element = "Low-Cardinality Integer", confidence = "medium",
                    reason = paste0(np$n_unique, " unique integer values: ",
                                   paste(head(vals, 10), collapse = ", "))))
      }

      # Continuous numeric — report range and spread
      return(list(element = "Continuous Numeric", confidence = "medium",
                  reason = paste0(np$n_unique, " unique values, range [",
                                  round(np$min_val, 1), ", ",
                                  round(np$max_val, 1), "], median=",
                                  round(np$median_val, 1), ", SD=",
                                  round(np$sd_val, 1))))
    }
  }

  list(element = "Unknown", confidence = "none", reason = "No pattern detected")
}


# --- 1D: Reconciliation and Main Profile Function -------------------------

# Reconcile Name and Content Signals ----
#
# Five rules:
#   1. Name matched + content doesn't contradict → name element, high
#   2. Name matched + content semantic from different family → CONFLICT
#   3. Name unknown + content semantic → content element, high
#   4. Name unknown + content structural + high uniqueness → FLAG
#   5. Name unknown + content structural + low/moderate uniqueness → not identifying
#
reconcile_signals <- function(name_result, content_result, uniqueness = 0) {
  ne <- name_result$element
  nc <- name_result$confidence
  ce <- content_result$element
  cc <- content_result$confidence

  # Semantic content elements — matched a specific pattern, not just structure
  semantic_elements <- c(
    "Email Address", "URL", "IP Address", "Postal Code",
    "Health Card Number", "SIN/SSN", "Diagnosis", "Date",
    "Gender", "Province", "Marital Status",
    "Phone Number", "Street Address", "Free Text / Notes",
    "Possible Name"
  )

  content_is_semantic <- ce %in% semantic_elements
  name_matched <- nc != "none"

  # Families for conflict detection (semantic vs semantic only)
  date_family <- c("Date of Birth", "Date of Death", "Admission Date",
                   "Discharge Date", "Procedure Date", "Date (Other)", "Date")
  name_family <- c("Full Name", "First Name", "Last Name", "Middle Name",
                   "Possible Name", "Family Member Names", "Physician Name")
  id_family   <- c("MRN / Patient ID", "Health Card Number", "SIN/SSN")
  contact_family <- c("Email Address", "URL", "IP Address", "Phone Number",
                      "Street Address")
  geo_family  <- c("Postal Code", "City", "Location (Vague)", "Province",
                   "Country")
  demo_family <- c("Gender", "Ethnicity", "Nationality", "Marital Status",
                   "Age", "Year of Birth", "Height", "Weight")
  clinical_family <- c("Diagnosis", "Medication", "Treatment", "Procedure Code",
                       "Lab / Test Code", "Adverse Event")

  all_families <- list(date_family, name_family, id_family, contact_family,
                       geo_family, demo_family, clinical_family)

  get_family <- function(elem) {
    for (fam in all_families) {
      if (elem %in% fam) return(fam)
    }
    NULL
  }

  # --- Rule 5/3: Neither matched ---
  if (!name_matched && cc == "none") {
    return(list(
      element = "Unknown", confidence = "none",
      basis = "Neither name nor content produced a match",
      name_signal = NA_character_, content_signal = NA_character_
    ))
  }

  # --- Rules 3/4/5: Name unknown, content matched ---
  if (!name_matched) {
    if (content_is_semantic) {
      # Rule 3: Content matched a specific pattern
      return(list(
        element = ce, confidence = "high",
        basis = paste0("Content: ", content_result$reason),
        name_signal = NA_character_, content_signal = ce
      ))
    }
    if (uniqueness > 0.8) {
      # Rule 4: Structural + high uniqueness → flag
      return(list(
        element = ce, confidence = "low",
        basis = paste0("FLAG: Unknown name, high uniqueness (",
                       round(uniqueness, 2), "). ", content_result$reason),
        name_signal = NA_character_, content_signal = ce
      ))
    }
    # Rule 5: Structural + low/moderate uniqueness → not identifying
    return(list(
      element = ce, confidence = "high",
      basis = paste0("Content: ", content_result$reason,
                     ". No identifying information detected."),
      name_signal = NA_character_, content_signal = ce
    ))
  }

  # --- Rules 1/2: Name matched ---

  # Check for semantic conflict (Rule 2)
  if (content_is_semantic) {
    ne_fam <- get_family(ne)
    ce_fam <- get_family(ce)

    # Same family or same element → agreement (Rule 1)
    if (!is.null(ne_fam) && !is.null(ce_fam) && identical(ne_fam, ce_fam)) {
      return(list(
        element = ne, confidence = "high",
        basis = "Name + content agree",
        name_signal = ne, content_signal = ce
      ))
    }
    if (ne == ce) {
      return(list(
        element = ne, confidence = "high",
        basis = "Name + content agree (exact match)",
        name_signal = ne, content_signal = ce
      ))
    }

    # Different families → conflict (Rule 2)
    if (!is.null(ne_fam) && !is.null(ce_fam)) {
      return(list(
        element = ne, confidence = "low",
        basis = paste0("CONFLICT: name='", ne, "' but content='", ce, "'"),
        name_signal = ne, content_signal = ce
      ))
    }
  }

  # Rule 1: Name matched, content is structural or compatible or absent
  return(list(
    element = ne, confidence = "high",
    basis = if (cc != "none") {
      paste0("Name: ", name_result$matched_on, " | Content: ", content_result$reason)
    } else {
      paste0("Name: ", name_result$matched_on)
    },
    name_signal = ne,
    content_signal = if (cc != "none") ce else NA_character_
  ))
}


# Profile a Single Column (Main Entry Point) ----
# Returns a named list with all profiling information.
# uniqueness_threshold: passed to classification for physician name logic
profile_column <- function(x, colname = "", uniqueness_threshold = 0.8) {
  # General stats
  gen <- compute_column_profile(x)

  # Two-pass classification
  name_result    <- classify_by_name(colname)
  content_result <- classify_by_content(x, colname)
  final          <- reconcile_signals(name_result, content_result,
                                      uniqueness = gen$uniqueness)

  # UHN classification and suggested action (uniqueness-dependent)
  uhn_class <- get_uhn_classification(final$element,
                                      uniqueness = gen$uniqueness,
                                      uniqueness_threshold = uniqueness_threshold)
  action    <- get_suggested_action(final$element,
                                    uniqueness = gen$uniqueness,
                                    uniqueness_threshold = uniqueness_threshold)

  # Sample values for display
  x_nona <- x[!is.na(x)]
  samples <- if (length(x_nona) > 0) {
    paste(head(unique(x_nona), 5), collapse = " | ")
  } else {
    "(all NA)"
  }

  list(
    column_name      = colname,
    r_class          = gen$r_class,
    n_unique         = gen$n_unique,
    pct_missing      = gen$pct_na,
    uniqueness       = round(gen$uniqueness, 3),
    sample_values    = samples,
    detected_element = final$element,
    confidence       = final$confidence,
    basis            = final$basis,
    name_signal      = if (!is.null(final$name_signal)) final$name_signal else NA_character_,
    content_signal   = if (!is.null(final$content_signal)) final$content_signal else NA_character_,
    uhn_class        = uhn_class,
    suggested_action = action
  )
}


# ============================================================================
# SECTION 2: UHN Classification and Action Mapping
# ============================================================================

# Classify Element into UHN Category ----
# Four categories:
#   "Direct Identifier"  — contains identifying information, REMOVE/PSEUDONYMIZE
#   "Quasi-Identifier"   — contributes to re-identification, RECODE/TREAT/INTERVAL
#   "Non-Identifier"     — no re-identification risk, RETAIN
#   "To Be Removed"      — no analytical value or too risky, REMOVE
#
# uniqueness: the column's uniqueness ratio (n_unique / n_rows)
# uniqueness_threshold: above this, physician names are flagged for removal
#
get_uhn_classification <- function(detected_element, uniqueness = 0,
                                   uniqueness_threshold = 0.8) {

  # Direct identifiers
  direct <- c("Email Address", "URL", "IP Address", "Health Card Number",
              "SIN/SSN", "Phone Number", "Driver's License Number",
              "MRN / Patient ID", "Possible Name", "Full Name",
              "First Name", "Last Name", "Middle Name",
              "Street Address", "Family Member Names",
              "Unique Numeric ID")

  if (detected_element %in% direct) return("Direct Identifier")

  # Physician Name — depends on uniqueness
  if (detected_element == "Physician Name") {
    if (uniqueness > uniqueness_threshold) return("Direct Identifier")
    else return("Quasi-Identifier")
  }

  # To Be Removed — no analytical value or unmanageable risk
  to_remove <- c("Free Text / Notes", "Empty Column",
                 "High-Uniqueness String", "Initials",
                 "Phone Extension")

  if (detected_element %in% to_remove) return("To Be Removed")

  # Quasi-identifiers — dates
  date_qi <- c("Date", "Date of Birth", "Date of Death",
               "Admission Date", "Discharge Date",
               "Procedure Date", "Date (Other)")

  if (detected_element %in% date_qi) return("Quasi-Identifier")

  # Quasi-identifiers — continuous (treated per UHN standard)
  continuous_qi <- c("Age", "Year of Birth", "Height", "Weight",
                     "Postal Code", "City", "Location (Vague)")

  if (detected_element %in% continuous_qi) return("Quasi-Identifier")

  # Quasi-identifiers — categorical (recoded)
  categorical_qi <- c("Gender", "Ethnicity", "Nationality",
                      "Marital Status", "Occupation", "Organization",
                      "Medical Organization")

  if (detected_element %in% categorical_qi) return("Quasi-Identifier")

  # Non-identifiers — explicitly listed in standard
  non_id <- c("Province", "Country", "Medication", "Diagnosis",
              "Procedure Code", "Lab / Test Code", "Treatment",
              "Adverse Event")

  if (detected_element %in% non_id) return("Non-Identifier")

  # Structural content labels (no name match, low/moderate uniqueness)
  structural <- c("Low-Cardinality Integer", "Low-Cardinality String",
                  "Moderate-Cardinality String", "Continuous Numeric")

  if (detected_element %in% structural) return("Non-Identifier")

  # Unknown — analyst must classify
  "Unknown"
}


# Map Element to Suggested Action ----
get_suggested_action <- function(detected_element, uniqueness = 0,
                                uniqueness_threshold = 0.8) {

  action_map <- list(
    # Direct identifiers → remove
    "Email Address"           = "REMOVE",
    "URL"                     = "REMOVE",
    "IP Address"              = "REMOVE",
    "Health Card Number"      = "REMOVE",
    "SIN/SSN"                 = "REMOVE",
    "Phone Number"            = "REMOVE",
    "Driver's License Number" = "REMOVE",
    "MRN / Patient ID"        = "PSEUDONYMIZE",
    "Possible Name"           = "REMOVE",
    "Full Name"               = "REMOVE",
    "First Name"              = "REMOVE",
    "Last Name"               = "REMOVE",
    "Middle Name"             = "REMOVE",
    "Street Address"          = "REMOVE",
    "Family Member Names"     = "REMOVE",
    "Unique Numeric ID"       = "FLAG — Possible direct identifier, verify",

    # Dates → intervals
    "Date"                    = "INTERVAL — Days from anchor date",
    "Date of Birth"           = "INTERVAL — DOB to age at anchor",
    "Date of Death"           = "INTERVAL — Days from anchor date",
    "Admission Date"          = "INTERVAL — Days from anchor date",
    "Discharge Date"          = "INTERVAL — Days from anchor date",
    "Procedure Date"          = "INTERVAL — Days from anchor date",
    "Date (Other)"            = "INTERVAL — Days from anchor date",

    # Continuous QIs → treat
    "Age"                     = "TREAT — Age (90+ bucket, small-cell bins)",
    "Year of Birth"           = "TREAT — Age (90+ bucket, small-cell bins)",
    "Height"                  = "TREAT — Height (sex-specific thresholds)",
    "Weight"                  = "TREAT — Weight (sex-specific thresholds)",
    "Postal Code"             = "TREAT — Postal code (first 3 chars / FSA)",
    "City"                    = "TREAT — City (replace if pop < 5,000)",
    "Location (Vague)"        = "RECODE — Anonymous factor codes",

    # Categorical QIs → recode
    "Gender"                  = "RECODE — Anonymous factor codes",
    "Ethnicity"               = "RECODE — Anonymous factor codes",
    "Nationality"             = "RECODE — Anonymous factor codes",
    "Marital Status"          = "RECODE — Anonymous factor codes",
    "Occupation"              = "RECODE — Anonymous factor codes",
    "Organization"            = "RECODE — Anonymous factor codes",
    "Medical Organization"    = "RECODE — Anonymous factor codes",

    # To Be Removed — no analytical value
    "Free Text / Notes"       = "REMOVE — Free text may contain PHI",
    "Empty Column"            = "REMOVE — No data",
    "High-Uniqueness String"  = "REMOVE — Unstructured, high uniqueness",
    "Initials"                = "REMOVE — Non-informative for analysis",
    "Phone Extension"         = "REMOVE — Non-informative for analysis",

    # Non-identifiers → retain
    "Province"                = "RETAIN — Non-identifier",
    "Country"                 = "RETAIN — Non-identifier",
    "Medication"              = "RETAIN — Non-identifier",
    "Diagnosis"               = "RETAIN — Non-identifier",
    "Procedure Code"          = "RETAIN — Non-identifier",
    "Lab / Test Code"         = "RETAIN — Non-identifier",
    "Treatment"               = "RETAIN — Non-identifier",
    "Adverse Event"           = "RETAIN — Non-identifier",

    # Structural content labels → retain
    "Low-Cardinality Integer" = "RETAIN — Non-identifier",
    "Low-Cardinality String"  = "RETAIN — Non-identifier",
    "Moderate-Cardinality String" = "RETAIN — Non-identifier",
    "Continuous Numeric"      = "RETAIN — Non-identifier"
  )

  # Physician Name — uniqueness-dependent
  if (detected_element == "Physician Name") {
    if (uniqueness > uniqueness_threshold) {
      return("REMOVE — High-uniqueness physician name")
    } else {
      return("RECODE — Anonymous factor codes")
    }
  }

  result <- action_map[[detected_element]]
  if (is.null(result)) "FLAG — Requires manual classification"
  else result
}


# ============================================================================
# SECTION 3: Date Interval Conversion (Coded Data Mode)
# Converts all date columns to days relative to a per-patient anchor date.
# Converts DOB to age (in years, continuous) at the anchor date.
# This preserves all time intervals for analysis while removing calendar dates.
# ============================================================================

# Parse Date Column (handles many formats) ----
parse_date_column <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x))

  x_char <- as.character(x)
  formats <- c(
    "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d",
    "%d-%b-%Y", "%b %d, %Y", "%B %d, %Y", "%Y%m%d",
    "%d-%m-%Y", "%m-%d-%Y", "%d.%m.%Y", "%Y.%m.%d"
  )
  parsed <- rep(as.Date(NA), length(x_char))
  for (fmt in formats) {
    still_na <- is.na(parsed)
    if (!any(still_na)) break
    parsed[still_na] <- suppressWarnings(
      as.Date(x_char[still_na], format = fmt)
    )
  }
  parsed
}


# Convert Dates to Per-Patient Intervals ----
# This is the core function. It takes the entire data frame and converts
# all date columns to days relative to each patient's anchor date.
#
# Parameters:
#   df           — the data frame
#   date_cols    — character vector of column names containing dates
#   anchor_col   — name of the column to use as Day 0 per patient
#                  (e.g., "admit_date", "dx_date", "enrollment_date")
#   dob_col      — name of the DOB column (converted to age at anchor, not interval)
#                  Set to NA if no DOB column exists.
#
# Returns a list with:
#   df      — the modified data frame
#   log     — a data frame logging what was done
#
convert_dates_to_intervals <- function(df, date_cols, anchor_col, dob_col = NA) {

  log_entries <- data.frame(
    column_name = character(),
    action      = character(),
    detail      = character(),
    stringsAsFactors = FALSE
  )

  # Parse anchor column ----
  if (!(anchor_col %in% names(df))) {
    stop("Anchor column '", anchor_col, "' not found in data.")
  }
  anchor_dates <- parse_date_column(df[[anchor_col]])
  n_anchor_parsed <- sum(!is.na(anchor_dates))
  n_anchor_failed <- sum(is.na(anchor_dates) & !is.na(df[[anchor_col]]))

  cat("  Anchor column: ", anchor_col, "\n")
  cat("  Parsed:        ", n_anchor_parsed, "/", nrow(df), "\n")
  if (n_anchor_failed > 0) {
    cat("  WARNING:       ", n_anchor_failed, "non-NA values could not be parsed as dates.\n")
  }

  # Convert DOB to age at anchor ----
  if (!is.na(dob_col) && dob_col %in% names(df)) {
    dob_dates <- parse_date_column(df[[dob_col]])

    # Age in years (continuous, with decimal for precision)
    age_at_anchor <- as.numeric(difftime(anchor_dates, dob_dates, units = "days")) / 365.25
    age_at_anchor <- round(age_at_anchor, 2)

    # Replace DOB column with age
    new_col_name <- paste0("age_at_", anchor_col)
    df[[dob_col]] <- NULL
    df[[new_col_name]] <- age_at_anchor

    log_entries <- rbind(log_entries, data.frame(
      column_name = dob_col,
      action      = "CONVERTED — DOB to age at anchor",
      detail      = paste0("Replaced '", dob_col, "' with '", new_col_name,
                           "' (continuous years). Anchor: ", anchor_col, ".")
    ))
    cat("  DOB column '", dob_col, "' -> '", new_col_name, "' (age in years)\n")
  }

  # Convert each date column to days from anchor ----
  for (col in date_cols) {
    if (col == dob_col) next
    if (!(col %in% names(df))) next

    col_dates <- parse_date_column(df[[col]])

    # Days from anchor (can be negative if event is before anchor)
    days_from_anchor <- as.numeric(difftime(col_dates, anchor_dates, units = "days"))

    # Build new column name
    if (col == anchor_col) {
      # Anchor column itself becomes Day 0 (all zeros, or NA)
      new_col_name <- paste0(col, "_day")
      df[[col]] <- NULL
      df[[new_col_name]] <- ifelse(is.na(anchor_dates), NA_real_, 0)

      log_entries <- rbind(log_entries, data.frame(
        column_name = col,
        action      = "CONVERTED — Anchor date (Day 0)",
        detail      = paste0("Replaced '", col, "' with '", new_col_name,
                             "'. All values = 0 (reference point).")
      ))
    } else {
      new_col_name <- paste0(col, "_day")
      df[[col]] <- NULL
      df[[new_col_name]] <- days_from_anchor

      log_entries <- rbind(log_entries, data.frame(
        column_name = col,
        action      = "CONVERTED — Days from anchor",
        detail      = paste0("Replaced '", col, "' with '", new_col_name,
                             "' (days relative to ", anchor_col, ").",
                             " Range: [", min(days_from_anchor, na.rm = TRUE),
                             ", ", max(days_from_anchor, na.rm = TRUE), "].")
      ))
    }

    cat("  ", col, " -> ", new_col_name, "\n")
  }

  list(df = df, log = log_entries)
}


# ============================================================================
# SECTION 3b: Categorical Recoding (Anonymization via Factor Codes)
# Replaces categorical QI values with anonymous letter codes (A, B, C...).
# The codebook mapping codes back to original labels is stored separately.
# ============================================================================

# Generate Letter Codes for Factor Levels ----
# For <=26 levels uses A, B, C... For >26 uses AA, AB, etc.
make_letter_codes <- function(n) {
  if (n <= 26) {
    LETTERS[seq_len(n)]
  } else {
    codes <- character(n)
    for (i in seq_len(n)) {
      if (i <= 26) {
        codes[i] <- LETTERS[i]
      } else {
        prefix <- LETTERS[((i - 1) %/% 26)]
        suffix <- LETTERS[((i - 1) %% 26) + 1]
        codes[i] <- paste0(prefix, suffix)
      }
    }
    codes
  }
}


# Recode a Single Categorical Column ----
# Returns a list with:
#   recoded  — the recoded vector (A, B, C...)
#   codebook — a data frame mapping code -> original_value
recode_categorical <- function(x, colname = "") {
  unique_vals <- sort(unique(x[!is.na(x)]))
  codes <- make_letter_codes(length(unique_vals))
  lookup <- setNames(codes, as.character(unique_vals))

  recoded <- lookup[as.character(x)]
  recoded[is.na(x)] <- NA_character_

  codebook <- data.frame(
    column   = rep(colname, length(unique_vals)),
    code     = codes,
    original = as.character(unique_vals),
    stringsAsFactors = FALSE
  )

  list(recoded = recoded, codebook = codebook)
}


# ============================================================================
# SECTION 3c: Anonymization Treatment Functions
# These apply the UHN standard's specific rules for treating continuous
# quasi-identifiers so they become non-identifiers.
# ============================================================================

# Treat Age per UHN Standard ----
treat_age <- function(age_vec) {
  age_num <- suppressWarnings(as.numeric(age_vec))
  result <- character(length(age_num))

  result[is.na(age_num)] <- NA_character_

  # Over 89 → "90+"
  result[!is.na(age_num) & age_num > 89] <- "90+"

  # Under 31 days: would need separate flag; assume ages in years here
  # Under 2 years

  result[!is.na(age_num) & age_num < 2 & age_num >= 0] <- "<2 years"

  # Normal range: keep as integer
  normal <- !is.na(age_num) & age_num >= 2 & age_num <= 89
  result[normal] <- as.character(floor(age_num[normal]))

  # Check for small cells (<=10 instances per age) and bin if needed
  age_table <- table(result[normal])
  small_cells <- names(age_table)[age_table < 10]
  if (length(small_cells) > 0) {
    # Bin into 5-year groups for ages with <10 instances
    for (i in seq_along(result)) {
      if (!is.na(result[i]) && result[i] %in% small_cells) {
        a <- floor(age_num[i])
        bin_lower <- (a %/% 5) * 5
        bin_upper <- bin_lower + 4
        if (bin_upper > 89) bin_upper <- 89
        result[i] <- paste0(bin_lower, "-", bin_upper)
      }
    }
  }

  result
}


# Treat Date per UHN Standard (convert to year or interval) ----
treat_date_to_year <- function(date_vec) {
  # Try to parse if character
  if (is.character(date_vec)) {
    formats <- c(
      "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d",
      "%d-%b-%Y", "%b %d, %Y", "%B %d, %Y", "%Y%m%d",
      "%d-%m-%Y", "%m-%d-%Y"
    )
    parsed <- rep(as.Date(NA), length(date_vec))
    for (fmt in formats) {
      still_na <- is.na(parsed)
      if (!any(still_na)) break
      parsed[still_na] <- suppressWarnings(
        as.Date(date_vec[still_na], format = fmt)
      )
    }
    date_vec <- parsed
  }

  if (inherits(date_vec, "POSIXct")) date_vec <- as.Date(date_vec)

  format(date_vec, "%Y")
}


# Treat Date as Interval (days from earliest date) ----
treat_date_to_interval <- function(date_vec) {
  if (is.character(date_vec)) {
    formats <- c(
      "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d",
      "%d-%b-%Y", "%b %d, %Y", "%B %d, %Y", "%Y%m%d",
      "%d-%m-%Y", "%m-%d-%Y"
    )
    parsed <- rep(as.Date(NA), length(date_vec))
    for (fmt in formats) {
      still_na <- is.na(parsed)
      if (!any(still_na)) break
      parsed[still_na] <- suppressWarnings(
        as.Date(date_vec[still_na], format = fmt)
      )
    }
    date_vec <- parsed
  }

  if (inherits(date_vec, "POSIXct")) date_vec <- as.Date(date_vec)

  day_zero <- min(date_vec, na.rm = TRUE)
  as.numeric(date_vec - day_zero)
}


# Treat Postal Code (retain first 3 characters / FSA only) ----
treat_postal_code <- function(pc_vec) {
  substr(gsub(" ", "", as.character(pc_vec)), 1, 3)
}


# Treat Height per UHN Standard (requires sex column) ----
treat_height <- function(height_vec, sex_vec, unit = "cm") {
  h <- suppressWarnings(as.numeric(height_vec))
  sex_lower <- tolower(as.character(sex_vec))
  result <- as.character(h)

  for (i in seq_along(h)) {
    if (is.na(h[i]) || is.na(sex_lower[i])) next

    if (sex_lower[i] %in% c("m", "male")) {
      if (unit == "cm") {
        if (h[i] > 199) result[i] <- "199+ cm"
        else if (h[i] < 155) result[i] <- "<155 cm"
      }
    } else if (sex_lower[i] %in% c("f", "female")) {
      if (unit == "cm") {
        if (h[i] > 184) result[i] <- "184+ cm"
        else if (h[i] < 142) result[i] <- "<142 cm"
      }
    }
  }
  result
}


# Treat Weight per UHN Standard (requires sex column) ----
treat_weight <- function(weight_vec, sex_vec, unit = "kg") {
  w <- suppressWarnings(as.numeric(weight_vec))
  sex_lower <- tolower(as.character(sex_vec))
  result <- as.character(w)

  for (i in seq_along(w)) {
    if (is.na(w[i]) || is.na(sex_lower[i])) next

    if (sex_lower[i] %in% c("m", "male")) {
      if (unit == "kg") {
        if (w[i] > 137) result[i] <- "137+ kg"
        else if (w[i] < 40) result[i] <- "<40 kg"
      } else if (unit == "lbs") {
        if (w[i] > 300) result[i] <- "300+ lbs"
        else if (w[i] < 88) result[i] <- "<88 lbs"
      }
    } else if (sex_lower[i] %in% c("f", "female")) {
      if (unit == "kg") {
        if (w[i] > 116) result[i] <- "116+ kg"
        else if (w[i] < 23) result[i] <- "<23 kg"
      } else if (unit == "lbs") {
        if (w[i] > 254) result[i] <- "254+ lbs"
        else if (w[i] < 51) result[i] <- "<51 lbs"
      }
    }
  }
  result
}


# Generate Sequential Participant IDs ----
generate_participant_ids <- function(n) {
  seq_len(n)
}


# ============================================================================
# SECTION 4: Equivalence Class Check
# ============================================================================

# Check Equivalence Class Sizes ----
check_equivalence_classes <- function(df, quasi_id_cols) {
  if (length(quasi_id_cols) == 0) {
    return(list(
      min_class_size   = Inf,
      meets_threshold  = TRUE,
      message          = "No quasi-identifiers remain. Equivalence class check not applicable."
    ))
  }

  # Count unique combinations
  qi_subset <- df[, quasi_id_cols, drop = FALSE]
  qi_subset[] <- lapply(qi_subset, as.character)
  combo_counts <- as.data.frame(table(qi_subset), stringsAsFactors = FALSE)
  min_size <- min(combo_counts$Freq)

  list(
    min_class_size   = min_size,
    meets_threshold  = min_size >= 10,
    small_classes    = combo_counts[combo_counts$Freq < 10, ],
    message          = if (min_size >= 10) {
      paste0("PASS: Minimum equivalence class size is ", min_size, " (>= 10).")
    } else {
      paste0(
        "WARNING: Minimum equivalence class size is ", min_size,
        " (< 10). Consider generalizing quasi-identifiers further ",
        "or removing small-cell records."
      )
    }
  )
}


# ============================================================================
# SECTION 5: High-Risk Factor Checklist
# ============================================================================

# Default Risk Checklist ----
get_risk_checklist <- function() {
  data.frame(
    risk_factor = c(
      "1. Public release or no information practices agreement",
      "2. Audience acquainted with patients / has access to source data",
      "3. Data valuable enough to motivate re-identification attempts",
      "4. Rare diagnosis, presentation, or adverse event present",
      "5. Sensitive data (mental health, sexual health, genetic)"
    ),
    present = c("NO", "NO", "NO", "NO", "NO"),
    notes = rep("", 5),
    stringsAsFactors = FALSE
  )
}


# ============================================================================
# SECTION 6: Sample Values Helper
# ============================================================================

# Get Representative Sample Values (for display) ----
get_sample_values <- function(x, n = 5) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("(all NA)")
  samples <- head(unique(x), n)
  paste(samples, collapse = " | ")
}


# ============================================================================
# SECTION 7: UHN Data Elements Reference Table
# ============================================================================

# UHN Reference Table ----
get_uhn_reference_table <- function() {
  data.frame(
    data_element = c(
      "Address", "Age", "City (pop > 5000)", "City (pop < 5000)",
      "Country", "Date (any)", "Date of Birth", "Date of Death",
      "Driver's License", "Email Address", "Ethnicity",
      "Family Member Names", "First Name", "Full Name",
      "Gender", "Health Card Number", "Height",
      "ICD9/10 Code", "Unique ID / MRN", "International Phone",
      "Initials", "IP Address", "Last Name", "Marital Status",
      "Medical Organization", "Medical Record Number", "Medication",
      "Middle Name", "Nationality", "Occupation", "Organization",
      "Participant ID (research)", "Phone Number", "Physician Name",
      "Postal Code", "Province/Territory", "Sex (assigned at birth)",
      "SIN", "SSN", "Street Address", "URL", "Weight", "Year of Birth"
    ),
    classification = c(
      "Direct Identifier",
      "Quasi-ID (Non-ID if treated)",
      "Quasi-ID (Non-ID if treated)",
      "Quasi-ID (Non-ID if treated)",
      "Non-Identifier",
      "Quasi-ID (Non-ID if treated)",
      "Quasi-ID (Non-ID if treated)",
      "Quasi-ID (Non-ID if treated)",
      "Direct Identifier",
      "Direct Identifier",
      "Quasi-ID",
      "Quasi-ID (Non-ID if treated)",
      "Direct Identifier",
      "Direct Identifier",
      "Quasi-ID",
      "Direct Identifier",
      "Quasi-ID (Non-ID if treated)",
      "Non-ID (unless rare Dx)",
      "Direct Identifier",
      "Direct Identifier",
      "Quasi-ID",
      "Direct Identifier",
      "Direct Identifier",
      "Quasi-ID",
      "Quasi-ID",
      "Direct Identifier",
      "Non-ID (unless rare Dx)",
      "Direct Identifier",
      "Quasi-ID",
      "Quasi-ID",
      "Quasi-ID",
      "Quasi-ID",
      "Direct Identifier",
      "Quasi-ID",
      "Quasi-ID (Non-ID if treated)",
      "Non-Identifier",
      "Quasi-ID",
      "Direct Identifier",
      "Direct Identifier",
      "Direct Identifier",
      "Direct Identifier",
      "Quasi-ID (Non-ID if treated)",
      "Quasi-ID (Non-ID if treated)"
    ),
    treatment = c(
      "Remove",
      "Over 89 -> 90+; under 2 -> <2 yrs; bin small cells",
      "Retain if pop > 5,000",
      "Use County/Region (pop > 5,000)",
      "Retain, if necessary",
      "Year only or intervals from Day 0",
      "Year only or intervals from Day 0",
      "Year only or intervals from Day 0",
      "Remove",
      "Remove",
      "Retain if population >= 5,000",
      "Remove",
      "Remove",
      "Remove",
      "Retain if even distribution; remove unique records",
      "Remove",
      "Apply sex-specific thresholds",
      "Retain; flag rare diagnoses",
      "Remove or pseudonymize",
      "Remove",
      "Retain, if necessary",
      "Remove",
      "Remove",
      "Retain, if necessary",
      "Retain if dataset >= 5,000 UHN subjects",
      "Remove",
      "Retain; flag if rare or rarely prescribed",
      "Remove",
      "Retain if population >= 5,000",
      "Retain, if necessary",
      "Retain, if necessary",
      "Retain; pseudonymize for external sharing",
      "Remove",
      "Retain, if necessary",
      "First 3 digits only (FSA)",
      "Retain, if necessary",
      "Retain if even distribution; min 5 of each",
      "Remove",
      "Remove",
      "Remove",
      "Remove",
      "Apply sex-specific thresholds",
      "Apply equivalent Age treatment"
    ),
    stringsAsFactors = FALSE
  )
}
