# Fixed-effect meta-analysis of influenza vaccine effectiveness
#
# This script reproduces the fixed-effect analyses summarised in Table S2 for:
#   * Influenza A
#   * Influenza B
#   * Any influenza
# across:
#   * All populations
#   * Children under 5 years
#   * General population
#   * Adults aged 65 years and over
#
# Required packages: readxl, dplyr, metafor
#
# Interactive use in RStudio:
#   1. Place this script and IVE_SLR_datapoints_Updated_06032024.xlsx in the
#      same folder.
#   2. Set that folder as the working directory.
#   3. Source this script.
#
# Command-line use:
#   Rscript IVE_fixed_effect_meta_analysis.R [input_workbook] [output_folder]

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(metafor)
})


# Configuration -----------------------------------------------------------

default_input_file <- "IVE_SLR_datapoints_18092026.xlsx"
default_output_dir <- "output"

# The example plot uses a reasonably sized analysis (27 estimates). Change
# either label to create a forest plot for another analysis in Table S2.
forest_plot_influenza_category <- "Influenza A"
forest_plot_population <- "Under 5's"

command_line_args <- commandArgs(trailingOnly = TRUE)

if (!interactive() && length(command_line_args) >= 1) {
  input_file <- command_line_args[[1]]
} else {
  input_file <- default_input_file
}

if (!interactive() && length(command_line_args) >= 2) {
  output_dir <- command_line_args[[2]]
} else {
  output_dir <- default_output_dir
}

if (!file.exists(input_file)) {
  stop(
    "Input workbook not found: ", input_file, "\n",
    "Place the workbook in the working directory or supply its path as the ",
    "first command-line argument."
  )
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


# Import and validate the source data -------------------------------------

# Reading all columns as text prevents Excel's display formatting from
# changing identifiers or mixed-format descriptive columns. Numeric analysis
# fields are converted explicitly below.
ive_data <- read_excel(
  path = input_file,
  sheet = "all",
  col_types = "text"
)

required_columns <- c(
  "id_unique",
  "id_ref",
  "stu_author",
  "stu_yop",
  "est_country",
  "est_season",
  "est_population",
  "est_outbeyer",
  "est_measure",
  "est_design",
  "est_value",
  "est_lci",
  "est_uci",
  "est_virustype_rec2_all",
  "est_virustype_rec2_a",
  "est_virustype_rec2_b"
)

missing_columns <- setdiff(required_columns, names(ive_data))

if (length(missing_columns) > 0) {
  stop(
    "The input workbook is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

ive_data <- ive_data %>%
  mutate(
    est_value = as.numeric(est_value),
    est_lci = as.numeric(est_lci),
    est_uci = as.numeric(est_uci)
  )


eligible_data <- ive_data %>%
  filter(
    est_design == "tn",
    est_outbeyer == "lab",
    est_measure == "ave"
  )


# Analysis definitions ----------------------------------------------------

influenza_categories <- c(
  "Influenza A" = "est_virustype_rec2_a",
  "Influenza B" = "est_virustype_rec2_b",
  "Any influenza" = "est_virustype_rec2_all"
)

age_65_labels <- c(
  "adults >= 65 years",
  "adults ≥ 65 years",
  "adults ‚â• 65 years"
)

population_filters <- list(
  "All" = function(data) data,
  "Under 5's" = function(data) {
    data %>% filter(est_population == "children < 5 years")
  },
  "General population" = function(data) {
    data %>% filter(est_population == "general population")
  },
  "65 and over" = function(data) {
    data %>% filter(est_population %in% age_65_labels)
  }
)


# Fixed-effect model ------------------------------------------------------

run_fixed_effect_model <- function(data, influenza_category, rec2_variable,
                                   population) {
  analysis_data <- data %>%
    filter(.data[[rec2_variable]] == "include")

  if (nrow(analysis_data) == 0) {
    stop(
      "No eligible estimates for ", influenza_category,
      " in population group ", population, "."
    )
  }

  invalid_rows <- analysis_data %>%
    filter(
      is.na(est_value) | is.na(est_lci) | is.na(est_uci) |
        est_lci >= 100 | est_uci >= 100 |
        est_lci >= est_uci
    )

  if (nrow(invalid_rows) > 0) {
    stop(
      "Invalid or non-transformable IVE confidence intervals found for ",
      influenza_category, " / ", population, "."
    )
  }


  analysis_data <- analysis_data %>%
    mutate(
      # IVE = (1 - OR) * 100, hence OR = 1 - IVE / 100.
      odds_ratio = 1 - est_value / 100,
      odds_ratio_lower = 1 - est_uci / 100,
      odds_ratio_upper = 1 - est_lci / 100,
      yi = log(odds_ratio),
      # A 95% CI spans 2 * 1.96 = 3.92 standard errors.
      sei = (log(odds_ratio_upper) - log(odds_ratio_lower)) / 3.92,
      vi = sei^2
    )

  if (any(!is.finite(analysis_data$yi)) ||
      any(!is.finite(analysis_data$vi)) ||
      any(analysis_data$vi <= 0)) {
    stop(
      "Non-finite effect sizes or non-positive variances found for ",
      influenza_category, " / ", population, "."
    )
  }

  model <- rma(
    yi = yi,
    vi = vi,
    method = "FE",
    data = analysis_data
  )

  pooled_or <- exp(as.numeric(model$b))
  pooled_or_lci <- exp(model$ci.lb)
  pooled_or_uci <- exp(model$ci.ub)

  # Because IVE = (1 - OR) * 100 is a decreasing transformation, the OR
  # confidence limits reverse when converted back to IVE.
  result <- data.frame(
    influenza_category = influenza_category,
    rec2_variable = rec2_variable,
    population = population,
    ive_percent = (1 - pooled_or) * 100,
    ive_lower_ci = (1 - pooled_or_uci) * 100,
    ive_upper_ci = (1 - pooled_or_lci) * 100,
    number_of_ve_estimates = model$k,
    i2_percent = model$I2,
    heterogeneity_q = model$QE,
    heterogeneity_p = model$QEp,
    pooled_odds_ratio = pooled_or,
    odds_ratio_lower_ci = pooled_or_lci,
    odds_ratio_upper_ci = pooled_or_uci,
    stringsAsFactors = FALSE
  )

  list(
    result = result,
    model = model,
    data = analysis_data
  )
}


# Run the 12 Table S2 analyses -------------------------------------------

analysis_objects <- list()
result_rows <- list()
result_index <- 1

for (influenza_category in names(influenza_categories)) {
  rec2_variable <- influenza_categories[[influenza_category]]

  for (population in names(population_filters)) {
    population_data <- population_filters[[population]](eligible_data)

    analysis <- run_fixed_effect_model(
      data = population_data,
      influenza_category = influenza_category,
      rec2_variable = rec2_variable,
      population = population
    )

    analysis_key <- paste(influenza_category, population, sep = " | ")
    analysis_objects[[analysis_key]] <- analysis
    result_rows[[result_index]] <- analysis$result
    result_index <- result_index + 1
  }
}

results <- bind_rows(result_rows) %>%
  mutate(
    influenza_category = factor(
      influenza_category,
      levels = names(influenza_categories)
    ),
    population = factor(
      population,
      levels = names(population_filters)
    )
  ) %>%
  arrange(influenza_category, population) %>%
  mutate(
    influenza_category = as.character(influenza_category),
    population = as.character(population),
    p_display = ifelse(
      heterogeneity_p < 0.001,
      "<0.001",
      formatC(heterogeneity_p, digits = 3, format = "f")
    )
  )

results_file <- file.path(output_dir, "Table_S2_fixed_effect_results.csv")
write.csv(results, results_file, row.names = FALSE)


# Example forest plot -----------------------------------------------------

plot_key <- paste(
  forest_plot_influenza_category,
  forest_plot_population,
  sep = " | "
)

if (!plot_key %in% names(analysis_objects)) {
  stop(
    "Forest-plot selection does not match an analysis: ", plot_key
  )
}

plot_analysis <- analysis_objects[[plot_key]]
plot_data <- plot_analysis$data
plot_model <- plot_analysis$model

forest_file <- file.path(
  output_dir,
  "Example_forest_plot_influenza_A_under_5.pdf"
)

pdf(
  file = forest_file,
  width = 10,
  height = max(8, 3.5 + 0.24 * nrow(plot_data)),
  onefile = TRUE
)

forest(
  plot_model,
  atransf = exp,
  at = log(c(0.05, 0.25, 1, 4)),
  xlim = c(-6, 4),
  slab = paste(
    plot_data$stu_author,
    plot_data$est_country,
    plot_data$stu_yop,
    plot_data$est_season,
    sep = "_"
  ),
  header = c(
    "Author_Country_Year_Season",
    "Odds Ratio [95% CI]"
  ),
  cex = 0.75,
  xlab = "Odds Ratio (log scale)",
  mlab = "",
  main = paste0(
    "Fixed-effect analysis: ", forest_plot_influenza_category,
    "\nPopulation: ", forest_plot_population,
    "; laboratory-confirmed; adjusted VE; test-negative design"
  ),
  cex.main = 0.8
)

text(
  -6,
  -1,
  pos = 4,
  cex = 0.75,
  bquote(
    paste(
      "FE Model (Q = ",
      .(formatC(plot_model$QE, digits = 2, format = "f")),
      ", df = ", .(plot_model$k - plot_model$p),
      ", p = ", .(format.pval(plot_model$QEp, digits = 3, eps = 0.001)),
      "; ", I^2, " = ",
      .(formatC(plot_model$I2, digits = 1, format = "f")), "% )"
    )
  )
)

dev.off()


# Reproducibility information --------------------------------------------

session_info_file <- file.path(output_dir, "R_session_info.txt")
capture.output(sessionInfo(), file = session_info_file)


# Console summary ---------------------------------------------------------

display_results <- results %>%
  transmute(
    influenza_category,
    population,
    IVE = round(ive_percent, 2),
    lower_CI = round(ive_lower_ci, 2),
    upper_CI = round(ive_upper_ci, 2),
    k = number_of_ve_estimates,
    I2 = round(i2_percent, 2),
    p = p_display
  )

print(display_results, row.names = FALSE)
cat("\nCreated:\n")
cat(" - ", results_file, "\n", sep = "")
cat(" - ", forest_file, "\n", sep = "")
cat(" - ", session_info_file, "\n", sep = "")
