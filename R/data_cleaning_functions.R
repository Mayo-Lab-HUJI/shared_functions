#' Remove irrelevant rows and columns from a dataset
#'
#' This function removes the first two rows and specific columns that are irrelevant for analysis,
#' typically introduced by Qualtrics.
#'
#' @param myData A data frame.
#' @return A cleaned data frame with the first two rows removed and specific columns dropped.
#' @export
remove_qualtrics_default_fields <- function(myData) {

  # Define the list of column names to remove
  col_names_to_exclude <- c("StartDate", "EndDate", "Status", "IPAddress", "Finished", "RecordedDate",
                            "ResponseId", "RecipientLastName", "RecipientFirstName", "RecipientEmail",
                            "ExternalReference", "LocationLatitude", "LocationLongitude", "DistributionChannel",
                            "UserLanguage")

  # Remove the first two rows
  myData <- myData[-c(1,2), ]

  # Remove columns by name (only if they exist in the dataset)
  existing_cols <- intersect(col_names_to_exclude, colnames(myData))
  myData <- myData[, !colnames(myData) %in% existing_cols]

  # Print the number of remaining rows
  cat("Raw data:", nrow(myData), "\n")

  return(myData)
}



#' Remove irrelevant rows and columns from a Dataset
#'
#' This function removes rows and columns introduced by Question Pro.
#'
#' @param myData A data frame.
#' @return A data frame with row 1 and columns 1,3, 4-5, 7-20 removed.
#' @export
remove_QP_default_fields <- function(myData) {

  # omit row 1
  myData <- myData[2:nrow(myData), ]

  # omit columns 1,3,4-5, 7-20
  myData <- myData[ ,-c(1,3,4:5,7:20)]

  # Return the cleaned data
  cat ("Raw data: ", nrow(myData))
  return(myData)
}



#' Remove click counter and timer columns
#'
#' This function removes Qualtrics click counters and timers by identifying columns with the following suffixes:
#' "First.Click", "Last.Click", "Page.Submit", "Click.Count"
#'
#' @param myData A data frame.
#' @return A data frame with no click counters and timers.
#' @export
remove_clicks_counters <- function(myData) {
  # Hardcoded suffixes to remove
  suffixes <- c("First.Click", "Last.Click", "Page.Submit", "Click.Count")
  pattern <- paste0("(", paste(suffixes, collapse = "|"), ")$")

  # Find columns with matching suffixes
  matching_columns <- grepl(pattern, colnames(myData))
  columns_to_remove <- colnames(myData)[matching_columns]

  # Identify suffixes not found in any column
  suffix_found <- sapply(suffixes, function(suffix) any(grepl(paste0(suffix, "$"), colnames(myData))))
  missing_suffixes <- suffixes[!suffix_found]

  # Messages about column removal
  if (length(columns_to_remove) > 0) {
    myData <- myData[, !matching_columns, drop = FALSE]
    message("Removed columns:\n", paste(columns_to_remove, collapse = ", "))
  } else {
    message("No columns with the specified suffixes were found.")
  }

  # Messages about missing suffixes
  if (length(missing_suffixes) > 0) {
    message("Suffixes not found in any column:\n", paste(missing_suffixes, collapse = ", "))
  } else {
    message("All specified suffixes were found in the myDataset.")
  }

  return(myData)
}




#' Find Duplicate IDs and Their Progress Values
#'
#' This function identifies duplicate IDs in a dataset and retrieves the corresponding
#' progress values for each occurrence.
#'
#' @param myData A data frame containing the ID and progress columns.
#' @param id A character string specifying the column name of the IDs. Default is `"Prolific_ID"`.
#' @param progress A character string specifying the column name of the progress values. Default is `"Progress"`.
#'
#' @return A data frame containing duplicated IDs and their corresponding progress values.
#' @export
find_duplicate_ids <- function(myData, id = "Prolific_ID", progress = "Progress") {
  # Ensure the ID and Progress columns exist in the data frame
  if (!id %in% names(myData)) {
    stop("The specified ID column does not exist in the data frame.")
  }

  if (!progress %in% names(myData)) {
    stop("The specified Progress column does not exist in the data frame.")
  }

  # Count occurrences of each ID
  id_counts <- table(myData[[id]])

  # Extract IDs that appear more than once
  duplicate_ids <- names(id_counts[id_counts > 1])

  # Filter data for only duplicated IDs and select relevant columns
  duplicate_data <- myData[myData[[id]] %in% duplicate_ids, c(id, progress)]

  # Return the filtered data
  return(duplicate_data)
}



#' Exclude Participants Based on Predefined Criteria
#'
#' This function filters participants from a dataset based on various exclusion criteria, including readiness, duration, consent, progress, and more.
#' It tracks participants who fail each criterion, summarizes their failures, and removes them step by step from the dataset.
#'
#' @param myData A data frame containing participant data.
#' @param id A character string specifying the column name that holds unique participant IDs. Defaults to "Prolific_ID".
#' @param ready A character string specifying the column name that indicates readiness. Defaults to "Ready".
#' @param duration A character string specifying the column name for the duration in seconds. Defaults to "Duration..in.seconds.".
#' @param consent A character string specifying the column name for participant consent. Defaults to "Consent".
#' @param progress A character string specifying the column name for progress. Defaults to "Progress".
#' @param consecutively A character string specifying the column name for consecutive completions. Defaults to "Consecutively".
#' @param disturbance A character string specifying the column name for disturbance status. Defaults to "Disturbance".
#' @param quality_check A character string specifying the column name for quality check status. Defaults to "Quality_check".
#' @param age A character string specifying the column name for participant age. Defaults to "Age".
#' @param ageRange A numeric vector of length two specifying the valid age range. Defaults to `c(18, 80)`.
#'
#' @return A list with the following components:
#' \itemize{
#'   \item `filtered_data`: The filtered data frame after applying all exclusion criteria.
#'   \item `summary_table`: A summary table containing:
#'     \itemize{
#'       \item `Participant_ID`: IDs of participants who failed at least one criterion.
#'       \item `Failed_Conditions_Count`: Number of conditions each participant failed.
#'       \item `Failed_Conditions`: A comma-separated list of criteria failed by each participant.
#'     }
#' }
#'
#' @details
#' The function applies a series of exclusion criteria to the dataset:
#' \itemize{
#'   \item Participants with missing or empty IDs are removed.
#'   \item Readiness: Participants with `Ready` not equal to 1 are removed.
#'   \item Duration: Participants with duration 0, exceeding a timeout value (2640 seconds), or exceeding the mean duration plus three standard deviations are removed.
#'   \item Consent: Participants with `Consent` not equal to 1 are removed.
#'   \item Progress: Participants with `Progress` not equal to 100 are removed.
#'   \item Consecutiveness: Participants with `Consecutively` not equal to 2 are removed.
#'   \item Disturbance: Participants with `Disturbance` not equal to 1 are removed.
#'   \item Quality Check: Participants with `Quality_check` not equal to 1 are removed.
#'   \item Age: Participants outside the specified `ageRange` are removed.
#'
#' }
#'
#' @export
#'
exclude_participants_by_criteria <- function(myData, id = "Prolific_ID", ready = "Ready", duration = "Duration..in.seconds.",
                                             consent = "Consent", progress = "Progress",
                                             consecutively = "Consecutively", disturbance = "Disturbance",
                                             quality_check = "Quality_check", age = "Age", ageRange = c(18, 80)) {

  # Convert all the variables to numeric
  numeric_vars <- c(ready, duration, consent, progress, consecutively, disturbance, quality_check, age)
  for (var in numeric_vars) {
    if (var %in% colnames(myData)) {
      myData[[var]] <- as.numeric(myData[[var]])
    }
  }

  # Helper function to check if a column exists
  validate_column <- function(column_name) {
    if (!column_name %in% colnames(myData)) {
      message("Column missing: ", column_name, " - skipping this criterion.")
      return(FALSE)
    }
    return(TRUE)
  }

  # Initialize exclusion tracking
  exclusions <- list()

  # Remove empty rows (no ID)
  emptyIdRows <- sum(is.na(myData[[id]]) | myData[[id]] == "")
  myData <- myData[!(is.na(myData[[id]]) | myData[[id]] == ""), ]
  message("Removed rows with no ID: ", emptyIdRows)

  # Data before exclusions
  message("N before exclusions: ", nrow(myData))

  # Record exclusions without removal
  if (validate_column(ready)) {
    exclusions$unready <- myData[[id]][myData[[ready]] != 1]
  }

  if (validate_column(duration)) {
    myData[[duration]] <- as.numeric(myData[[duration]])
    exclusions$zeroDuration <- myData[[id]][myData[[duration]] == 0]
    exclusions$timeOut <- myData[[id]][myData[[duration]] > 2640]
    durationMean <- mean(myData[[duration]], na.rm = TRUE)
    durationSD <- sd(myData[[duration]], na.rm = TRUE)
    cutOffVal <- durationMean + 3 * durationSD
    exclusions$exceededDuration <- myData[[id]][myData[[duration]] > cutOffVal]
  }

  if (validate_column(consent)) {
    exclusions$dissent <- myData[[id]][myData[[consent]] != 1]
  }

  if (validate_column(progress)) {
    exclusions$incomplete <- myData[[id]][myData[[progress]] != 100]
  }

  if (validate_column(consecutively)) {
    exclusions$notConsecutively <- myData[[id]][myData[[consecutively]] != 2]
  }

  if (validate_column(disturbance)) {
    exclusions$disturbed <- myData[[id]][myData[[disturbance]] != 1]
  }

  if (validate_column(quality_check)) {
    exclusions$qualityFailed <- myData[[id]][myData[[quality_check]] != 1]
  }

  if (validate_column(age)) {
    exclusions$wrongAge <- myData[[id]][!(myData[[age]] >= ageRange[1] & myData[[age]] <= ageRange[2])]
  }

  # Create a summary table before exclusions
  all_exclusions <- unique(unlist(exclusions))
  summary_table <- data.frame(Participant_ID = all_exclusions)
  for (criterion in names(exclusions)) {
    summary_table[[criterion]] <- summary_table$Participant_ID %in% exclusions[[criterion]]
  }

  # Add number of failed conditions
  summary_table$Failed_Conditions_Count <- rowSums(summary_table[-1])

  # Add failed conditions as a comma-separated string
  summary_table$Failed_Conditions <- apply(
    summary_table[-c(1, ncol(summary_table))],
    1,
    function(row) paste(names(row)[row], collapse = ", ")
  )

  # Keep only desired columns
  summary_table <- summary_table[, c("Participant_ID", "Failed_Conditions_Count", "Failed_Conditions")]

  # Print summary table
  message("Summary of participants failing criteria:")
  print(summary_table)

  # Start removing participants based on each condition
  if (validate_column(ready)) {
    unreadyRows <- sum(myData[[ready]] != 1)
    myData <- myData[myData[[ready]] == 1, ]
    message("Removed unready participants: ", unreadyRows)
  }

  if (validate_column(duration)) {
    zeroDurationRows <- sum(myData[[duration]] == 0)
    myData <- myData[myData[[duration]] > 0, ]
    message("Removed participants with zero duration: ", zeroDurationRows)

    timeOutRows <- sum(myData[[duration]] > 2640)
    myData <- myData[myData[[duration]] <= 2640, ]
    message("Removed participants that timed out: ", timeOutRows)

    exceededDurationRow <- sum(myData[[duration]] > cutOffVal)
    myData <- myData[myData[[duration]] <= cutOffVal, ]
    message("Removed participants exceeding mean + 3*SD: ", exceededDurationRow)
  }

  if (validate_column(consent)) {
    dissentRows <- sum(myData[[consent]] != 1)
    myData <- myData[myData[[consent]] == 1, ]
    message("Removed dissenting participants: ", dissentRows)
  }

  if (validate_column(progress)) {
    incompleteRows <- sum(myData[[progress]] != 100)
    myData <- myData[myData[[progress]] == 100, ]
    message("Removed incomplete participants: ", incompleteRows)
  }

  if (validate_column(consecutively)) {
    notConsecutivelyRows <- sum(myData[[consecutively]] != 2)
    myData <- myData[myData[[consecutively]] == 2, ]
    message("Removed participants not completing consecutively: ", notConsecutivelyRows)
  }

  if (validate_column(disturbance)) {
    disturbedRows <- sum(myData[[disturbance]] != 1)
    myData <- myData[myData[[disturbance]] == 1, ]
    message("Removed disturbed participants: ", disturbedRows)
  }

  if (validate_column(quality_check)) {
    qualityFailedRows <- sum(myData[[quality_check]] != 1)
    myData <- myData[myData[[quality_check]] == 1, ]
    message("Removed participants failing quality check: ", qualityFailedRows)
  }

  if (validate_column(age)) {
    wrongAgeRows <- sum(!(myData[[age]] >= ageRange[1] & myData[[age]] <= ageRange[2]))
    myData <- myData[(myData[[age]] >= ageRange[1] & myData[[age]] <= ageRange[2]), ]
    message("Removed participants with wrong age: ", wrongAgeRows)
  }

  # Final N after exclusions
  message("Final N after exclusions: ", nrow(myData))

  return(list(filtered_data = myData, summary_table = summary_table))
}

