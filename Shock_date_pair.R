### Section to Update Identified Events Post-2008 ###

# create date column based on the known range of observations
hp_data$date <- seq.Date(
  from = as.Date("1990-01-01"),  # start date based on known data range
  by = "month",                  # monthly frequency
  length.out = nrow(hp_data)     # match the number of rows in hp_data
)

# verify date alignment
print("Range of dates in hp_data:")
print(range(hp_data$date))

# filter data to include only observations from November 1, 2008, to November 1, 2024
filtered_data <- hp_data %>%
  filter(date >= as.Date("2008-11-01") & date <= as.Date("2024-11-01"))

# ensure there are rows to process
if (nrow(filtered_data) == 0) {
  stop("No valid data available for the specified date range (November 1, 2008 to November 1, 2024).")
}

# compute the threshold for significance (mean + 2 * standard deviation)
uncertainty_threshold <- mean(filtered_data$Uncertainty_Cycle, na.rm = TRUE) + 
  2 * sd(filtered_data$Uncertainty_Cycle, na.rm = TRUE)

# debugging: print threshold
print(paste("Uncertainty Threshold:", uncertainty_threshold))

# function to detect local maxima
find_peaks <- function(series, window = 3) {
  rollapply(series, width = window, by = 1, partial = FALSE, align = "center", 
            FUN = function(x) which.max(x) == ceiling(length(x) / 2), 
            fill = NA)  # avoid edge padding
}

# identify significant peaks in the Uncertainty Cycle
filtered_data <- filtered_data %>%
  mutate(
    Uncertainty_Peak = find_peaks(Uncertainty_Cycle) & 
      (Uncertainty_Cycle > uncertainty_threshold)
  )

# filter for dates with significant peaks
significant_uncertainty_peaks <- filtered_data %>%
  filter(Uncertainty_Peak) %>%
  dplyr::select(date, Uncertainty_Cycle) %>%
  mutate(Event_Type = "Significant Uncertainty Peak")

# debugging: ensure no invalid dates or observations are included
print("Range of significant peaks:")
print(range(significant_uncertainty_peaks$date))

# print identified significant peaks
print("Identified significant peaks in the uncertainty series:")
print(significant_uncertainty_peaks)


