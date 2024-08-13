
# This SwissCanto page page lists the sybmol: <https://products.swisscanto.com/products/product>
# For example: <https://www.bloomberg.com/quote/ZKBOAAF:SW>
# https://www.bxswiss.com/instruments/CH0215804680

# Security Number 21580468
# ISIN-Number CH0215804680
# Bloomberg SWCSLCG SW

# library(quantmod)

# download links:
	# https://products.swisscanto.com/products/api/factsheet/isin/CH0325172903/file-name/FSD_CH0325172903_SWC_CH_en.pdf
# 
# ISIN_variable <-  "CH0325172903"
# https://products.swisscanto.com/products/api/factsheet/isin/"ISIN_variable"/file-name/FSD_"ISIN_variable"_SWC_CH_en.pdf


library(pdftools)
library(dplyr)
library(tidyr)
library(stringr)

# Example to extract text from a single PDF
text <- pdf_text("../data/FSD_CH1201876484_SWC_CH_en.pdf")[1]  # assuming data is on the first page

# Splitting text into lines
text <- strsplit(text, "\n")[[1]]

# Remove excessive whitespaces and normalize the spaces
text <- str_replace_all(text, "\\s+", " ")

df <- as.data.frame(text)

df

# Categorization of Data Types
# Currency Values:
# 	
# 	NAV per Share
# 52-Week High
# 52-Week Low
# Share Class Volume (million)
# Fund Volume (million)
# Last distribution
# Flat Fee p.a.
# Total Expense Ratio p.a.
# Issue Fees in Favour of the Fund
# Redemption Fees in Favour of the Fund
# Dates:
# 	
# 	Share Class Launch Date
# Inception Date Fund
# Start Performance Calculation
# Accounting Year End
# Counts/Numbers:
# 	
# 	Shares Outstanding
# Security Number
# Percentages:
# 	
# 	Flat Fee p.a.
# Total Expense Ratio p.a.
# Issue Fees in Favour of the Fund
# Redemption Fees in Favour of the Fund
# Plain Text:
# 	
# 	Fund Domicile
# Share Class Currency
# Fund Currency
# Investment Method
# Securities Lending
# Benchmark
# Management Company
# Portfolio Management
# Custodian Bank
# Bloomberg Code (I added this for completeness)



# Function to extract monetary values considering Swiss number formatting
extract_currency <- function(key, df) {
	pattern <- sprintf("%s\\s*\\([^\\)]*\\)?\\s*CHF\\s*([0-9'\\.]+)", key)
	for (i in 1:nrow(df)) {
		# Check if the current row's text contains the key
		line <- df$text[i]
		if (grepl(key, line, fixed = TRUE)) {
			# Attempt to extract the value using regex
			matches <- str_extract(line, pattern)
			if (!is.na(matches)) {
				# Clean the extracted value to ensure it's in a usable format
				value <- sub(pattern, "\\1", matches)
				value <- gsub("[^0-9\\.]", "", value)  # Remove apostrophes used as thousands separators
				return(as.numeric(value))  # Convert to numeric and return
			}
		}
	}
	return(NA)  # Return NA if no matches are found
}


# Function to extract date values in the format "dd.mm.yyyy"
extract_date <- function(key, df) {
	# Updated regex pattern to be more flexible with the format of text before the date
	pattern <- sprintf("%s\\s+([0-9]{2}\\.[0-9]{2}\\.[0-9]{4})", key)
	for (i in 1:nrow(df)) {
		# Check if the current row's text contains the key
		line <- df$text[i]
		if (grepl(key, line, fixed = TRUE)) {
			# Attempt to extract the date using regex
			matches <- str_extract(line, pattern)
			if (!is.na(matches)) {
				# Extract and return the date
				value <- sub(pattern, "\\1", matches)
				return(value)  # Return the extracted date
			}
		}
	}
	return(NA)  # Return NA if no match is found
}


# Function to extract counts or large numbers handling Swiss formatting
extract_count <- function(key, df) {
	# Regex pattern to match the key followed by a space and the number including Swiss commas and decimals
	pattern <- sprintf("%s\\s+([0-9']+\\.?[0-9]*)", key)
	for (i in 1:nrow(df)) {
		# Check if the current row's text contains the key
		line <- df$text[i]
		if (grepl(key, line, fixed = TRUE)) {
			# Attempt to extract the number using regex
			matches <- str_extract(line, pattern)
			if (!is.na(matches)) {
				# Clean the extracted value to ensure it's in a usable numeric format
				value <- sub(pattern, "\\1", matches)
				value <- gsub("[']", "", value)  # Remove apostrophes used as thousands separators
				return(as.numeric(value))  # Convert to numeric and return
			}
		}
	}
	return(NA)  # Return NA if no matches are found
}

# Function to extract plain text following a key
extract_text <- function(key, df) {
	# Regex pattern to capture any text following the key up to the end of the line
	pattern <- sprintf("%s\\s+(.*)", key)
	for (i in 1:nrow(df)) {
		# Check if the current row's text contains the key
		line <- df$text[i]
		if (grepl(key, line, fixed = TRUE)) {
			# Attempt to extract the text using regex
			matches <- str_extract(line, pattern)
			if (!is.na(matches)) {
				# Extract and return the text
				value <- sub(pattern, "\\1", matches)
				return(value)  # Return the extracted text
			}
		}
	}
	return(NA)  # Return NA if no match is found
}


# Function to extract percentage values
extract_percentage <- function(key, df) {
	# Regex pattern to capture numbers followed by a percentage symbol after the key
	pattern <- sprintf("%s\\s+([0-9.]+)%%", key)
	for (i in 1:nrow(df)) {
		line <- df$text[i]
		if (grepl(key, line, fixed = TRUE)) {
			matches <- str_extract(line, pattern)
			if (!is.na(matches)) {
				value <- sub(pattern, "\\1", matches)
				return(as.numeric(value))  # Convert the extracted string to numeric
			}
		}
	}
	return(NA)  # Return NA if no match is found
}

# Function to extract plain text following a key, improved to handle any subsequent characters
extract_text <- function(key, df) {
	pattern <- sprintf("%s\\s+(.*)", key)
	for (i in 1:nrow(df)) {
		line <- df$text[i]
		if (grepl(key, line, fixed = TRUE)) {
			matches <- str_extract(line, pattern)
			if (!is.na(matches)) {
				value <- sub(pattern, "\\1", matches)
				return(value)  # Return the extracted text
			}
		}
	}
	return(NA)  # Return NA if no match is found
}




# Function to process a single PDF file
process_pdf <- function(file_path) {
	text <- pdf_text(file_path)[1]  # Read text from the first page
	text <- strsplit(text, "\n")[[1]]  # Split text into lines
	text <- str_replace_all(text, "\\s+", " ")  # Normalize spaces
	df <- as.data.frame(text, stringsAsFactors = FALSE)
	
	# Extract data using previously defined functions
	extracted_data <- data.frame(
		File = basename(file_path),
		NAV_Per_Share = extract_currency("NAV per Share", df),
		High_52_Week = extract_currency("52-Week High", df),
		Low_52_Week = extract_currency("52-Week Low", df),
		Share_Class_Volume = extract_currency("Share Class Volume", df),
		Fund_Volume = extract_currency("Fund Volume", df),
		Last_Distribution = extract_currency("Last distribution", df),
		Share_Class_Launch_Date = extract_date("Class Launch Date", df),
		Inception_Date_Fund = extract_date("Inception Date Fund", df),
		Start_Performance_Calculation = extract_date("Start Performance Calculation", df),
		Shares_Outstanding = extract_count("Shares Outstanding", df),
		Investment_Method = extract_text("Investment Method", df),
		Flat_Fee = extract_percentage("Flat Fee p.a.", df),
		Benchmark = extract_text("Benchmark", df)
	)
	return(extracted_data)
}

# List PDF files in the directory that match the specific pattern
file_paths <- list.files("../data/", pattern = "^FSD_.*\\.pdf$", full.names = TRUE)

# The rest of the code remains the same
# Process each file and collect results
results <- do.call(rbind, lapply(file_paths, process_pdf))
# 
# 
# # Assuming 'df' is your data frame containing the cleaned lines of text
# data <- data.frame(
# 	# currency
# 	NAV_Per_Share = extract_currency("NAV per Share", df),
# 	High_52_Week = extract_currency("52-Week High", df),
# 	Low_52_Week = extract_currency("52-Week Low", df),
# 	Share_Class_Volume = extract_currency("Share Class Volume", df),
# 	Fund_Volume = extract_currency("Fund Volume", df),
# 	Last_Distribution = extract_currency("Last distribution", df),
# 	
# 	# dates
# 	Share_Class_Launch_Date = extract_date("Share Class Launch Date", df),
# 	Inception_Date_Fund = extract_date("Inception Date Fund", df),
# 	Start_Performance_Calculation = extract_date("Start Performance Calculation", df),
# 
# 	# count
# 	Shares_Outstanding = extract_count("Shares Outstanding", df),
# 
# 	# text
# 	Investment_Method = extract_text("Investment Method", df),
# 	Securities_Lending = extract_text("Securities Lending", df),
# 
# 	# percentage
# 	Flat_Fee = extract_percentage("Flat Fee p.a.", df),
# 	Total_Expense_Ratio = extract_percentage("Total Expense Ratio p.a.", df),
# 	Issue_Fees = extract_percentage("Issue Fees in Favour of the Fund", df),
# 	Redemption_Fees = extract_percentage("Redempt. Fees in Fav. of the Fund", df),
# 
# 	# text
# 	Benchmark = extract_text("Benchmark", df),
# 	Management_Company = extract_text("Management Company", df),
# 	Portfolio_Management = extract_text("Portfolio Management", df),
# 	Custodian_Bank = extract_text("Custodian Bank", df),
# 	Bloomberg_Code = extract_text("Bloomberg", df),
# 	Fund_Domicile = extract_text("Fund Domicile", df),
# 	Share_Class_Currency = extract_text("Share Class Currency", df),
# 	Fund_Currency = extract_text("Fund Currency", df)
# 
# )

# Print the resulting data frame to verify results
print(data)

# all PDF factsheets ----

# library(httr)
# List of ISIN codes
# isin_codes <- c("CH0325172903", "ANOTHER_ISIN", "YET_ANOTHER_ISIN")  # Add your ISIN codes here

# Base URL
# base_url <- "https://products.swisscanto.com/products/api/factsheet/isin/"

# Directory to save PDFs
# dir.create("downloaded_pdfs")  # Create a directory if it doesn't exist

# # Loop through each ISIN code to download the corresponding PDF
# for (isin in isin_codes) {
# 	pdf_url <- paste0(base_url, isin, "/file-name/FSD_", isin, "_SWC_CH_en.pdf")
# 	pdf_path <- paste0("downloaded_pdfs/FSD_", isin, "_SWC_CH_en.pdf")  # Local path to save file
# 	
# 	# Download the PDF
# 	download.file(pdf_url, pdf_path, mode = "wb")  # mode "wb" is used for Windows compatibility
# 	cat("Downloaded: ", pdf_path, "\n")
# }



# swisscanto fund 367 share class(es) found ----
# Stability and Profitability Metrics
# Stability might be assessed based on lower volatility in performance metrics (like the standard deviation of returns) and risk ratings. Profitability could be measured using returns over different periods (like YTD, 3 years, and 5 years) and comparing the total expense ratio (TER).


df_swisscanto <- readxl::read_xlsx("../data/Fund info  Swisscanto.xlsx")

df <- df_swisscanto
names(df_swisscanto)
names(df)

# colnames(df)[colnames(df) == 'oldName'] <- 'newName'

# Mapping old names to new names
new_names <- c(
	"YTD %" = "YTD_pc", 
	"3yrs p.a.% net" = "3yrs_pa_pc_net", 
	"5yrs p.a.% net" = "5yrs_pa_pc_net", 
	"TER p.a %" = "TER_pa_pc",
	"Fund name" = "Fund_name"
)

# Loop through the new_names vector and update column names
for (old_name in names(new_names)) {
	if (old_name %in% colnames(df)) {
		colnames(df)[colnames(df) == old_name] <- new_names[old_name]
	}
}

# Assume df is already loaded into your R environment

# Convert percentage fields from character to numeric
convert_percent <- function(x) as.numeric(sub("%", "", x)) * 100

df$`YTD_pc` <- sapply(df$`YTD_pc`, convert_percent)
df$`3yrs_pa_pc_net` <- sapply(df$`3yrs_pa_pc_net`, convert_percent)
df$`5yrs_pa_pc_net` <- sapply(df$`5yrs_pa_pc_net`, convert_percent)
df$`TER_pa_pc` <- sapply(df$`TER_pa_pc`, convert_percent)

# Handling NAs if needed
df$`3yrs_pa_pc_net`[is.na(df$`3yrs_pa_pc_net`)] <- mean(df$`3yrs_pa_pc_net`, na.rm = TRUE)
df$`5yrs_pa_pc_net`[is.na(df$`5yrs_pa_pc_net`)] <- mean(df$`5yrs_pa_pc_net`, na.rm = TRUE)

# Calculate stability and profitability scores
# Here, stability could be inversely related to the variability in returns and directly to the risk rating
# df$stability_score <- 1 / sd(cbind(df$`3yrs_pa_pc_net`, df$`5yrs_pa_pc_net`), na.rm = TRUE) * df$Risk
# 
# head(cbind(df$`3yrs_pa_pc_net`, df$`5yrs_pa_pc_net`))
# head(sd(cbind(df$`3yrs_pa_pc_net`, df$`5yrs_pa_pc_net`), na.rm = TRUE))
# head( 1 / sd(cbind(df$`3yrs_pa_pc_net`, df$`5yrs_pa_pc_net`), na.rm = TRUE) )
# head(1 / sd(cbind(df$`3yrs_pa_pc_net`, df$`5yrs_pa_pc_net`), na.rm = TRUE) * df$Risk)
# head(df$Risk)

# Calculate the SD of returns individually for each fund across 3yrs_pa_pc_net and 5yrs_pa_pc_net
df <- df %>% rowwise() %>% mutate(sd_returns = sd(c(`3yrs_pa_pc_net`, `5yrs_pa_pc_net`), na.rm = TRUE)) %>% ungroup()

# Calculate stability score using the correct SD for each fund
df$stability_score <- 1 / df$sd_returns * df$Risk

# Profitability could be a weighted average of YTD, 3yr, and 5yr returns adjusted by TER
df$profitability_score <- (df$`YTD_pc` * 0.5 + df$`3yrs_pa_pc_net` * 0.3 + df$`5yrs_pa_pc_net` * 0.2) - df$`TER_pa_pc`

# Sharpe Ratio as an Alternative Stability Metric
# The Sharpe Ratio is a well-known measure that adjusts returns by their risk (volatility). While it typically considers excess returns over a risk-free rate, you can adapt the concept to your data. If you have access to expected returns, you could calculate something similar:
	
# Example of Sharpe-like ratio using YTD returns and standard deviation of returns
# Calculate the standard deviation for each fund individually across 3yrs_pa_pc_net and 5yrs_pa_pc_net
df <- df %>% rowwise() %>% mutate(sd_returns = sd(c(`3yrs_pa_pc_net`, `5yrs_pa_pc_net`), na.rm = TRUE)) %>% ungroup()
df$sharpe_like_stability <- df$YTD_pc / df$sd_returns

# Determine the most stable and profitable funds
most_stable_funds <- df[order(-df$stability_score), ]
most_profitable_funds <- df[order(-df$profitability_score), ]
most_sharpe_stable_funds <- df[order(-df$sharpe_like_stability), ]

# Print top 3 stable and profitable funds
print(head(most_stable_funds, 3))
print(head(most_profitable_funds, 3))

names(most_stable_funds)

names(most_profitable_funds)

library(ggplot2)

df$TER_pa_pc <- as.numeric(df$TER_pa_pc)

# Assuming df contains all necessary data including 'stability_score' and 'profitability_score'
ggplot(df, aes(x = stability_score, y = profitability_score)) +
	geom_point(aes(color = as.factor(Risk), size = TER_pa_pc), alpha = 0.6) +  
	# geom_text(aes(label = Fund_name), vjust = 1.5, hjust = 1.5, check_overlap = TRUE, size = 3) +  # Adding fund names
	geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + 
	scale_color_manual(values = c("1" = "green", "2" = "yellow", "3" = "red")) +
	labs(title = "Comparison of Fund Stability vs. Profitability",
		  x = "Stability Score",
		  y = "Profitability Score",
		  color = "Risk Level") +
	theme_minimal() +
	theme(legend.position = "top")  # Adjusting theme and legend


max(as.numeric(df$profitability_score))

# stability ----
library(ggplot2)

# Histogram of Stability Scores
ggplot(df, aes(x = stability_score)) +
	geom_histogram(bins = 100, fill = "skyblue", color = "black") +
	labs(title = "Histogram of Stability Scores", x = "Stability Score", y = "Count") +
	theme_minimal()

# Scatter plot of Stability Score vs. YTD % Gains
ggplot(df, aes(x = log(stability_score), y = YTD_pc)) +
	geom_point(aes(color = (Risk)), alpha = 0.6) +
	geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear model fit
	labs(title = "Stability Score vs. Year-to-Date Gains",
		  x = "log(Stability Score)",
		  y = "Year-to-Date Gains (%)",
		  color = "Risk Level") +
	theme_minimal()

# Save or display the plot
# ggsave("Stability_vs_YTD_Gains.png", width = 10, height = 6, dpi = 300)


# Statistical Analysis
# Perform a correlation analysis to statistically assess the relationship between stability and gains.

# Assuming df$YTD_pc is already converted to numeric
cor_test <- cor.test(df$stability_score, df$YTD_pc, use = "complete.obs")  # Pearson correlation test
print(cor_test)

# Interpretation: Carefully interpret the results. A high stability score may not always correlate with high returns, especially if the score heavily penalizes volatility which might be acceptable in high-gain scenarios.

# stability plot ----
# Calculate the top 5% threshold for YTD gains across the entire dataset
top_percent_threshold <- quantile(df$YTD_pc, 0.98, na.rm = TRUE)

# Filter the dataset for the top 5% YTD gains across the whole dataset
# Also include those with the maximum YTD gain within each risk group
max_ytd_per_risk <- df %>%
	group_by(Risk) %>%
	mutate(max_YTD = max(YTD_pc, na.rm = TRUE)) %>%
	ungroup() %>%
	filter(YTD_pc >= top_percent_threshold | YTD_pc == max_YTD)

# Wrap the text in the Fund_name column at 20 characters for better readability
max_ytd_per_risk$Fund_name <- str_wrap(max_ytd_per_risk$Fund_name, width = 30)

# Create the plot
p1 <- ggplot(df, aes(x = log(stability_score), y = YTD_pc)) +
	# geom_jitter(aes(color = as.factor(Risk)), alpha = 0.6) +
	geom_jitter(aes(color = (Risk)), alpha = 0.6, width = 0, height = 0) + 
	scale_color_gradient(low="red", high = "orange") +
	# geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear model fit
	ggrepel::geom_text_repel(
		data = max_ytd_per_risk,
		aes(label = Fund_name),  # Fund_name should match exactly with the column name in df
		# nudge_y = 0.001,
		# direction = "y",
		arrow = arrow(length = unit(0.02, "npc")),  # Add arrows to labels
		size = 3,                # Size of the text
		max.overlaps = Inf,      # Prevents labels from being omitted
		# box.padding = 0.5,       # Adds padding around text labels
		# point.padding = 0.3      # Adds padding around points
	) +
	labs(title = "Stability Score vs. Year-to-Date Gains",
		  x = "log(Stability Score)",
		  y = "Year-to-Date Gains (%)",
		  color = "Risk Level") +
	theme_minimal()
p1

# profitability plot ----
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)

# Calculate the top 5% threshold for profitability across the entire dataset
top_profitability_threshold <- quantile(df$profitability_score, 0.95, na.rm = TRUE)

# Filter the dataset for the top 5% profitability across the whole dataset
# Also include those with the maximum profitability score within each risk group
max_profitability_per_risk <- df %>%
	group_by(Risk) %>%
	mutate(max_profitability = max(profitability_score, na.rm = TRUE)) %>%
	ungroup() %>%
	filter(profitability_score >= top_profitability_threshold | profitability_score == max_profitability)

# Wrap the text in the Fund_name column at 20 characters for better readability
max_profitability_per_risk$Fund_name <- str_wrap(max_profitability_per_risk$Fund_name, width = 30)

# Create the plot with jittered points and labels for top performers
p2 <- ggplot(df, aes(x = log(stability_score), y = profitability_score)) +
	geom_jitter(aes(color = (Risk)), alpha = 0.6, width = 0, height = 0) +
	# geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear model fit
	scale_color_gradient(low="red", high = "orange") +
	ggrepel::geom_text_repel(
		data = max_profitability_per_risk,
		aes(label = Fund_name),  # Use the wrapped Fund_name
		arrow = arrow(length = unit(0.02, "npc")),  # Add arrows to labels pointing to correct points
		size = 3,                # Size of the text
		# nudge_y = 0.001,          # Adjust the vertical nudging for better positioning
		# direction = "y",         # Restrict movement to y-direction for cleaner layout
		max.overlaps = Inf,      # Prevents labels from being omitted
		# box.padding = 0.5,       # Adds padding around text labels
		# point.padding = 0.3      # Adds padding around points
	) +
	labs(title = "Stability Score vs. Profitability Score",
		  x = "log(Stability Score)",
		  y = "Profitability Score",
		  color = "Risk Level") +
	theme_minimal()

p2

library(patchwork)
caption_text <-  str_wrap(width = 80, "Data source: SwissCanto 20240812")
p_stab_prof <- p1 / p2 + labs(title = "", subtitle = "",
	# x = "Year",
	# y = "Total number of cases",
	# color = "Indicator",
	caption = paste0(caption_text))

p_stab_prof

ggsave("../output/plot_swisscanto_p_stab_prof.pdf", p_stab_prof, width = 10, height = 12)
ggsave("../output/plot_swisscanto_p_stab_prof.png", p_stab_prof, width = 10, height = 12)



# sharpe stability plot ----

# Calculate the top 5% threshold for YTD gains across the entire dataset
top_percent_threshold <- quantile(df$YTD_pc, 0.98, na.rm = TRUE)
top_percent_sharpe <- quantile(df$sharpe_like_stability, 0.98, na.rm = TRUE)


# Filter the dataset for the top 5% YTD gains across the whole dataset
# Also include those with the maximum YTD gain within each risk group
max_ytd_per_risk <- df %>%
	# group_by(Risk) %>%
	# mutate(max_YTD = max(YTD_pc, na.rm = TRUE)) %>%
	ungroup() %>%
	filter(YTD_pc >= top_percent_threshold | sharpe_like_stability >= top_percent_sharpe)

# Wrap the text in the Fund_name column at 30 characters for better readability
max_ytd_per_risk$Fund_name <- str_wrap(max_ytd_per_risk$Fund_name, width = 30)

# Create the plot for Sharpe-like stability vs. YTD gains
p_sharpe <- ggplot(df, aes(x = log(sharpe_like_stability), y = YTD_pc)) +
	geom_jitter(aes(color = as.factor(Risk)), alpha = 0.6, width = 0.02, height = 0) +  # Jitter the points
	# geom_smooth(method = "lm", se = FALSE, color = "red", style = "dotted") +  # Linear model fit
	ggrepel::geom_text_repel(
		data = max_ytd_per_risk,
		aes(label = Fund_name),  # Use the wrapped Fund_name
		arrow = arrow(length = unit(0.02, "npc")),  # Add arrows to labels
		size = 3,                # Size of the text
		max.overlaps = Inf,      # Prevents labels from being omitted
		box.padding = 0.5,       # Adds padding around text labels
		point.padding = 0.3      # Adds padding around points
	) +
	geom_vline(xintercept =log(top_percent_sharpe), linetype = "dotted" ) +
	geom_hline(yintercept =top_percent_threshold, linetype = "dotted" ) +
	
	labs(title = "Sharpe-like Stability Score vs. Year-to-Date Gains",
		  x = "Sharpe-like Stability Score",
		  y = "Year-to-Date Gains (%)",
		  color = "Risk Level") +
	theme_minimal()

# Display the plot
print(p_sharpe)

ggsave("../output/plot_swisscanto_p_stab_prof_sharpe.pdf", p_sharpe, width = 8, height = 8)
ggsave("../output/plot_swisscanto_p_stab_prof_sharpe.png", p_sharpe, width = 8, height = 8)

# Sharpe-like Stability Metric:
# 	
# 	The Sharpe-like stability ratio is calculated as the YTD gains divided by the standard deviation of 3-year and 5-year returns. This metric provides a measure of how much return the fund is generating per unit of volatility, similar to the Sharpe Ratio.
# Plot Details:
# 	
# 	Higher Sharpe-like Scores: Points with higher Sharpe-like scores are more stable relative to their YTD returns, potentially indicating better performance with lower volatility.
# Highlighted Points: The highlighted points represent the funds with the highest YTD gains within their risk group or those in the top 5% of YTD gains overall, helping identify the most promising funds.

# function ----
library(ggplot2)
library(ggrepel)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(stringr)

caption_text <- str_wrap("Data source: SwissCanto 20240812", width = 80)

create_plot <- function(data, x_var, x_label, y_var, y_label, file_name) {
	# Calculate the top 1% threshold for both the X and Y variables
	top_percent_x <- quantile(data[[x_var]], 0.99, na.rm = TRUE)
	top_percent_y <- quantile(data[[y_var]], 0.99, na.rm = TRUE)
	
	# Filter the dataset for points where either the X or Y variable is in the top 1%, or both are above their thresholds
	max_per_risk <- data %>%
		filter((.data[[x_var]] >= top_percent_x) | (.data[[y_var]] >= top_percent_y) | 
				 	(.data[[x_var]] >= top_percent_x & .data[[y_var]] >= top_percent_y))
	
	# Wrap the text in the Fund_name column at 30 characters for better readability
	max_per_risk$Fund_name <- str_wrap(max_per_risk$Fund_name, width = 30)
	
	# Create the plot
	p <- ggplot(data, aes(x = log(.data[[x_var]]), y = .data[[y_var]])) +
		geom_jitter(aes(color = as.factor(Risk)), alpha = 0.6, width = 0.02, height = 0) +
		ggrepel::geom_text_repel(
			data = max_per_risk,
			aes(label = Fund_name),
			arrow = arrow(length = unit(0.02, "npc")),
			size = 3,
			max.overlaps = Inf,
			box.padding = 0.5,
			point.padding = 0.3
		) +
		geom_vline(xintercept = log(top_percent_x), linetype = "dotted") +
		geom_hline(yintercept = top_percent_y, linetype = "dotted") +
		labs(title = paste(x_label, "vs.", y_label),
			  x = paste("Log of", x_label),
			  y = y_label,
			  color = "Risk level",
		caption = paste0(caption_text, 
							  "\nLabel filters: top 1% for either X and Y variables or any of both X and Y.",
							  "\nX = ",  x_label,
							  ", and Y = ", y_label, ".")) +
		theme_minimal()
	
	# Save the plot
	ggsave(paste("../output/", file_name, ".pdf", sep = ""), p, width = 8, height = 8)
	ggsave(paste("../output/", file_name, ".png", sep = ""), p, width = 8, height = 8)
	
	return(p)
}

# Plot and save filtered winners
create_plot_and_filter <- function(data, x_var, x_label, y_var, y_label, file_name) {
	top_percent_x <- quantile(data[[x_var]], 0.99, na.rm = TRUE)
	top_percent_y <- quantile(data[[y_var]], 0.99, na.rm = TRUE)
	
	filtered_data <- data %>%
		filter((.data[[y_var]] >= top_percent_y) | (sharpe_like_stability >= top_percent_x) | 
				 	(.data[[y_var]] >= top_percent_y & sharpe_like_stability >= top_percent_x))
	
	p <- ggplot(data, aes(x = log(.data[[x_var]]), y = .data[[y_var]])) +
		geom_jitter(aes(color = as.factor(Risk)), alpha = 0.6, width = 0.02, height = 0) +
		ggrepel::geom_text_repel(data = filtered_data, aes(label = Fund_name), arrow = arrow(length = unit(0.02, "npc")), size = 3) +
		geom_vline(xintercept = log(top_percent_x), linetype = "dotted") +
		geom_hline(yintercept = top_percent_y, linetype = "dotted") +
		labs(title = paste(x_label, "vs.", y_label), x = paste("Log of", x_label), y = y_label, color = "Risk level") +
		theme_minimal()
	
	return(list(plot = p, filtered_data = filtered_data))
}

# Example usage of the function
results_ytd = create_plot_and_filter(df, "sharpe_like_stability", "Sharpe-like stability score", "YTD_pc", "Year-to-date gains (%)", "plot_swisscanto_p_stab_prof_sharpe_ytd")
results_3yr = create_plot_and_filter(df, "sharpe_like_stability", "Sharpe-like stability score", "3yrs_pa_pc_net", "3 year performance (%)", "plot_swisscanto_p_stab_prof_sharpe_3yr")
results_5yr = create_plot_and_filter(df, "sharpe_like_stability", "Sharpe-like stability score", "5yrs_pa_pc_net", "5 year performance (%)", "plot_swisscanto_p_stab_prof_sharpe_5yr")

p_sharpe_ytd
p_sharpe_3yr
p_sharpe_5yr


# 
# # Combine the filtered data from each test
# all_filtered <- bind_rows(
# 	mutate(results_ytd$filtered_data, test = "YTD"),
# 	mutate(results_3yr$filtered_data, test = "3YR"),
# 	mutate(results_5yr$filtered_data, test = "5YR")
# 	# mutate(df, test = "FAILED"),
# )
# 
# # Count unique funds passing the thresholds in one or more tests
# unique_funds <- all_filtered %>%
# 	distinct(Fund_name) %>%
# 	summarise(count = n())
# 
# print(unique_funds)
# 
# # Additionally, to see how many funds pass more than one test
# funds_multiple_tests <- all_filtered %>%
# 	group_by(Fund_name) %>%
# 	summarise(tests_passed = n_distinct(test)) %>%
# 	filter(tests_passed > 1)
# 
# print(funds_multiple_tests)
# 
# library(ggplot2)
# 
# # Example setup for the plot
# # Assuming you might want to use a relevant metric like 'sharpe_like_stability' or another for the axes,
# # you'll need to join this data back to the main dataset to get those metrics.
# 
# # For illustration, let's assume we use 'sharpe_like_stability' and 'YTD_pc' for the axes:
# funds_plot_data <- funds_multiple_tests %>%
# 	left_join(df, by = "Fund_name") %>%
# 	select(Fund_name, tests_passed, sharpe_like_stability, YTD_pc)
# 
# # Now create the plot
# p <- ggplot(funds_plot_data, aes(x = log(sharpe_like_stability), y = YTD_pc, label = Fund_name)) +
# 	geom_point(alpha = 0.6, aes( color =tests_passed, size = tests_passed)) +  # Using point size to indicate the number of tests passed
# 	# ggrepel::geom_text_repel() +  # Adding labels
# 	scale_size_continuous(range = c(3, 10), breaks = 2:3, labels = c("Passed 2 Tests", "Passed 3 Tests")) +  # Adjust size
# 	labs(title = "Performance of Funds Across Different Metrics",
# 		  x = "Sharpe-like Stability Score",
# 		  y = "Year-to-Date Gains (%)",
# 		  size = "Number of Tests Passed") +
# 	theme_minimal()
# 
# # Display the plot
# print(p)

# Mark each filtered dataset with its respective test
results_ytd$filtered_data$test <- "YTD"
results_3yr$filtered_data$test <- "3YR"
results_5yr$filtered_data$test <- "5YR"

# Combine the filtered data from each test
all_tests <- bind_rows(
	results_ytd$filtered_data,
	results_3yr$filtered_data,
	results_5yr$filtered_data
)

# Include all funds, marking those not in 'all_tests' as 'FAILED'
df$test <- ifelse(df$Fund_name %in% all_tests$Fund_name, NA, "FAILED")
full_dataset <- bind_rows(all_tests, df)

# Calculate how many tests each fund passed
funds_tests_summary <- full_dataset %>%
	group_by(Fund_name) %>%
	summarise(tests_passed = n_distinct(test), .groups = 'drop') %>%
	mutate(tests_passed = ifelse(is.na(tests_passed), 0, tests_passed))

# Prepare data for plotting
funds_plot_data <- left_join(df, funds_tests_summary, by = "Fund_name")
library(ggplot2)
library(ggrepel)

# Create the plot
p <- ggplot(funds_plot_data, aes(x = log(sharpe_like_stability), y = YTD_pc)) +
	geom_point(aes(color = as.factor(tests_passed), size = tests_passed), alpha = 1) +
	scale_color_manual(values = c("gray", "pink", "salmon", "red"), 
							 labels = c("0", "1", "2", "3")) +
	labs(title = "Performance of funds across different metrics",
		  x = "Log of Sharpe-like stability score",
		  y = "Year-to-date gains (%)",
		  size = "Number of tests passed",
		  color = "Tests passed") +
	theme_minimal() +
	ggrepel::geom_text_repel(
		
		data = subset(funds_plot_data, tests_passed > 3),
		aes(label = str_wrap(Fund_name, 20)),
		# aes(label = Fund_name),
		size = 3,
		box.padding = 0.35,
		point.padding = 0.3,
		max.overlaps = Inf
	)

print(p)

# Optionally, save the plot
ggsave("../output/plot_performance_of_funds_across_metrics.pdf", p, width = 10, height = 8)
ggsave("../output/plot_performance_of_funds_across_metrics.png", p, width = 10, height = 8)

