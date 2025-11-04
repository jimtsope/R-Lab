#1a. Load the data in an object named Drugs. 
Drugs <- read.csv("Drugs.txt",header = TRUE)
head(Drugs)
str(Drugs)


#1b. You will notice there is an additional variable called FLAG CODES. 
#    Remove this variable from the dataset.

Drugs <- subset(Drugs, select = -FLAG_CODES)
str(Drugs)
head(Drugs)

#2a. How many countries are there?

number_of_countries <- length(unique(Drugs$LOCATION)) 
number_of_countries

#2b. Make a table with the countries’ acronyms and the number of datapoints (years) per country, 
#    sorted in ascending order (the country with the least datapoints appears first).

c <- table(Drugs$LOCATION)
sorted_countries <- sort(c, decreasing = FALSE)
countries_table <- as.data.frame(sorted_countries)
colnames(countries_table) <- c("Acronyms", "Years")
countries_table

#3. Make a selection of countries based on the number of datapoints available. 
#   Specifically, select the countries that have a number of points in the top 13% of the distribution of the number
#   of datapoints, thus representing the countries with the most information available. 
#   Hint: functions subset(), quantile() and %in% operator might come in handy here


filter1 <- quantile(sorted_countries, probs = 0.87, na.rm = TRUE)
filter1


filter2 <- names(sorted_countries)[sorted_countries >= filter1]
filter2

# top-13% countries info
top_countries <- subset(Drugs, LOCATION %in% filter2)
str(top_countries)


#4. Using only the data selected in the previous task, create a graph with 4 plots (in the same window).
#   Each plot should depict the development of the drug spending for all countries in the reduced dataset,
#   over the available years. Each graph should depict one of the metrics (4 metrics = 4 graphs).
#   In each graph, a separate line should represent a country. The legend should show which line represents each country. 
#   The main title of each plot should mention “Drug spending in XX (type of metric)”. 
#   Hint: function par(mfrow=...), define the x-axis limits by the range found in the dataset,
#   that is, the smallest year found in the dataset up to the largest year found in the dataset. 


  
# In order to do not repeat 4 times the same code, we create a function and we call it 4 times

plot_metric <- function(table, data, title) {
  countries <- sort(unique(table$LOCATION))
  cols <- setNames(rainbow(length(countries)), countries)

  plot(NA, NA,
       xlim = range(table$TIME, na.rm = TRUE),
       ylim = range(table[[data]], na.rm = TRUE),
       xlab = "Year", ylab = data,
       main = title)

  for (loc in countries) {                       
    d <- table[table$LOCATION == loc, c("TIME", data)]
    d <- d[order(d$TIME), ]
    lines(d$TIME, d[[data]], lwd = 2, col = cols[loc])
  }

  legend("topleft", countries, col = cols, lwd = 2, cex = 0.75, bty = "n")
}

# 2×2 layout
op <- par(mfrow = c(2,2), mar = c(5,5,3,1))
plot_metric(top_countries, "PC_HEALTHXP", "Drug spending in XX (PC_HEALTHXP)")
plot_metric(top_countries, "PC_GDP",      "Drug spending in XX (PC_GDP)")
plot_metric(top_countries, "USD_CAP",     "Drug spending in XX (USD_CAP)")
plot_metric(top_countries, "TOTAL_SPEND", "Drug spending in XX (TOTAL_SPEND)")
par(op)

#5a. Your client is a multinational pharmaceutical company. They are interested in the probability that 
#   Greece (GRC) will increase its drug expenditure in at least 4 of the 5 following
#   consecutive years, in order to assess the investing opportunities. Assume that we are at
#   the year following the last record for Greece. Estimate the probability of drug expenditure
#   increase in any given year by the number of years where the expenditure was higher than
#   the year before.


GRC <- subset(Drugs, LOCATION == "GRC")  # we keep only the GRC data
head(GRC)
str(GRC)

# For each metric: diffs → fraction > 0 ,binomial 
# PC_HEALTHXP
d_health <- diff(GRC$PC_HEALTHXP)
p_health <- if (length(d_health)) mean(d_health > 0, na.rm = TRUE) else NA_real_
health <- if (is.na(p_health)) NA_real_ else 1 - pbinom(3, size = 5, prob = p_health)

# PC_GDP
d_gdp <- diff(GRC$PC_GDP)
p_gdp <- if (length(d_gdp)) mean(d_gdp > 0, na.rm = TRUE) else NA_real_
gdp <- if (is.na(p_gdp)) NA_real_ else 1 - pbinom(3, size = 5, prob = p_gdp)

# USD_CAP
d_usd <- diff(GRC$USD_CAP)
p_usd <- if (length(d_usd)) mean(d_usd > 0, na.rm = TRUE) else NA_real_
usd <- if (is.na(p_usd)) NA_real_ else 1 - pbinom(3, size = 5, prob = p_usd)

# TOTAL_SPEND
d_total <- diff(GRC$TOTAL_SPEND)
p_total <- if (length(d_total)) mean(d_total > 0, na.rm = TRUE) else NA_real_
total <- if (is.na(p_total)) NA_real_ else 1 - pbinom(3, size = 5, prob = p_total)


#5b. Create a list with the following elements, named accordingly:
# $Data: The data for Greece.
# $Years: The range (in years) of available data points for Greece, a vector with two elements, the minimum and maximum years.
# $DataPoints: The number of available data points for Greece.
# $YearlyProbs: The yearly probabilities of increase in expenditure, in all the four metrics available, thus a vector with 4 elements and names according to the metric.
# $FiveYearsProbs: The requested probabilities for all metrics, thus a vector with 4 elements and names according to the metric.


GRC_Result <- list(
  Data           = GRC,
  Years          = range(GRC$TIME, na.rm = TRUE),
  DataPoints     = nrow(GRC),
  YearlyProbs    = c(PC_HEALTHXP = p_health, PC_GDP = p_gdp, USD_CAP = p_usd, TOTAL_SPEND = p_total),
  FiveYearsProbs = c(PC_HEALTHXP = health, PC_GDP = gdp, USD_CAP = usd, TOTAL_SPEND = total)
)

print(GRC_Result)

#6a. Your client asks for a function that can calculate the above probabilities for a variable amount
#    of years and for any country desired. Create a function that takes as arguments

client <- function(COUNTRY = NULL, METRIC = "PC_HEALTHXP", nofY = 5) {
  if (is.null(COUNTRY)) return("Please provide a country code (COUNTRY).")
  if (!METRIC %in% names(Drugs)) return("Unknown METRIC name.")

  dt <- Drugs[Drugs$LOCATION == COUNTRY, c("TIME", METRIC)]
  if (is.unsorted(dt$TIME)) dt <- dt[order(dt$TIME), ]

  diffs <- diff(dt[[METRIC]])
  cpr <- sum(!is.na(diffs))
  if (cpr < 10) return("Unable to calculate probability (n<10)")

  fr <- mean(diffs > 0, na.rm = TRUE)
  prb_at_least <- 1 - pbinom(nofY - 2, size = nofY, prob = fr)

  minT <- min(dt$TIME, na.rm = TRUE)
  maxT <- max(dt$TIME, na.rm = TRUE)
  datapoints <- sum(!is.na(dt[[METRIC]]))

  paste0(
    "Based on (", datapoints, ") datapoints from years (", minT, ") to (", maxT, "), ",
    "the probability that (", COUNTRY, ") will increase its drug expenditure, ",
    "in terms of (", METRIC, "), in at least (", nofY - 1, 
    ") years in the period (", maxT + 1, ") to (", maxT + nofY, 
    ") is (", round(prb_at_least, 5), ")."
  )
}

#6b. The outcome of the function should be a sentence of the form:
#    “Based on (X) datapoints from years (minYear) to (maxYear), the probability that (countrycode) 
#    will increase its drug expenditure, in terms of (metric chosen), 
#    in at least (nofY-1) years in the period (maxYear+1) to (maxYear+1+nofY)is (estimated probability)”.
#    If the number of available datapoints for the calculation of the yearly increase probability
#    is less than 10 (thus, less than 11 years of data), the function should return: “Unable to
#    calculate probability (n<10)”, without any other output.


#   Examples
client("RUS", "PC_HEALTHXP", 5)
client("NOR", "USD_CAP", 5)
client(,,)
