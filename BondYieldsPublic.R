library(tidyverse)
library(httr)
library(jsonlite) #Interprets data in JSON format
library(fredr) #US FRED data
library(pdfetch)


#NOTE: May be possible to get this to run on the WIX site. (This is from Google's AI overview so take it with a grain of salt)
#Run the code R Posit Cloud (has a certain amount of free credits or hours per month)
#Use the plumber package to create an API endpoint
#Request data through the API from Wix

###### SETUP ######
output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE)


#For fredr
fredr_set_key(Sys.getenv("FRED_API_KEY"))

#For filtering
NinetyDaysAgo = Sys.Date() - days(90)

##### GETTING DATA #####

#EU
#Data taken from here
#https://data.ecb.europa.eu/data/datasets/YC/YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_10Y
#For the url below - show the data table on the site, right click, select "Inspect". Go to the Network tab, 
#and refresh the page. Right click on one of the entries that pops up and copy the URL
url = "https://data.ecb.europa.eu/data-detail-api/YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_10Y"
EUdf = GET(url) %>% content("text") %>% fromJSON(flatten = TRUE)
EUdf = EUdf %>% select(Date = "PERIOD","10Y" = "OBS")
EUdf = EUdf %>% filter(Date >= NinetyDaysAgo)
EUdf = EUdf %>% mutate(Date = as.Date(Date),"10Y" = as.numeric(EUdf$"10Y"))
EUdf = EUdf %>% arrange(Date)


#US
#This is available through the Treasury as well. Treasury may be a day more up to date. But FRED is easy
#https://home.treasury.gov/resource-center/data-chart-center/interest-rates/TextView?type=daily_treasury_yield_curve&field_tdr_date_value=2025
USdf = fredr(series_id = "DGS10", observation_start = NinetyDaysAgo)
USdf = USdf %>% select(Date = "date", "10Y" = "value")

#Japan
#They inconveniently have two CSVs. One for this month and one for all other data.

#This month
tmp <- tempfile(fileext = ".csv")
download.file("https://www.mof.go.jp/english/policy/jgbs/reference/interest_rate/jgbcme.csv", tmp)
JPdf = read_csv(tmp,skip = 3,col_names = c("Date","1Y","2Y","3Y","4Y","5Y","6Y","7Y","8Y","9Y","10Y","15Y","20Y","25Y","30Y","40Y")) %>% head(-2) #Head here removes the last two rows, which were a footnote an NA
JPdf = JPdf %>% select("Date","10Y")
JPdf <- JPdf %>% mutate(Date = as.Date(Date, format = "%Y/%m/%d"))

#Historical Data
tmp_hist <- tempfile(fileext = ".csv")
download.file("https://www.mof.go.jp/english/policy/jgbs/reference/interest_rate/historical/jgbcme_all.csv", tmp_hist)
#Need to cut out the header because it is tab delimited, not comma separated. So the columns then must be selected by index.
JPHistdf = read_csv(tmp_hist,skip = 3,col_select = c(1,11))
JPHistdf = JPHistdf %>% select("Date" = 1, "10Y" = 2) #Fixing the column names
JPHistdf = JPHistdf %>% filter(!is.na(Date))
JPHistdf <- JPHistdf %>% mutate(Date = as.Date(Date, format = "%Y/%m/%d"))
#JPHistdf = JPHistdf %>% mutate(Date = as.Date(Date))
JPHistdf = JPHistdf %>% filter(Date >= NinetyDaysAgo)

#Combining this month's data with historical data
JPdf = rbind(JPHistdf,JPdf)
JPdf = JPdf %>% mutate(Date = as.Date(Date),"10Y" = as.numeric(JPdf$"10Y"))
rm(JPHistdf)

# UK
# Bank of England series IUDMNPY
UKdf = pdfetch_BOE("IUDMNPY",NinetyDaysAgo,Sys.Date())
UKdf = as_tibble(UKdf,rownames = "Date") %>% select("Date", "10Y" = "V1")
UKdf = UKdf %>% mutate(Date = as.Date(Date))

#Combining to one dataset
bondYields <- bind_rows(
  USdf %>% mutate(Country = "United States"),
  UKdf %>% mutate(Country = "United Kingdom"),
  EUdf %>% mutate(Country = "European Union"),
  JPdf %>% mutate(Country = "Japan")
)


#Renaming 10Y to Yield, having a number at the start of the name becomes annoying
bondYields = bondYields %>% select(Date, Yield = "10Y", Country)

#Drop any NAs - EU has NAs for the weekends that make the plots choppy
bondYields = bondYields %>% filter(!is.na(Yield))

##### PLOTS ######
#Useful dates
lastFriday = as.Date(Sys.Date() - wday(Sys.Date())) # the dates are setting a time window of the last full week
twoFridaysAgo = lastFriday - days(8)
lastMonday = lastFriday - days(5) #Last Monday used when saving the plots

#Setting colors
colorMap = c(
  "European Union" = "dodgerblue4",
  "United Kingdom" = "green3",
  "United States" = "black",
  "Japan" = "darkred"
)

#3 month plot - Bond yields

# Adjust y-axis limits based on the most volatile bond yield
max_yield <- max(abs(bondYields$Yield),na.rm=TRUE)
y_limits <- c(0, max_yield)

#Create and save a plot with refined aesthetics
plot <- ggplot(bondYields, aes(x = Date, y = Yield, color = Country)) +
  geom_line() + 
  #geom_line(data = filter(bondYields, !is.na(Yield))) + #This one smooths out those gaps
  labs(
    #title = "10-Year Government Bond Yields over the last 7 Days",
    x = "Date",
    y = "Percentage (%) Yield on 10-Year Government Bond") +
  scale_y_continuous(limits = y_limits) +
  theme_bw() +
  scale_color_manual(values = colorMap) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(plot.background = element_rect(fill = "white"),
        legend.title = element_blank())

# Displays the plot you just made.
#plot

#Saving the plot
formatted_date <- gsub("^0", "", format(lastMonday, "%m %d %y"))
file_name <- paste("Bond Yields Weekly Change ", formatted_date, ".png", sep = "")

# Use ggsave to save the ggplot as a .png file
#ggsave(file_name, plot, width = 10, height = 6, dpi = 300)  # Adjust width, height, and dpi as needed


#One week plot - Percentage change
bondYields = bondYields %>%
  filter(Date >= as.Date(twoFridaysAgo) & Date <= as.Date(lastFriday))

# Calculate the maximum absolute percentage change among currencies
max_yield <- max(abs(bondYields$Yield),na.rm=TRUE)

#Calculate the percentage change from the starting price for each currency
bondYields <- bondYields %>%
  group_by(Country) %>%
  mutate(pct_change = (Yield - Yield[1]) / Yield[1] * 100)

# Adjust y-axis limits based on the most volatile currency
max_yield <- max(abs(bondYields$pct_change),na.rm=TRUE)
y_limits <- c(-max_yield, max_yield)

#Create and save a plot with refined aesthetics
plot <- ggplot(bondYields, aes(x = Date, y = pct_change, color = Country)) +
  geom_line() +  #data = filter(prices, !is.na(pct_change))  PUT INSIDE GEOM LINE TO FIX WEEKEND BEING BLANK
  labs(
    #title = "10-Year Government Bond Yields over the last 7 Days",
    x = "Date",
    y = "Percentage (%) Change on 10-Year Government Bond Yields") +
  scale_y_continuous(limits = y_limits) +
  theme_bw() +
  scale_color_manual(values = colorMap) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(plot.background = element_rect(fill = "white"),
        legend.title = element_blank())

# Displays the plot you just made.
#plot

#Saving the plot
formatted_date <- gsub("^0", "", format(lastMonday, "%m %d %y"))
file_name <- paste("Bond Yields Ninety Days ", formatted_date, ".png", sep = "")

# Use ggsave to save the ggplot as a .png file
#ggsave(file_name, plot, width = 10, height = 6, dpi = 300)  # Adjust width, height, and dpi as needed

#Saving the data as a CSV
write_csv(bondYields, file.path(output_dir, "bond_yields.csv"))

