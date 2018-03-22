
#This is just inputting the data and setting up the tables to run the regressions and graph from

path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Immanuel & Alex)/Working Files/Working Data/House Data R Working Cube.xlsx"
path2 <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Immanuel & Alex)/Working Files/Working Data/HPI and RBA Cash Rate_12022018.xlsx"

save_path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Immanuel & Alex)/Working Files/R images/"


raw_input <- readxl::read_xlsx(path = path, sheet = 1)
raw_input2 <- readxl::read_xlsx(path = path2, sheet = 1)
raw_input3 <- readxl::read_xlsx(path = path2, sheet = 2) %>%
  select(., -`Cash Rate`, -`House Price Index`) %>%
  filter(., is.na(`Bi Annual HPI`) != TRUE)
df_cash <- raw_input %>% select(Year, `Cash Rate`) %>%
  gather(key = Type, value = rate, -Year)
df_raw2adjust <- raw_input2 %>%
  filter(Year != as.Date("2017-09-01")) %>%
  select(-`RBA Cash Rate`) %>%
  gather(key = Index_Type, value = Index, -Year, -`Lag Cash Rate`)

#Now run the regressions...

#This is yearly
level_level_Annual <- lm(`HPI AUS` ~ `Cash Rate`, data = raw_input)
summary(level_level_Annual)

#This is using the quarterly data
level_level2 <- lm(`House Price Index` ~ `RBA Cash Rate`, data = raw_input2)
summary(level_level2)

#this is lagged annual
level_level_lag <- lm(`HPI AUS` ~ lag(`Cash Rate`, 1), data = raw_input)
summary(level_level_lag)

#This is lagged quarterly
level_lag2 <- lm(`House Price Index` ~ lag(`RBA Cash Rate`, 1) , data = raw_input2)
summary(level_lag2)

#this is the bi-annual
level_level2_6month <- lm(`Bi Annual HPI` ~ `Bi Annual Cash Rate`, data = raw_input3)
summary(level_level2_6month)

#this is the lag bi-annual
level_level2_6month_lag <- lm(`Bi Annual HPI` ~ lag(`Bi Annual Cash Rate`, 1), data = raw_input3)
summary(level_level2_6month_lag)

#Now you have the plots
#This is the cash rate
ggplot2::ggplot(data = df_cash, aes(x = Year, y = rate)) + geom_point() + geom_line(aes(group = Type))
#This is the annual
ggplot2::ggplot(data = raw_input, aes(x = `Cash Rate`, y = `HPI AUS`)) + geom_point() + geom_smooth(method = "lm")
#This is the Monthly
ggplot2::ggplot(data = raw_input2, aes(x = `RBA Cash Rate`, y = `House Price Index`)) +
  geom_point() +
  geom_line(aes(group = "Year")) +
  geom_smooth(method = "lm")
#This is the lagged monthly
ggplot2::ggplot(data = df_raw2adjust, aes(x = `Lag Cash Rate`, y = Index)) +
  geom_point(aes(colour = Index_Type)) +
  geom_line(aes(group = Index_Type, colour = Index_Type)) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  ggtitle("Cash Rate and HPI Correlation (Lagged Monthly)") +
  xlab("Lag Cash Rate %")

ggsave(filename = paste(save_path, "Cash Rate and HPI Correlation", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

#This is the bi-annual
ggplot2::ggplot(data = raw_input3, aes(x = `Cash Rate`, y = HPI)) +
  geom_point() +
  geom_line(aes(group = "Year")) +
  geom_smooth(method = "lm")

bptest(level_level_Annual) #hetero
bptest(level_level2) #homo
bptest(level_level_lag) #hetero
bptest(level_lag2) #homo
bptest(level_level2_6month) # hetero
bptest(level_level2_6month_lag) #homo
