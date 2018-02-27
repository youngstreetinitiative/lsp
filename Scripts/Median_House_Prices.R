library(directlabels)

path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Immanuel & Alex)/Working Files/Working Data/Median_House_Price_Cube.xlsx"

save_path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Immanuel & Alex)/Working Files/R images/"

raw_input <- readxl::read_xlsx(path = path, sheet = 1)

#I had to has this out because it was actually producing some slightly incorrect results fro the Rolling Average Growth rate
#This was because it was adding Brisbane with ta value of zero and still dividing by 4, when really it was a NA entry.
#raw_input[is.na(raw_input)] <- 0

df_median_growth <- raw_input %>% select(Year, `Median Trans Price of House Sydney`,
                                  `Median Trans Price of House Melbourne`,
                                  `Median Trans Price of House Brisbane`,
                                  `Median Trans Price of House Adelaide`,
                                  `Median Trans Price of House Perth`,
                                  `Median Trans Price of House Hobart`,
                                  `Median Trans Price of House Darwin`,
                                  `Median Trans Price of House Canberra`) %>%
  `colnames<-`(c("Year", "Sydney", "Melbourne", "Brisbane", "Adelaide", "Perth", "Hobart", "Darwin", "Canberra")) %>%
  mutate(Growth_Rate_Sydney = (Sydney - lag(Sydney, n = 1L)) / lag(Sydney, n = 1L),
         Growth_Rate_Melbourne = (Melbourne - lag(Melbourne, n = 1L)) / lag(Melbourne, n = 1L),
         Growth_Rate_Brisbane = (Brisbane - lag(Brisbane, n = 1L)) / lag(Brisbane, n = 1L),
         Growth_Rate_Perth = (Perth - lag(Perth, n = 1L)) / lag(Perth, n = 1L),
         RollMeanGrowth_Sydney = (Growth_Rate_Sydney + lag(Growth_Rate_Sydney, 1) + lag(Growth_Rate_Sydney, 2) + lag(Growth_Rate_Sydney, 3) +
                                    lag(Growth_Rate_Sydney, 4) + lag(Growth_Rate_Sydney, 5) + lag(Growth_Rate_Sydney, 6))/7,
         RollMeanGrowth_Melbourne = (Growth_Rate_Melbourne + lag(Growth_Rate_Melbourne, 1) + lag(Growth_Rate_Melbourne, 2) + lag(Growth_Rate_Melbourne, 3) +
                                       lag(Growth_Rate_Melbourne, 4) + lag(Growth_Rate_Melbourne, 5) + lag(Growth_Rate_Melbourne, 6))/7,
         RollMeanGrowth_Brisbane = (Growth_Rate_Brisbane + lag(Growth_Rate_Brisbane, 1) + lag(Growth_Rate_Brisbane, 2) + lag(Growth_Rate_Brisbane, 3) +
                                      lag(Growth_Rate_Brisbane, 4) + lag(Growth_Rate_Brisbane, 5) + lag(Growth_Rate_Brisbane, 6))/7,
         RollMeanGrowth_Perth = (Growth_Rate_Perth + lag(Growth_Rate_Perth, 1) + lag(Growth_Rate_Perth, 2) + lag(Growth_Rate_Perth, 3) +
                                   lag(Growth_Rate_Perth, 4) + lag(Growth_Rate_Perth, 5) + lag(Growth_Rate_Perth, 6))/7,
         Average_Price = (Sydney + Melbourne + Brisbane + Perth) / 4,
         Growth_Rate_Average = (Average_Price - lag(Average_Price, n = 1L)) / lag(Average_Price, n = 1L),
         RollMeanGrowth_Average = (Growth_Rate_Average + lag(Growth_Rate_Average, 1) + lag(Growth_Rate_Average, 2) + lag(Growth_Rate_Average, 3) +
           lag(Growth_Rate_Average, 4) + lag(Growth_Rate_Average, 5) + lag(Growth_Rate_Average, 6))/7)

#df_median_growth[is.na(df_median_growth)] <- 0

df_median_growth <- df_median_growth %>% filter(Year >= 1977) %>%
  mutate(Forty_Year_Average = sum(.$Growth_Rate_Average)/length(.$Growth_Rate_Average))

df_growth <- df_median_growth %>%
  select(Year,
         Growth_Rate_Sydney,
         Growth_Rate_Melbourne,
         Growth_Rate_Brisbane,
         Growth_Rate_Perth,
         Growth_Rate_Average) %>%
  gather(key = Growth_City, value = Growth_Rate, -Year) %>%
  filter(Growth_City != "Growth_Rate_Brisbane",
         Growth_City != "Growth_Rate_Perth")

df_rolling_growth <- df_median_growth %>%
  select(Year,
         RollMeanGrowth_Sydney,
         RollMeanGrowth_Melbourne,
         RollMeanGrowth_Brisbane,
         RollMeanGrowth_Perth,
         RollMeanGrowth_Average) %>%
  gather(key = Roll_Growth_City, value = Rolling_Growth, -Year) %>%
  filter(Roll_Growth_City != "RollMeanGrowth_Brisbane",
         Roll_Growth_City != "RollMeanGrowth_Perth",
         is.na(Rolling_Growth) == FALSE)

df_rolling_growth2 <- df_median_growth %>%
  select(Year,
         Forty_Year_Average) %>%
  gather(key = Roll_Growth_City, value = Rolling_Growth, -Year) %>%
  filter(Roll_Growth_City != "RollMeanGrowth_Brisbane",
         Roll_Growth_City != "RollMeanGrowth_Perth",
         is.na(Rolling_Growth) == FALSE)

ggplot(data = df_growth, aes(x = Year, y = Growth_Rate)) +
  geom_point(aes(colour = Growth_City)) +
  geom_line(aes(group = Growth_City, colour = Growth_City), size = 1)

ggplot(data = df_rolling_growth, aes(x = Year, y = Rolling_Growth)) +
  geom_point(data = df_rolling_growth2) +#, aes(colour = Roll_Growth_City)) +
  geom_line(aes(group = Roll_Growth_City, colour = Roll_Growth_City), size = 1.5) +
  geom_dl(aes(colour = Roll_Growth_City, label = Roll_Growth_City),
          method = list("smart.grid")) +
  geom_dl(data = df_rolling_growth2, aes(label = Roll_Growth_City),
          method = list("last.points", hjust = 1.8, vjust = -0.8)) +
  ggtitle("Median House Price Growth in Major Australian Cities (Sydney, Melbourne, Brisbane and Perth)") +
  xlab("Year") +
  ylab("Median Price Growth Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = seq(min(df_rolling_growth$Year), max(df_rolling_growth$Year), by = 5)) +
  scale_color_manual(name = "7 Year Rolling Growth Rate",
                  breaks = c("RollMeanGrowth_Sydney", "RollMeanGrowth_Melbourne", "RollMeanGrowth_Average"),
                  labels = c("Sydney", "Melbourne", "Average 4 Cities"),
                  values = c("#FFCC33", "#3399FF", "#FF3333")) +
  theme(legend.position = c(0.8, 0.8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

ggsave(filename = paste(save_path, "7 Year Rolling Average Median House Price Growth in Major Cities", ".jpeg", sep = ""), width = 45, height = 20, units = "cm")

df_median_prices <- df_median_growth %>% select(Year, Sydney, Melbourne, Brisbane, Perth) %>%
  gather(key = City, value = Median_Price, -Year)

ggplot(data = df_median_prices, aes(x = Year, y = Median_Price)) + geom_line(aes(group = City, colour = City), size = 1.5) +
  geom_dl(aes(colour = City, label = City),
          method = list("smart.grid")) +
  ggtitle("Median House Prices in Major Cities") +
  xlab("Year") +
  ylab("Median House Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_y_continuous(labels = dollar) +
  scale_x_continuous(breaks = seq(min(df_median_prices$Year), max(df_median_prices$Year), by = 5)) +
  scale_color_manual(name = "City",
                     breaks = c("Sydney", "Melbourne", "Brisbane", "Perth"),
                     labels = c("Sydney", "Melbourne", "Brisbane", "Perth"),
                     values = c("#006633", "#FFCC33", "#3399FF", "#FF3333")) +
  guides(colour = "none")

ggsave(filename = paste(save_path, "Median House Prices in Major Cities", ".jpeg", sep = ""), width = 45, height = 20, units = "cm")
