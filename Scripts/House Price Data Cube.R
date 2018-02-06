path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/Housing Pitch Work/Working Files/House Data R Working Cube.xlsx"

save_path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/Housing Pitch Work/Working Files/R images/"

city_list <- c("Sydney", "Melbourne", "Brisbane", "Adelaide", "Perth", "Hobart", "Darwin", "Canberra")

raw_input <- readxl::read_xlsx(path = path, sheet = 1)

raw_input[is.na(raw_input)] <- 0

df <- raw_input %>% mutate(`All Average Approval Value` = `Total FIRB Apps Value`/`Total FIRB Apps`,
                           `Real Estate Average Approval Value` = `Real Estate FIRB Apps Value`/`Real Estate FIRB Apps`,
                           Group = "One")


df_median <- df %>% select(-`Average Price AUS`, -`All Average Approval Value`, -Group, -CPI, -`Cash Rate`, -`HPI AUS`, -`Total FIRB Apps`, -`Total FIRB Apps Value`, -`Real Estate FIRB Apps`, -`Real Estate FIRB Apps Value`, -`Real Estate Average Approval Value`, -`Total Value of Dwelling Stock AUS`, -`New Building Job Residential Value AUS`, -`Total No of New Houses Approved`, -`Total Number of New Other Residential Approved`) %>%
  gather(key = `Trans Type`, value = Price, -Year)

df_median_city_house <- df %>% select(Year, `Median Trans Price of House Sydney`,
                                `Median Trans Price of House Melbourne`,
                                `Median Trans Price of House Brisbane`,
                                `Median Trans Price of House Adelaide`,
                                `Median Trans Price of House Perth`,
                                `Median Trans Price of House Hobart`,
                                `Median Trans Price of House Darwin`,
                                `Median Trans Price of House Canberra`) %>%
  `colnames<-`(c("Year", "Sydney", "Melbourne", "Brisbane", "Adelaide", "Perth", "Hobart", "Darwin", "Canberra")) %>%
  gather(key = `Trans Type`, value = Price, -Year) %>%
  mutate(`Trans Type` = factor(`Trans Type`, levels = city_list, ordered = TRUE))

df_average <- df %>% select(Year, `All Average Approval Value`, `Real Estate Average Approval Value`) %>%
  gather(key = `Value Type`, value = Dollars, -Year)

df_turnover <- df %>% select(Year, `Total Dwelling Transfers Aus`, `Total Number of Residential Dwellings AUS`) %>%
  mutate(`Percentage of Turnover` = (`Total Dwelling Transfers Aus`/`Total Number of Residential Dwellings AUS`)) %>%
  filter(Year >= 2011)

df_turnover2 <- df %>% select(Year, `Total Dwelling Transfers Aus`) %>%
  filter(Year >= 2002) %>%
  gather(key = Transfers, value = Number, -Year)


ggplot2::ggplot(data = df, aes(x = Year, y = `Real Estate Average Approval Value`)) +
  geom_point(aes(colour = Group)) +
  geom_line(aes(group = Group, colour = Group)) #I need to check with Jackson how to get my y axis values to display in full numerical format.

ggplot2::ggplot(data = df_median, aes(x = Year, y = Price)) + geom_point() + geom_line(aes(group = `Trans Type`, colour = `Trans Type`))

ggplot2::ggplot(data = df_median_city_house, aes(x = Year, y = Price)) +
  geom_point(aes(colour = `Trans Type`)) +
  geom_line(aes(group = `Trans Type`, colour = `Trans Type`)) +
  xlab("Year") +
  ylab("Price") +
  ggtitle("Median Price of House Transfers in Capital Cities") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = dollar) +
  scale_colour_brewer(palette = "Paired")

ggsave(filename = paste(save_path, "Median Price of House Transfers in Capital Cities", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

ggplot2::ggplot(data = df_turnover, aes(x = Year, y = `Percentage of Turnover`)) +
  geom_bar(stat = "identity", fill = "blue") +
  ggtitle("Percentage of Total Residential Dwellings Annual Turnover") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 0.1)) +
  scale_colour_brewer(palette = "Paired")

ggsave(filename = paste(save_path, "Percentage of Total Residential Dwellings Annual Turnover", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")


ggplot2::ggplot(data = df_turnover2, aes(x = Year, y = Number)) +
  geom_point(aes(colour = Transfers)) +
  geom_line(aes(group = Transfers, colour = Transfers)) +
  xlab("Year") +
  ylab("Number of Transfers") +
  ggtitle("Number of Residential Dwelling Transfers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_colour_brewer(palette = "Paired")

ggsave(filename = paste(save_path, "Number of residential Dwelling Transfers", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")


ggplot2::ggplot(data = df_average, aes(x = Year, y = `Dollars`)) +
  geom_point(aes(colour = `Value Type`)) +
  geom_line(aes(group = `Value Type`, colour = `Value Type`)) +
  ylab("") +
  scale_y_continuous(labels = dollar) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Average Value of FIRB Approvals")

ggsave(filename = paste(save_path, "Average Value of FIRB Approvals", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

df_HPI <- df %>% select(Year, `HPI AUS`) %>%
  gather(key = Type, value = Index, -Year) %>%
  filter(Year >= 2002)

ggplot2::ggplot(data = df_HPI, aes(x = Year, y = Index)) +
  geom_point(aes(colour = Type)) +
  geom_line(aes(group = Type, colour = Type)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Australian House Price Index")

ggsave(filename = paste(save_path, "Australian House Price Index", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")


#This is a table and graph that plots the Change HPI

df_change <- readxl::read_xlsx(path = path, sheet = 6) %>%
  gather(key = Type, value = Percentage, -Year)

ggplot2::ggplot(data = df_change, aes(x = Year, y = Percentage)) +
  geom_point(aes(colour = Type)) +
  geom_line(aes(group = Type, colour = Type)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = percent) +
  ggtitle("Change in Australian House Price Index")

ggsave(filename = paste(save_path, "Change in Australian House Price Index", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

# ggplot(data = , aes(x = , y = )) +
#   geom_line(aes(group = , colour = ), size = 0.8) +
#   geom_point(aes(colour = )) +
#   xlab("") +
#   ylab("") +
#   ggtitle("") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   scale_colour_brewer(palette = "Paired")


#ggsave(filename = , width = 35, height = 20, units = "cm")
