#this is a sandbox

path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/Aradynes Solutions Housing Research/Working Files/Alex_HousingPrice_MelbourneSydney.xlsx"
save_path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/Aradynes Solutions Housing Research/Working Files/R images/"
df_raw <- readxl::read_xlsx(path, sheet = 1)

city_order <- c("Sydney Price", "Melbourne Price", "Australian Capital Cities Price")

df_price <- df_raw %>%
  select(Year, CPI, `Sydney Price`, `Melbourne Price`, `Australian Capital Cities Price`) %>%
  gather(key = City, value = Price, -Year, -CPI) %>%
  mutate(Year = as.Date(paste0(str_extract(Year, "[0-9]+" ), "-06-01")),
         Price = Price * 1000,
         City = factor(City, levels = city_order, ordered = TRUE))

ggplot(data = df_price %>% filter(City != "Australian Capital Cities Price"), aes(x = Year, y = Price)) +
  geom_line(aes(group = City, colour = City)) +
  geom_line(data = df_price %>%
              filter(City == "Australian Capital Cities Price",
                     Year < "2002-06-01"), aes(group = City), colour = "red") +
  xlab("Year") +
  ylab("Median Detached House Price") +
  ggtitle("Median Detached House Prices in Sydney and Melbourne") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  scale_color_brewer(palette = "Paired") +
  scale_x_date(labels = date_format("%Y"))

ggsave(filename = paste(save_path, "Median Detached House Prices in Sydney and Melbourne", ".jpeg", sep = ""), width = 40, height = 20, units = "cm")


df_pop_raw <- readxl::read_xlsx(path, sheet = 2)

df_pop <- df_pop_raw %>% mutate(Region = "Australia",
                                `Estimated Resident Population` = `Estimated Resident Population` * 1000,
                                Year = as.Date(Year))

ggplot(data = df_pop, aes(x = Year, y = `Estimated Resident Population`)) +
  geom_line(aes(group = Region, colour = Region), size = 1.5) +
  xlab("Date") +
  ylab("Population") +
  ggtitle("Estimated Resident Population in Australia") +
  theme() +
  theme_minimal() +
  scale_colour_brewer(palette = "Paired") +
  scale_x_date(labels = date_format("%Y"))

ggsave(filename = paste(save_path, "Estimated Resident Population in Australia", ".jpeg", sep = ""), width = 40, height = 20, units = "cm")


df_increase_raw <- readxl::read_xlsx(path, sheet = 3)

df_increase <- df_increase_raw %>%
  select(-`Change Over Previous Quarter`) %>%
  gather(key = `Pop Type`, value = `Pop Increase`, -Year)

ggplot(data = df_increase, aes(x = Year, y = `Pop Increase`)) +
  geom_line(aes(group = `Pop Type`, colour = `Pop Type`),size = 1) +
  xlab("Date") +
  ylab("Population Increase") +
  ggtitle("Population Increase Split Between Natural and NOM") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() #+
  #scale_colour_brewer(palette = "Paired")

ggsave(filename = paste(save_path, "Population Increase Split Between Natural and NOM", ".jpeg", sep = ""), width = 40, height = 20, units = "cm")

#df_growth <- df_raw %>%

path1 <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/Aradynes Solutions Housing Research/Data/Bond YIelds and Lending Rates.xlsx"

df_bonds_raw <- readxl::read_xlsx(path1, sheet = 1)

df_bonds <- df_bonds_raw %>% gather(key = Type, value = Index, -Date) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Type != "Difference")


ggplot(data = df_bonds, aes(x = Date, y = Index)) +
  geom_line(aes(group = Type, colour = Type)) +
  xlab("Date") +
  ylab("Rate") +
  ggtitle("Comparison Between Bank Equity Rates and Government Bond Rates") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal() +
  #scale_colour_brewer(palette = "Paired") +
  scale_x_date(labels = date_format("%m-%Y"))

ggsave(filename = paste(save_path, "Comparison Between Bank Equity Rates and Government Bond Rates", ".jpeg", sep = ""), width = 40, height = 20, units = "cm")
