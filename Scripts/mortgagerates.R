#this script is for graphing and comparing the mortgage lending rates with the overseas
#wholeslae money rate

path1 <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/Aradynes Solutions Housing Research/Data/Bond YIelds and Lending Rates.xlsx"
path2 <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/Aradynes Solutions Housing Research/Data/FRED_3monthLIBORbyUSD.csv"
path3 <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/Aradynes Solutions Housing Research/Data/FRED_1monthLIBORbyUSD.csv"
path4 <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/Aradynes Solutions Housing Research/Data/APRA_WholesaleComposition.xlsx"

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


df_fred_raw <- read.csv(path3, stringsAsFactors = FALSE)

df_fred <- df_fred_raw %>%
  `colnames<-`(c("Date", "LIBOR")) %>%
  filter(LIBOR != ".") %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         LIBOR = as.numeric(LIBOR),
         Index = "LIBOR"
         ) %>%
  #filter(str_match(Date, "01") == TRUE)

ggplot(data = df_fred, aes(x = Date, y = LIBOR)) +
  geom_line(aes(group = Index))

df_wholesale_raw <- readxl::read_xlsx(path4, sheet = 4, skip = 1)



df_wholesale <- df_wholesale_raw %>%
  select(Period, `Insitution Name`, `Cash and liquid assets`, `Total deposits`)

