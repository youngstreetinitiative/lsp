library(directlabels)
library(ggplot2)

path1 <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Millard & Whitehair 2018)/Working Files/axis.xlsx"
path2 <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Millard & Whitehair 2018)/Working Files/axis1.xlsx"

save_path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Millard & Whitehair 2018)/Working Files/R images/"


df <- readxl::read_xlsx(path1, sheet = 1)
df <- cbind(seq(2002, 2016, 1), df$Transfers, 100 * df$Change)
colnames(df) <- c("Date", "Transfers", "Change")
df <- data.frame(df)


df_clean <- df %>% mutate(Change_HPI = Change * 30000) %>%
  select(Date, Transfers, Change_HPI) %>%
  gather(key = Var, value = Num, -Date) %>%
  filter(Date > 2002)

ggplot2::ggplot(data = df_clean, aes(x = Date, y = Num)) +
  geom_line(aes(group = Var, colour = Var), size = 1.5) +
  geom_dl(aes(colour = Var, label = Var),
              method = list("smart.grid")) +
  ggtitle("Dwelling Transfers and the Change in HPI") +
  ylab("Number of \nTransfers") +
  xlab("") +
  scale_y_continuous(labels = comma, sec.axis = sec_axis(~./3000000, name = "Perchange \nChange", labels = percent)) +
  scale_x_continuous(breaks = seq(min(df_clean$Date), max(df_clean$Date), by = 2)) +
  theme_classic() +
  theme(axis.title.y = element_text(margin = margin(t = 10), angle = 0),
        axis.title.y.right = element_text(margin = margin(t = 10), angle = 0, vjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  guides(colour = "none")

ggsave(filename = paste(save_path,
                        "Dwelling Transfers and the Change in HPI",
                        ".jpeg",
                        sep = ""),
       width = 35, height = 20, units = "cm")



df1 <- readxl::read_xlsx(path2, sheet = 1)
df1 <- cbind(seq(1990, 2017, 1), df1)
colnames(df1) <- c("Date", "CashRate", "HPI")
df1 <- data.frame(df1)

df1_clean <- df1 %>% mutate(CashRate = CashRate * 300) %>%
  gather(key = Var, value = Num, -Date)

ggplot2::ggplot(data = df1_clean, aes(x = Date, y = Num)) +
  geom_line(aes(group = Var, colour = Var), size = 1.5) +
  geom_dl(aes(colour = Var, label = Var),
          method = list("smart.grid")) +
  scale_y_continuous(sec.axis = sec_axis(~./30000, name = "Cash Rate", labels = percent)) +
  scale_x_continuous(breaks = seq(min(df1_clean$Date), max(df1_clean$Date), by = 2)) +
  ggtitle("Cash Rate and HPI Comparison") +
  xlab("Year") +
  ylab("House Price Index") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme_minimal() +
  guides(colour = "none")


ggsave(filename = paste(save_path,
                        "Cash Rate and HPI Comparison Double Axis",
                        ".jpeg",
                        sep = ""),
       width = 35, height = 20, units = "cm")

df2 <- df1 %>% mutate(HPI_Growth = (HPI - lag(HPI, n = 1L)) / lag(HPI, n = 1L)) %>%
  select(-HPI)

df2_clean <- df2 %>% mutate(CashRate = CashRate / 100) %>%
  gather(key = Var, value = Num, -Date)

colours <- c("#FF6633", "#6699CC")
names(colours) <- c("CashRate", "HPI_Growth")

ggplot2::ggplot(data = df2_clean, aes(x = Date, y = Num)) +
  geom_line(aes(group = Var, colour = Var), size = 1.5) +
  geom_dl(aes(colour = Var, label = Var),
          method = list("smart.grid")) +
  scale_y_continuous(labels = percent,
                     sec.axis = sec_axis(~., name = "Cash Rate", labels = percent),
                     expand = c(0 ,0),
                     breaks = pretty_breaks(n = 7)) +#sec_axis(~./30000, name = "Cash Rate", labels = percent)) +
  scale_x_continuous(breaks = seq(min(df1_clean$Date), max(df1_clean$Date), by = 2)) +
  ggtitle("Cash Rate and HPI Comparison") +
  xlab("") +
  ylab("House Price Index \nGrowth Rate") +
  theme_classic() +
  theme(axis.title.y = element_text(margin = margin(t = 10), angle = 0),
        axis.title.y.right = element_text(margin = margin(t = 10), angle = 0, vjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual(values = colours) +
  guides(colour = "none")

ggsave(filename = paste(save_path,
                        "Cash Rate and HPI Growth Comparison",
                        ".jpeg",
                        sep = ""),
       width = 35, height = 20, units = "cm")



#This is a redo of the above, because the methodology is strange...
#what I will do here is read in the Housing Data Working Cube and use what is on there.


path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Millard & Whitehair 2018)/Working Files/Working Data/House Data R Working Cube.xlsx"

save_path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Millard & Whitehair 2018)/Working Files/R images/"

raw_input <- readxl::read_xlsx(path = path, sheet = 1)

df <- raw_input %>% select(Year, `HPI AUS`, `Total Dwelling Transfers Aus`) %>%
  filter(Year > 2003) %>%
  mutate(`Change in HPI` = (`HPI AUS` - lag(`HPI AUS`, n = 1L)) / lag(`HPI AUS`, n = 1L),
         `Change in Dwelling Transfers` = (`Total Dwelling Transfers Aus` - lag(`Total Dwelling Transfers Aus`, n = 1L)) / lag(`Total Dwelling Transfers Aus`, n = 1L)) %>%
  select(Year, `Change in Dwelling Transfers`, `Change in HPI`) %>%
  gather(key = Change, value = Percent, -Year)

colours <- c("#6699CC","#FF6633")
names(colours) <- c("Change in Dwelling Transfers", "Change in HPI")

ggplot2::ggplot(data = df, aes(x = Year, y = Percent)) +
  geom_line(aes(group = Change, colour = Change), size = 1.5) +
  geom_dl(aes(colour = Change, label = Change),
          method = list("smart.grid")) +
  ggtitle("Year on Year Change in \nNumber of Dwelling Transfers and HPI") +
  ylab("Percent\nChange") +
  xlab("") +
  scale_y_continuous(labels = percent, sec.axis = dup_axis()) +
  scale_x_continuous(breaks = seq(min(df$Year), max(df$Year), by = 2)) +
  theme_classic() +
  theme(axis.title.y = element_text(margin = margin(t = 10), angle = 0),
        axis.title.y.right = element_text(margin = margin(t = 10), angle = 0, vjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual(values = colours) +
  guides(colour = "none")

ggsave(filename = paste(save_path,
                        "Change in Number of Dwelling Transfers and HPI",
                        ".jpeg",
                        sep = ""),
       width = 35, height = 20, units = "cm")
