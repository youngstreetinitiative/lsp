library(directlabels)
library(ggplot2)

path1 <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Immanuel & Alex)/Working Files/axis.xlsx"
path2 <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Immanuel & Alex)/Working Files/axis1.xlsx"

save_path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Immanuel & Alex)/Working Files/R images/"


df <- readxl::read_xlsx(path1, sheet = 1)
df <- cbind(seq(2002, 2016, 1), df$Transfers, 100 * df$Change)
colnames(df) <- c("Date", "Transfers", "Change")
df <- data.frame(df)


df_clean <- df %>% mutate(Change_HPI = Change * 30000) %>%
  select(Date, Transfers, Change_HPI) %>%
  gather(key = Var, value = Num, -Date)

ggplot2::ggplot(data = df_clean, aes(x = Date, y = Num)) +
  geom_line(aes(group = Var, colour = Var), size = 1.5) +
  geom_dl(aes(colour = Var, label = Var),
              method = list("smart.grid")) +
  ggtitle("Dwelling Transfers and the Change in HPI") +
  ylab("Number of Transfers") +
  xlab("Year") +
  scale_y_continuous(labels = comma, sec.axis = sec_axis(~./3000000, name = "Perchange Change", labels = percent)) +
  scale_x_continuous(breaks = seq(min(df_clean$Date), max(df_clean$Date), by = 2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme_minimal() +
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


