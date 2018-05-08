#This script is for working with the comparison of HPI with Market Health in General
library(directlabels)
library(ggplot2)

path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Immanuel & Alex)/Working Files/Working Data/House Data R Working Cube.xlsx"

save_path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Immanuel & Alex)/Working Files/R images/"

raw_input <- readxl::read_xlsx(path = path, sheet = 1)

df <- raw_input %>% mutate(`All Average Approval Value` = `Total FIRB Apps Value`/`Total FIRB Apps`,
                           `Real Estate Average Approval Value` = `Real Estate FIRB Apps Value`/`Real Estate FIRB Apps`)

df_turnover <- df %>% select(Year, `Total Dwelling Transfers Aus`, `Total Number of Residential Dwellings AUS`) %>%
  mutate(`Percentage of Turnover` = (`Total Dwelling Transfers Aus`/`Total Number of Residential Dwellings AUS`)) %>%
  filter(Year >= 2011)

cutoff <- data.frame(yintercept = 0, cutoff = factor(0))

ggplot2::ggplot(data = df_turnover, aes(x = Year, y = `Percentage of Turnover`)) +
  geom_bar(stat = "identity", fill = "#66CCFF") +
  ggtitle("Percentage of Total Residential Dwellings Annual Turnover") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 0.1)) +
  scale_x_continuous(breaks = seq(min(df_turnover$Year), max(df_turnover$Year), by = 1))

ggplot2::ggsave(filename = paste(save_path, "Percentage of Total Residential Dwellings Annual Turnover", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

df_turnover2 <- df %>% select(Year, `Total Dwelling Transfers Aus`) %>%
  filter(Year >= 2003,
         Year < 2017) %>%
  gather(key = Transfers, value = Number, -Year)

ggplot2::ggplot(data = df_turnover2, aes(x = Year, y = Number)) +
  geom_line(aes(group = Transfers, colour = Transfers), size = 1.5) +
  geom_dl(aes(colour = Transfers, label = Transfers),
          method = list("last.points", hjust = 1.5, vjust = -1.8)) +
  xlab("") +
  ylab("Number of Transfers") +
  ggtitle("Number of Residential Dwelling Transfers") +
  theme_classic() +
  theme(axis.title.y = element_text(margin = margin(t = 10), angle = 0),
        axis.title.y.right = element_text(margin = margin(t = 10), angle = 0, vjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_colour_brewer(palette = "Paired") +
  scale_y_continuous(labels = comma,
                     expand = c(0, 0),
                     sec.axis = dup_axis()) +
  scale_x_continuous(breaks = seq(min(df_turnover2$Year), max(df_turnover2$Year), by = 2)) +
  guides(colour = "none")

ggsave(filename = paste(save_path,
                        "Number of Residential Dwelling Transfers",
                        ".jpeg",
                        sep = ""),
       width = 35, height = 20, units = "cm")

df_hpigrowth <- df %>% select(Year, `HPI AUS`) %>%
  mutate(HPI_Growth = (`HPI AUS` - lag(`HPI AUS`, n = 1L)) / lag(`HPI AUS`, n = 1L)) %>%
  gather(key = Category, value = Growth, -Year) %>%
  filter(Category == "HPI_Growth")

ggplot(data = df_hpigrowth, aes(x = Year, y = Growth)) +
  geom_line(aes(group = Category, colour = Category), size = 1.5) +
  geom_dl(aes(colour = Category, label = Category),
          method = list("last.points", hjust = 1.5, vjust = -3.5)) +
  xlab("") +
  ylab("HPI Growth") +
  ggtitle("The Growth of the Australian House Price Index") +
  theme_classic() +
  theme(axis.title.y = element_text(margin = margin(t = 10), angle = 0),
        axis.title.y.right = element_text(margin = margin(t = 10), angle = 0, vjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = percent,
                     expand = c(0, 0),
                     sec.axis = dup_axis()) +
  scale_x_continuous(breaks = seq(min(df_hpigrowth$Year), max(df_hpigrowth$Year), by = 2)) +
  guides(colour = "none") +
  geom_hline(data = cutoff, aes(yintercept = yintercept, linetype = cutoff), show.legend = FALSE)

ggsave(filename = paste(save_path,
                        "The Growth of the Australian House Price Index",
                        ".jpeg",
                        sep = ""),
       width = 35, height = 20, units = "cm")

raw_input2 <- readxl::read_xlsx(path = path, sheet = 7)

df_FIRB_Value <- raw_input2 %>%
  select(Year,
         Real_Estate_Approval_Value,
         Non_Real_Estate_Approval_Value,
         Value_of_Total_Approvals) %>%
  gather(key = Approval_Kind_Value,
         value = Dollars,
         -Year)

df_FIRB_Number <- raw_input2 %>%
  select(Year,
         Real_Estate_Approvals,
         Non_Real_Estate_Approvals,
         Total_Approvals) %>%
  gather(key = Approval_Kind_Number,
         value = Number_of_Approvals,
         -Year) %>%
  select(-Year)

df_FIRB_prop <- raw_input2 %>%
  mutate(Proportion_RE_Number = Real_Estate_Approvals / Total_Approvals,
         Proportion_RE_Value = Real_Estate_Approval_Value / Value_of_Total_Approvals,
         Year = as.Date(paste0(as.numeric(str_extract(Year, "[0-9]+")) + 1, "-06-01"))) %>%
  select(Year, Proportion_RE_Number, Proportion_RE_Value) %>%
  gather(key = Proportion_Type, value = Proportion, -Year)

df_FIRB <- cbind(df_FIRB_Value, df_FIRB_Number)

df_FIRB <- df_FIRB %>%
  mutate(Year = as.Date(paste0(as.numeric(str_extract(Year, "[0-9]+")) + 1, "-06-01")))

ggplot2::ggplot(data = df_FIRB, aes(x = Year, y = Number_of_Approvals)) +
  geom_line(aes(group = Approval_Kind_Number, colour = Approval_Kind_Number), size = 1.5) +
  geom_dl(aes(colour = Approval_Kind_Number, label = Approval_Kind_Number),
          method = list("smart.grid")) +
  xlab("Financial Year") +
  ylab("Number of Foreign Approvals") +
  ggtitle("Number of Foreign Investment Approvals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "5 years",
               labels = date_format("%Y")) +
  guides(colour = "none")

ggsave(filename = paste(save_path,
                        "Number of Foreign Investment Approvals",
                        ".jpeg",
                        sep = ""),
       width = 35, height = 20, units = "cm")

ggplot2::ggplot(data = df_FIRB, aes(x = Year, y = Dollars)) +
  geom_line(aes(group = Approval_Kind_Value, colour = Approval_Kind_Value), size = 1.5) +
  geom_dl(aes(colour = Approval_Kind_Value, label = Approval_Kind_Value),
          method = list("smart.grid")) +
  xlab("Financial Year") +
  ylab("Value of Foreign Approvals") +
  ggtitle("Value of Foreign Investment Approvals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_y_continuous(labels = dollar) +
  scale_x_date(date_breaks = "5 years",
               labels = date_format("%Y")) +
  guides(colour = "none")

ggsave(filename = paste(save_path,
                        "Value of Foreign Investment Approvals",
                        ".jpeg",
                        sep = ""),
       width = 35, height = 20, units = "cm")


df_FIRB_RealEstate <- df_FIRB %>%
  filter(Approval_Kind_Value == "Real_Estate_Approval_Value",
         Approval_Kind_Number == "Real_Estate_Approvals")

ggplot2::ggplot(data = df_FIRB_RealEstate, aes(x = Year, y = Number_of_Approvals)) +
  geom_line(aes(group = Approval_Kind_Number, colour = Approval_Kind_Number), size = 1.5) +
  #geom_dl(aes(colour = Approval_Kind_Number, label = Approval_Kind_Number),
          #method = list("smart.grid")) +
  xlab("Financial Year") +
  ylab("Number of Foreign Approvals") +
  ggtitle("Number of Foreign Investment Approvals in Real Estate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "5 years",
               labels = date_format("%Y")) +
  guides(colour = "none")

ggsave(filename = paste(save_path,
                        "Number of Foreign Investment Approvals in Real Estate",
                        ".jpeg",
                        sep = ""),
       width = 35, height = 20, units = "cm")

ggplot2::ggplot(data = df_FIRB_RealEstate, aes(x = Year, y = Dollars)) +
  geom_line(aes(group = Approval_Kind_Value, colour = Approval_Kind_Value), size = 1.5) +
  #geom_dl(aes(colour = Approval_Kind_Value, label = Approval_Kind_Value),
          #method = list("smart.grid")) +
  xlab("Financial Year") +
  ylab("Value of Foreign Approvals") +
  ggtitle("Value of Foreign Investment Approvals in Real Estate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_y_continuous(labels = dollar) +
  scale_x_date(date_breaks = "5 years",
               labels = date_format("%Y")) +
  guides(colour = "none")

ggsave(filename = paste(save_path,
                        "Value of Foreign Investment Approvals in Real Estate",
                        ".jpeg",
                        sep = ""),
       width = 35, height = 20, units = "cm")

ggplot2::ggplot(data = df_FIRB_prop %>% filter(Proportion_Type == "Proportion_RE_Number"),
                aes(x = Year, y = Proportion)) +
  geom_line(aes(group = Proportion_Type, colour = Proportion_Type), size = 1.5) +
  xlab("Proportion of Total Number of Approvals") +
  ylab("Financial Year") +
  ggtitle("Proportion of Real Estate Approvals of Total FIRB Approvals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_breaks = "5 years",
                     labels = date_format("%Y")) +
  guides(colour = "none")

ggsave(filename = paste(save_path,
                        "Proportion of Real Estate Approvals of Total FIRB Approvals",
                        ".jpeg",
                        sep = ""),
       width = 35, height = 20, units = "cm")

ggplot2::ggplot(data = df_FIRB_prop %>% filter(Proportion_Type == "Proportion_RE_Value"),
                aes(x = Year, y = Proportion)) +
  geom_line(aes(group = Proportion_Type, colour = Proportion_Type), size = 1.5) +
  xlab("Proportion of Total Value of Approvals") +
  ylab("Financial Year") +
  ggtitle("Proportion of Real Estate Value of Total FIRB Approvals Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_breaks = "5 years",
               labels = date_format("%Y")) +
  guides(colour = "none")

ggsave(filename = paste(save_path,
                        "Proportion of Real Estate Value of Total FIRB Approvals Value",
                        ".jpeg",
                        sep = ""),
       width = 35, height = 20, units = "cm")

path2 <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Immanuel & Alex)/Data/ABS_Overseas Migration_310101.xls"

raw_input3 <- readxl::read_xls(path = path2, sheet = 2)

#this function is still pretty rudimentary but it basically locates the element that has the specified value and then deletes its row and all above it.
#It is therefore the first rough pass at a function that can deal with cleaning the junk out of ABS excel files
ABS_cleanser <- function(df = NULL, val = NULL) {

  pin <- data.frame(which(df == val, arr.ind = TRUE))

  junk_set <- c(1:pin$row[1])

  df_clean <- df[-junk_set, ]

  return(df_clean)
}


df_NOM <- ABS_cleanser(df = raw_input3, val  = "Series ID") %>%
  `colnames<-`(c("Date",
                 "Births",
                 "Deaths",
                 "Natural_Inc",
                 "Interstate_Arr",
                 "Interstate_Dep",
                 "Overseas_Arr",
                 "Overseas_Dep",
                 "Net_Perm_and_Long_Movement",
                 "Migration_Adj",
                 "NOM",
                 "ERP",
                 "ERP_change_over_prev_year",
                 "Perc_ARP_change_over_prev_year")) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
         NOM = as.numeric(NOM),
         NOM_ROLL = (NOM + lag(NOM, 1) + lag(NOM, 2) + lag(NOM, 3)) / 4) %>%
  select(Date, NOM_ROLL) %>%
  gather(key = CAT, value = Val, -Date)

ggplot2::ggplot(data = df_NOM, aes(x = Date, y = Val)) +
  geom_line(aes(group = CAT, colour = CAT), size = 1.5) +
  ylab("Net Overseas Migration '000") +
  xlab("Date") +
  ggtitle("Rolling Quarterly Net Overseas Migration To Australia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "5 years",
               labels = date_format("%Y")) +
  guides(colour = "none")

ggsave(filename = paste(save_path,
                        "Rolling Quarterly Net Overseas Migration To Australia",
                        ".jpeg",
                        sep = ""),
       width = 35, height = 20, units = "cm")
