#This script is to show the methodology that I used to fuse the Abelson Index and the Index from the ABS

library(directlabels)
library(ggplot2)

path1 <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Millard & Whitehair 2018)/Working Files/Working Data/House Data R Working Cube.xlsx"

save_path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Millard & Whitehair 2018)/Working Files/R images/"

raw_input <- readxl::read_xlsx(path = path1, sheet = 1)

change <- raw_input$`HPI AUS`[24] / 100

df_HPI <- raw_input %>% select(Year, `HPI AUS`) %>%
  mutate(HPI = 100*`HPI AUS` / `HPI AUS`[Year == 2003],
         Series = ifelse(Year >= 2003, "ABS", "Abelson")) %>%
  select(-`HPI AUS`)
df_HPI <- rbind(df_HPI,
                data.frame(Year = 2003,
                           HPI = 100,
                           Series = "Abelson"))

colours <- c("#FF6633", "#6699CC")
names(colours) <- c("Abelson", "ABS")

ggplot2::ggplot(data = df_HPI, aes(x = Year, y = HPI)) +
  geom_line(aes(group = Series, colour = Series), size = 1.5) +
  geom_dl(aes(colour = Series, label = Series), method = list("smart.grid")) +
  ggtitle("Different Concatenated HPI Series") +
  xlab("") +
  ylab("House Price \nIndices") +
  scale_x_continuous(breaks = seq(min(df_HPI$Year), max(df_HPI$Year), by = 2)) +
  scale_y_continuous(labels = comma,
                     expand = c(0, 0),
                     sec.axis = dup_axis(),
                     breaks = pretty_breaks(n = 3)) +
  theme_classic() +
  theme(axis.title.y = element_text(margin = margin(t = 10), angle = 0),
        axis.title.y.right = element_text(margin = margin(t = 10), angle = 0, vjust = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_colour_manual(values = colours) +
  guides(colour = "none")

ggsave(filename = paste(save_path,
                        "Different Concatenated HPI Series",
                        ".jpeg",
                        sep = ""),
       width = 50, height = 25, units = "cm")


df_HPI_check <- raw_input %>% select(Year, `HPI AUS`) %>%
  mutate(Group = "Test")

ggplot2::ggplot(data = df_HPI_check, aes(x = Year, y = `HPI AUS`)) +
  geom_line(aes(group = Group, colour = Group), size = 1.5) +
  geom_dl(aes(colour = Group, label = Group), method = list("smart.grid")) +
  ggtitle("Different Concatenated HPI Series") +
  xlab("Date") +
  ylab("House Price Indices") +
  scale_x_continuous(breaks = seq(min(df_HPI$Year), max(df_HPI$Year), by = 2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme_minimal() +
  guides(colour = "none")

raw_input2 <- readxl::read_xlsx(path = path1, sheet = 10)

df_comparison <- raw_input2 %>% gather(key = City, value = HPI, -Date)

df_comparison_growth <- raw_input2 %>%
  mutate(Hong_Kong_Growth = (Hong_Kong - lag(Hong_Kong, n = 1L)) / lag(Hong_Kong, n = 1L),
         San_Jose_Growth = (San_Jose - lag(San_Jose, n = 1L)) / lag(San_Jose, n = 1L),
         Singapore_Growth = (Singapore - lag(Singapore, n = 1L)) / lag(Singapore, n = 1L),
         London_Growth = (London - lag(London, n = 1L)) / lag(London, n = 1L),
         AUS_Growth = (AUS - lag(AUS, n = 1L)) / lag(AUS, n = 1L),
         Sydney_Growth = (Sydney - lag(Sydney, n = 1L)) / lag(Sydney, n = 1L),
         Melbourne_Growth = (Melbourne - lag(Melbourne, n = 1L)) / lag(Melbourne, n = 1L)) %>%
  select(-Hong_Kong, -San_Jose, -London, -AUS, -Sydney, -Melbourne, -Singapore) %>%
  gather(key = City, value = Growth, -Date) #%>%
  #filter(City == "Sydney_Growth" | City == "AUS_Growth")

ggplot2::ggplot(data = df_comparison, aes(x = as.Date(Date), y = HPI)) +
  geom_line(aes(group = City, colour = City), size = 1.5) +
  geom_dl(aes(colour = City, label = City), method = list("smart.grid")) +
  ggtitle("Comparison of HPIs") +
  xlab("") +
  ylab("HPIs") +
  scale_x_date(date_breaks = "10 months",
               labels = date_format("%Y-%m")) +
  scale_y_continuous(labels = comma,
                     sec.axis = dup_axis(),
                     expand = c(0 ,0),
                     breaks = pretty_breaks(n = 7)) +
  theme_classic() +
  theme(axis.title.y = element_text(margin = margin(t = 10), angle = 0),
        axis.title.y.right = element_text(margin = margin(t = 10), angle = 0, vjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  guides(colour = "none")

ggsave(filename = paste(save_path,
                        "Comparison of Foreign and Domestic HPIs",
                        ".jpeg",
                        sep = ""),
       width = 35, height = 20, units = "cm")

ggplot2::ggplot(data = df_comparison_growth, aes(x = as.Date(Date), y = Growth)) +
  geom_line(aes(group = City, colour = City), size = 1.5) +
  geom_dl(aes(colour = City, label = City), method = list("smart.grid")) +
  ggtitle("Comparison of HPI Growth Rates") +
  xlab("Date") +
  ylab("") +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_breaks = "10 months",
               labels = date_format("%Y-%m")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme_minimal() +
  guides(colour = "none")

ggsave(filename = paste(save_path,
                        "Comparison of Foreign and Domestic HPI Growth",
                        ".jpeg",
                        sep = ""),
       width = 35, height = 20, units = "cm")


path2 <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Millard & Whitehair 2018)/Working Files/Working Data/HPI_Methodology.xlsx"

raw_input <- readxl::read_xlsx(path = path2, sheet = 1)

df <- raw_input %>% filter(str_detect(Date, "06")) %>%
  select(Date, OLD_ABS_Adj, NEW_ABS_Adj, ABELSON_Adj) %>%
  `colnames<-`(c("Date", "Old_ABS", "New_ABS", "Abelson")) %>%
  gather(key = Series, value = Index, -Date) %>%
  filter(Series != "Old_ABS")


ggplot2::ggplot(data = df, aes(x = as.Date(Date), y = Index)) +
  geom_line(aes(group = Series, colour = Series), size = 1) +
  geom_dl(aes(colour = Series, label = Series), method = list("smart.grid")) +
  ggtitle("Comparison of Different EHPI Methods/Sources") +
  xlab("") +
  ylab("Index") +
  scale_y_continuous(labels = comma,
                     sec.axis = dup_axis(),
                     expand = c(0 ,0),
                     breaks = pretty_breaks(n = 7)) +
  scale_x_date(labels = date_format("%Y")) +
  theme_classic() +
  theme(axis.title.y = element_text(margin = margin(t = 10), angle = 0),
        axis.title.y.right = element_text(margin = margin(t = 10), angle = 0, vjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  guides(colour = "none")
