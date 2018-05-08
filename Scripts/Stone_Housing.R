#This Script is for creating graphs for Stone's Chapter on Housing

path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Household Incomes-Wealth & Inequality/Working files/Working Data/Household_Income_Cube.xlsx"

save_path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Narrative and Package/Andrews Mod/Chptr3_Housing Market/Images/"

raw_input1 <- readxl::read_xlsx(path, sheet = 3)

df <- raw_input1 %>% `colnames<-`(c("Date",
                                    "Debt / Income (LHS)",
                                    "Housing Debt / Income (LHS)",
                                    "OO Housing Debt / Income",
                                    "Interest Payments / Income (RHS)"))

colours <- c("#FF6633", "#6699CC")
colours2 <- c("#FF6633", "#6699CC")

names(colours) <- c("Interest Payments / Income (RHS)", "Housing Debt / Income (LHS)")
names(colours2) <- c("Interest Payments / Income (RHS)", "Debt / Income (LHS)")

df_debt_mortgage <- df %>% select(Date,
                                  `Interest Payments / Income (RHS)`,
                                  `Housing Debt / Income (LHS)`) %>%
  mutate(`Interest Payments / Income (RHS)` = `Interest Payments / Income (RHS)` * 10) %>%
  gather(key = Index, value = Ratio, -Date) %>%
  mutate(Ratio = Ratio / 100)

df_debt <- df %>% select(Date,
                                  `Interest Payments / Income (RHS)`,
                                  `Debt / Income (LHS)`) %>%
  mutate(`Interest Payments / Income (RHS)` = `Interest Payments / Income (RHS)` * 10) %>%
  gather(key = Index, value = Ratio, -Date) %>%
  mutate(Ratio = Ratio / 100)


ggplot2::ggplot(data = df_debt_mortgage, aes(x = as.Date(Date), y = Ratio, colour = Index)) +
  geom_line(aes(group = Index), size = 1.5) +
  directlabels::geom_dl(aes(label = Index),
                        method = list("smart.grid")) +
  ggtitle("Debt and Mortgage Payments to Disposable Income Ratios") +
  xlab("") +
  ylab("") +
  theme_classic() +
  scale_x_date(date_breaks = "2 years",
               labels = date_format("%Y")) +
  scale_y_continuous(sec.axis = sec_axis(~./10,
                                         labels = percent),
                     labels = percent) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  scale_colour_manual(values = colours) +
  guides(colour = "none")

ggsave(filename = paste(save_path, "Debt and Mortgage Payments to Disposable Income Ratios", ".jpeg", sep = ""),
       width = 9, height = 5, units = "in")

ggplot2::ggplot(data = df_debt, aes(x = as.Date(Date), y = Ratio, colour = Index)) +
  geom_line(aes(group = Index), size = 1.5) +
  directlabels::geom_dl(aes(label = Index),
                        method = list("smart.grid")) +
  ggtitle("Debt Payments to Disposable Income Ratios") +
  xlab("") +
  ylab("") +
  theme_classic() +
  scale_x_date(date_breaks = "2 years",
               labels = date_format("%Y")) +
  scale_y_continuous(sec.axis = sec_axis(~./10,
                                         labels = percent),
                     labels = percent) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  scale_colour_manual(values = colours2) +
  guides(colour = "none")

ggsave(filename = paste(save_path, "Debt Payments to Disposable Income Ratios", ".jpeg", sep = ""),
       width = 9, height = 5, units = "in")
