path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/Housing Pitch Work/asset class returns/asset class returns 2_Alexedit.xlsx"
save_path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/Housing Pitch Work/Working Files/R images/"
Index <- factor(c("All_Ords_Adj", "All_Ords_Acc_Adj", "Gold_Adj", "HPI_Adj", "Term_Deposit_Adj", "HPI_Ryield_Adj"),
                   levels = c("All_Ords_Adj", "All_Ords_Acc_Adj", "Gold_Adj", "HPI_Adj", "Term_Deposit_Adj", "HPI_Ryield_Adj"),
                   ordered = TRUE)
colours <- factor(c("#0000CC", "#FF3300", "#FF6600", "#006633", "#FFCC66", "#9933CC"),
                  levels = c("#0000CC", "#FF3300", "#FF6600", "#006633", "#FFCC66", "#9933CC"),
                  ordered = TRUE)
Leveler <- Index
df_colours <- data.frame(Index, colours)
raw_df <- readxl::read_xlsx(path = path, sheet = 3)
raw_df2 <- readxl::read_xlsx(path = path, sheet = 4)
df <- raw_df %>% select(Date, All_Ords_Adj, All_Ords_Acc_Adj, Gold_Adj, HPI_Adj, Term_Deposit_Adj) %>%
  gather(key = Index, value = One_Dollar, -Date) %>%
  mutate(Index = factor(Index, levels = Leveler, ordered = TRUE))
df <- merge(df, df_colours, by = "Index")
df <- df %>% mutate(colours = as.character(colours))
df_2 <- raw_df %>% select(Date, All_Ords_Adj, Gold_Adj, HPI_Adj, Term_Deposit_Adj) %>%
  gather(key = Index, value = One_Dollar, -Date)
colour <- unique(df$colours)
names(colour) <- unique(df$Index)
df_3 <- raw_df2 %>% gather(key = Index, value = One_Dollar, -Date) %>%
  mutate(Index = factor(Index, levels = Leveler, ordered = TRUE))
df_3 <- merge(df_3, df_colours, by = "Index")
df_3 <- df_3 %>% mutate(colours = as.character(colours))
colour_2 <- unique(df_3$colours)
names(colour_2) <- unique(df_3$Index)

#No the plots
ggplot2::ggplot(data = df, aes(x = Date, y = One_Dollar, colour = Index)) +
  geom_line(aes(group = Index), size = 1) +
  ggtitle("Asset Class Comparison") +
  ylab("Accumulation of One Dollar") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_colour_manual(values = colour)

ggsave(filename = paste(save_path, "Asset Class Comparison_All", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

ggplot2::ggplot(data = df_2, aes(x = Date, y = One_Dollar, colour = Index)) +
  geom_line(aes(group = Index), size = 1) +
  ggtitle("Asset Class Comparison Excluding Accumulated All Ords") +
  ylab("Accumulation of One Dollar") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_colour_manual(values = colour)

ggsave(filename = paste(save_path, "Asset Class Comparison_sansAccumulatedOrds", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

ggplot2::ggplot(data = df_3, aes(x = Date, y = One_Dollar, colour = Index)) +
  geom_line(aes(group = Index), size = 1) +
  ggtitle("Asset Class Comparison from 1997 with Rental Yields") +
  ylab("Accumulation of One Dollar") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_colour_manual(values = colour_2)

ggsave(filename = paste(save_path, "Asset Class Comparison from 1997 with Rental Yields", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

#regressions
library(lmtest)
df_lm <- raw_df %>% mutate(Map = "Group_1")

All_Ords_HPI <- lm(All_Ords_Adj ~ HPI_Adj, data = df_lm)
summary(All_Ords_HPI)
ggplot2::ggplot(data = df_lm, aes(y = All_Ords_Adj, x = HPI_Adj)) +
  geom_line(aes(group = Map, colour = "red")) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
  ggtitle("All Ords_HPI Regression")

ggsave(filename = paste(save_path, "All Ords_HPI Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

Gold_HPI <- lm(Gold_Adj ~ HPI_Adj, data = df_lm)
summary(Gold_HPI)
ggplot2::ggplot(data = df_lm, aes(y = Gold_Adj, x = HPI_Adj)) +
  geom_line(aes(group = Map, colour = "red")) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
  ggtitle("Gold_HPI Regression")

ggsave(filename = paste(save_path, "Gold_HPI Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

Term_Deposit_HPI <- lm(Term_Deposit_Adj ~ HPI_Adj, data =  df_lm)
summary(Term_Deposit_HPI)
ggplot2::ggplot(data = df_lm, aes(y = Term_Deposit_Adj, x = HPI_Adj)) +
  geom_line(aes(group = Map, colour = "red")) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
  ggtitle("Term Deposit_HPI Regression")

ggsave(filename = paste(save_path, "Term Deposit_HPI Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

#Test the erros
lmtest::bptest(All_Ords_HPI) #hetero
lmtest::bptest(Gold_HPI) #homo
lmtest::bptest(Term_Deposit_HPI) #hetero

#now lagged
All_Ords_HPI_lag <- lm(lag(All_Ords_Adj, 1) ~ HPI_Adj, data = df_lm)
summary(All_Ords_HPI_lag)


Gold_HPI_lag <- lm(lag(Gold_Adj, 1) ~ HPI_Adj, data = df_lm)
summary(Gold_HPI_lag)


Term_Deposit_HPI_lag <- lm(lag(Term_Deposit_Adj, 1) ~ HPI_Adj, data =  df_lm)
summary(Term_Deposit_HPI_lag)

#Test the errors
lmtest::bptest(All_Ords_HPI_lag) #homo
lmtest::bptest(Gold_HPI_lag) #homo
lmtest::bptest(Term_Deposit_HPI_lag) #hetero
