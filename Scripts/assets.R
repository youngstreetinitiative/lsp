library(ggplot2)

path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Immanuel & Alex)/asset class returns/asset class returns 2_Alexedit.xlsx"

save_path <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Immanuel & Alex)/Working Files/R images/"

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

#Now the plots
ggplot2::ggplot(data = df, aes(x = as.Date(Date), y = One_Dollar, colour = Index)) +
  geom_line(aes(group = Index), size = 1) +
  ggtitle("Asset Class Comparison") +
  ylab("Accumulation of One Dollar") +
  xlab("Date") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_colour_manual(values = colour,
                      labels = c("All Ords", "Accumulated All Ords", "Gold", "HPI", "Term Deposits")) +
  scale_x_date(date_breaks = "5 years",
               labels = date_format("%Y"))

ggsave(filename = paste(save_path, "Asset Class Comparison_All", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

ggplot2::ggplot(data = df_2, aes(x = as.Date(Date), y = One_Dollar, colour = Index)) +
  geom_line(aes(group = Index), size = 1) +
  ggtitle("Asset Class Comparison Excluding Accumulated All Ords") +
  ylab("Accumulation of One Dollar") +
  xlab("Date") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_colour_manual(values = colour,
                      labels = c("All Ords", "Gold", "HPI", "Term Deposits")) +
  scale_x_date(date_breaks = "5 years",
               labels = date_format("%Y"))

ggsave(filename = paste(save_path, "Asset Class Comparison_sansAccumulatedOrds", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

ggplot2::ggplot(data = df_3, aes(x = as.Date(Date), y = One_Dollar, colour = Index)) +
  geom_line(aes(group = Index), size = 1) +
  ggtitle("Asset Class Comparison from 1997 with Rental Yields") +
  ylab("Accumulation of One Dollar") +
  xlab("Date") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_colour_manual(values = colour_2,
                      labels = c("Accumulated All Ords", "HPI", "HPI + Rental Yeild")) +
  scale_x_date(date_breaks = "5 years",
               labels = date_format("%Y"))

ggsave(filename = paste(save_path, "Asset Class Comparison from 1997 with Rental Yields", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

#regressions
library(lmtest)
df_lm <- raw_df %>% mutate(Map = "Group_1")

# All_Ords_HPI <- lm(All_Ords_Adj ~ HPI_Adj, data = df_lm)
# summary(All_Ords_HPI)
# ggplot2::ggplot(data = df_lm, aes(y = All_Ords_Adj, x = HPI_Adj)) +
#   geom_line(aes(group = Map, colour = "red")) +
#   geom_smooth(method = "lm") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
#   ggtitle("All Ords_HPI Regression") +
#   ylab("Adjusted All Ords Index") +
#   xlab("Adjusted House Price Index")
#
# ggsave(filename = paste(save_path, "All Ords_HPI Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")
#
# Gold_HPI <- lm(Gold_Adj ~ HPI_Adj, data = df_lm)
# summary(Gold_HPI)
# ggplot2::ggplot(data = df_lm, aes(y = Gold_Adj, x = HPI_Adj)) +
#   geom_line(aes(group = Map, colour = "red")) +
#   geom_smooth(method = "lm") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
#   ggtitle("Gold_HPI Regression") +
#   ylab("Adjusted Gold Index") +
#   xlab("Adjusted House Price Index")
#
#
# ggsave(filename = paste(save_path, "Gold_HPI Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")
#
# Term_Deposit_HPI <- lm(Term_Deposit_Adj ~ HPI_Adj, data =  df_lm)
# summary(Term_Deposit_HPI)
# ggplot2::ggplot(data = df_lm, aes(y = Term_Deposit_Adj, x = HPI_Adj)) +
#   geom_line(aes(group = Map, colour = "red")) +
#   geom_smooth(method = "lm") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
#   ggtitle("Term Deposit_HPI Regression") +
#   ylab("Adjusted Term Deposits Index") +
#   xlab("Adjusted House Price Index")
#
#
# ggsave(filename = paste(save_path, "Term Deposit_HPI Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")
#
# #Test the erros
# lmtest::bptest(All_Ords_HPI) #hetero
# lmtest::bptest(Gold_HPI) #homo
# lmtest::bptest(Term_Deposit_HPI) #hetero
#
# #now lagged
# All_Ords_HPI_lag <- lm(lag(All_Ords_Adj, 1) ~ HPI_Adj, data = df_lm)
# summary(All_Ords_HPI_lag)
#
#
# Gold_HPI_lag <- lm(lag(Gold_Adj, 1) ~ HPI_Adj, data = df_lm)
# summary(Gold_HPI_lag)
#
#
# Term_Deposit_HPI_lag <- lm(lag(Term_Deposit_Adj, 1) ~ HPI_Adj, data =  df_lm)
# summary(Term_Deposit_HPI_lag)
#
# #Test the errors
# lmtest::bptest(All_Ords_HPI_lag) #homo
# lmtest::bptest(Gold_HPI_lag) #homo
# lmtest::bptest(Term_Deposit_HPI_lag) #hetero




#New Regressions after TB meeting 13/02/2018
#So what I need to do is make regressions of all the different asset classes against each other
#The significant outcome is the coefficient -- so if it was 0.5, it means that for every 1 unit of Gold going up, HPI would go up by 0.5 (that is if the p value is good)

AllOrds_HPI <- lm(All_Ords_Adj ~ HPI_Adj, data = df_lm)
summary(AllOrds_HPI)
ggplot2::ggplot(data = df_lm, aes(y = All_Ords_Adj, x = HPI_Adj)) +
  geom_line(aes(group = Map, colour = "red")) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
  ggtitle("All Ords_HPI Regression") +
  ylab("Adjusted All Ords Index") +
  xlab("Adjusted House Price Index")
ggsave(filename = paste(save_path, "All Ords_HPI Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")


AllOrds_AllOrdsAcc <- lm(All_Ords_Adj ~ All_Ords_Acc_Adj, data = df_lm)
summary(AllOrds_AllOrdsAcc)
ggplot2::ggplot(data = df_lm, aes(y = All_Ords_Adj, x = All_Ords_Acc_Adj)) +
  geom_line(aes(group = Map, colour = "red")) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
  ggtitle("All Ords_All Ords Acc Regression") +
  ylab("Adjusted All Ords Index") +
  xlab("Adjusted All Ords Acc Index")
ggsave(filename = paste(save_path, "All Ords_All Ords Acc Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

AllOrds_Gold <- lm(All_Ords_Adj ~ Gold_Adj, data = df_lm)
summary(AllOrds_Gold)
ggplot2::ggplot(data = df_lm, aes(y = All_Ords_Adj, x = Gold_Adj)) +
  geom_line(aes(group = Map, colour = "red")) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
  ggtitle("All Ords_Gold Regression") +
  ylab("Adjusted All Ords Index") +
  xlab("Adjusted Gold Index")
ggsave(filename = paste(save_path, "All Ords_Gold Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

AllOrds_Term <- lm(All_Ords_Adj ~ Term_Deposit_Adj, data = df_lm)
summary(AllOrds_Term)
ggplot2::ggplot(data = df_lm, aes(y = All_Ords_Adj, x = Term_Deposit_Adj)) +
  geom_line(aes(group = Map, colour = "red")) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
  ggtitle("All Ords_Term Deposit Regression") +
  ylab("Adjusted All Ords Index") +
  xlab("Adjusted Term Deposit Index")
ggsave(filename = paste(save_path, "All Ords_Term Deposit Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

AllOrdsAcc_Gold <- lm(All_Ords_Acc_Adj ~ Gold_Adj, data = df_lm)
summary(AllOrdsAcc_Gold)
ggplot2::ggplot(data = df_lm, aes(y = All_Ords_Acc_Adj, x = Gold_Adj)) +
  geom_line(aes(group = Map, colour = "red")) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
  ggtitle("All Ords Acc_Gold Regression") +
  ylab("Adjusted All Ords Acc Index") +
  xlab("Adjusted Gold Index")
ggsave(filename = paste(save_path, "All Ords Acc_Gold Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

AllOrdsAcc_HPI <- lm(All_Ords_Acc_Adj ~ HPI_Adj, data = df_lm)
summary(AllOrdsAcc_HPI)
ggplot2::ggplot(data = df_lm, aes(y = All_Ords_Acc_Adj, x = HPI_Adj)) +
  geom_line(aes(group = Map, colour = "red")) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
  ggtitle("All Ords Acc_HPI Regression") +
  ylab("Adjusted All Ords Acc Index") +
  xlab("Adjusted HPI Index")
ggsave(filename = paste(save_path, "All Ords Acc_HPI Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

AllOrdsAcc_Term <- lm(All_Ords_Acc_Adj ~ Term_Deposit_Adj, data = df_lm)
summary(AllOrdsAcc_Term)
ggplot2::ggplot(data = df_lm, aes(y = All_Ords_Acc_Adj, x = Term_Deposit_Adj)) +
  geom_line(aes(group = Map, colour = "red")) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
  ggtitle("All Ords Acc_Term Deposit Regression") +
  ylab("Adjusted All Ords Acc Index") +
  xlab("Adjusted Term Deposit Index")
ggsave(filename = paste(save_path, "All Ords Acc_Term Deposit Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

Gold_HPI <- lm(Gold_Adj ~ HPI_Adj, data = df_lm)
summary(Gold_HPI)
ggplot2::ggplot(data = df_lm, aes(y = Gold_Adj, x = HPI_Adj)) +
  geom_line(aes(group = Map, colour = "red")) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
  ggtitle("Gold_HPI Regression") +
  ylab("Adjusted Gold Index") +
  xlab("Adjusted HPI Index")
ggsave(filename = paste(save_path, "Gold_HPI Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

Gold_Term <- lm(Gold_Adj ~ Term_Deposit_Adj, data = df_lm)
summary(Gold_Term)
ggplot2::ggplot(data = df_lm, aes(y = Gold_Adj, x = Term_Deposit_Adj)) +
  geom_line(aes(group = Map, colour = "red")) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
  ggtitle("Gold_Term Deposit Regression") +
  ylab("Adjusted Gold Index") +
  xlab("Adjusted Term Deposit Index")
ggsave(filename = paste(save_path, "Gold_Term Deposit Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")

Term_HPI <- lm(Term_Deposit_Adj ~ HPI_Adj, data = df_lm)
summary(Term_HPI)
ggplot2::ggplot(data = df_lm, aes(y = Term_Deposit_Adj, x = HPI_Adj)) +
  geom_line(aes(group = Map, colour = "red")) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
  ggtitle("Term Deposit_HPI Regression") +
  ylab("Adjusted Term Deposit Index") +
  xlab("Adjusted HPI Index")
ggsave(filename = paste(save_path, "Term Deposit_HPI Regression", ".jpeg", sep = ""), width = 35, height = 20, units = "cm")
