library(tidyverse)
library(ggplot2)

df <- mtcars
cars <- row.names(df)
df <- cbind(df, cars)



# Index <- factor(c("cyl", "hp"),
#                 levels = c("cyl", "hp"),
#                 ordered = TRUE)

# colours <- factor(c("#FF6600", "#0000FF"),
#                   levels = c("#FF6600", "#0000FF"),
#                   ordered = TRUE)

colours <- c("#FF6600", "#0000FF", "#CC0033")

#names(Index) <- Index

names(colours) <- c("cyl", "hp", "Boop")

#Leveler <- Index

#df_key = data.frame(Index, colours)

df_cars <- df %>% select(cars, cyl ,hp) %>%
  mutate(Boop = 30) %>%
  gather(key = Index, value = Value, -cars) #%>%
  #mutate(Index = factor(Index, levels = Leveler, ordered = TRUE))#,
         #colours = as.character(colours[Index]))

#df_cars <- merge(df_cars, df_key, by = "Index")


ggplot2::ggplot(data = df_cars, aes(x = cars, y = Value)) +
  geom_line(aes(group = Index, colour = Index)) +
  scale_colour_manual(values = colours)
