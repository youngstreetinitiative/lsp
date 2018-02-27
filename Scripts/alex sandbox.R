df <- data.frame(One = c("Hello", "Mellow", "Swellow", "Fellow"),
                 Two = c("Bradbury", "Cadbury", "Londonderry", "Pike"),
                 Three = c("Half", "Masts", "Keep", "Fast"), stringsAsFactors = FALSE)

pin <- data.frame(which(df == "Masts", arr.ind = TRUE))

print(pin)

#df[pin$row[1], pin$col[1]]

junk_set <- c(1:pin$row[1])

print(junk_set)

df_clean <- df[-junk_set, ]

print(df_clean)









ABS_cleanser <- function(df = NULL, x = NULL) {

  pin <- data.frame(which(df == x, arr.ind = TRUE))

  junk_set <- c(1:pin$row[1])

  df_clean <- df[-junk_set, ]

  return(df_clean)
}


