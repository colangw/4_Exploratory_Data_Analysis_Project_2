c1 <- c("A", 1, 2, 3)
c2 <- c("B", 4, 5, 6)
c3 <- c("C", 7, 8, 9)
c4 <- c("A", 7, 8, 9)
c5 <- c("C", 7, 8, 9)

df <- data.frame(rbind(c1, c2, c3, c4, c5))
df$X1 <- as.character(df$X1)

lst <- c("A", "C")

df$X1[1] %in% lst
df[which(df$X1 %in% lst), ]
