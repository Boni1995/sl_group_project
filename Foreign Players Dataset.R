library(tidyr)
foreign_born = read.xlsx('Dataset Two Approaches Migrants World Cup 1930-2018 HARVARD.xlsx')


#Number of rows same as foreign_born
foreign_born$foreign <- numeric(nrow(foreign_born))  

#Loop to add a column that assesses whether a football player is of foreign origins or not
for (i in 1:nrow(foreign_born)) {
  if (any(foreign_born[i, c("Nationality.Father", "Nationality.Mother", "Nationality.Grandfather", "Nationality.Grandmother", "Country.of.birth")] != foreign_born[i, "International"], na.rm = TRUE)) {
    foreign_born$foreign[i] <- 1
  } else {
    foreign_born$foreign[i] <- 0
  }
}

#Remove not needed columns
foreign_born <- foreign_born[, c("International", "FIFA.World.Cup", "foreign")]

#Group by World Cup and Country
foreign_born_aggregate <- aggregate(foreign ~ International + FIFA.World.Cup, data = foreign_born, FUN = sum)
