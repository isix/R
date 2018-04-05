library(dplyr)
library(wakefield)
df <- 
  r_data_frame(
  n = 254,
  id,
  race,
  age,
  sex,
  hour,
  iq,
  height,
  died,
  Scoring = rpois(50),
  Smoker = valid
  ) %>%
  r_na(prob=0.10)
  
df <- data.frame(df)

df

  