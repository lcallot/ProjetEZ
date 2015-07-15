# Long-term interest rate
LTrate <- read.csv("data/OECD/LTrate.csv", sep=";")
colnames(LTrate)<-c("time","LTR_AUS","LTR_BEL","LTR_DEN","LTR_FIN","LTR_FRA",
  "LTR_GER","LTR_GRE","LTR_IRL","LTR_ITA","LTR_LUX",
  "LTR_NET","LTR_POR","LTR_SPA","LTR_SWE","LTR_UKI")

# Long-term interest rate
STrate <- read.csv("data/OECD/STrate.csv", sep=";")
colnames(STrate)<-c("time","STR_AUS","STR_BEL","STR_DEN","STR_FIN","STR_FRA",
                    "STR_GER","STR_GRE","STR_IRL","STR_ITA","STR_LUX",
                    "STR_NET","STR_POR","STR_SPA","STR_SWE","STR_UKI")

