require(readxl)


df <- read_xlsx("./Множ и линейная регрессия/data/data.xlsx", range = "B4:F26")

volume <- df[1]

data <- df[2:5]

scatter.smooth(x=volume, y=data, main="Dist ~ Speed")


linearMod <- lm(dist ~ speed, data=cars)