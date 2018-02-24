library(ggplot2)
setwd("C:/Users/anu21/Downloads/DIC-Lab1-Data-Collection-and-Exploratory-Data-Analysis-master/data")
data <- read.csv("National-Summary.csv")
ggplot(data, aes(x=Week))+
  geom_line(aes(y=X..Positive*14000/30), colour= "blue")+
  geom_line(aes(y=Percent.Positive.A*14000/30), colour= "blue")+
  geom_line(aes(y=Percent.Positive.B*4000/30), colour= "blue")+
  geom_bar(aes(y=Total.A), color = "red",stat = "identity")+
  geom_bar(aes(y=Total.B), color = "blue",stat = "identity")+
  scale_y_continuous(name="Number of Positive Specimens",sec.axis = sec_axis(~ . * 30 / 14000, name = "Percent Positive"))
