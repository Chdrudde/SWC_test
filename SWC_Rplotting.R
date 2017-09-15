# R script SWC part Ruben R on plotting and part FM on dplyr data wrangling

##### Loading libraries ####
library(ggplot2)
library(dplyr)
library(gapminder)

#### Reading the data ####

df<-read.csv("Metadata.csv",nrows=77)
View(df)

# Which variables are in the dataframe?
# Output: each variable with class

str(df)

##### Start plotting ####

#Make first plot

#make reactor cycle a factor to avoid continuous legend

df$Reactor.cycle<-factor(df$Reactor.cycle)


# Plot nicely and store it in an object
p1<-ggplot(data=df, aes(x=Timepoint, y=ph,color=Reactor.cycle))
p2<- p1 +geom_point(size=2, alpha=0.5)+
geom_line()+
ggtitle("Awesome plot")

p3<-p2+ facet_grid((~Reactor.cycle))

p3
# Some NA values because of empty lines at the end; common when you import csv from Excel

#What inside reactor phase?
levels(df$Reactor.phase)

#Make a new plot
p4<-p3 + facet_grid(Reactor.phase~Reactor.cycle)

# Improve the plot by removing redundant information

p4<-p4 + (aes(fill=Reactor.phase,color=Reactor.phase))

#Meanwhile, git again: save the file, open git in tab, click stage

##### Challenge time ####
## Right side: Conductivity (us)
## Middle: Diversity
## Left: Cell density

# Added a dashed line to connect points from different reactor phases
p5<-ggplot(data=df, aes(x=Timepoint, y=Conductivity))+
  facet_grid(~Reactor.cycle)+
  geom_line(lty=2,colour="black")+
  geom_point(size=3, alpha=0.5, aes(fill=Reactor.phase,color=Reactor.phase))+
  geom_line(aes(color=Reactor.phase),size=1)+
  theme_bw()+
  ggtitle("Another awesome plot")
p5

#### Data frame manipulation with dplyr ####


#### load data ####
df<-read.csv("Metadata.csv")

#### calculate the mean of  ####

mean(df[df$Reactor.phase=="Control","ph"])
levels(df$Reactor.phase)

##### Select, filter and pipe #####
physicochem <- dplyr::select(df,ph,temp, Conductivity)

# R has an equivalent of piping %>% (Ctrl + Shift + M is shortcut for this) aaaaaaannnd you can use this with tab completion
# so no more spelling mistakes and pipes can be combined

physicochem <- df %>% select(ph,temp,Conductivity)

physicochem.control <- df %>% 
  filter(Reactor.phase == "Control") %>% 
  select(ph,temp,Conductivity)

# Challenge: select diversity parameters for reactor phase startup
diversity.startup <- df %>% 
  filter(Reactor.phase == "Startup") %>% 
  #select(Diversity...D0,Diversity...D1,Diversity...D2)
  select(contains("Diversity"))

#### group_by and summarize ####

