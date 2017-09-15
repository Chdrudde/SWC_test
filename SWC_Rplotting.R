# R script SWC part Ruben R

df<-read.csv("Metadata.csv",nrows=77)
View(df)

# Which variables are in the dataframe?
# Output: each variable with class

str(df)

##### Start plotting ####
library(ggplot2)

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

# Challenge time
## Right side: Conductivity
## Middle: Diversity
## Left: Cell density


