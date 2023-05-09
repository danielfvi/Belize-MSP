####Biomass estimates
# # Run once devtools is successfully installed
# devtools::install_github("cfree14/datalimited2")
library(datalimited2)
library(tidyverse)
library(rfishbase)

SAU <- read_csv("data/SAU EEZ 84 v50-1.csv")

sau = SAU %>% 
  filter(year>1999)

sau_catch = sau %>% 
  group_by(common_name, scientific_name) %>% 
  summarise(tons = sum(tonnes))

spp = c("Caranx hippos")

##Caranx
caranx = sau %>% 
  filter(scientific_name == "Caranx hippos") %>% 
  group_by(scientific_name, year) %>%
  summarise(tonnes = sum(tonnes))

output <- cmsy2(year=caranx$year, catch=caranx$tonnes, r.low=0.2, r.hi=0.8)
plot_dlm(output)  


dat = output[[2]]

ggplot(data = dat) +
  geom_line(aes(x = year, y = catch), size = 2)+
  theme_classic()

ggplot(data = dat) +
  geom_line(aes(x = year, y = bbmsy), size = 2)+
  theme_classic()

ggplot(data = dat) +
  geom_line(aes(x = year, y = ffmsy), size = 2)+
  theme_classic()

dat2 = output[[1]] 
#Scenario
#If f stays the same
b0 = dat$b[nrow(dat)]
f = dat$f[nrow(dat)]
k = dat2$est[2]
r = dat2$est[1]


for(n in 1:50){
  if(n==1){
    b=b0
  }else(
    b = rbind(b, 
              b[n-1] + r*b[n-1]*(b[n-1]/k) - f*b[n-1])
  )
  print(b)
}
