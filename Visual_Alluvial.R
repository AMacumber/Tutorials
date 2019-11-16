### Tutorial: Alluvial Diagrams
## https://towardsdatascience.com/alluvial-diagrams-783bbbbe0195
###


### load packages
library(tidyverse)
library(RCurl)
library(magrittr)
###


## we want values for depress and dep16 at each of time 1,2,3,4 broken as columns
datW <-
  
  # start a pipeline
  datL %>%
  
  #  select variables of interest
  select(ID, w1group, time, depress, dep16) %>%
  
  # organise by ID, by treatment, by time
  # create dim: "variable" (l: depressed, not depressed)
  # create dim: "value" (l: depress value)
  pivot_longer(c(-ID, -w1group, -time), names_to = "variable", values_to = "value") %>%
  
  # create dim: "variableT"; 
  # pastes together "variable" and "time"
  # pastes together "value" and "time"
  unite(variableT, variable, time, sep=".") %>%
  
  # breaks "variableT" into new columns
  spread(variableT, value)
##


## alluvial prep
datAlluvial <-
  
  # start a pipeline
  datW %>%
  
  # return cases with no missing values
  filter(complete.cases(.)) %>%  
  
  # dep16 is a binary indicator of depression
  group_by(dep16.1, dep16.2, dep16.3, dep16.4) %>%
  
  # create dim: "n" count of each of unique combos of dep16 at time 1,2,3,4
  summarise(n = n()) %>%
  
  # does not change visual structure, but required for next method
  ungroup() %>%
  
  # change binary measures to text labels
  mutate_at(vars(-n), funs(factor(., levels=c(0, 1), labels=c("not depressed", 
                                                              "depressed"))))
##


## alluvial diagram

library(alluvial)

windows(10,7)

alluvial(datAlluvial[,1:4],  # dep16 at time 1 to 4
         freq=datAlluvial$n,  # counts of each unique combination
         col = ifelse(datAlluvial$dep16.4 == "depressed", "#ff0000", "#D3D3D3"),
         axis_labels = c("Year 1", "Year 2", "Year 3", "Year 4"),
         cex = 0.7)
##
###