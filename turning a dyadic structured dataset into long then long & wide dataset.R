##How to turn a dyadic structured dataset with indistinguishabl dyads into an individual (long) and then pairwise (long and wide) dataset
#Adapted from https://www.ethan-young.com/code/restructuring-dyadic-data/
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)

dyad_pair <- dyad.data %>% #the new dataframe will be called dyad_pair. dyad.data is the name of the dyadic structured dataset 
  gather(key,value,-dyad) %>% 
  separate(key, c("key", "partnum"), sep = "_")%>%
  mutate(partnum = ifelse(partnum =="A", 1, 2)) %>% 
  spread(key,value) %>% #these first 5 lines changes the dataset from dyadic to invididual (long)
  split(.$dyad) %>% #from here changes the dataset from individual to pairwise
  map_df(function(MapD2P){
    
    person1 <- MapD2P %>% #creates a row for person 1
      mutate(act.par = ifelse(partnum == 1,"A","P")) %>% 
      gather(key,value,-dyad,-act.par) %>% 
      unite(new_key,key,act.par) %>% 
      spread(new_key,value)
    
    person2 <- MapD2P %>% #creates a row for person 2
      mutate(act.par = ifelse(partnum == 1,"P","A")) %>% 
      gather(key,value,-dyad,-act.par) %>% 
      unite(new_key,key,act.par) %>% 
      spread(new_key,value)
    
    bind_rows(person1,person2) #combines the two rows
  }) %>% 
  mutate(partnum = ifelse(partnum_A == 1, 1, 2)) %>% #re-adds the partnum var
  select(dyad,partnum,y_A, y_P,x_A,x_P)