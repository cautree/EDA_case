devtools::install_github("garrettgman/DSR")

library(ggplot2)
diamonds_by_cut = diamonds %>%
  group_by(cut) %>%
  summarize(countofcut = n(),
            mean.price = mean(price)) %>%
  arrange(mean.price)

# view categorical by categorial
diamonds%>%
  ggplot(aes(color,fill=cut))+
  geom_bar(position="dodge")

diamonds_by_cut_color = diamonds%>%
  group_by(cut, color) %>%
  summarize(total=n(),
            mean = mean(price)) %>%
  arrange(mean)

diamonds_by_cut_color

good_verygood = diamonds_by_cut_color %>%
  filter(cut %in% c("Good","Very Good"))


good_verygood %>%
  ggplot(aes(color,mean,color=cut))+  ## notice where to put the color
  geom_point()



#tidy function in the broom package
library(broom)
diamond_mod = lm(price~carat,data=diamonds)
tidy(diamond_mod)

diamond_mod2 =lm(price~carat+color, data=diamonds)
tidy(diamond_mod2)

# use bind_rows function to bind results from two models together
bind_rows(tidy(diamond_mod), tidy(diamond_mod2))

data(midwest)
head(midwest)
library(tidyr)
#use nest function from tidyr
nested=midwest %>%
  nest(-state)  %>% # get tibble for each state
unnest(data)  # will unnest at a later step


#This "nested" data has an interesting structure. 
#The second column, data, is a list, a type of R object that allows 
#complicated objects to be stored within each row. 
#This is because each item of the data column is itself a data frame.

#You can use nested$data to access this list column and double brackets 
#to access a particular element. For example, 
#nested$data[[1]] would give you the data frame with IL data , 
#since IL is the first row of the table.

IL=nested$data[[1]]

dim(IL)


#The opposite of the nest() operation is the unnest() operation.
#This takes each of the data frames in the list column and 
#brings those rows back to the main data frame.

#map() applies an operation to each item in a list

library(purrr)
v=list(1,2,3)
map(v,~.*10)
?map()

names(midwest)

state_coefficiency=midwest %>%
  nest(-state)  %>%
  mutate(models= map(data,~lm(percelderlypoverty~percollege,.))) %>%
  mutate(tidied = map(models,tidy)) %>%
  unnest(tidied)  # unnest combine the tidies model


# Not all slopes are significant, and you can use the p-value to guess which are and which are not.
#However, when you have lots of p-values, like one for each country, 
#you run into the problem of multiple hypothesis testing, where you have to set a stricter threshold. 
#The p.adjust() function is a simple way to correct for this, where p.adjust(p.value) on 
#a vector of p-values returns a set that you can trust.
state_coefficiency %>%
  filter(term=="percollege")  %>%
  
  mutate(p.adjusted = p.adjust(p.value))%>%
  filter(p.adjusted <0.05)


library(DSR)
table1
#http://garrettgman.github.io/tidying/

table2

table3

table4
