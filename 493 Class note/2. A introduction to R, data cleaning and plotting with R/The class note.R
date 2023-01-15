install.packages("tidyverse")
install.packages("lmtest")
library("tidyverse")
library("ggplot2")
install.packages("gapminder")

# load library
library("gapminder")
library("tidyverse")
library("ggplot2")


#the tip
  #i wantto have the same bersion of hte software as my student
  #bUT I DO'T WANT AN UPDATE TO break anything
  # do not update anything until you are down.

# squrt(400)

#An object's name represet tthe information store in that object, so you can treat the obect's name as it it were the vaule stored inside

#An vestoryt is seris of elements such as numbers, you can cereate a vector and store it as an obejct in the same way. 
#the function can do the al the timne.


# we ofthen work with. dat that are cartegorical, to created a vector of rtest - strings in programming term
  #we must place the txt in quotes
  #string vecotr

#categrica data can aso bes stored as a factor, which has a underlying nunmberic
  #presentation, model witll convert factors to dummies
  #factor? 

#the same priples can be used tp. create more complex obejct like matrices, arrys lists and daterframe
#most data sets you will work with will be road into R and stored as dataframe
# so this course will mainly focus on ,mainpulating and visulaizing these objets. 

#The the same priciple  can be used to create more complsx tobjects lie matrix, arrys list anda datafreamas
#most data sets you will work with will be read into and stored as data frame so this cource will ianly foucisng

#so, other wat of rreathing obejctive, you have a lot of tip for creatinga lot of thing, 

# you need to save it. 

#save(The class note, file = "wulala gei")
# this thing need to using the save the enviroment it can help on saivng both the script. 

data(cars)
View(cars)

#strs 
  #Can tell you which kind of data you got.
# you can create a data
# when you looking at the table. 
cars2 <- as_tibble(cars)
head(cars2)
summary(cars)

# The histgrame
# how to access only one bvairlce

str(cars$speed)
# if you put an asisgnment suchas x<-y in parenteses (), R will print the outut of hte assignmnet out for tou in our document.

#Linear regress ion 
fit <- lm(dist ~ speed, data = cars)
summary(fit)

str(fit)

#another ugly ploy
plot(cars$speed, cars$dist)
abline()

#a nicer ploy with ggplots
ggplot(data = cars, aes(x = speed, y = dist)) + geom_point() + geom_smooth(method = "lm")

#working with acutally data
  #using the function of read.cvs

#hwo to rea it
  #package foreign
    #part of base R
  #package haven
    # paryt of the tidyverse family

#the data set miderml.cvs
#th header = false/ture 
  #the first linke of the first code is eqmpty or not.

#na_rm = ture
  #remove the missing the value form the varble. 

#coord_cartestian ()  
  #set the grade size that you need to do. 


#clanfear.github.io/Csss508
ddd 
# R programming 
  
#magrittr and piple (%>%)
#log(mean(gapminder$pop))
#gapminder$pop %>% mean() %>% log()
#send gapminder$pop to mean(), then send the output of that to log().”

#Stuff to the left of the piple is assed to the first argument of the function on the right, other argument go on the right in the funtion. 
#if you ever find yourself piple a function where data are not the first argument use. in the data argument instead. 
#gapminder %>% lm(pop ~ year, data = .)

#No matter how long the chain of function is, assignment is always down at the top
  #this is just a stylistic convention, you. can do assignment at the end of the chain. 

#There are five key dplyr verbs that need to learn
  #f1 filter: Filter (i.e. subset) rows based on their values.
  #arrange: Arrange (i.e. reorder) rows based on their values.
  # select: Select (i.e. subset) columns by their names:
  # mutate: Create new columns.
  #summarize: Collapse multiple rows into a single summary value.

#filer() data frames
  #gapminder %>% filter(country == "Canada" & year > 1980)
  #give me rows where the coutnry is Canada and the year is after 1980
  #or using 
    #gapminder %>% filter(country == "Canada" | year > 1980)
      #give me rows wher the country is Canada or the year is after 1980 or the both. 

#%in% 
  # we can use %in% but for matching any element in a vector 
former_yugoslavia <-
  c("Bosnia and Herzegovina", "Croatia","Montenegro", "Serbia", "Slovenia")
yugoslavia <- gapminder %>%
  filter(country %in% former_yugoslavia)
tail(yugoslavia, 2)

  #Along with filtering the data to see cetain rows, we might want to sort it
    yugoslavia %>%
arrange(year, desc(pop)) %>%
  head(4)
  
#selection function 
  #selection function has a variety of helper functions like start_with()
  #end_with() and matches(), or can be given a range of contiguous columns starver:end
  
#Selective(where())
    #An especially useful helper for select is where() which can be used
    #for selecting columns based on functions that check column types.
    
    gapminder %>%
      select(where(is.numeric)) %>%
      head(4)
    
  # Renaming columns with select()
    #We can rename columns using select(), but that drops everything that isn't mentioned 
    yugoslavia %>%
      select(Life_Expectancy = lifeExp) %>%
      head(4)
      #choising the yougolsaveis, rename the life_expenctancy = life exp 
    
    
  #rename colums with rename()
    # rename() renames variables using the same syntax as select()
      #without dropping unmentioned variables.
    
    yugoslavia %>%
    select(country, year, lifeExp) %>%
      rename(Life_Expectancy = lifeExp) %>%
      head(4)

  #Mutate()
      #In dplyr, you can add new columns to a data frame using mutate().
    yugoslavia %>%
      filter(country == "Serbia") %>%
      select(year, pop, lifeExp) %>%
      mutate(pop_million = pop / 1000000) %>%
      head(2)
    
  #ifelse()
     #A common function used in mutate() is ifelse(). It returns a
      #vector of values depending on a logical test.
    
    ifelse(test = x==y, yes = first_value , no = second_value)
    #output from ifelse() if x==y is
      #True: first_value
      #Faluse: second_value
      #Na:NA
    
    #example
    example <- c(1, 0, NA, -2)
    ifelse(example > 0, "Positive", "Not Positive")
  
  #ifelse() example 
    yugoslavia %>%
    mutate(
      short_country =
        ifelse(country == "Bosnia and Herzegovina","B and H", as.character(country))
    ) %>%
      select(country, short_country, year, pop) %>%
      arrange(year, short_country) %>%
      head(2)
    
  ######################################################################################################################################### 
    #case_when()
      #case_when() performs multiple ifelse() operations at the same
    #time. case_when() allows you to create a new variable with values
    #based on multiple logical statements. This is useful for making
    #categorical variables or variables from combinations of other
    #variables
    
    #note here: ifsle
    #ths is test
    
    