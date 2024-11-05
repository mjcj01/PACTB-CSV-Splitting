### Remove the "###" if you need to install the tidyverse package.
### install.packages("tidyverse")
library(tidyverse)

### A few notes up top:

### I have tried to include annotations to not only make it easy to replicate the
### CSV file splitting, but also to demystify how code works. Anything that has
### a set of "###" in front of it is an annotation and not actual code. If you remove
### the "###", R will read it as code and get very confused.

### The "%>%" is a pipe operator. This means that it takes the output of the code
### from one function and "pipes it" into the next function.

### Reading in CSV File
read_csv("LIT-SHORTLIST.csv") %>%
  ### Removing slashes since they are not allowed in file names
  mutate(Genre = gsub("\\ / ", " or ", Genre),
         Genre = gsub("\\/", " or ", Genre)) %>%
  ### Creating location line and website line columns.
  ### Basically, if the Extended Bio column is an NA value (null value), only return the city and county columns.
  ### Same for website links; if the personal webpage is null, only return PSU catalog link.
  mutate("location_line" = ifelse(is.na(`Extended Bio`),
                                  paste(City, "|", County, sep = " "),
                                  paste(City, "|", County, "|", `Extended Bio`, sep = " ")),
         "web_line" = ifelse(is.na(`Personal Webpage`),
                                   paste(`PSU Catalog Link`),
                                   paste(`PSU Catalog Link`, "|", `Personal Webpage`, sep = " "))) %>%
  ### This tells R to remember to group rows with the same value in the Genre column together.
  group_by(Genre) %>%
  ### I found this next code in a help forum, so I do not entirely understand it.
  ### However, this is what splits the CSV file based on the group_by() line.
  group_walk(function(.x, .y) {
    write.csv(.x, file = paste0("CSV Files/Literary Static Map Files/lit_", .y$Genre, ".csv", sep = ""))
  })

### Same process as above, except where noted.
read_csv("ALL-TIMEAWARE.csv") %>%
  ### This tells R to read the BIRTH and DEATH columns as dates instead of characters.
  ### This is important for below.
  mutate("BIRTH" = as.Date(TimeBirth, "%m/%d/%Y"),
         "DEATH" = as.Date(DEATH, "%m/%d/%Y")) %>%
  ### This is a long if/else statement.
  ### Basically, it says "if the birth date is within a given set of dates, return this value.
  ### If it is not with those two dates, move to the next parameter.
  ### If anything falls outside these dates, it will return a "Check!" so that you can see
  ### which rows are giving trouble.
  mutate("TimePeriod" = ifelse(BIRTH < "1775-01-01", "Colonial",
                         ifelse(BIRTH >= "1775-01-01" & BIRTH < "1800-01-01", "Revolutionary",
                         ifelse(BIRTH >= "1800-01-01" & BIRTH < "1865-01-01", "Romanticism",
                         ifelse(BIRTH >= "1865-01-01" & BIRTH < "1900-01-01", "Realism",
                         ifelse(BIRTH >= "1900-01-01" & BIRTH < "1945-01-01", "Modernism",
                         ifelse(BIRTH >= "1945-01-01", "Contemporary", "Check!"))))))) %>%
  mutate(TimePeriod = gsub("\\ / ", " or ", TimePeriod),
         TimePeriod = gsub("\\/", " or ", TimePeriod)) %>%
  ### Instead of grouping by genre, we are grouping by time period since this is for the
  ### chronological maps.
  group_by(TimePeriod) %>%
  group_walk(function(.x, .y) {
    write.csv(.x, file = paste0("CSV Files/Cultural Chronological Map Files/", .y$TimePeriod, ".csv", sep = ""))
  })


### The two CSV splits below are identical to the two that were annotated above, so
### they are not annotated.

read_csv("LIT-TIMEAWARE.csv") %>%
  mutate("BIRTH" = as.Date(TimeBirth, "%m/%d/%Y"),
         "DEATH" = as.Date(TimeDeath, "%m/%d/%Y")) %>%
  mutate("TimePeriod" = ifelse(BIRTH < "1775-01-01", "Colonial",
                         ifelse(BIRTH >= "1775-01-01" & BIRTH < "1800-01-01", "Revolutionary",
                         ifelse(BIRTH >= "1800-01-01" & BIRTH < "1865-01-01", "Romanticism",
                         ifelse(BIRTH >= "1865-01-01" & BIRTH < "1900-01-01", "Realism",
                         ifelse(BIRTH >= "1900-01-01" & BIRTH < "1945-01-01", "Modernism",
                         ifelse(BIRTH >= "1945-01-01", "Contemporary", "Check!"))))))) %>%
  mutate(TimePeriod = gsub("\\ / ", " or ", TimePeriod),
         TimePeriod = gsub("\\/", " or ", TimePeriod)) %>%
  group_by(TimePeriod) %>%
  group_walk(function(.x, .y) {
    write.csv(.x, file = paste0("CSV Files/Literary Chronological Map Files/", .y$TimePeriod, ".csv", sep = ""))
  })

read_csv("ALL-SHORTLIST.csv") %>%
  mutate(Vocation = gsub("\\ / ", " or ", Vocation),
         Vocation = gsub("\\/", " or ", Vocation)) %>%
  mutate("location_line" = ifelse(is.na(`Extended Bio`),
                                  paste(City, "|", County, sep = " "),
                                  paste(City, "|", County, "|", `Extended Bio`, sep = " ")),
         "web_line" = ifelse(is.na(`Personal Webpage`),
                             paste(`PSU Catalog Link`),
                             paste(`PSU Catalog Link`, "|", `Personal Webpage`, sep = " "))) %>%
  group_by(Vocation) %>%
  group_walk(function(.x, .y) {
    write.csv(.x, file = paste0("CSV Files/Cultural Static Map Files/", .y$Vocation, ".csv", sep = ""))
  })
