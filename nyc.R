
#Load packages
library(tidyverse)
library(sjlabelled)
library(descr)
library(forcats)
library(gt)
library(glue)
library(ggplot2)
library(haven)
library(foreign)
library(scales)
library(stringi)
library(stringr)
library(sjPlot)
library(likert)
library(knitr)
library(tibble)
library(reshape)
library(ggalt)  
library(survey)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(janitor)
library(srvyr)
library(webshot)

# Info on GT tables: https://gt.rstudio.com/articles/intro-creating-gt-tables.html

#Load in SPSS file
#set working directory
setwd("S:\\Departments\\Research & Insights\\2020 Racial Injustice\\Round 1 FollowUp\\Muslim-American and Industry-OrgSize")
getwd()

SPSS = read_sav("Industry_OrgSize.sav")



SPSS <- filter(SPSS, !(CID %in% omit))


#Factor labeled data
SPSS <- as_factor(SPSS, only_labelled = TRUE)

## @knitr numb_rows
#Review data set
nrow(SPSS)
str(SPSS)
class(SPSS)
# Make sure data is of class: data.frame
SPSS <- as.data.frame(SPSS)
class(SPSS)


##################### Q5
# Create survey design with srvy package
SPSS.unweighted <- svydesign(ids=~1, 
                             data=SPSS)


### TO BE FILLED IN BY USER:
variables <- list(~Q5_1, ~Q5_2, ~Q5_3, ~Q5_4, ~Q5_5, ~Q5_6, ~Q5_7, ~Q5_8, ~Q5_9)
group_name <- "Response to Protests"
Num_of_variables <- length(variables)

item_name <- c("I feel emotionally safe sharing how I feel about protests against racial injustice with co-workers",
               "I feel safe voicing my opinions about racial justice issues",
               "I feel respected and valued at work",
               "My manager supports discussions of racial justice issues at work",
               "My workplace discourages discussion of racial justice issues at work",
               "My workplace is not doing enough to promote racial justice in the world",
               "My workplace is doing enough to provide opportunities for Black people in our organization",
               "I feel comfortable engaging in candid/honest conversations about race at work",
               "I think it is inappropriate to discuss race at work")


output <- list() # Initiate empty list for frequencies

# Process frequencies
table_function <- function(var){
  tbl <- data.frame(round(svytable(var, SPSS.unweighted)))
  tbl <- tbl %>% 
    dplyr::mutate(Percent = Freq/sum(Freq)) %>%  # Add percent
    #slice_head(n = 4) %>% 
    rename(Answer = names(.)[1])  
  return (tbl)
}

### Loop through variables in grid to get frequency
for (x in variables){
  results <- list(table_function(x))
  output <- c(output, results)
}


### Convert results into dataframe and name columns using answer choices
df_unweighted <- data.frame(matrix(unlist(output), nrow=Num_of_variables , byrow=T),stringsAsFactors=FALSE)
df_unweighted <- df_unweighted %>% 
  mutate(disagree_n=X5+X6)%>% # collapse agree
  mutate(agree_n=X7+X8) %>%  # collapse disagree
  mutate(disagree_per=X9+X10)%>% # collapse agree
  mutate(agree_per=X11+X12) %>% # collapse disagree
  select(agree_n, agree_per, disagree_n,
         disagree_per)

row.names(df_unweighted)<-item_name
# Add row names
df_unweighted <- rownames_to_column(df_unweighted, var = "Item")

### Combined Table for output
percent_vars <- c("agree_per", "disagree_per")

table_counts<- df_unweighted %>%
  gt(df_unweighted) %>%
  tab_header(
    title = ("To what extent do you agree or disagree with the following statements about your workplace?")) %>% 
  tab_options(table.font.size = 12, heading.title.font.weight ="bold",
              heading.title.font.size = 16) %>% 
  fmt_percent(columns = vars(percent_vars), decimals = 0, scale_values = TRUE) %>% 
  tab_options(table.font.size = 12, heading.title.font.weight ="bold",
              heading.title.font.size = 16) %>% 
  tab_spanner(
    label = "Agree",
    columns = vars(agree_n, agree_per)) %>% 
  tab_spanner(
    label = "Disagree",
    columns = vars( disagree_n, disagree_per))%>% 
  cols_label(
    agree_n = "Unweighted n",
    agree_per = "Unweighted %",
    disagree_n = "Unweighted n",
    disagree_per = "Unweighted %")
table_counts <- opt_row_striping(table_counts, row_striping = TRUE)
table_counts


# Save image
table_counts %>%
  gtsave(
    "Q5_total.png",
    path = getwd()
  )




##################### Q5 - BY INDUSTRY
# Create survey design with srvy package
SPSS.unweighted <- svydesign(ids=~1, 
                             data=SPSS)

SPSS.unweighted <- subset(SPSS.unweighted, INDUSTRY == "Healthcare")
SPSS.unweighted <- subset(SPSS.unweighted, INDUSTRY == "Manufacturing")
SPSS.unweighted <- subset(SPSS.unweighted, INDUSTRY == "Hotel or restaurant")
SPSS.unweighted <- subset(SPSS.unweighted, INDUSTRY == "Technology")
SPSS.unweighted <- subset(SPSS.unweighted, INDUSTRY == "Retail")

output <- list() # Initiate empty list for frequencies

# Process frequencies
table_function <- function(var){
  tbl <- data.frame(round(svytable(var, SPSS.unweighted)))
  tbl <- tbl %>% 
    dplyr::mutate(Percent = Freq/sum(Freq)) %>%  # Add percent
    #slice_head(n = 4) %>% 
    rename(Answer = names(.)[1])  
  return (tbl)
}

### Loop through variables in grid to get frequency
for (x in variables){
  results <- list(table_function(x))
  output <- c(output, results)
}


### Convert results into dataframe and name columns using answer choices
df_unweighted <- data.frame(matrix(unlist(output), nrow=Num_of_variables , byrow=T),stringsAsFactors=FALSE)
df_unweighted <- df_unweighted %>%
  rename(strong_disagree=X9)%>%
  rename(disagree=X10)%>%
  rename(agree=X11)%>%
  rename(strong_agree=X12)%>%
  select(strong_agree, agree, disagree,
         strong_disagree)

# #fOR TABLE
# df_unweighted <- df_unweighted %>%
#   mutate(disagree_n=X5+X6)%>% # collapse agree
#   mutate(agree_n=X7+X8) %>%  # collapse disagree
#   mutate(disagree_per=X9+X10)%>% # collapse agree
#   mutate(agree_per=X11+X12) %>% # collapse disagree
#   select(agree_n, agree_per, disagree_n,
#          disagree_per)

row.names(df_unweighted)<-item_name
# Add row names
df_unweighted <- rownames_to_column(df_unweighted, var = "Item")

# Save separate data sets for each category
Healthcare <- df_unweighted
Healthcare$group <- "Healthcare"
Healthcare$neutral <-0

Manufacturing <- df_unweighted
Manufacturing$group <- "Manufacturing"
Manufacturing$neutral <-0

Hotel_Rest <- df_unweighted
Hotel_Rest$group <- "Hotel_Rest"
Hotel_Rest$neutral <-0

Technology <- df_unweighted
Technology$group <- "Technology"
Technology$neutral <-0

Retail<- df_unweighted
Retail$group <- "Retail"
Retail$neutral <-0

group_combined<- rbind(Healthcare, Manufacturing, Hotel_Rest, 
                                Technology, Retail)

# Save separate data sets for each category - FOR WIDE TABLE
Healthcare <- df_unweighted 
Healthcare <- Healthcare %>% 
  rename(HAgree_n = agree_n) %>% 
  rename(HAgree_per = agree_per ) %>% 
  rename(HDisagree_n = disagree_n) %>% 
  rename(HDisagree_per = disagree_per ) 


Manufacturing <- df_unweighted
Manufacturing <- Manufacturing %>% 
  rename(MAgree_n = agree_n) %>% 
  rename(MAgree_per = agree_per ) %>% 
  rename(MDisagree_n = disagree_n) %>% 
  rename(MDisagree_per = disagree_per ) 

Hotel_Rest <- df_unweighted
Hotel_Rest <- Hotel_Rest %>% 
  rename(HRAgree_n = agree_n) %>% 
  rename(HRAgree_per = agree_per ) %>% 
  rename(HRDisagree_n = disagree_n) %>% 
  rename(HRDisagree_per = disagree_per ) 

Technology <- df_unweighted
Technology <- Technology %>% 
  rename(TAgree_n = agree_n) %>% 
  rename(TAgree_per = agree_per ) %>% 
  rename(TDisagree_n = disagree_n) %>% 
  rename(TDisagree_per = disagree_per ) 

Retail <- df_unweighted
Retail <- Retail %>% 
  rename(RAgree_n = agree_n) %>% 
  rename(RAgree_per = agree_per ) %>% 
  rename(RDisagree_n = disagree_n) %>% 
  rename(RDisagree_per = disagree_per ) 



Retail <- Retail %>% 
  select (-Item)

group_combined <- cbind(Healthcare, Manufacturing, Hotel_Rest, Technology, Retail )
percent_vars <- c("HAgree_per", "HDisagree_per", "MAgree_per", "MDisagree_per",
                  "HRAgree_per", "HRDisagree_per","TAgree_per", "TDisagree_per",
                  "RAgree_per", "RDisagree_per")

table_counts<- group_combined %>%
  gt(group_combined) %>%
  tab_header(
    title = ("To what extent do you agree or disagree with the following statements about your workplace?")) %>% 
  tab_options(table.font.size = 12, heading.title.font.weight ="bold",
              heading.title.font.size = 16) %>% 
  fmt_percent(columns = vars(percent_vars), decimals = 0, scale_values = TRUE) %>% 
  tab_options(table.font.size = 12, heading.title.font.weight ="bold",
              heading.title.font.size = 16) %>% 
  tab_spanner(
    label = "Healthcare",
    columns = vars(HAgree_n, HAgree_per, HDisagree_n, HDisagree_per)) %>% 
  tab_spanner(
    label = "Manufacturing",
    columns = vars(MAgree_n, MAgree_per, MDisagree_n, MDisagree_per)) %>% 
  tab_spanner(
    label = "Hotel_Rest",
    columns = vars(HRAgree_n, HRAgree_per, HRDisagree_n, HRDisagree_per))%>% 
  tab_spanner(
    label = "Technology",
    columns = vars(TAgree_n, TAgree_per, TDisagree_n, TDisagree_per)) %>% 
  tab_spanner(
    label = "Retail",
    columns = vars(RAgree_n, RAgree_per, RDisagree_n, RDisagree_per)) %>% 
  cols_label(
    HAgree_n = "Agree",
    HAgree_per = "%",
    HDisagree_n = "Disagree",
    HDisagree_per = "%",
    MAgree_n = "Agree",
    MAgree_per = "%",
    MDisagree_n = "Disagree",
    MDisagree_per = "%",
    HRAgree_n = "Agree",
    HRAgree_per = "%",
    HRDisagree_n = "Disagree",
    HRDisagree_per = "%",
    TAgree_n = "Agree",
    TAgree_per = "%",
    TDisagree_n = "Disagree",
    TDisagree_per = "%",
    RAgree_n = "Agree",
    RAgree_per = "%",
    RDisagree_n = "Disagree",
    RDisagree_per = "%")
table_counts <- opt_row_striping(table_counts, row_striping = TRUE)
table_counts

table_counts %>%
  gtsave(
    "Q5_Industry.png",
    path = getwd()
  )


##### LIKERT
## Color depends on number of response
color5 <-c("darkseagreen", "darkseagreen2", "gray87", "indianred3", "indianred4")
color4 <-c("darkseagreen", "darkseagreen2", "black", "indianred3", "indianred4")
color3 <-c("darkseagreen", "gray91", "indianred4")

group_combined$group <- as.factor(group_combined$group)
group_combined$Response <- as.factor(group_combined$Item)
group_combined <- group_combined %>%
  select(group, Item, strong_agree, agree, neutral,disagree, strong_disagree)

group_combined$strong_agree <- group_combined$strong_agree*100
group_combined$agree <- group_combined$agree*100
group_combined$disagree <- group_combined$disagree*100
group_combined$strong_disagree <- group_combined$strong_disagree*100



group_combined1 <- group_combined %>% 
  filter(str_detect(Item, "I "))

group_combined2 <- group_combined %>% 
  filter(str_detect(Item, "My"))
#Plot1
likert_data_grouped <- likert(summary=group_combined1[-1], grouping=group_combined1$group)
plot(likert_data_grouped)

plot(likert_data_grouped, color=color5, plot.neutral=TRUE, plot.percent.neutral=FALSE) +
       theme(legend.position="none",
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             axis.text.y = element_text(size=8))+
       ggtitle("To what extent do you agree or disagree with the following statements about your workplace?")
     
ggsave("Q5_likert_Industry_1.png")

#Plot2
likert_data_grouped <- likert(summary=group_combined2[-1], grouping=group_combined2$group)
plot(likert_data_grouped)

plot(likert_data_grouped, color=color5, plot.neutral=TRUE, plot.percent.neutral=FALSE) +
  theme(legend.position="bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=8))

ggsave("Q5_likert_Industry_2.png")






##################### Q5 - BY B2B
# Create survey design with srvy package
SPSS.unweighted <- svydesign(ids=~1, 
                             data=SPSS)

SPSS.unweighted <- subset(SPSS.unweighted, B2B == "2-99 employees")
SPSS.unweighted <- subset(SPSS.unweighted, B2B == "100-499 employees")
SPSS.unweighted <- subset(SPSS.unweighted, B2B == "500-4999 employees")
SPSS.unweighted <- subset(SPSS.unweighted, B2B == "5000-9999 employees")
SPSS.unweighted <- subset(SPSS.unweighted, B2B == "10000+ employees")

output <- list() # Initiate empty list for frequencies

# Process frequencies
table_function <- function(var){
  tbl <- data.frame(round(svytable(var, SPSS.unweighted)))
  tbl <- tbl %>% 
    dplyr::mutate(Percent = Freq/sum(Freq)) %>%  # Add percent
    #slice_head(n = 4) %>% 
    rename(Answer = names(.)[1])  
  return (tbl)
}

### Loop through variables in grid to get frequency
for (x in variables){
  results <- list(table_function(x))
  output <- c(output, results)
}


### Convert results into dataframe and name columns using answer choices
df_unweighted <- data.frame(matrix(unlist(output), nrow=Num_of_variables , byrow=T),stringsAsFactors=FALSE)
# df_unweighted <- df_unweighted %>%
#   rename(strong_disagree=X9)%>% 
#   rename(disagree=X10)%>% 
#   rename(agree=X11)%>% 
#   rename(strong_agree=X12)%>% 
#   select(strong_agree, agree, disagree,
#          strong_disagree)

#fOR TABLE
df_unweighted <- df_unweighted %>%
  mutate(disagree_n=X5+X6)%>% # collapse agree
  mutate(agree_n=X7+X8) %>%  # collapse disagree
  mutate(disagree_per=X9+X10)%>% # collapse agree
  mutate(agree_per=X11+X12) %>% # collapse disagree
  select(agree_n, agree_per, disagree_n,
         disagree_per)

row.names(df_unweighted)<-item_name
# Add row names
df_unweighted <- rownames_to_column(df_unweighted, var = "Item")

# Save separate data sets for each category
group2_99 <- df_unweighted
group2_99$group <- "group2_99"
group2_99$neutral <-0

group100_499 <- df_unweighted
group100_499$group <- "group100_499"
group100_499$neutral <-0

group500_4999 <- df_unweighted
group500_4999$group <- "group500_4999"
group500_4999$neutral <-0

group5000_9999 <- df_unweighted
group5000_9999$group <- "group5000_9999"
group5000_9999$neutral <-0

group10Kplus<- df_unweighted
group10Kplus$group <- "group10Kplus"
group10Kplus$neutral <-0

group_combined<- rbind(group2_99, group100_499, group500_4999, 
                       group5000_9999, group10Kplus)

# Save separate data sets for each category - FOR WIDE TABLE
group2_99 <- df_unweighted 
group2_99 <- group2_99 %>% 
  rename(group2_99Agree_n = agree_n) %>% 
  rename(group2_99Agree_per = agree_per ) %>% 
  rename(group2_99Disagree_n = disagree_n) %>% 
  rename(group2_99Disagree_per = disagree_per ) 


group100_499 <- df_unweighted
group100_499 <- group100_499 %>% 
  rename(group100_499Agree_n = agree_n) %>% 
  rename(group100_499Agree_per = agree_per ) %>% 
  rename(group100_499Disagree_n = disagree_n) %>% 
  rename(group100_499Disagree_per = disagree_per ) 

group500_4999 <- df_unweighted
group500_4999 <- group500_4999 %>% 
  rename(group500_4999Agree_n = agree_n) %>% 
  rename(group500_4999Agree_per = agree_per ) %>% 
  rename(group500_4999Disagree_n = disagree_n) %>% 
  rename(group500_4999Disagree_per = disagree_per ) 

group5000_9999 <- df_unweighted
group5000_9999 <- group5000_9999 %>% 
  rename(group5000_9999Agree_n = agree_n) %>% 
  rename(group5000_9999Agree_per = agree_per ) %>% 
  rename(group5000_9999Disagree_n = disagree_n) %>% 
  rename(group5000_9999Disagree_per = disagree_per ) 

group10Kplus <- df_unweighted
group10Kplus <- group10Kplus %>% 
  rename(group10KplusAgree_n = agree_n) %>% 
  rename(group10KplusAgree_per = agree_per ) %>% 
  rename(group10KplusDisagree_n = disagree_n) %>% 
  rename(group10KplusDisagree_per = disagree_per ) 

group10Kplus <-group10Kplus %>% 
  select(-Item)

names(group_combined)


group_combined <- cbind(group2_99, group100_499, group500_4999, group5000_9999, group10Kplus )
percent_vars <- c("group2_99Agree_per", "group2_99Disagree_per", "group100_499Agree_per", "group100_499Disagree_per",
                  "group500_4999Agree_per", "group500_4999Disagree_per","group5000_9999Agree_per", "group5000_9999Disagree_per",
                  "group10KplusAgree_per", "group10KplusDisagree_per")

table_counts<- group_combined %>%
  gt(group_combined) %>%
  tab_header(
    title = ("To what extent do you agree or disagree with the following statements about your workplace?")) %>% 
  tab_options(table.font.size = 12, heading.title.font.weight ="bold",
              heading.title.font.size = 16) %>% 
  fmt_percent(columns = vars(percent_vars), decimals = 0, scale_values = TRUE) %>% 
  tab_options(table.font.size = 12, heading.title.font.weight ="bold",
              heading.title.font.size = 16) %>% 
  tab_spanner(
    label = "2-99",
    columns = vars(group2_99Agree_n, group2_99Agree_per, group2_99Disagree_n, group2_99Disagree_per)) %>% 
  tab_spanner(
    label = "100-499",
    columns = vars(group100_499Agree_n, group100_499Agree_per, group100_499Disagree_n, group100_499Disagree_per)) %>% 
  tab_spanner(
    label = "500-4999",
    columns = vars(group500_4999Agree_n, group500_4999Agree_per, group500_4999Disagree_n, group500_4999Disagree_per)) %>%
  tab_spanner(
    label = "5000-9999",
    columns = vars(group5000_9999Agree_n, group5000_9999Agree_per, group5000_9999Disagree_n, group5000_9999Disagree_per)) %>%
  tab_spanner(
    label = "10k+",
    columns = vars(group10KplusAgree_n, group10KplusAgree_per, group10KplusDisagree_n, group10KplusDisagree_per)) %>%
  cols_label(
    group2_99Agree_n = "Agree",
    group2_99Agree_per = "%",
    group2_99Disagree_n = "Disagree",
    group2_99Disagree_per = "%",
    group100_499Agree_n = "Agree",
    group100_499Agree_per = "%",
    group100_499Disagree_n = "Disagree",
    group100_499Disagree_per = "%",
    group500_4999Agree_n = "Agree",
    group500_4999Agree_per = "%",
    group500_4999Disagree_n = "Disagree",
    group500_4999Disagree_per = "%",
    group5000_9999Agree_n = "Agree",
    group5000_9999Agree_per = "%",
    group5000_9999Disagree_n = "Disagree",
    group5000_9999Disagree_per = "%",
    group10KplusAgree_n = "Agree",
    group10KplusAgree_per = "%",
    group10KplusDisagree_n = "Disagree",
    group10KplusDisagree_per = "%")
table_counts <- opt_row_striping(table_counts, row_striping = TRUE)
table_counts

table_counts %>%
  gtsave(
    "Q5_OrgSize.png",
    path = getwd()
  )


##### LIKERT
## Color depends on number of response
color5 <-c("darkseagreen", "darkseagreen2", "gray87", "indianred3", "indianred4")
color4 <-c("darkseagreen", "darkseagreen2", "black", "indianred3", "indianred4")
color3 <-c("darkseagreen", "gray91", "indianred4")

group_combined$group <- as.factor(group_combined$group)
group_combined$Response <- as.factor(group_combined$Item)
group_combined <- group_combined %>%
  select(group, Item, strong_agree, agree, neutral,disagree, strong_disagree)

group_combined$strong_agree <- group_combined$strong_agree*100
group_combined$agree <- group_combined$agree*100
group_combined$disagree <- group_combined$disagree*100
group_combined$strong_disagree <- group_combined$strong_disagree*100



group_combined1 <- group_combined %>% 
  filter(str_detect(Item, "I "))

group_combined2 <- group_combined %>% 
  filter(str_detect(Item, "My"))
#Plot1
likert_data_grouped <- likert(summary=group_combined1[-1], grouping=group_combined1$group)
plot(likert_data_grouped)

plot(likert_data_grouped, color=color5, plot.neutral=TRUE, plot.percent.neutral=FALSE) +
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=8))+
  ggtitle("To what extent do you agree or disagree with the following statements about your workplace?")

ggsave("Q5_likert_OrgSize_1.png")

#Plot2
likert_data_grouped <- likert(summary=group_combined2[-1], grouping=group_combined2$group)
plot(likert_data_grouped)

plot(likert_data_grouped, color=color5, plot.neutral=TRUE, plot.percent.neutral=FALSE) +
  theme(legend.position="bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=8))

ggsave("Q5_likert_OrgSize_2.png")





