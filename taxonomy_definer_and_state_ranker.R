library(tidyverse)
library(ggthemes)


# Read in data and make it longform ---------------------------------------


taxonomy_longer <- function(df,person) {
  df %>%
    select(-Count) %>%
    pivot_longer(-Specialty,
                names_to = "taxonomy",
                values_to = person)
}

brian <- read_csv("data/Taxonomy Checklist - Brian Erly.csv") %>%
  taxonomy_longer("brian")

jwd <- read_csv("data/Taxonomy Checklist - JWD.csv")%>%
  taxonomy_longer("jackie")

amirtha <- read_csv("data/Taxonomy Checklist - Amirtha.csv")%>%
  taxonomy_longer("amirtha")

# Combine and find differences --------------------------------------------



all_taxonomy <- brian %>%
  left_join(jwd, by = c("Specialty", "taxonomy")) %>%
  left_join(amirtha, by = c("Specialty", "taxonomy"))

consensus <- all_taxonomy %>%
  rowwise() %>%
  mutate(consensus = mean(c(brian,jackie,amirtha)) > 0.5 ,
         jwd_agree = (brian == jackie),
         amirtha_agree = (brian == amirtha))



# Do summary stats --------------------------------------------------------

jwd_pct_checked <- consensus$jackie %>% mean()

jwd_agree <- consensus$jwd_agree %>% mean()

amirtha_pct_checked <- consensus$amirtha %>% mean()  

amirtha_agree <- consensus$amirtha_agree %>% mean()

coworkers_df <- data.frame(
  state = c("jackie","amirtha"),
  hardworking = c(jwd_pct_checked,amirtha_pct_checked),
  agrees = c(jwd_agree,amirtha_agree)
)


# Calculate state differences ---------------------------------------------

state_data <- read_csv("data/States vs Brian - Sheet1.csv") 

state_ranks <- state_data %>%
  transmute(State = State,
            hardworking = (`Hardworking Rank` * 2) -1,
            agrees = (`Agrees with Brian`) * 10 -5) %>%
  mutate(label = 
           case_when(
             State %in% c("Colorado","New York","New Mexico",
                       "Arizona","Florida",
                       "California") ~ State,
             TRUE ~ NA_character_
           )
  )

ggplot(state_ranks,aes(x=hardworking,y=agrees,label = label)) +
  geom_point() +
  ggrepel::geom_label_repel()+
  theme_fivethirtyeight()


state_ranks_dist <- state_ranks %>%
  bind_rows(
    coworkers_df
  )
         
         
         

         