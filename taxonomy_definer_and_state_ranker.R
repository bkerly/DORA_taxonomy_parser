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

main_specialties <- read_csv("data/main_specialties.csv")

consensus %>%
  filter(Specialty %in% main_specialties$main_specialties) %>%
  pivot_wider(id_cols = Specialty,
              names_from = taxonomy,
              values_from = consensus) %>%
  write_csv("data/consensus.csv")



# Do summary stats --------------------------------------------------------

jwd_pct_checked <- consensus$jackie %>% mean()

jwd_agree <- consensus$jwd_agree %>% mean()

amirtha_pct_checked <- consensus$amirtha %>% mean()  

amirtha_agree <- consensus$amirtha_agree %>% mean()

brian_pct_checked <- consensus$brian %>% mean()

brian_agree <- 1

coworkers_df <- data.frame(
  State = c("Jackie","Amirtha","Brian"),
  hardworking = c(jwd_pct_checked*100,amirtha_pct_checked*100,brian_pct_checked*100),
  agrees = c(jwd_agree*100,amirtha_agree*100,brian_agree*100)
)


# Calculate state differences ---------------------------------------------

state_data <- read_csv("data/States vs Brian - Sheet1.csv") 

state_ranks <- state_data %>%
  transmute(State = State,
            hardworking = 100-((`Hardworking Rank`) * 2),
            agrees = (`Agrees with Brian`) * 10 -5) %>%
  mutate(label = 
           case_when(
             State %in% c("Colorado","New York","New Mexico",
                       "Arizona","Florida",
                       "California",
                       "Jackie",
                       "Amirtha",
                       "Brian") ~ State,
             TRUE ~ NA_character_
           )
  )

ggplot(state_ranks,aes(x=hardworking,y=agrees,label = label)) +
  geom_point() +
  ggrepel::geom_label_repel()+
  theme_clean()+
  ylim(0,100)+
  xlim(0,100)+
  labs(title = "State Rankings",
       subtitle = "In Terms of Their Respective Dilligence and Agreeableness with Brian") +
  xlab("Hardworking")+
  ylab("Agrees with Brian")

CalculateEuclideanDistance <- function(vect1, vect2) sqrt(sum((vect1 - vect2)^2)) 

state_ranks_dist <- state_ranks %>%
  bind_rows(
    coworkers_df
  ) %>%
  mutate(
    label = 
      case_when(
        State %in% c("Colorado",
                     "New York",
                     "New Mexico",
                     "Arizona",
                     "Florida",
                     "California",
                     "Jackie",
                     "Amirtha",
                     "Brian",
                     "Louisiana",
                     "Hawaii",
                     "North Carolina") ~ State,
        TRUE ~ NA_character_
      ),
    color =
      case_when(
        label %in% c("Jackie","Amirtha","Brian") ~ "blue",
        TRUE ~ "black"
      )
  ) %>%
  rowwise() %>%
  mutate(
    ED_jackie = CalculateEuclideanDistance(c(jwd_pct_checked*100,jwd_agree*100),c(hardworking,agrees)),
    ED_amirtha = CalculateEuclideanDistance(c(amirtha_pct_checked*100,amirtha_agree*100),c(hardworking,agrees)),
    ED_brian = CalculateEuclideanDistance(c(brian_pct_checked*100,brian_agree*100),c(hardworking,agrees))
  )

ggplot(state_ranks_dist,aes(x=hardworking,y=agrees,label = label,color=color)) +
  geom_point() +
  ggrepel::geom_label_repel()+
  theme_clean()+
  ylim(0,100)+
  xlim(0,100)+
  labs(title = "Similarity Between Coworkers and States",
        subtitle = "In Terms of Their Respective Dilligence and Agreeableness with Brian") +
  xlab("Hardworking / \n Percent Boxes Checked")+
  ylab("Agrees with Brian")+
  scale_color_manual(values =c("black","blue"))


# Find Top 3 States for Each ----------------------------------------------

state_ranks_dist %>%
  arrange((ED_jackie)) %>%
  #filter(!(State %in% c("Jackie","Amirtha"))) %>%
  select(-label,-color,-ED_amirtha) %>%
  head(7)


state_ranks_dist %>%
  arrange((ED_amirtha)) %>%
  #filter(!(State %in% c("Jackie","Amirtha"))) %>%
  select(-label,-color,-ED_jackie) %>%
  head(7)
         
state_ranks_dist %>%
  arrange((ED_brian)) %>%
  #filter(!(State %in% c("Jackie","Amirtha"))) %>%
  select(-label,-color,-ED_jackie) %>%
  head(7)
         

         