library(tidyverse)
library(ggthemes)
library(wesanderson)
library(reshape2)

data <- read_csv("data/DPO_Health_Alert_Network_CDPHE.csv") %>%
  select(Credential_CredentialNumber,Credential_Type_Prefix1,Certification_Specialty1,emailAddress) %>%
  `colnames<-`(c("cred_num","cred_type","cert_spec","email"))


parse_specialties <- function(input = data)
{
# Here's the format:
# (Certification) Internal Medicine, (Certification) Pediatrics, (Certification) Preventive Medicine: Addiction Medicine, (Specialty) Emergency Medicine, (Specialty) Internal Medicine 

data2 <- data %>%
  mutate(cert_spec = case_when(
    cred_type == "APN" ~ "(Specialty) Nurse Practitioner",
    cred_type == "PA" ~ "(Specialty) Physician Assistant",
    cred_type == "PHA" ~ "(Specialty) Pharmacist",
    TRUE ~ cert_spec
  )) %>%
  select(-cred_type) %>%
  separate(cert_spec, into=c("a","b","c","d","e","f","g","h","i","j","k","l","m"),
    sep = ", ") %>%
  group_by(cred_num) %>%
  slice(1) %>%
  ungroup()


data3 <- data2 %>%
  pivot_longer(-c(cred_num,email)) %>%
  filter(!is.na(value)) %>%
  mutate(specialty = 
           startsWith(value, prefix = "(Specialty)"),
         certification = 
           startsWith(value, prefix = "(Certification)")
  ) %>%
  mutate(value = gsub(".*) ", "", value))

data_spec <- data3 %>%
  filter(specialty) %>%
  filter(value != "Other") %>%
  group_by(cred_num,email) %>%
  summarize(specialties = list(value))

return(data_spec)
}

specialties_df <- parse_specialties(input=data)

main_specialties <- specialties_df %>%
  ungroup() %>%
  select(specialties) %>% 
  unlist() %>%
  data.frame() %>%
  `colnames<-`("main_specialties") %>%
  mutate(main_specialties = gsub(":.*", "", main_specialties)) %>%
  count(main_specialties) %>%
  arrange()

main_specialties %>%
  data.frame() %>%
  write_csv("data/main_specialties.csv")
