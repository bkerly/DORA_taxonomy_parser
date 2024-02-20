library(tidyverse)
library(ggthemes)
library(wesanderson)
library(reshape2)

data <- read_csv("data/DPO_Health_Alert_Network_CDPHE.csv") %>%
  select(Credential_CredentialNumber,Certification_Specialty1,emailAddress) %>%
  `colnames<-`(c("cred_num","cert_spec","email"))


parse_specialties <- function(input = data)
{
# Here's the format:
# (Certification) Internal Medicine, (Certification) Pediatrics, (Certification) Preventive Medicine: Addiction Medicine, (Specialty) Emergency Medicine, (Specialty) Internal Medicine 

data2 <- data %>%
  separate(cert_spec, into=c("a","b","c","d","e","f","g","h","i","j","k","l","m"),
    sep = ", ") 

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

specialties <- parse_specialties(input=data)
