source("code/setup.R")

# load coded data
load(here::here("data", "ej_data_clean.Rdata"))


comments_coded %<>% rename(success1 = sucess1)

comments_coded %<>% group_by(coalition) %>% 
  fill(success, .direction = "updown") %>% 
  fill(success1, .direction = "updown") %>% 
  fill(success2, .direction = "updown") %>% 
  fill(success3, .direction = "updown") %>% 
  fill(coalition_type, .direction = "updown") 


comments_coded$org_type

comments_coded %<>% mutate(ej_org = str_dct(org_type, "ej"),
                           EJ_org = ifelse(ej_org, "EJ Org", "Other"))

comments_coded %>% filter(ej_org) %>% kablebox()

comments_coded %>% 
  filter(president != "Bush") %>% 
  mutate(success = ifelse(success > 0, "Success", "Falure")) %>% 
  drop_na(success, president, coalition_type, ej_org) %>% 
  group_by(president,  EJ_org) %>% 
  add_count() %>% 
  group_by(n, success, president,  EJ_org) %>% 
  count(.drop = F) %>% 
  mutate(percent = (nn/n) ) %>% 
  ggplot() + 
  aes(x = success, fill = EJ_org, y = percent, label = nn) +
  geom_col(alpha = .5, position = "dodge") + 
  geom_text(check_overlap = T, nudge_y = 0.01) + 
  facet_grid(. ~ president) + 
  labs(x = paste("Lobbying Success (Were Substantive Demands Met?)\n N = ", nrow(comments_coded)), 
       y = "", 
       fill = "") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

comments_coded %>% 
  filter(president != "Bush") %>% 
  drop_na(ej_org, success, coalition_type)%>% 
  mutate(across(starts_with("success"), as.numeric)) %>% 
  #pivot_longer(cols = c("success", "success1", "success2", "success3")) %>% 
  #drop_na(value) %>% 
  #rename(success = value) %>% 
  mutate(success = success) %>% 
  ggplot() + 
  aes(x = success, fill = EJ_org) +
  geom_density(alpha = .5) + 
  facet_grid(coalition_type ~ president)



  comments_coded_ej %>% 
  filter(coalition_type == "Private",
         ej_org) %>%
  kablebox()
  
  

# N = 134
comments_coded_ej <- comments_coded %>%
  dplyr::select(-posted_date, -docket_id, -organization, -number_of_comments_received,
                -submitter_name) %>% 
  inner_join(ejcomments)

ejcomments %<>% mutate(docket_id = id %>% str_remove("-[09]*$"))

ejFRejPRchange <- ejFRejPR %>% filter(change)


comments_coded_ej %<>% 
  ungroup() %>% 
  mutate(ejPR = docket_id %in% ejPR$docket_id,
         win1 = !ejPR & docket_id %in% ejFR$docket_id,
         win2 = docket_id %in% ejFRejPRchange$docket_id)


names(ejFRejPR)

d <- comments_coded_ej %>% 
  mutate(across(starts_with("success"), as.numeric)) %>% 
  pivot_longer(cols = c("success", "success1", "success2", "success3")) %>% 
  drop_na(value) %>% 
  rename(success = value)

 d <- comments_coded_ej
 
 
 
 
 

dejpr <- d %>% filter(ejPR)
d_pr <- d %>% filter(!ejPR) 

# n = 35 / 116
d_pr %>% 
  ggplot() + 
  aes(x = win1, y = success, color = president) +
  geom_jitter() + 
  facet_wrap("ej_org")

# n = 99 / 320
dejpr %>% 
  ggplot() + 
  aes(x = win2, y = success, color = president) +
  geom_jitter() + 
  facet_grid(. ~ ej_org)

d_pr %>% filter(ej_org) %>% kablebox()

cor.test(d_pr$success, d_pr$win1 %>% as.numeric())

dejpr %>% filter(ej_org) %>% kablebox()

cor.test(dejpr$success, dejpr$win2 %>% as.numeric())

lm(success ~ win1 + president + coalition_size + 
     position,
   data = d_pr) %>% modelsummary(stars = T)

lm(success ~ win2 + president + coalition_size + 
     position,
   data = dejpr) %>% modelsummary(stars = T)

comments_coded %>% names()






comments_coded %<>% 
  mutate(ej_comment = document_id %in% ejcomments$document_id)


comments_coded %>% count(org_type)

comments_coded$org_type %<>%
  str_replace("faith", "religious") %>% 
  str_replace("tribal", "tribe") %>% 
  str_replace("law firm", "legal") %>% 
  str_replace("labor", "union") %>% 
  str_replace("federal", "Other Federal Agency") %>% 
str_replace("environmental advocacy|environmental|environment", "environmental group") %>% 
            str_replace("federal credit union", "credit union") 




# #FIXME unlist success
# comments_coded %<>% 
#   mutate(across(starts_with("success"), as.numeric)) %>% 
#   pivot_longer(cols = c("success", "success1", "success2", "success3")) %>% 
#   drop_na(value) %>% 
#   rename(success = value)

winrates <- comments_coded %>% 
  mutate(org_type = str_split(org_type, ";")) %>% 
  unnest(org_type) %>% 
  ungroup() %>% 
  group_by(org_type) %>% 
  mutate(win = success > 1 ) %>% 
  summarise(n = n(),
            winrate = sum(win,na.rm = T)/n,
            winrate = winrate %>% round(2)) %>% 
  arrange(-n) 

orgs <- "ej, ngo, corp, corp group, state, other federal agency, pressure group, professional, tribe, university, religious, environmental group, credit union, law firm, finance, city, union"

orgs %<>% str_split(", ") %>% flatten() %>% unlist()

winrates %>% 
  #filter(org_type %in% orgs) %>% 
  kablebox()


# now with ej subset
winrates_ej <- comments_coded %>% 
  drop_na(org_type) %>% 
  filter(ej_comment) %>%  
  mutate(org_type = str_split(org_type, ";")) %>% 
  unnest(org_type) %>% 
  ungroup() %>% 
  group_by(org_type) %>% 
  mutate(win = success > 1 ) %>% 
  summarise(n_ej = n(),
            winrate_ej = sum(win,na.rm = T)/n_ej,
            winrate_ej = winrate_ej %>% round(2)) %>% 
  arrange(-n_ej)  %>% 
  filter(n_ej > 2)

 winrates_ej %>% 
  #filter(org_type %in% orgs) %>% 
  kablebox()
 
winrates %<>% left_join(winrates_ej)


winrates %<>% 
  rename(`Organization Type` = org_type,
         N = n, 
         `N raising EJ` = n_ej,
         `Overall Success Rate` = winrate,
         `EJ Success Rate` = winrate_ej)

winrates %<>% 
  filter(`Organization Type` %in% orgs) %>% 
  mutate(`Organization Type` = `Organization Type` %>% 
           str_replace("corp group", "Trade Association") %>% 
           str_replace("corp", "Business") %>% 
           str_replace("professional", "Professional Assn.") %>% 
           str_to_title() %>% 
           str_replace("Ngo", "NGO") %>% 
           str_replace("Ej", "Frontline EJ") )

winrates %>% 
  dplyr::select(-`N raising EJ`) %>%
  kablebox()

save(winrates, file = here::here("data", "winrate.Rdata"))

comments_coded$docket_id %>% unique()



comments_coded_ej %>% 
  #filter(!is.na(success)) %>% 
  dplyr::select(document_id, text = summary) %>% 
  write_csv("ej.csv")
