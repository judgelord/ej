



# load coded comments 
load(here::here("data", "coalitions_coded.Rdata"))

# load 
load(here::here("data", "ej_data_clean.Rdata"))
load(here::here("data", "ejFRejPR.Rdata"))
load(here::here("data", "ejFR_PR.Rdata"))

ejFRejPRchanged <- ejFRejPR %>% filter(change)


ej_changed_dockets <- c(ejFRejPRchanged$docket_id, ejFR_PR$docket_id)

ej_framed_dockets <- intersect(ej_changed_dockets, ejcomments$docket_id)

ej_agencies <- unique( ejFR$agency_id)

save(ej_framed_dockets, ej_agencies, file = "data/ej_framed_dockets.Rdata") 


coalitions_coded %<>% 
  mutate(ej_framed = docket_id %in% ej_framed_dockets) %>% 
  mutate(win = ifelse(coalition_success > 0, "Win", "Loss"))


coalitions_coded %>% 
  filter(agency %in% ejFR$agency_id) %>% 
  drop_na(win) %>% #.$ej_framed
  ggplot() + 
  aes(x = coalition_type, fill = win) +
  geom_bar() + 
  facet_wrap("ej_framed")



# backwards 
look <- rules %>% 
  filter(docket_id %in% ejPR$docket_id, 
         !docket_id %in% ejFR$docket_id,
         document_type == "Rule")
