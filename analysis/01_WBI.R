wbc <- wbstats::wb_countries() 
iso3cs <- wbc %>% mutate(continent = countrycode::countrycode(iso3c, "iso3c", "continent")) %>% 
  filter(continent == "Africa") %>% pull(iso3c)

dat <- wbstats::wb_data(
  country = iso3cs, 
  indicator = c("SP.DYN.LE00.IN","NY.GDP.PCAP.CD","EG.ELC.ACCS.ZS", "IT.NET.USER.ZS",
                "SH.STA.MMRT", "SP.ADO.TFRT", "SE.SEC.CMPT.LO.ZS", "SH.H2O.BASW.ZS"), 
  start_date = 2000, end_date = 2022)

dat %>% pivot_longer(EG.ELC.ACCS.ZS:SP.DYN.LE00.IN) %>% 
  group_by(name, iso3c) %>% 
  mutate(value = value/max(value)) %>% 
  ggplot(aes(as.integer(as.character(date)), value, color = name)) +
  geom_line() +
  facet_wrap(~iso3c)
