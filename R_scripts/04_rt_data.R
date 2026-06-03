# script to obtain the Rt data for a specific country

get_rt=function(rt_path){
  
Rt = read.csv(paste0(rt_path,"estimated_R.csv")) # Data taken from OWID
Rt %>%
  filter(Entity == aux_country) %>%
  rename(date=Day) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  select(Entity, date, Reproduction.rate) %>%
  rename(country=Entity,Rt = Reproduction.rate) -> rt 

rt$country=NULL

return(rt)

}
