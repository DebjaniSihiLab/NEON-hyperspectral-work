library(tidyverse)
dir="/Users/zhuonanwang/local_folder/CADES_NEON_hs/cloud"
setwd(dir)
outfiles=list.files( pattern = ".csv$",full.names = TRUE)

data <- outfiles %>%
  map(read_csv) %>% 
  bind_rows()

data.df <- data %>%
  mutate(cloud_indx = case_when( grepl("Green",cloud)  ~ "Green",
                          grepl("Yellow",cloud)  ~ "Yellow",
                          grepl("Red",cloud)  ~ "Red",
                          TRUE ~ "Unknown"
                          )
         ) %>% 
  mutate(site= substr(names,10,13))
  
    
df=data.df %>% 
  group_by(site) %>% 
  mutate(totallines=n()) %>% 
  group_by(site,cloud_indx) %>% 
  summarise(n=n(),
            totallines=unique(totallines)) %>% 
  mutate(cloud_indx=factor(cloud_indx,levels=c("Red", "Yellow","Green", "Unknown")))
 
 
fig <- ggplot(data=df, aes(x=site, y=n, fill=cloud_indx,order=cloud_indx)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom")+
  scale_fill_manual(breaks = c("Yellow","Green","Red",  "Unknown"),
                    values=c("#E7B800","#00BA38","red",  "black"),
                    labels=c("Yellow (10 - 50%) cloud cover", "Green (<10%) cloud cover" ,
                             "Red (> 50%) cloud cover","Unknown cloud cover conditions"))

ggsave("flights_cloud.png", fig,width = 10, height = 4,dpi = 300)
