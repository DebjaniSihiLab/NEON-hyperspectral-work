library(tidyverse)

#setwd to home, then check which pc.
setwd("~")
whichpc=getwd()
cat(whichpc)
if(whichpc=="/Users/zwang61"){
  user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
}else{
  user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
}

cat(user_wd)
wd = paste0(user_wd,"/NEON-AOP/data/hs_lai/")
setwd(wd)
getwd()

neon_hs_table <- read_csv("temporary_neon_hs_table.csv")
soc_df <- read_csv("NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC.csv")
soc_df$plotID=1:nrow(soc_df)

final_df <- left_join(neon_hs_table,soc_df,by="plotID") %>% 
  rename(OC.percent=`wtmean.estimatedOC.percent`)  

sitesobs <- final_df %>% group_by(siteID,plotID) %>% 
  summarise(n=n()) %>% 
  group_by(siteID) %>% 
  summarise(n=n())

p<-ggplot(data=sitesobs, aes(x=siteID, y=n)) +
  geom_bar(stat="identity")+
  ylab("number of obervations each site")+ 
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
p

ggsave(file="RFigures/number of obervations each site.png",
       width=8,height=3,dpi=400)  


domainobs <- final_df %>% group_by(domainID,siteID,plotID) %>% 
  summarise(n=n()) %>% 
  group_by(domainID) %>% 
  summarise(n=n())

p<-ggplot(data=domainobs, aes(x=domainID, y=n)) +
  geom_bar(stat="identity")+
  ylab("number of obervations each domain")+ 
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
p
ggsave(file="RFigures/number of obervations each domain.png",
       width=8,height=3,dpi=400)  

max(final_df$OC.percent)
min(final_df$OC.percent)

breaks=seq(min(final_df$OC.percent), max(final_df$OC.percent), length.out = 50)
scaleFUN <- function(x) sprintf("%.1f", x)

p<-ggplot(data=final_df, aes(x=OC.percent)) +
  geom_histogram( bins = 60)+
  scale_x_continuous(breaks = breaks,labels=scaleFUN)+ 
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
  
p
ggsave(file="RFigures/OC distribution.png",
       width=10,height=3,dpi=400)  

hist(final_df$OC.percent)
min(final_df$OC.percent)
max(final_df$OC.percent)

