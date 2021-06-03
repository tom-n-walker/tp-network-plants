#Table S2

load("~/Git/OTC_synthesis_analyses/Data/brms_output/fit_m7x_flow.Rdata")
load("~/Git/OTC_synthesis_analyses/Data/brms_output/fit_m7x_flowend.Rdata")
load("~/Git/OTC_synthesis_analyses/Data/brms_output/fit_m7x_green.Rdata")
load("~/Git/OTC_synthesis_analyses/Data/brms_output/fit_m7x_disp.Rdata")
load("~/Git/OTC_synthesis_analyses/Data/brms_output/fit_m7x_fruit.Rdata")
load("~/Git/OTC_synthesis_analyses/Data/brms_output/fit_m7x_sen.Rdata")

#m7 OTC x ambient temp (site T and site year delta T) 
postflow7<- as_tibble(fixef(fit_m7x_flow, summary=F))%>%
  dplyr::select(grep("treatmentOTC", colnames(.)), -treatmentOTC)%>%mutate(phen="Flower")
postflowend7<- as_tibble(fixef(fit_m7x_flowend, summary=F))%>%
  dplyr::select(grep("treatmentOTC", colnames(.)), -treatmentOTC)%>%mutate(phen="Flowerend")
postfruit7<- as_tibble(fixef(fit_m7x_fruit, summary=F))%>%
  dplyr::select(grep("treatmentOTC", colnames(.)), -treatmentOTC)%>%mutate(phen="Fruit")
postdisp7<- as_tibble(fixef(fit_m7x_disp, summary=F))%>%
  dplyr::select(grep("treatmentOTC", colnames(.)), -treatmentOTC)%>%mutate(phen="Disp")
postgreen7<- as_tibble(fixef(fit_m7x_green, summary=F))%>%
  dplyr::select(grep("treatmentOTC", colnames(.)), -treatmentOTC)%>%mutate(phen="Green")
postsen7<- as_tibble(fixef(fit_m7x_sen, summary=F))%>%
  dplyr::select(grep("treatmentOTC", colnames(.)), -treatmentOTC)%>%mutate(phen="Sen")

post<-as_tibble(rbind(postflow7, postflowend7, postfruit7, postgreen7, postdisp7, postsen7))%>%
  pivot_longer(cols = c("treatmentOTC:siteT", "treatmentOTC:siteyear_deltaT"),  names_to = "Parameter")

post7<-dplyr::group_by(post,Parameter, phen)%>%
  dplyr::summarise(Est = mean(value), Err=sd(value))

flow_eti <- ci(postflow7, method = "ETI", ci= c(0.9, 0.95))%>%mutate(phen='Flower')
flowend_eti <- ci(postflowend7, method = "ETI", ci =c(0.9, 0.95))%>%mutate(phen='Flowerend')
fruit_eti <- ci(postfruit7, method = "ETI", ci =c(0.9, 0.95))%>%mutate(phen='Fruit')
disp_eti <- ci(postdisp7, method = "ETI", ci =c(0.9, 0.95))%>%mutate(phen='Disp')
green_eti <- ci(postgreen7, method = "ETI", ci = c(0.9, 0.95))%>%mutate(phen='Green')
sen_eti <- ci(postsen7, method = "ETI", ci = c(0.9, 0.95))%>%mutate(phen='Sen')
eti<-rbind(flow_eti, flowend_eti, fruit_eti, disp_eti, green_eti, sen_eti)
eti7<-pivot_wider(eti, names_from = "CI", values_from = c("CI_low", "CI_high"))
eti7<-left_join(post7, eti7)%>%dplyr::select("Parameter","phen","CI_low_90",
                                             "CI_low_95","CI_high_90", "CI_high_95", "Est","Err")%>%as.data.frame(.)

tableS2<-rbind(eti2, eti3, eti4,eti5, eti6, eti7, eti8)%>%
  dplyr::select("Parameter","phen","Est","Err", 
                "CI_low_90","CI_low_95","CI_high_90", "CI_high_95")