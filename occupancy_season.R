library(tidyverse)
library(reshape2)
library(data.table)
library(unmarked)
## occupancy analyses for bird abundance/range
###Requires tidyverse, reshape2, data.table and unmarked####
#### The following function takes four arguments; 
## 1. data, 2. Species name 3. season names as "Sum", "Win", "Mon". 4. Start value = , str.val = c(0.5,-3,0.5)
occufreqsea = function(data,species,season,str.val)
{
  require(tidyverse)
  require(reshape2)
  require(data.table)
  require(unmarked)
  
  selexp = expandbyspecies(data,species)
  eff = quantile(selexp$EFFORT.DISTANCE.KM, 0.975, na.rm=TRUE)
  
  lpg = selexp %>%
    group_by(gridg4) %>% summarize(lpg = n())
  listcutoff = quantile(lpg$lpg, 0.95, na.rm=TRUE)
  
  selexp = selexp %>%
    filter(EFFORT.DISTANCE.KM < eff)
  
  selexp = selexp[sample(1:nrow(selexp)),]
  
  selexp = selexp %>% 
    arrange(gridg4) %>%
    mutate(gridg = as.numeric(gridg4)) %>%
    group_by(gridg) %>% mutate(group.id = 1:n())
  
  selexp$month[selexp$month %in% c(11,12,1,2)] = "Win"
  selexp$month[selexp$month %in% c(5,6,7,8)] = "Sum"
  selexp$month[selexp$month %in% c(7,8,9,10)] = "Mon"
  
  if(season == "Sum"){
    selexp = selexp %>%
      filter(month == "Sum")
  }
  
  if(season == "Mon"){
    selexp = selexp %>%
      filter(month == "Mon")
  }
  
  if(season == "Win"){
    selexp = selexp %>%
      filter(month == "Win")
  }
  setDT(selexp)
  
  det = dcast(selexp, gridg ~ group.id, value.var = "OBSERVATION.COUNT")
  cov.month = dcast(selexp, gridg ~ group.id, value.var = "month")
  cov.nosp = dcast(selexp, gridg ~ group.id, value.var = "no.sp")
  
  det = setDF(det)
  cov.month = setDF(cov.month)
  cov.nosp = setDF(cov.nosp)
  
  det = det[,1:listcutoff]
  cov.month = cov.month[,1:listcutoff]
  cov.nosp = cov.nosp[,1:listcutoff]
  
  umf = unmarkedFrameOccu(y=det[,-1], siteCovs =NULL, obsCovs = list(cov1 = cov.nosp[,-1]))
  
  occ_det = occu(~cov1 ~1, data=umf, start = str.val)
  g = backTransform(occ_det, type="state")
  
  newdat = data.frame(cov1=20)
  f = predict(occ_det, newdata = newdat, type = "det")
  
  f = mean(f$Predicted)
  
  fg = data.frame(species)
  names(fg) = "species"
  fg$detection = f
  fg$occupancy = g@estimate
  print(occ_det)
  return(fg)
}

occufreqsea(data, "Indian Courser", "Win",str.val = c(0.5, -5, 0.1))

