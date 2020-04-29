## How many templates coincide at the same place?

## threshold of within how many seconds?

templateCoincide <- function(x, round = 0, temps, 
                             subset = T, thresh_score = 0.4, thresh_temp = 0, keepMax = F,
                             save.label = T, dir, w){
  
  
  # library(dplyr)
  library(magrittr)
  
  spp_temps <- as.data.frame.table(table(monitoR::templateComment(temps)))
  colnames(spp_temps) <- c("species", "totTemps")
  
  # x is getdetections object, or data frame with getdetections object, and id (filename) as extra column and or species
  # round tolerance (in seconds) that hits will be treated as single event
  # only results will be returned where count of templates exceeds or is equal to threshold 
  # w is character vector to append to filenames if saving labels
  # dir is where to save labels
  
  if(!"id" %in% colnames(x)) x$id <- "one"
  
  # if no threshold, then use 60% of number of templates
  # if(missing(threshold)) threshold <- ceiling(length(unique(x$template)) * 0.6)
  
  # round time to nearest round... 
  if(round == 0) x$round.time <- round(x$time, 0) else x$round.time <- x$time - (x$time %% round)
  # head(x)
  
  
  # for each hit time, get names of templates
  #z <- split(x, list(x$id, x$round.time, x$species), drop = T)
  # names(z)[1]
  # z[[1]]
  # z["Apalharpactes reinwardti_16ix2018_915_612.wav.0.JavCro1"]
  # z["Pomatorhinus montanus_3viii1981_xxx_5B_41.wav.3.CheBacSci1"]
  # When rounding time, more than one hit can happen with a single template, see above example
  # therefore, the total templates possible for that round time is more than the just the total number
  # of templates for a species.. 
  # eg above. there are two hits per single tempalte within the 3 second round time.
  # Could add a correction favctor to totTemplates that is dependent on roundtime.. and length of template..
  # Below, group by time and template to get max of templates at same time, then group by again
  
  # tmp <-  x %>%
  #   
  #   dplyr::group_by(id, round.time, species) %>%
  #   dplyr::summarise(max = max(score), 
  #                    n = sum(score > thresh_score)) %>%
  #   as.data.frame()
  
  tmp <-  x %>%
    
    dplyr::group_by(id, round.time, species, template) %>%
    dplyr::summarise(maxSC = max(score)) %>% # gets max score per template, if more than one
    dplyr::group_by(id, round.time, species) %>%
    dplyr::summarise(max = max(maxSC), 
                     n = sum(maxSC > thresh_score)) %>%
    as.data.frame()
  
  # n is the number of templates scoring above the threshold at particular time per species
  
  # head(tmp)
  # merge with number of templates
  res <- merge(spp_temps, tmp, all.x = T, sort = F)
  res$pcTemp <- res$n/res$totTemps
  #head(res)
  
  ## Identify max of scores at same round time
  
  tmp2 <- res %>%
    
    dplyr::group_by(id, round.time) %>%
    dplyr::summarise(max = max(max))
  
  tmp2$maxR <- T
  # head(tmp2)
  res <- merge(res, tmp2, by = c("id", "round.time", "max"), all.x = T)
  #head(res)
  
  # subset so that results only include those where the threshold number of templates (of the total) match
  if(subset){
    res <- subset(res, pcTemp > thresh_temp)
    
    if(keepMax) res <- subset(res, maxR)
  }
  
  
  # re order by id, round time, species
  res <- res[order(res$id, res$round.time, res$species),]
  res <- res[,c("id", "round.time", "species", "n", "max", "pcTemp")]
  
  if(save.label) {
    
    tmp2 <- split(res, res$id)
    
    if(missing(w)) fn <- "_labels" else fn <- paste0("_", gsub(" ", "_", w))
    if(missing(dir)) dir <- getwd()
    
    lapply(tmp2, function(x) write.table(cbind(x[,c("round.time", "round.time","species")]),
                                         row.names = F, col.names = F, sep = "\t",
                                         file = file.path(dir, paste0(sub("\\.[[:alpha:]]{3}", "", basename(unique(x[,"id"]))), fn, ".txt"))))
    
    
  }
  
  res
}