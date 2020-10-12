source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")


############################################################################################################
# script reads in annual pollution monitor readings for each state (CA, OR, WA) then 
#       (1) calculates 5 year average PM for each doy 2015-2019
#       (2) calculates difference between 2020 daily PM readings and the 2015-2019 average for that doy
#       (3) scales differences from (2) by mortality impact estimate to get PM attributable deaths per million for 65+
#       (4) scales estimate in (3) by 65+ population in each location to get excess deaths
###############################################################################################################

    
    pmca <- pmor <- pmwa <- list() #list for reading in annual files for each state
    yr <- 2015:2020
    
    #not positive every state has same format so load and check separately [update: all formated same]
    
      #CA    
      for (y in 1:length(yr)){  pmca[[y]] <- read_csv(paste("data/fig2/CApm",yr[y],".csv",sep=""))  }
      pmca <- data.frame(data.table::rbindlist(pmca))

      #WA    
      for (y in 1:length(yr)){  pmwa[[y]] <- read_csv(paste("data/fig2/WApm",yr[y],".csv",sep=""))  }
      pmwa <- data.frame(data.table::rbindlist(pmwa))

      #OR    
      for (y in 1:length(yr)){  pmor[[y]] <- read_csv(paste("data/fig2/ORpm",yr[y],".csv",sep=""))  }
      pmor <- data.frame(data.table::rbindlist(pmor))
      
      #double check all files are formatted the same:
      all.equal(names(pmca), names(pmwa))
      all.equal(names(pmca), names(pmor))
      pmwf <- rbind(pmca, pmwa, pmor) #all the same so combine
      
      #easier names
      names(pmwf) <- c("date","source","siteID","POC","pm25","units","aqi","site_name","daily_obs_count","perc_comp","aqs_parameter_code","aqs_parameter_desc","cbsa_code","cbsa_name","state_code","state","county_code","county","lat","lon")
      
      
      #For each CBSA by day-of-year calculate the difference between 2020 PM and average PM over the past 5 years 
      pm_doy <- pmwf %>% 
              mutate(
                year = as.numeric(format(as.Date(date, format = "%m/%d/%Y"), "%Y")),
                dom =  as.numeric(format(as.Date(date, format = "%m/%d/%Y"), "%d")),
                month =  as.numeric(format(as.Date(date, format = "%m/%d/%Y"), "%m")),
                doy = as.numeric(format(as.Date(date, format = "%m/%d/%Y"), "%j")),
                year = replace(year, year==20,2020), #CA 2020 date format is slightly diff so gets coded as 0020
                period = "pm25_pre", period = replace(period, year == "2020", "pm25_post" ) #divided into groups 2015-2019 vs 2020
                      ) %>% 
              dplyr::filter(doy %in% 214:259 & !is.na(cbsa_name)) %>% #restrict to period of 2020 wildfires thus far
              group_by(cbsa_name, period,  doy) %>% #collapse to pre (2015-2019) and post (2020) periods by CBSA 
              summarise(pm25 = mean(pm25, na.rm = T), .groups = 'drop') %>% 
              pivot_wider(names_from = period, values_from = pm25) %>% 
              mutate(pm25_diff = pm25_post - pm25_pre)
      
      
      
      #figure out second axis
      pm_doy <- dplyr::filter(pm_doy,cbsa_name %in%  c("Seattle-Tacoma-Bellevue, WA","Portland-Vancouver-Hillsboro, OR-WA","Sacramento--Roseville--Arden-Arcade, CA","San Francisco-Oakland-Hayward, CA","Los Angeles-Long Beach-Anaheim, CA"))
      pm_doy <- pm_doy %>% mutate(excess_mort_rate = pm25_diff*0.68) #pm attributable deaths per million
      
      
      #pull out pop by age for each CBSA
      pop <- read_csv("data/fig2/ACSST1Y2019.S0101_cbsa_pop_by_age_group.csv")
      popvars <- pop[1,]
      pop <- pop[2:nrow(pop),]
      pop <- pop[,c(2,which(popvars == "Estimate!!Total!!Total population!!SELECTED AGE CATEGORIES!!65 years and over"))]
      pop <- as.data.frame(pop) %>% rename(pop = S0101_C01_030E) %>% rename(cbsa_name = NAME)
      pop$cbsa_name <- sort(c("Seattle-Tacoma-Bellevue, WA","Portland-Vancouver-Hillsboro, OR-WA","Sacramento--Roseville--Arden-Arcade, CA","San Francisco-Oakland-Hayward, CA","Los Angeles-Long Beach-Anaheim, CA"))
      pop$pop <- as.numeric(pop$pop)
      pop$pop <- pop$pop/1e6 #convert to millions of people since rates per million
      
      pm_doy <- left_join(pm_doy, pop) %>%  #merge back in
                mutate(excess_mort = excess_mort_rate*pop) #scale rate by 65+ population
      
      
      
      #write out data for plotting
      write_rds(pm_doy, path = "data/fig2/fig2-plot-data-clean.rds")
      write_csv(pm_doy, path = "data/fig2/fig2-plot-data-clean.csv")
      
      
      
      
      ########################### Plot #########################################################
      pdf(file = "figures/raw/fig1-raw.pdf", width = 10, height = 12)                        
      
      par(mfrow = c(5,1))
      par(mar = c(1,4,2,4))
      par(oma = c(2,0,0,0))
      
      for(LOC in c("Seattle-Tacoma-Bellevue, WA","Portland-Vancouver-Hillsboro, OR-WA","Sacramento--Roseville--Arden-Arcade, CA","San Francisco-Oakland-Hayward, CA","Los Angeles-Long Beach-Anaheim, CA")){
        
        #x-axis is doy, y-axis on left is difference in pm2.5 between 2020 and past 5 yr ave
        plot(pm_doy$doy[pm_doy$cbsa_name==LOC], pm_doy$pm25_diff[pm_doy$cbsa_name==LOC], axes = F, xlab = "", ylab = "",type = "l",lwd =2, col = NA, ylim = c(-25,340))
        polygon(c(pm_doy$doy[pm_doy$cbsa_name==LOC],pm_doy$doy[pm_doy$cbsa_name==LOC][length(pm_doy$doy[pm_doy$cbsa_name==LOC])], pm_doy$doy[pm_doy$cbsa_name==LOC][1]), c(pm_doy$pm25_diff[pm_doy$cbsa_name==LOC], 0, 0), col = add.alpha('red3', 1), lwd = 0.25)
        
        #lines, y-axis on left and city label
        abline(h = 0)
        axis(2, tick = T, las =2)
        mtext(side = 3, adj = 0, text = LOC,cex=1)
        
        #every panel gets its own second y-axis because excess deaths calculation depends on population which varies by city
        #also because we just want to add right y-axis to values already plotted we can pick mortality y-axis values we want to show then scale them
        #to be in PM units shown on left y-axis 
        if (LOC =="Seattle-Tacoma-Bellevue, WA" ){axis(4, at = seq(0,150,25)/(.68*pop$pop[pop$cbsa_name==LOC]), labels = seq(0,150,25),las=2)}        
        if (LOC =="Portland-Vancouver-Hillsboro, OR-WA" ){axis(4, at = seq(0,120,20)/(.68*pop$pop[pop$cbsa_name==LOC]), labels = seq(0,120,20),las=2)}        
        if (LOC =="Sacramento--Roseville--Arden-Arcade, CA" ){axis(4, at = seq(0,75,15)/(.68*pop$pop[pop$cbsa_name==LOC]), labels = seq(0,75,15),las=2)}        
        if (LOC =="San Francisco-Oakland-Hayward, CA" ){axis(4, at = seq(0,300,50)/(.68*pop$pop[pop$cbsa_name==LOC]), labels = seq(0,300,50),las=2)}        
        if (LOC =="Los Angeles-Long Beach-Anaheim, CA" ){axis(4, at = seq(0,500,100)/(.68*pop$pop[pop$cbsa_name==LOC]), labels = seq(0,500,100),las=2)}        
        
        #label the x-axis on the bottom panel only
        if(LOC == "Los Angeles-Long Beach-Anaheim, CA"){        axis(1, at = c(214,228,245,259), labels = c("Aug 1","Aug 15","Sep 1","Sep 15"))}

        #include total excess pm and total excess deaths as text
        text(y = 325, x = 220, labels = paste("total ug increase = ",round(sum(pm_doy$pm25_diff[pm_doy$cbsa_name==LOC])), sep = ""))
        text(y = 300, x = 220, labels = paste("total excess deaths = ",round((.68*pop$pop[pop$cbsa_name==LOC])*sum(pm_doy$pm25_diff[pm_doy$cbsa_name==LOC])), sep = ""))
        
      }
      
      dev.off()
      
        
