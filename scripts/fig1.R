source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")

# script written by @marshallburke and @amd112 for NBER paper 02/2020. (https://www.nber.org/papers/w27423?utm_campaign=ntwh&utm_medium=email&utm_source=ntwg25)
# data updated through 2020 and figures reworked by @sheftneal for policy brief 9/2020.

##########################################################################################################################################################################
## Read in data
##########################################################################################################################################################################

#(top left) burned area (2020 numbers recorded as of 9/15 and scaled to estimate year-end totals)

    #National
      #Data source for national burned area 1980s - 2019: https://www.nifc.gov/fireInfo/fireInfo_stats_totalFires.html
      #Data source for updating to 2020: https://www.nifc.gov/fireInfo/nfn.htm 
      #historical data suggest about 85% of year-end burned area occurs by 9/15
        
        ba <- read_csv("data/fig1/inputs/burned_area_national.csv") %>% rename(ba_acres = burned_area) %>% 
              mutate(ba_acres = replace(ba_acres, year==2020, ba_acres[year==2020]/.85), ba_ha = (ba_acres*0.404686)/1e6) #scale to year end totals and convert to millions of ha
    
    
    #California
      #Data source for california burned area 1980s - 2018: https://www.fire.ca.gov/media/11397/fires-acres-all-agencies-thru-2018.pdf
      #Data source for updating 2019 and 2020: https://www.fire.ca.gov/incidents/2019/  and https://www.fire.ca.gov/incidents/2020/
        
    caba <- read_csv("data/fig1/inputs/burned_area_california.csv") %>% dplyr::select(year, ba_acres) %>% dplyr::filter(!is.na(year)) 
    caba_2020 <- data.frame(year = 2019:2020, ba_acres = c(259823, 3371624)/.85) #add in 2019 and 2020 to date numbers
    caba <- caba %>% rbind(caba_2020) %>% mutate(ba_ha = (ba_acres*0.404686)/1e6) #convert to millions of ha

#(bottom left) PM2.5 trends
    
    #Data source: https://www.epa.gov/air-trends/particulate-matter-pm25-trends
    #State level data for 2020 through 9/15 source: https://www.epa.gov/outdoor-air-quality-data/download-daily-data
    #2020 state level averages were calculated and merged with EPA regions to match the trends data. 
    #Historically 9/15 PM2.5 averages 95% of year-end totals so 2020 values scaled accordingly
    
        f <- list.files('data/fig1/inputs/pm25_regional/')
        epa <- data.frame(Year=2000:2019)
        
        for (fl in f) {
          regionname <- unlist(strsplit(unlist(strsplit(fl,".csv")),"PM25"))[2]
          baa <- read_csv(paste0('data/fig1/inputs/pm25_regional/',fl)) 
          baa <- baa[,1:2]
          names(baa)[2] <- unlist(strsplit(unlist(strsplit(fl,".csv")),"PM25"))[2]
          epa <- left_join(epa,baa)
        }
        epa <- epa %>% rename(year = Year)
        
        epa2020 <- read_csv("data/fig1/inputs/pm25_regional_2020.csv")
        
        epa2020 <- epa2020[,names(epa)] #reorg columsn to match
        epa[epa$year==2020,2:ncol(epa)]<-epa[epa$year==2020,2:ncol(epa)]/.95 #scale 2020 values as of 9/15 to approximate year end averages
        
        pm_regional <- epa[,names(epa)!='National']
        pm_nat <- epa[,c("year","National")]
        
#(top right) smoke days per year
        
        #Data source: https://www.ospo.noaa.gov/Products/land/hms.html
        #for details on how smoke polygons were aggregated to pop weighted EPA region averages, see Supplement from our NBER paper: https://www.nber.org/papers/w27423?utm_campaign=ntwh&utm_medium=email&utm_source=ntwg25 
        #2020 values were calculated through 9/15 and then scaled up according to % of smoke days that have occurred historically by 9/15 in each respective region
        
        smoke_days_regional <- read_csv("data/fig1/inputs/smoke_days_regional.csv")
        smoke_days_nat <- read_csv("data/fig1/inputs/smoke_days_national.csv")
        
#(bottom right)  Model predicted PM2.5 from wildfire smoke (through 2018) 
      
      #Data source: https://www.nber.org/papers/w27423?utm_campaign=ntwh&utm_medium=email&utm_source=ntwg25
      
        pmsmoke_regional <- read_csv("data/fig1/inputs/pm25_from_smoke_regional.csv")
        pmsmoke_nat <- read_csv("data/fig1/inputs/pm25_from_smoke_national.csv")

##########################################################################################################################################################################
# Make the plot
##########################################################################################################################################################################
          
    
    pdf(file="figures/raw/fig1-raw.pdf",width=8,height=8,useDingbats = F)
    par(mar=c(3,4,0,1),mfcol=c(2,2))
            
    ######################################################################################
    #top left - burned area
            
    #note:  for version appearing in policy brief trend was fit to data before 2020 values as of 9/15 was scaled to get preliminary year end estimate.
    #       fitting trend after scaling 2020 values gives slightly steeper slope of 0.07 million ha/yr 

    #National
        
        #Plot observed data through 2019
         plot(ba[(ba$year<2020),c("year","ba_ha")],type="l",col="grey",ylab="",axes=F,ylim=c(0,4.5),xlab="", xlim = c(1985,2020))
        #extend to 2020 with dotted line to indicate estimates not final
          segments(x0 = 2019, x1 = 2020, y0 = ba$ba_ha[ba$year==2019],y1 =ba$ba_ha[ba$year==2020],lty=2, col = 'gray' )
          points(2020, ba$ba_ha[ba$year==2020], pch = 21, bg = 'white', col = 'gray',cex=1.5)
          
        #fit trend and plot it over time series  
          modtrend <- lm(ba_ha ~ year, data = ba)
          predtrend <- predict(modtrend)
          lines(ba$year, predtrend, col = 'red')
          
        #plot text, labels, axes
          mtext("Burned area", side=3,line=-1,adj=0,cex=0.8)
          cf <- coef(modtrend)["year"] %>% round(2) %>% as.numeric()
          mtext(paste0("trend = ",cf, "million hectares/year"), side=3,line=-2,adj=0,cex=0.7)
          
          title(ylab="million hectares", line=2.2, cex.lab=1.2)
          
          axis(1,at=seq(1985,2020,5))
          axis(2,las=1)
    
    #California
      
          lines(caba$year[caba$year<2020], caba$ba_ha[caba$year<2020],col ='gray20')
          segments(x0 = 2019, x1 = 2020, y0 = caba$ba_ha[caba$year==2019],y1 = caba$ba_ha[caba$year==2020],col ='gray20',lty=2)
          points(2020, caba$ba_ha[caba$year==2020], pch = 21, bg = 'white', col = 'gray20',cex=1.5)
          lines(x = 1987:2020,y = predict(lm(ba_ha ~ year, data = caba), newdata = data.frame(year = 1987:2020)), col = 'red3')
            
          
    ######################################################################################
    #bottom left - PM2.5
          
          #initialize plot
          plot(1,type="n",xlim=c(2000,2020),ylim=c(4, 18),las=1,ylab="",axes=F,xlab="")
          
          
          #loop over regions and plot time series for each through 2019
          
          for (i in 2:dim(pm_regional)[2]) {
            nm <- names(pm_regional)[i]
            if (nm == "National") {next} #don't plot time-series for national averages
            if (nm %in% c('West','Northwest')) { #W and NW regions
              col="grey20"
              text(2017,pm_regional[pm_regional$year==2018,i],nm,cex=0.4)
              lines(pm_regional$year,pm_regional[,i],col=col)
            } else { #other regions
              col="grey"
              lines(pm_regional$year,pm_regional[,i],col=col)
            }
          }
          
          #extend plot from 2019 to 2020 with dotted lines and points
          for (i in 2:dim(pm_regional)[2]) {
            nm <- names(pm_regional)[i]
            if (nm == "National") {next} #don't plot time-series for national averages            
            #Highlight W and NW regions
            if (nm %in% c('West','Northwest')) {
              col="grey20"
              
              segments(x0 = 2019, x1 = 2020, y0 = pm_regional[pm_regional$year==2019,i], y1 =as.numeric(epa2020[nm]),col = col, lty=1)
              points(2020, as.numeric(epa2020[nm]) , pch =21, bg='white', col = col,cex=1.5)
              
            } else { #all other regions gray
              col="grey"
              segments(x0 = 2019, x1 = 2020, y0 = pm_regional[pm_regional$year==2019,i], y1 =as.numeric(epa2020[nm]),col = col, lty=1)
              points(2020, as.numeric(epa2020[nm]) , pch =21, bg='white', col = col,cex=1.5)
            }
          }
          
          
          
          
          
          
          #add the red line for average
          modtrend <- lm(National ~ year, data = pm_nat)
          predtrend <- predict(modtrend)
          cf <- coef(modtrend)[2] %>% round(2) %>% as.numeric()
          
          
          lines(pm_nat$year,predtrend,col="red3")
          
          
          
          title(ylab="PM2.5 (ug/m3)", line=2.2, cex.lab=1.2)
          
          mtext("PM2.5", side=3,line=-1,adj=0,cex=0.8)
          mtext(paste0("trend = ",cf," ug/m3/year"), side=3,line=-2,adj=0,cex=0.7)
          
          axis(1,at=seq(2000,2020,5))
          axis(2,las=1)
          
          
          
          
    #####################################################################################
    # top right - smoke days 
            
          #initialize plot
          plot(1,type="n",xlim=c(2000,2020),ylim=c(0, 100),las=1, ylab="",axes=F,xlab="")
          
          #loop over regions and plot time series for each through 2019
          for (i in 1:10) {
            f = smoke_days_regional[smoke_days_regional$epa_region == i & smoke_days_regional$year<2020, ]
            #Highlight W and NW regions
            if (i %in% 9:10) { 
              if (i == 10) {nm="Northwest"}
              if (i == 9) {nm = "West"}
              col="grey20"
              text(2017,f[f$year==2018,"smoke_days"],nm,cex=0.4)
              lines(f$year,f$smoke_days,col=col)
            } else { #all other regions gray
              col="grey"
              lines(f$year,f$smoke_days,col=col)
            }
          }
          
          #extend plot from 2019 to 2020 with dotted lines and points
          for (i in 1:10) {
            f = smoke_days_regional[smoke_days_regional$epa_region == i & smoke_days_regional$year>=2019, ]
            #Highlight W and NW regions
            if (i %in% 9:10) {
              if (i == 10) {nm="Northwest"}
              if (i == 9) {nm = "West"}
              col="grey20"
              
              segments(x0 = 2019, x1 = 2020, y0 = f$smoke_days[1], y1 = predict(lm(smoke_days~year, data = f[f$year>2018,]), newdata = data.frame(year = 2020)) ,col = col, lty=2)
              points(2020, predict(lm(smoke_days~year, data = f[f$year>2018,]), newdata = data.frame(year = 2020)) , pch =21, bg='white', col = 'gray20',cex=1.5)
              
            } else { #all other regions gray
              col="grey"
              segments(x0 = 2019, x1 = 2020, y0 = f$smoke_days[1], y1 = predict(lm(smoke_days~year, data = f[f$year>2018,]), newdata = data.frame(year = 2020)) ,col = col, lty=2)
              points(2020, predict(lm(smoke_days~year, data = f[f$year>2018,]), newdata = data.frame(year = 2020)) , pch =21, bg='white', col = 'gray',cex=1.5)
            }
          }
          
          
          
        # Add trend line          
          
          modtrend <- lm(smoke_days ~ year, data = smoke_days_nat)
          predtrend <- predict(modtrend)
          lines(smoke_days_nat$year, predtrend, col = 'red3')
          
          
        #Add text and axes 
        
         title(ylab="smoke days", line=2.2, cex.lab=1.2)
          
          mtext("Smoke days per year", side=3,line=-1,adj=0,cex=0.8)
          mtext(paste0("trend = ",round(coef(modtrend)[2],2)," more days/year"), side=3,line=-2,adj=0,cex=0.7)
          
          axis(1,at=seq(2000,2020,5))
          axis(2,las=1)
          
          
   
            
            
            
    ######################################################################################
    #bottom right - predicted PM2.5 from smoke
            
          #initialize plot
          plot(1,type="n",xlim=c(2000,2020),ylim=c(0,60),las=1, ylab="",axes=F,xlab="")

          #loop over epa regions and plot time series for each
              for (i in 1:10) {
              f = pmsmoke_regional[pmsmoke_regional$epa_region == i, ]
              if (i %in% 9:10) {
                if (i == 10) {nm="Northwest"}
                if (i == 9) {nm = "West"}
                col="grey20"
                text(2017,f[f$year==2018,"perc"]*100,nm,cex=0.4)
                lines(f$year,f$perc*100,col=col)
              } else {
                col="grey"
                lines(f$year,f$perc*100,col=col)
              }
            }
            
           
            #plot trend on top
            modtrend <- lm(perc*100 ~ year, data = pmsmoke_nat)
            predtrend <- predict(modtrend)            
            lines(pmsmoke_nat$year,predtrend,col="red3")
            
            
            #add axes and labels
            mtext("Predicted % of PM2.5 from wildfire smoke", side=3,line=-1,adj=0,cex=0.8)
            cf <- coef(modtrend)[2] %>% round(2) %>% as.numeric()
            mtext(paste0("trend = ",cf," percentage points/year"), side=3,line=-2,adj=0,cex=0.7)
            
            title(ylab="% PM2.5 from smoke", line=2.2, cex.lab=1.2)
            axis(2,las=1)
            axis(1,at=seq(2000,2020,5))
            
    dev.off() 
    
    
########################################################################################################################        
# write out clean version of all plot data
# requires some reorganization and reshaping and joining to cajole each panel's plot data into a single object 
########################################################################################################################              
  
  #top left:
      
    plotdata1 <-  ba %>% dplyr::select(year, burned_area_nat = ba_ha) %>% left_join(caba) %>% dplyr::select(-ba_acres) %>% rename(burned_area_ca = ba_ha)
    
    write_csv(plotdata1, path = "data/fig1/clean_fig_data/fig1-burned-area-clean.csv")        
    
  #bottom left:
      
    plotdata2 <-   pm_regional %>% pivot_longer(cols = Central:West, names_to = "region_name", values_to = "pm25")
    pld2 <- pm_nat %>% mutate(region_name = "National") %>% rename(pm25= National)   %>% dplyr::select(year, region_name, pm25)
    plotdata2 <- plotdata2 %>% rbind(pld2)
    
    df<-left_join(pm_regional, pm_nat) 
    mod <-lm(National ~ Central + Northeast + NorthernRockies + Northwest + South + Southeast + Southwest + UpperMidwest + West, data = df)
    pld3 <- data.frame(year = 2020, region_name = names(epa2020)[2:ncol(epa2020)], pm25 = as.numeric(epa2020)[2:ncol(epa2020)])
    pld3 <- rbind(pld3, data.frame(year = 2020, region_name = "National",  pm25 = round(predict(mod, newdata = epa2020),10)))
        
    plotdata2 <- plotdata2 %>% rbind(pld3) %>% mutate(pm25 = round(pm25, 4)) %>% arrange(region_name, year)
    
    write_csv(plotdata2, path = "data/fig1/clean_fig_data/fig1-total-pm25-clean.csv")        
    
  #top right:
    
    plotdata3a <- smoke_days_nat %>% mutate(epa_region = 0) %>% dplyr::select(year, epa_region, smoke_days)
    plotdata3b <- smoke_days_regional %>%  dplyr::select(year, epa_region, smoke_days)
    plotdata3 <- rbind(plotdata3a, plotdata3b) %>% arrange(epa_region, year)

    write_csv(plotdata3, path = "data/fig1/clean_fig_data/fig1-smoke-days-clean.csv")        
    
    
  #bottom right
    
    plotdata4a <- pmsmoke_nat %>% mutate(epa_region = 0) %>% dplyr::select(year, epa_region, perc_pm_smoke = perc)
    plotdata4b <- pmsmoke_regional %>%  dplyr::select(year, epa_region, perc_pm_smoke = perc)
    plotdata4 <- rbind(plotdata4a, plotdata4b) %>% arrange(epa_region, year)
    
    
    write_csv(plotdata4, path = "data/fig1/clean_fig_data/fig1-pm-from-smoke-clean.csv")        
    
    
    
