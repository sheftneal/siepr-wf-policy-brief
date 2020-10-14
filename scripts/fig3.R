source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")


######################################################################################################
#read in data
######################################################################################################
                     

  #calculate share of wildfire burned area by jurisdiction
            
      baj_wf_share <- read_csv("data/fig3/inputs/wildfire_burned_area_by_jurisdiction.csv")  %>% 
                      dplyr::select(-year) %>% 
                      summarise_all(mean) %>% 
                      mutate(wf_total = wf_bia + wf_blm + wf_fs + wf_fws + wf_nps + wf_state,
                             share_bia = wf_bia/wf_total,
                             share_blm = wf_blm/wf_total,
                             share_fs = wf_fs/wf_total,
                             share_fws = wf_fws/wf_total,
                             share_nps = wf_nps/wf_total,
                             share_state = wf_state/wf_total
                      ) %>%
                      dplyr::select(starts_with("share")) %>% 
                      sort(decreasing = T)
      
      baj_wf_total <- read_csv("data/fig3/inputs/wildfire_burned_area_by_jurisdiction.csv")  %>% 
                      dplyr::select(-year) %>% 
                      summarise_all(mean) %>% 
                      unlist() %>% sum()
        
    
  col_top_left <- data.frame(col = c("#00A08A",add.alpha("#FF7F50", c(0.9,0.75, 0.5,0.25, 0.1 ))), group = names(baj_wf_share))   
      
      
  #calculate share of prescribed burned area by jurisdiction
    
      baj_rx_share <- read_csv("data/fig3/inputs/prescribed_burned_area_by_jurisdiction.csv")  %>% 
                      dplyr::select(-year) %>% 
                      summarise_all(mean) %>% 
                      mutate(rx_total = rx_bia + rx_blm + rx_fs + rx_fws + rx_nps + rx_state,
                             share_bia = rx_bia/rx_total,
                             share_blm = rx_blm/rx_total,
                             share_fs = rx_fs/rx_total,
                             share_fws = rx_fws/rx_total,
                             share_nps = rx_nps/rx_total,
                             share_state = rx_state/rx_total
                      ) %>%
                      dplyr::select(starts_with("share")) %>% 
                      sort(decreasing = T)
      
      baj_rx_total <- read_csv("data/fig3/inputs/prescribed_burned_area_by_jurisdiction.csv")  %>% 
                      dplyr::select(-year) %>% 
                      summarise_all(mean) %>% 
                      unlist() %>% sum()      
   
    col_top_right <- data.frame(group = names(baj_rx_share)) %>% left_join(col_top_left)  
      
  
  #calculate share of prescribed burned area by region
      
      barg_wf_share <-  read_csv("data/fig3/inputs/wildfire_burned_area_by_region.csv")  %>% 
                        mutate(wf = (ave_2010_2019 + ave_2000_2009 + ave_1990_1999)/3) %>%
                        dplyr::select(region, wf) 
      wf_total <- sum(barg_wf_share$wf)
      barg_wf_share <-  barg_wf_share %>% 
                        mutate(wf_share = wf/wf_total) %>% 
                        dplyr::select(-wf) %>% 
                        arrange(-wf_share)
      barg_wf_total <- read_csv("data/fig3/inputs/wildfire_burned_area_by_region.csv")  %>% 
                        mutate(wf = (ave_2010_2019 + ave_2000_2009 + ave_1990_1999)/3) %>% 
                        dplyr::select(wf) %>% sum()
      
      col_bottom_left <- data.frame(col = c(add.alpha("#000080", 0.5),"#D92120","#DE4128","#E46230","#E4B80E","#E8C520","#D1C74C","#9EBE91","#71B3C2","#2C4C8B"), group = barg_wf_share$region)
      
    #calculate share of prescribed burned area by region
      
      barg_rx_share <-  read_csv("data/fig3/inputs/prescribed_burned_area_by_region.csv")  %>% 
                        mutate(rx = (ave_2010_2019 + ave_2000_2009 + ave_1990_1999)/3) %>%
                        dplyr::select(region, rx) 
      rx_total <- sum(barg_rx_share$rx)
      barg_rx_share <-  barg_rx_share %>% 
                        mutate(rx_share = rx/rx_total) %>% 
                        dplyr::select(-rx) %>% 
                        arrange(-rx_share)
      
      barg_rx_total <- read_csv("data/fig3/inputs/prescribed_burned_area_by_region.csv")  %>% 
                       mutate(rx = (ave_2010_2019 + ave_2000_2009 + ave_1990_1999)/3) %>% 
                       dplyr::select(rx) %>% sum()
            
      col_bottom_right <- data.frame(group = barg_rx_share$region) %>% left_join(col_bottom_left)   
      
      
    #read in the shapefile of GACC regions and assign colors for the bottom panel legend
      
      gacc <- rgdal::readOGR("data/fig3/inputs/National-GACC-Boundaries/National_GACC_Current_20200226.shp")
        gacc@data$group <- c("AK","EA","GB","NO","NR","NW","RM","SA","SO","SW")
        gacc@data$plotOrder <- 1:nrow(gacc@data)
        gacc@data <- left_join(gacc@data, col_bottom_right) %>% arrange(plotOrder)
        
      
      
######################################################################################################
#Plot data
######################################################################################################

      
            
## plot pie chart
      
      
      pdf(file = "figures/raw/fig3-raw.pdf",width=10,height=8,useDingbats = F)
      
            par(bg = "white")
            par(mar = c(2,6,2,2))
            par(mfrow = c(2,2))
      
      
            
        #top left    
          
          doughnut(as.numeric(baj_wf_share)  , 
                   inner.radius=0,outer.radius =1, 
                   col= col_top_left$col , 
                   main ="Wildfire Burned Area", 
                   labels = paste(round(as.numeric(baj_wf_share)*100, 1),"%",sep=""),
                   border = 'white')
          mtext("Area by jurisdiction", side=2,line=4)
      
          
        #top right  
          
      
      
            doughnut(as.numeric(baj_rx_share)  , 
                     inner.radius=0,outer.radius = (baj_rx_total/baj_wf_total), 
                     col=col_top_right$col , 
                     main ="Prescribed Burned Area", 
                     labels = paste(round(as.numeric(baj_rx_share)*100, 1),"%",sep=""),
                     border = 'white')
            
      
            
          #bottom left  
      
          doughnut(as.numeric(barg_wf_share$wf_share) , 
                   inner.radius=0,outer.radius = 1, 
                   col=col_bottom_left$col, 
                   main ="", 
                   labels = paste(round(100*as.numeric(barg_wf_share$wf_share), 1),"%",sep=""),
                   border = 'white')
      
          mtext("Area by region", side = 2, line=4)
          
          #bottom right  
          
          doughnut(as.numeric(barg_rx_share$rx_share) , 
                   inner.radius=0,outer.radius = barg_rx_total/barg_wf_total, 
                   col=col_bottom_right$col , 
                   main ="", 
                   labels = paste(round(100*as.numeric(barg_rx_share$rx_share), 1),"%",sep=""),
                   border = 'white')
          
      
      
      dev.off()
  
  
      
#################################################
#################################################
            
    ## plot map insert for bottom panel legend
  
    pdf("figures/raw/fig3-bottom-legend-raw.pdf", width = 6, height = 3)
        par(mfrow = c(1,1))
        par(mar = c(0,0,0,0))
        plot(gacc, col = gacc@data$col, xlim = c(-160,-50), ylim = c(10,70), lwd = 0.5, border = 'white')
        text(x = coordinates(gacc)[,1], y = coordinates(gacc)[,2], labels = gacc@data$group, col = 'white', cex=0.5)
      dev.off()
      

#################################################
#################################################

  #write out the plot data   
      
      topleft <- data.frame(jurisdiction = unlist(lapply(strsplit(names(baj_wf_share),"_"),function(x){x[2]})),wf_share = as.numeric(baj_wf_share) )
      topright <- data.frame(jurisdiction = unlist(lapply(strsplit(names(baj_rx_share),"_"),function(x){x[2]})),rx_share = as.numeric(baj_rx_share) )
      top <- left_join(topleft, topright)

      bottom <- left_join(barg_wf_share, barg_rx_share)
      
      write_csv(top, path = "data/fig3/clean_fig_data/burned_area_by_jurisdiction.csv")
      write_csv(bottom, path = "data/fig3/clean_fig_data/burned_area_by_region.csv")
            
