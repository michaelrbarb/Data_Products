library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(stats)
library(utils)
library(devtools)
library(shinyapps)
library(shiny)
library(reshape)
library(varhandle)
library(ggplot2)
library(dplyr)
library(scales)

#**************************************************************************
#                         Read in data
#**************************************************************************

#***** Read in Passenger Miles Data*****************************************
passenger_miles<-read.csv("passenger_miles.csv",header=TRUE,sep=",")
colnames(passenger_miles)<-gsub("_", " ",  colnames(passenger_miles))
#yearinput<-renderPrint({input$year})

#***** Read in Auto Expense Data*******************************************
auto_cost<-read.csv("auto_cost.csv",header=TRUE,sep=",")
auto_cost<-select(
  auto_cost,year,total_cost_adj,gas_adj,maint_adj,cost_per_15k_adj,variable_adj,
  fixed_adj,CPI_1982_1984,Hyperloop)
colnames(auto_cost)<-gsub("_", " ",  colnames(auto_cost))

#***** Read in Passenger Fare Data*******************************************
transit_cost<-read.csv("transit_cost.csv",header=TRUE,sep=",")
colnames(transit_cost)<-gsub("_", " ",  colnames(transit_cost))
passenger_fare<-read.csv("passenger_fare.csv",header=TRUE,sep=",")
colnames(passenger_fare)<-gsub("_", " ",  colnames(passenger_fare))

#***** Read in Engery Intensity Data **************************************
energy_int<-read.csv("energy_int.csv",header=TRUE,sep=",")
colnames(energy_int)<-gsub("_", " ",  colnames(energy_int))




#***************************************************************************
#                      Shiny Server App      
#***************************************************************************

shinyServer(
  
#***************************************************************************
#                        Table Tab 
#**************************************************************************
  function(input,output,session){
    #output$omode_transportation<-renderPrint({input$mode_transportation})
    
    output$myTable<-renderDataTable({
      if (input$select=="auto_cost"){
        textinfo<-c("The checkbox options are not available for the Automobile Cost table. 
                     Cost is per mile traveled with the exception of the 'cost per 15k' category.   
                     Expenses are adjusted to 2013 USD.")
        output$odata_set<-renderText({textinfo})
        auto_cost<-auto_cost %>% 
          filter(year <= input$year[2] & year >= input$year[1])
        auto_cost<-select(auto_cost,year,matches("adj"))
        colnames(auto_cost)<-gsub("adj","",colnames(auto_cost))
        auto_cost
      }
      else if (is.null(input$mode_transportation)){
        textinfo<-c("")
        output$odata_set<-renderText({textinfo})
        #return(NULL)
        }
      
      else if(is.null(input$mode_transportation))
        return(NULL)
      
      else if (input$select=="passenger_miles"){
        textinfo<-c("Total passenger miles by mode of tranportation (millions of miles).")
        output$odata_set<-renderText({textinfo})
        matchInput <- paste(input$mode_transportation, collapse = "|")
        passenger_miles<-passenger_miles %>% 
          filter(year <= input$year[2] & year >= input$year[1])
        
        passenger_miles<-select(passenger_miles, year ,matches(matchInput))
        passenger_miles}
      
      else if (input$select=="passenger_fare"){
        textinfo<-c("Passenger fare is the cost per ticket for the modes of transportation 
                    displayed. The exception is travel by automobile which is the cost per 15k 
                    miles.The transportation fare / cost is adjusted to 2013 USD.")
        output$odata_set<-renderText({textinfo})
        matchInput <- paste(input$mode_transportation, collapse = "|")
        transit_cost<-transit_cost %>% 
          filter(year <= input$year[2] & year >= input$year[1])
        passenger_fare<-select(transit_cost,year,matches("adj"))
        colnames(passenger_fare)<-gsub("adj","",colnames(passenger_fare))
        passenger_fare<-select(passenger_fare,year,matches(matchInput),matches("CPI 1982 1984"))
        passenger_fare}
      else if (input$select=="energy_int"){ 
        textinfo<-c("Energy intensity is the measure of energy output per mile traveled measured 
                    in British Thermal Units (Btu).")
        output$odata_set<-renderText({textinfo})
        matchInput <- paste(input$mode_transportation, collapse = "|")
        energy_int<-energy_int %>% 
          filter(year <= input$year[2] & year >= input$year[1])
        energy_int<-select(energy_int,year,matches(matchInput))
        energy_int}
       }
    )
  
    
#****************************************************************************************    
#                              Plot Tab
#****************************************************************************************
    output$myPlot <- renderPlot({
      if (input$select2=="auto_cost"){
        textinfo<-c("The checkbox options are not available for the Automobile Cost table.
                     Expenses are adjusted to 2013 USD.")
        output$odata_set2<-renderText({textinfo})
        colnames(auto_cost)<-gsub("adj","",colnames(auto_cost))
        auto_expense<-auto_cost %>% 
          filter(year <= input$year2[2] & year >= input$year2[1])
        auto_expense <- melt(auto_expense, id=c("year"))
        colnames(auto_expense)<-c("year","expense","USD")
        auto_expense<-auto_expense %>% filter(!(expense %in% c("CPI 1982 1984")))
        auto_expense$year<-as.numeric(auto_expense$year)
        auto_expense$USD<-as.numeric(gsub(",", "",  auto_expense$USD))
        na.omit(auto_expense)
        p <- ggplot(auto_expense,aes(x=year,y=USD,group=expense,color=expense))
        p<-p + geom_line()+scale_y_continuous(labels = comma)+
          scale_color_discrete(name="Auto   Expense")+facet_grid(expense~.,scales="free")
        #+theme (strip.background =element_blank(),strip.text.y= element_blank())
      }
      
      else if (is.null(input$mode_transportation2)){
        textinfo<-c("")
        output$odata_set2<-renderText({textinfo})
        return(NULL)
      }
      else if (input$select2=="passenger_miles"){
        
        textinfo<-c("Total passenger miles by mode of tranportation (billions of miles).")
        output$odata_set2<-renderText({textinfo})
        passenger_miles<-passenger_miles %>% 
          filter(year < input$year2[2] & year > input$year2[1])  
        matchInput <- paste(input$mode_transportation2, collapse = "|")
        passenger_miles<-select(passenger_miles,year,matches(matchInput))
        passenger_miles <- melt(passenger_miles, id=c("year"))
        colnames(passenger_miles)<-c("year","mode_transportation","miles")
        passenger_miles$miles<-(as.numeric(gsub(",", "", passenger_miles$miles)))/1000
        na.omit(passenger_miles)
        p <- ggplot(passenger_miles,aes(x=year,y=miles,group=mode_transportation,
                                       color=mode_transportation))
        p <- p + geom_line()+facet_grid(mode_transportation~.,scales="free")+
        scale_y_continuous(labels = comma)+labs(y=expression("miles  (billions)"))+
        scale_color_discrete(name="Mode of Transportation")
      }
      
      else if (input$select2=="passenger_fare"){
        textinfo<-c("Passenger fare is the cost per ticket for the modes of transportation 
                    displayed. The exception is travel by automobile which is the cost per 15k 
                    miles. The fare/cost is adjusted to 2013 USD.")
        output$odata_set2<-renderText({textinfo})
        transit_cost<-transit_cost %>% 
          filter(year < input$year2[2] & year > input$year2[1]) 
        matchInput <- paste(input$mode_transportation2, collapse = "|")
        transit_cost<-select(transit_cost,year,matches(matchInput))
        transit_cost<-select(transit_cost,year,matches("adj"))
        colnames(transit_cost)<-gsub("adj","",colnames(transit_cost))
        transit_cost <- melt(transit_cost, id=c("year"))
        colnames(transit_cost)<-c("year","mode_transportation","USD")
        transit_cost$USD<-as.numeric(transit_cost$USD)
        na.omit(transit_cost)
        p <- ggplot(transit_cost,aes(x=year,y=USD,group=mode_transportation,
                                        color=mode_transportation))
        p <- p + geom_line()+facet_grid(mode_transportation~.,scales="free")+ 
          scale_color_discrete(name="Mode of Transportation")+scale_y_continuous(labels = comma)
      }
      else if (input$select2=="energy_int"){
        textinfo<-c("Energy intensity is the measure of energy output per mile traveled measured 
                    in British Thermal Units (Btu).")
        output$odata_set2<-renderText({textinfo})
        energy_int<-energy_int %>% 
          filter(year < input$year2[2] & year > input$year2[1]) 
        matchInput <- paste(input$mode_transportation2, collapse = "|")
        energy_int<-select(energy_int,year,matches(matchInput))
        e_int <- melt(energy_int, id=c("year"))
        colnames(e_int)<-c("year","mode_transportation","energy_intensity")
        e_int$energy_intensity<-as.numeric(gsub(",", "", e_int$energy_intensity))
        e_int$year<-as.numeric(e_int$year)
        e_int<-e_int %>% 
          filter(!(mode_transportation %in% c("light vehicle","passenger car",
                                              "light vehicle lwb")))
        na.omit(e_int)
        p <- ggplot(e_int,aes(x=year,y=energy_intensity,group=mode_transportation,
          color=mode_transportation))
        p<- p + geom_line()+facet_grid(mode_transportation~.,scales="free")+
          scale_y_continuous(labels = comma)+labs(y="Btu per passenger mile")+
          scale_color_discrete(name="Mode of Transportation")
      }
      else return(NULL)
      print(p) 
    })
    

#*******************************************************************************************
#                                Prediction Tab
#******************************************************************************************

    
# ***************************** Prediction Output*****************************************
output$myPrediction <- renderPrint({
      if (input$select3=="passenger_miles"){
        matchInput <- paste(input$mode_transportation3, collapse = "|")
        passenger_miles<-passenger_miles %>% filter(year<=input$year4[2] & year>=input$year4[1]) 
        passenger_miles<-select(passenger_miles,year,grep(matchInput,colnames(passenger_miles)))
        passenger_miles <- melt(passenger_miles, id=c("year"))
        colnames(passenger_miles)<-c("year","mode_transportation","miles")
        passenger_miles$miles<-(as.numeric(gsub(",", "", passenger_miles$miles)))/1000
        na.omit(passenger_miles)
        miles<-lm(miles~year,data=passenger_miles)
        year<-data_frame(year=as.numeric(input$year3))
        newpredict<-predict(miles,year)
        newpredict1<-format(newpredict,digits=0,big.mark=",")
        newpredict1<-paste(newpredict1,"billion miles")
        }
  else if (input$select3=="auto_cost"){
      matchInput <- c("cost")
      auto_expense<-auto_cost %>% filter(year<=input$year4[2] & year>=input$year4[1]) 
      auto_expense<-select(auto_expense,year,matches("15k"))
      auto_expense <- melt(auto_expense, id=c("year"))
      colnames(auto_expense)<-c("year","mode_transportation","USD")
      auto_expense$year<-as.numeric(auto_expense$year)
      auto_expense$USD<-as.numeric(gsub(",", "",  auto_expense$USD))
      na.omit(auto_expense)
      USD<-lm(USD~year,data=auto_expense)
      year<-data_frame(year=as.numeric(input$year3))
      newpredict<-predict(USD,year)
      newpredict1<-format(newpredict,digits=0,big.mark=",")
      newpredict1<-paste(newpredict1,"USD per 15k miles (calculated in 2013 USD)")
      }
  else if (input$select3=="passenger_fare"){
      transit_cost<-transit_cost %>% filter(year<=input$year4[2] & year>=input$year4[1]) 
      passenger_fare<-select(transit_cost,year,matches("adj"))
      colnames(passenger_fare)<-gsub("adj","",colnames(passenger_fare))
      matchInput <- paste(input$mode_transportation3, collapse = "|")
      passenger_fare<-select(passenger_fare,year,grep(matchInput,colnames(passenger_fare)))
      passenger_fare <- melt(passenger_fare, id=c("year"))
      colnames(passenger_fare)<-c("year","mode_transportation","USD")
      passenger_fare$USD<-(as.numeric(gsub(",", "", passenger_fare$USD)))
      na.omit(passenger_fare)
      USD<-lm(USD~year,data=passenger_fare)
      year<-data_frame(year=as.numeric(input$year3))
      newpredict<-predict(USD,year)
      newpredict1<-format(newpredict,digits=0,big.mark=",")
      if (input$mode_transportation3=="^auto|^highway|15k$"){
        newpredict1<-paste(newpredict1,"USD per 15k miles")
      }
  else
      newpredict1<-paste(newpredict1,"USD per ticket/fare")
  }
  else if (input$select3=="energy_int"){
      energy_int<-energy_int %>% filter(year<=input$year4[2] & year>=input$year4[1]) 
      matchInput <- paste(input$mode_transportation3, collapse = "|")
      energy_int<-select(energy_int,year,grep(matchInput,colnames(energy_int)))
      e_int <- melt(energy_int, id=c("year"))
      colnames(e_int)<-c("year","mode_transportation","energy_intensity")
      e_int$energy_intensity<-as.numeric(gsub(",", "", e_int$energy_intensity))
      e_int$year<-as.numeric(e_int$year)
      na.omit(e_int)
      energy<-lm(energy_intensity~year,data=e_int)
      year<-data_frame(year=as.numeric(input$year3))
      newpredict<-predict(energy,year)
      newpredict1<-format(newpredict,digits=0,big.mark=",")
      newpredict1<-paste(newpredict1,"Btu per mile")
      }
      else return(NULL)
      cat(newpredict1)
})

#**************************** Linear Fit Plot *******************************************  

output$predPlot<- renderPlot({
  if (input$select3=="passenger_miles"){
    passenger_miles<-passenger_miles %>% filter(year<=input$year4[2] & year>=input$year4[1]) 
    matchInput <- paste(input$mode_transportation3, collapse = "|")
    passenger_miles<-select(passenger_miles,year,grep(matchInput,colnames(passenger_miles)))
    passenger_miles <- melt(passenger_miles, id=c("year"))
    colnames(passenger_miles)<-c("year","mode_transportation","miles")
    passenger_miles$miles<-(as.numeric(gsub(",", "", passenger_miles$miles)))/1000
    na.omit(passenger_miles)
    p <- ggplot(passenger_miles,aes(x=year,y=miles,group=mode_transportation,
                                  color=mode_transportation,xmax=input$year4[2]))
    p<-p + geom_line()+facet_grid(mode_transportation~.)+labs(y=expression("miles  (billions)"))+
    scale_color_discrete(name="Mode of Transportation")+stat_smooth(method="lm",fullrange=TRUE)
    }
  else if (input$select3=="auto_cost"){
    auto_expense<-auto_cost %>% filter(year<=input$year4[2] & year>=input$year4[1]) 
    auto_expense<-select(auto_expense,year,matches("15k"))
    #matchInput <- paste(input$mode_transportation3, collapse = "|")
    #auto_expense<-select(auto_expense,year,grep(matchInput,colnames(auto_expense)))
    auto_expense <- melt(auto_expense, id=c("year"))
    colnames(auto_expense)<-c("year","mode_transportation","USD")
    auto_expense$USD<-as.numeric(gsub(",", "",  auto_expense$USD))
    na.omit(auto_expense)
    p <- ggplot(auto_expense,aes(x=year,y=USD,group=mode_transportation,
                                    color=mode_transportation,xmax=input$year4[2]))
    p<-p + geom_line()+facet_grid(mode_transportation~.)+
      stat_smooth(method="lm",fullrange=TRUE)
    }
  else if (input$select3=="passenger_fare"){
    transit_cost<-transit_cost %>% filter(year<=input$year4[2] & year>=input$year4[1]) 
    matchInput <- paste(input$mode_transportation3, collapse = "|")
    colnames(transit_cost)<-gsub("adj","",colnames(transit_cost))
    transit_cost<-select(transit_cost,year,grep(matchInput,colnames(transit_cost)))
    transit_cost <- melt(transit_cost, id=c("year"))
    colnames(transit_cost)<-c("year","mode_transportation","USD")
    transit_cost$USD<-(as.numeric(gsub(",", "", transit_cost$USD)))
    na.omit(transit_cost)
    p <- ggplot(transit_cost,aes(x=year,y=USD,group=mode_transportation,
                                    color=mode_transportation,xmax=input$year4[2]))
    p<-p + geom_line()+facet_grid(mode_transportation~.)+
      stat_smooth(method="lm",fullrange=TRUE)
    }
  else if (input$select3=="energy_int"){
    energy_int<-energy_int %>% filter(year<=input$year4[2] & year>=input$year4[1]) 
    matchInput <- paste(input$mode_transportation3, collapse = "|")
    energy_int<-select(energy_int,year,grep(matchInput,colnames(energy_int)))
    colnames(energy_int)<-gsub("adj","",colnames(energy_int))
    energy_int <- melt(energy_int, id=c("year"))
    colnames(energy_int)<-c("year","mode_transportation","Btu")
    energy_int$Btu<-(as.numeric(gsub(",", "", energy_int$Btu)))
    na.omit(transit_cost)
    p <- ggplot(energy_int,aes(x=year,y=Btu,group=mode_transportation,
                                 color=mode_transportation,xmax=input$year4[2]))
    p<-p + geom_line()+facet_grid(mode_transportation~.)+
      stat_smooth(method="lm",fullrange=TRUE)
    }
  else return(NULL)
  print(p)
})

})










