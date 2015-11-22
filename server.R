#Server Code
county_center <- readRDS('data/county_center.Rds')
county_shp    <- readRDS('data/county_shp.Rds')
state_shp <- readRDS('data/state_shp.Rds')
unempdata_t   <- readRDS('data/unempdata.Rds')
year_range <-readRDS('data/year_range.Rds')
unemp_state <-readRDS('data/unempdata_state.Rds')
delete_states <- c('02','15','72')
max_date <-readRDS('data/max_date.Rds')

legend_names <-data.frame(
  var_list=c("unemprate", "emp","lf", "unemp", "d12unemprate", "g12emp", "g12unemp", "g12lf"),
  legend_list=c("Rate",'Persons','Persons','Persons','Change','Percent Change','Percent Change','Percent Change'))


library(ggplot2)
library(dplyr)
library(reshape2)

shinyServer(function(input, output) {
  
  #legend label
  legend_labels <- reactive({
    legend_names[which(legend_names$var_list==input$dataset),2]
  })
  
  
  date <- reactive({ 
          year <- as.character(input$year)
          month <-as.character(input$month)
          as.Date(paste(year,month,'01',sep='-'))
  })
   
 
  
  #This is the data for the histogram
  unemp_hist_data <- reactive({
  
    validate(
      need(try(date() <= max_date), paste("The last date for which data is available is",format.Date(as.character(max_date),"%B-%Y")))
    )

  
  #     unempdata_t %>%
  #       filter(year==input$year,month==input$month) %>%
  #       select(date,fips,eval(parse(text=input$dataset)))
  vars <- c('date','fips','st_fips',input$dataset)
  sel  <- with(unempdata_t, year==input$year & month==input$month & !st_fips %in% delete_states)
  unempdata_t <- unempdata_t[sel,vars]

  
  
  })
  
  #merge histrogram data with "shapefile" for mapping.
  unemp_map_data <- reactive({
    
    
  right_join(unemp_hist_data(),county_shp,by=c('fips'='id'))
    
      })

  ##extract the min and max counties
  
  min_max_centroids <- reactive({
    
    data <- unemp_hist_data()
    
    data$ranks<-unlist(with(data,tapply(data[,input$dataset],list(st_fips),rank,na.last='keep')))
    #data$ranks[data[which(input$dataset]==NA] <- NA
    #data$ranks <- data$ranks
    
    min_max<-  data %>%
      group_by(st_fips) %>%
      summarize(min=min(ranks,na.rm=TRUE),
                max=max(ranks,na.rm=TRUE))
    
    min_max <- melt(min_max, id="st_fips",value.name='ranks')
    min_max<- left_join(min_max,data)
    min_max<- left_join(min_max,county_center)
    
  })
  
  state_data <- reactive({
    validate(
      need(try(date() <= max_date), paste("The last date for which data is available is",as.character(max_date)))
    )
    
    vars <- c('date','st_fips','area_text',input$dataset)
    sel  <- with(unemp_state, year==input$year & month==input$month & !st_fips%in%delete_states)
    unemp_state1 <- unemp_state[sel,vars]

  })
  
  state_data_map <- reactive({
    
    left_join(state_data(),state_shp, by=c('st_fips'='id'))
    
  })
  
 
  state_data_min_max <- reactive ({
    
    unemp_state <- state_data()
    unemp_state$ranks<-rank(unemp_state[,input$dataset])
    
    min_max<- unemp_state %>%
       summarize(min=min(ranks),
                 max=max(ranks))
     
    min_max <- melt(min_max, value.name='ranks')
    min_max<- left_join(min_max,unemp_state)
    min_max<- left_join(min_max,state_shp, by=c('st_fips'='id'))
  })
  
  best_and_worst <- reactive ({
    
    unemp_state <- state_data_min_max()
    unemp_state <- unemp_state[,c(input$dataset,'area_text','variable')]
    unique(unemp_state)
  })
  
 

  output$slider_year <- renderUI({
    sliderInput("year","Choose Year",min=year_range[1],max=year_range[2],value=year_range[2],sep="")
  })
  
  output$slider_month <- renderUI({
    sliderInput("month","Choose Month",min=1,max=12,value=6,sep="")
  })
  
  output$choose_variable <- renderUI({
    #names <- colnames(unempdata_t)
    selectInput("dataset", "Variable", 
                choices = c("Unemployment Rate"="unemprate",
                               "Employed Persons"="emp",
                               "Labor Force"="lf",
                               "Unemployed Persons"="unemp",
                               "Unemployment Rate - 12 Month Change"="d12unemprate",
                               "Employed Persons  - 12 Month % Change"="g12emp",
                               "Unemployed Persons - 12 Month % Change"="g12unemp",
                               "Labor Force - 12 Month % Change"="g12lf"),
                selected="unemprate")
       })
  

  
  output$table2 <- renderDataTable({
    
    best_and_worst()
   
  })
  
  
  output$plot1 <- renderPlot({
    

    unempmap <- ggplot()
    unempmap + geom_polygon(data=unemp_map_data(),aes_string(x='long',y='lat',group='group',fill=input$dataset))+
               scale_fill_gradient(low="lightblue", high="darkblue",na.value='black',name= legend_labels())+
               geom_polygon(data=county_shp ,aes(x=long,y=lat,group=group),fill=NA,size=0.1,color="grey34") +
               geom_polygon(data=state_shp,aes(x=long,y=lat,group=group),fill=NA,size=1,color="black") + 
               geom_point(data=min_max_centroids(),aes(x=x,y=y,color=variable), size=2) + 
               scale_colour_manual(values=c("green", "red"),guide = guide_legend(title = " ")) +
               labs(x=" ", y=" ") 
              
      
   # geom_tile(aes(fill=z))+
      #scale_fill_gradientn(colours=c("blue","white","red"),breaks=b,labels=format(b))
  })
  
  output$plot2 <- renderPlot({
    unemphist <- ggplot(data=state_data(),aes_string('area_text', input$dataset))
    unemphist + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) +
      xlab(' ') +
      ylab(' ')
  })
  
   output$plot3 <- renderPlot({
     unempmap <- ggplot()
     unempmap +
        geom_polygon(data=state_data_map(),aes_string(x='long',y='lat',group='group',fill=input$dataset)) +
       scale_fill_gradient(low="lightblue", high="darkblue",na.value='black',name= legend_labels())+        geom_polygon(data=state_shp       ,aes_string(x='long',y='lat',group='group'),fill=NA,color="black")+
        geom_polygon(data=state_data_min_max()  ,aes_string(x='long',y='lat',group='group',color="variable"),fill=NA,size=1.5)+
        scale_colour_manual(values=c("green", "red")) + 
        xlab(' ') +
        ylab(' ')
   })

})