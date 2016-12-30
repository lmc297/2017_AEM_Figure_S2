# Supplemental Figure S2 for 2017 AEM Paper

library(shiny)
library(ggplot2)

# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel(h1("Supplemental Figure S2")),
   
   
   # Sidebar 
   sidebarLayout(position="right",
      sidebarPanel(
        helpText("Select a Salmonella antimicrobial resistance (AMR) data set to visualize."),
        selectInput("serotype",
                    label="Serotype(s):",
                    choices=c("All Serotypes (Typhimurium, Newport, Dublin)"=1,"Typhimurium"=2,
                              "Newport"=3,"Dublin"=4),
                    selected=1),
        selectInput("dataset",
                    label="AMR Data Set:",
                    choices=c("AMR Gene Sequences","AMR Phenotypes (Resistant/Susceptible)",
                              "Plasmid Replicon Presence/Absence"),
                    selected="AMR Gene Sequences"),
        conditionalPanel(
          condition = "input.serotype=='1'",
          selectInput("var",
                      label="Grouping Variable:",
                      choices=list("None Selected","Serotype","Source","Geographic Location"),
                      selected="None Selected")),
        conditionalPanel(
          condition = "input.serotype!='1'",
          checkboxGroupInput("newvar",
                       label=h4("Grouping Variable:"),
                       choices=list("Source","Geographic Location"))),
        conditionalPanel(
          condition = "input.serotype=='4' && input.dataset=='AMR Gene Sequences'",
          checkboxInput("ddrop",
                             label="Remove outlier isolate (HUM_DUBN_WA_10_R9_3256) from data set",
                        FALSE))),
      
      # Show a plot 
      mainPanel(
      plotOutput("plot", click="plotclick",
                 dblclick ="plotdb",
                 brush=brushOpts(id="plotbrush",resetOnNew = TRUE)),
      htmlOutput("badtext"),
      fluidRow(column(width=12,h3("Selected isolate(s)"),helpText("Click on a point to select isolate(s)."),verbatimTextOutput("clickinfo"), helpText("Drag mouse and double-click on plot to zoom in; double-click to reset plot.")))
      )
   )
)

# Define server logic 
server <- function(input, output) {
  ranges <- reactiveValues(x = NULL, y = NULL)

  p <- function(nmds.scores,hull.data,appgroup){
    if (appgroup!=0){
      if (appgroup=="Serotype"){
      newplot<-ggplot()  
      newplot<-newplot+geom_polygon(data=hull.data,
                                    aes_string(x="NMDS1",y="NMDS2",group=appgroup,colour=appgroup,fill=appgroup),
                                    alpha=0.30) + 
        scale_fill_manual(values=c("green","blue","red"))+
        geom_point(data=nmds.scores,aes_string(x="NMDS1",y="NMDS2",shape=appgroup,colour=appgroup),size=2)+  
        scale_colour_manual(values=c("Typhimurium" = "red", "Newport" = "blue", "Dublin" = "green")) +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y)
      return(newplot)}
      if (appgroup=="Source"){
        newplot<-ggplot()  
        newplot<-newplot+geom_polygon(data=hull.data,
                                      aes(x=NMDS1,y=NMDS2,group=Source,colour=Source,fill=Source),
                                      alpha=0.30) + 
          scale_fill_manual(values=c("blue","red"))+
          geom_point(data=nmds.scores,aes(x=NMDS1,y=NMDS2,shape=Source,colour=Source),size=2)+  
        scale_colour_manual(values=c("Bovine" = "blue", "Human" = "red")) +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y)
        return(newplot)}
      if (appgroup=="Geo"){
        newplot<-ggplot()  
        newplot<-newplot+geom_polygon(data=hull.data,
                                      aes(x=NMDS1,y=NMDS2,group=Geo,colour=Geo,fill=Geo),
                                      alpha=0.30) + 
          scale_fill_manual(values=c("purple","green"))+
          geom_point(data=nmds.scores,aes(x=NMDS1,y=NMDS2,shape=Geo,colour=Geo),size=2) + 
        scale_colour_manual(values=c("NY" = "purple", "WA" = "green")) +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y)
        return(newplot)}
      if (appgroup=="Group"){
        newplot<-ggplot()  
        newplot<-newplot+geom_polygon(data=hull.data,
                                      aes(x=NMDS1,y=NMDS2,group=Group,colour=Group,fill=Group),
                                      alpha=0.30) + 
          scale_fill_manual(values=c("green","purple","deeppink1","darkorange1"))+
          geom_point(data=nmds.scores,aes(x=NMDS1,y=NMDS2,shape=Group,colour=Group),size=2) + 
          scale_colour_manual(values=c("Bovine NY" = "green", "Human NY" = "deeppink1","Bovine WA" = "purple", "Human WA" = "darkorange1")) +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y)
        return(newplot)}
        
    
    }
    else{
      zeroplot<-ggplot()  
      zeroplot<-zeroplot+
        geom_point(data=nmds.scores,aes(x=NMDS1,y=NMDS2),size=2) +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y)
        return(zeroplot)
    }
  }
  
  testvars<-function(newvar){
    if (!(is.character(newvar))){
      appgroup<-0
      hullval<-0
      return(list(appgroup=appgroup,hullval=hullval))}
    else{
      ifelse("Source"%in%newvar & "Geographic Location"%in%newvar,appgroup<-"Group",ifelse("Source"%in%newvar & !("Geographic Location"%in%newvar),appgroup<-"Source",appgroup<-"Geo"))
      if (appgroup=="Group"){hullval<-"allhulls"}
      if (appgroup=="Source"){hullval<-"sourcehulls"}
      if (appgroup=="Geo"){hullval<-"geohulls"}
      return(list(appgroup=appgroup,hullval=hullval))
    }
  }

  
  output$plot <- renderPlot({
  if (input$serotype==1){
    insero<-"_sero_"
    if (input$var=="None Selected"){
      appgroup<-0
      hullval<-0
    }
    if (input$var=="Serotype"){
      appgroup<-"Serotype"
      hullval<-"serohulls"
    }
    if (input$var=="Source"){
      appgroup<-"Source"
      hullval<-"sourcehulls"
    }
    if (input$var=="Geographic Location"){
      appgroup<-"Geo"
      hullval<-"geohulls"
    }
  }
  if (input$serotype==2){
    insero<-"_typh_"
    finalvals<-testvars(input$newvar)
    appgroup<-finalvals$appgroup
    hullval<-finalvals$hullval
  }
  if (input$serotype==3){
    insero<-"_newp_"
    finalvals<-testvars(input$newvar)
    appgroup<-finalvals$appgroup
    hullval<-finalvals$hullval
  }
  if (input$serotype==4){
    if (input$dataset=="AMR Gene Sequences"){
    if (input$ddrop==FALSE){
    insero<-"_dubn_"
    finalvals<-testvars(input$newvar)
    appgroup<-finalvals$appgroup
    hullval<-finalvals$hullval}
    else{
      insero<-"_dubR_"
      finalvals<-testvars(input$newvar)
      appgroup<-finalvals$appgroup
      hullval<-finalvals$hullval}}
    else{
      insero<-"_dubn_"
      finalvals<-testvars(input$newvar)
      appgroup<-finalvals$appgroup
      hullval<-finalvals$hullval}}

  if (input$dataset=="AMR Gene Sequences"){
    indata<-"geno"
  }
  if (input$dataset=="AMR Phenotypes (Resistant/Susceptible)"){
    indata<-"pheno"
  }
  if (input$dataset=="Plasmid Replicon Presence/Absence"){
    indata<-"plasmid"
  }
      nmdsfile<-paste("nmds",insero,indata,"_points.txt",sep="")
      nmds.scores<-read.delim(nmdsfile,sep="\t")
      appgroup<-appgroup
      if (hullval!=0){
      hullfile<-paste("nmds",insero,indata,"_",hullval,".txt",sep="")
      hull.data<-read.delim(hullfile,sep="\t")
      if (appgroup=="Geo" && "State"%in%names(hull.data)){
        hull.data$Geo<-hull.data$State
      }}
      else{
        hull.data<-0
      }
      plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
      plot
   

      })
  
  output$badtext <- renderUI({
    mymessage<-"No NMDS solution could be reached for this dataset
    within 10,000 random starts. Please interpret plot with caution!"
    finalmessage<-paste("<h3 style='color:red' align='center'>",mymessage,"</h3>")
    if (input$serotype==3 | input$serotype==4){
      if (input$dataset!="AMR Gene Sequences"){
        return(HTML(finalmessage))}}
  })
  
  output$clickinfo <- renderPrint({
    if (input$serotype==1){
      insero<-"_sero_"
      if (input$var=="None Selected"){
        appgroup<-0
        hullval<-0
      }
      if (input$var=="Serotype"){
        appgroup<-"Serotype"
        hullval<-"serohulls"
      }
      if (input$var=="Source"){
        appgroup<-"Source"
        hullval<-"sourcehulls"
      }
      if (input$var=="Geographic Location"){
        appgroup<-"Geo"
        hullval<-"geohulls"
      }
    }
    if (input$serotype==2){
      insero<-"_typh_"
      finalvals<-testvars(input$newvar)
      appgroup<-finalvals$appgroup
      hullval<-finalvals$hullval
    }
    if (input$serotype==3){
      insero<-"_newp_"
      finalvals<-testvars(input$newvar)
      appgroup<-finalvals$appgroup
      hullval<-finalvals$hullval
    }
    if (input$serotype==4){
      insero<-"_dubn_"
      finalvals<-testvars(input$newvar)
      appgroup<-finalvals$appgroup
      hullval<-finalvals$hullval
    }
    if (input$dataset=="AMR Gene Sequences"){
      indata<-"geno"
    }
    if (input$dataset=="AMR Phenotypes (Resistant/Susceptible)"){
      indata<-"pheno"
    }
    if (input$dataset=="Plasmid Replicon Presence/Absence"){
      indata<-"plasmid"
    }
    nmdsfile<-paste("nmds",insero,indata,"_points.txt",sep="")
    nmds.scores<-read.delim(nmdsfile,sep="\t")
    if (input$serotype==1){
    nmds.final<-cbind(nmds.scores[,4:6],nmds.scores[,1:2])
    colnames(nmds.final)<-c("Serotype","Source","Geographic Location",
                            "NMDS1","NMDS2")}
    else{
      nmds.final<-cbind(nmds.scores[,4:5],nmds.scores[,1:2])
      colnames(nmds.final)<-c("Source","Geographic Location",
                              "NMDS1","NMDS2")}
   nearPoints(nmds.final,input$plotclick, addDist = TRUE)})
  observeEvent(input$plotdb, {
    brush <- input$plotbrush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  }

    

# Run the application 
shinyApp(ui = ui, server = server)

