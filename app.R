#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(h1("Supplemental Figure S2")),
   
   
   # Sidebar with a slider input for number of bins 
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
                       label=h3("Grouping Variable:"),
                       choices=list("Source","Geographic Location")))),
      
      # Show a plot of the generated distribution
      mainPanel(
      plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  p <- function(nmds.scores,hull.data,appgroup){
    if (appgroup!=0){
      if (appgroup=="Serotype"){
      newplot<-ggplot()  
      newplot<-newplot+geom_polygon(data=hull.data,
                                    aes_string(x="NMDS1",y="NMDS2",group=appgroup,colour=appgroup,fill=appgroup),
                                    alpha=0.30) + # add the convex hulls
        scale_fill_manual(values=c("green","blue","red"))+
        #geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5,size=3) +  # add the species labels
        geom_point(data=nmds.scores,aes_string(x="NMDS1",y="NMDS2",shape=appgroup,colour=appgroup),size=2)+  # add the point markers
        ##geom_text(data=nmds.scores,aes(x=NMDS1,y=NMDS2,label=site),size=8,vjust=0,hjust=0) +
        #geom_text_repel(data=nmds.scores,aes(x=NMDS1,y=NMDS2,label=site),size=3) +  # add the site labels
        # add the site labels
        scale_colour_manual(values=c("Typhimurium" = "red", "Newport" = "blue", "Dublin" = "green")) +
        coord_equal()
      return(newplot)}
      if (appgroup=="Source"){
        newplot<-ggplot()  
        print(head(nmds.scores))
        newplot<-newplot+geom_polygon(data=hull.data,
                                      aes(x=NMDS1,y=NMDS2,group=Source,colour=Source,fill=Source),
                                      alpha=0.30) + # add the convex hulls
          scale_fill_manual(values=c("blue","red"))+
          #geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5,size=3) +  # add the species labels
          geom_point(data=nmds.scores,aes(x=NMDS1,y=NMDS2,shape=Source,colour=Source),size=2)+  # add the point markers
        ##geom_text(data=nmds.scores,aes(x=NMDS1,y=NMDS2,label=site),size=8,vjust=0,hjust=0) +
        #geom_text_repel(data=nmds.scores,aes(x=NMDS1,y=NMDS2,label=site),size=3) +  # add the site labels
        # add the site labels
        scale_colour_manual(values=c("Bovine" = "blue", "Human" = "red")) +
        coord_equal()
        return(newplot)}
      if (appgroup=="Geo"){
        newplot<-ggplot()  
        newplot<-newplot+geom_polygon(data=hull.data,
                                      aes(x=NMDS1,y=NMDS2,group=Geo,colour=Geo,fill=Geo),
                                      alpha=0.30) + # add the convex hulls
          scale_fill_manual(values=c("purple","green"))+
          #geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5,size=3) +  # add the species labels
          geom_point(data=nmds.scores,aes(x=NMDS1,y=NMDS2,shape=Geo,colour=Geo),size=2) + # add the point markers
        ##geom_text(data=nmds.scores,aes(x=NMDS1,y=NMDS2,label=site),size=8,vjust=0,hjust=0) +
        #geom_text_repel(data=nmds.scores,aes(x=NMDS1,y=NMDS2,label=site),size=3) +  # add the site labels
        # add the site labels
        scale_colour_manual(values=c("NY" = "purple", "WA" = "green")) +
          coord_equal()
        return(newplot)}
      if (appgroup=="Group"){
        newplot<-ggplot()  
        newplot<-newplot+geom_polygon(data=hull.data,
                                      aes(x=NMDS1,y=NMDS2,group=Group,colour=Group,fill=Group),
                                      alpha=0.30) + # add the convex hulls
          scale_fill_manual(values=c("green","deeppink1","purple","darkorange1"))+
          #geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5,size=3) +  # add the species labels
          geom_point(data=nmds.scores,aes(x=NMDS1,y=NMDS2,shape=Group,colour=Group),size=2) + # add the point markers
          ##geom_text(data=nmds.scores,aes(x=NMDS1,y=NMDS2,label=site),size=8,vjust=0,hjust=0) +
          #geom_text_repel(data=nmds.scores,aes(x=NMDS1,y=NMDS2,label=site),size=3) +  # add the site labels
          # add the site labels
          scale_colour_manual(values=c("Bovine NY" = "green", "Human NY" = "deeppink1","Bovine WA" = "purple", "Human WA" = "darkorange1")) +
          coord_equal()
        return(newplot)}
    
    }
    else{
      print("appgroup is zero")
      zeroplot<-ggplot()  
      zeroplot<-zeroplot+
        geom_point(data=nmds.scores,aes(x=NMDS1,y=NMDS2),size=2) + # add the point markers
        #geom_text(data=nmds.scores,aes(x=NMDS1,y=NMDS2,label=site),size=8,vjust=0,hjust=0) +
        #geom_text_repel(data=nmds.scores,aes(x=NMDS1,y=NMDS2,label=site),size=3) +  # add the site labels
        # add the site labels
        #scale_colour_manual(values=c("Typhimurium" = "red", "Newport" = "blue", "Dublin" = "green")) +
        coord_equal() 
        return(zeroplot)
    }
    }
    output$plot <- reactivePlot(function(){
      print(input$serotype)
      print(input$dataset)
      print(input$var)
      if (input$serotype==1){
        if (input$dataset=="AMR Gene Sequences"){
          nmds.scores<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_sero_geno_points.txt",sep="\t")
          if (input$var=="None Selected"){
            appgroup<-0
            hull.data<-0
            zeroplot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
            print(zeroplot)}
          if (input$var=="Serotype"){
            print(input$var)
            hull.data<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_sero_geno_serohulls.txt",sep="\t")
            appgroup<-"Serotype"
            plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
            print(plot)}
          if (input$var=="Source"){
            hull.data<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_sero_geno_sourcehulls.txt",sep="\t")
            appgroup<-"Source"
            print(head(hull.data))
            plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
            print(plot)}
         if (input$var=="Geographic Location"){
            hull.data<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_sero_geno_geohulls.txt",sep="\t")
            print(head(hull.data))
            appgroup<-"Geo"
            plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
            print(plot)}
        }
        
        
        }
      else{
        if (input$serotype==2){
          if (input$dataset=="AMR Gene Sequences"){
            print(input$newvar)
            print(class(input$newvar))
            nmds.scores<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_typh_geno_points.txt",sep="\t")
            if (!(is.character(input$newvar))){
              print("this is null")
              appgroup<-0
              hull.data<-0
              print(appgroup)
              plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
              print(plot)}
            else{
            ifelse("Source"%in%input$newvar & "Geographic Location"%in%input$newvar,appgroup<-"Group",ifelse("Source"%in%input$newvar & !("Geographic Location"%in%input$newvar),appgroup<-"Source",appgroup<-"Geo"))
            if (appgroup=="Group"){
                hull.data<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_typh_geno_allhulls.txt",sep="\t")
                plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
                print(plot)}
            if (appgroup=="Source"){
              hull.data<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_typh_geno_sourcehulls.txt",sep="\t")
              plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
              print(plot)
            }
            if (appgroup=="Geo"){
              hull.data<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_typh_geno_geohulls.txt",sep="\t")
              plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
              print(plot)}
            }

            
          }
        }
        if (input$serotype==3){
          
          if (input$dataset=="AMR Gene Sequences"){
            print(input$newvar)
            print(class(input$newvar))
            nmds.scores<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_newp_geno_points.txt",sep="\t")
            if (!(is.character(input$newvar))){
              print("this is null")
              appgroup<-0
              hull.data<-0
              print(appgroup)
              plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
              print(plot)}
            else{
              ifelse("Source"%in%input$newvar & "Geographic Location"%in%input$newvar,appgroup<-"Group",ifelse("Source"%in%input$newvar & !("Geographic Location"%in%input$newvar),appgroup<-"Source",appgroup<-"Geo"))
              if (appgroup=="Group"){
                hull.data<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_newp_geno_allhulls.txt",sep="\t")
                plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
                print(plot)}
              if (appgroup=="Source"){
                hull.data<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_newp_geno_sourcehulls.txt",sep="\t")
                plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
                print(plot)
              }
              if (appgroup=="Geo"){
                hull.data<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_newp_geno_geohulls.txt",sep="\t")
                plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
                print(plot)}
            }
            
            
          }
          
          
        }
        
        
        
      ###
        if (input$serotype==4){
          
          if (input$dataset=="AMR Gene Sequences"){
            print(input$newvar)
            print(class(input$newvar))
            nmds.scores<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_dubn_geno_points.txt",sep="\t")
            if (!(is.character(input$newvar))){
              print("this is null")
              appgroup<-0
              hull.data<-0
              print(appgroup)
              plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
              print(plot)}
            else{
              ifelse("Source"%in%input$newvar & "Geographic Location"%in%input$newvar,appgroup<-"Group",ifelse("Source"%in%input$newvar & !("Geographic Location"%in%input$newvar),appgroup<-"Source",appgroup<-"Geo"))
              if (appgroup=="Group"){
                hull.data<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_dubn_geno_allhulls.txt",sep="\t")
                plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
                print(plot)}
              if (appgroup=="Source"){
                hull.data<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_dubn_geno_sourcehulls.txt",sep="\t")
                plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
                print(plot)
              }
              if (appgroup=="Geo"){
                hull.data<-read.delim("/Users/lmcarrol/Documents/wsu/manuscript/november28/WSU_plots/nmds_dubn_geno_geohulls.txt",sep="\t")
                plot<-p(nmds.scores=nmds.scores,hull.data = hull.data,appgroup = appgroup)
                print(plot)}
            }
            
            
          }
          
          
        }
        
        
        
        
        }
      
      })
  }

    
   
  #})
     
#}

# Run the application 
shinyApp(ui = ui, server = server)

