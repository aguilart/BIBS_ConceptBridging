##
##
#From our meeting on the 6th of June (Tina, Masahiro and I), we decided to create various diagrams
#based on: 

#1- Scheiner and Willig 2007
#2- Biodiversity as are response variable or driver
#3- General hypothesis
#4- Vellend 2006
#5- Classical fields
#6- Taxonomic groups
#7-Sutherland 100 questions in ecology
#8-People

#The most updated set of the data is available at: 

#https://docs.google.com/spreadsheets/d/1gSACnoSIBW2Yeaa77o9U_5G2qWclRVb62VosrX4PQ5A/edit#gid=941463247

#Objectives for the general meeting in September

#1. Show other how we have categorized the concepts of BIBS and visualize it

#2. In general, to show which drivers or aspects of response are over or under-represented in BIBS
# and also which concepts are under or over respresented (concepts from the different references we use)

devtools::install_github("AdeelK93/collapsibleTree")


library(data.tree)
library(tidyverse)
library(collapsibleTree)
library(igraph)


haber<-function(x){if(!is.na(x)){
  if(nchar(x)>40){
    paste(
      strwrap(x,width=40,simplify=T)[1],
      "\\n",
      strwrap(x,width=40,simplify=T)[2]
    )}else{x<-x}
}else{x<-x}
}
#############################################################################################
###############Data assembly#################################################################
#############################################################################################


#Loading the files as csv directly from the googletable

ConceptBridiging<-
                  read.csv("ProjectsData/20170210Synthesis_database_MasahirosDriveVersionProject_list_Concepts.csv",
                           header = T,stringsAsFactors = F)

                  names(ConceptBridiging)[6]<-"EcologicalConcept_Scheiner_Willig"
                  
                  names(ConceptBridiging)[10]<-"EcologicalProceses_Vellend"
                  
                  names(ConceptBridiging)[8]<-"JustMonitoringBiodiversity"
                  
                  #ConceptBridiging[2:6]<-apply(ConceptBridiging[2:6],1:2,haber)
                  
                  ConceptBridiging$Biodiversity.as<-NA
                  ConceptBridiging$Biodiversity.as[
                    ConceptBridiging$Biodiversity.as.Response.variabe=="Yes"]<-"Response variable"
                  ConceptBridiging$Biodiversity.as[
                    ConceptBridiging$Biodiversity.as.explanatory.variable=="Yes"]<-"Explanatory variable"
                  ConceptBridiging$Biodiversity.as[
                    rowSums(ConceptBridiging[,c(7,9)]=="Yes")==2]<-"Explanatory and response variable"

#Adding metadata

MetaData_A<-
            read.csv("ProjectsData/20170210Synthesis_database_Masahiros_ProjectList.csv",
                     header = F, stringsAsFactors = F)
            
            names(MetaData_A)[1:5]<-MetaData_A[1,c(1:5)]
            names(MetaData_A)[4]<-"Project.title"

#Merging medatada with the concept table
ConceptBridiging<-
                left_join(ConceptBridiging,MetaData_A[-2,c(1:5)])
                
                ConceptBridiging$Who<-sub("&","and",ConceptBridiging$Who)
                ConceptBridiging$Who<-sub("\\+","and",ConceptBridiging$Who)
                ConceptBridiging$Who<-sub("/","and",ConceptBridiging$Who)
                ConceptBridiging$Who<-sub("\\,","and",ConceptBridiging$Who)

Taxa<-MetaData_A[-1,c(4,14:18)]
              names(Taxa)[2:6]<-Taxa[1,2:6]
              Taxa<-Taxa[-1,];
              Taxa$Vertebrate[grep("x|X",Taxa$Vertebrate)]<-"Vertebrate"
              Taxa$Invertebrate[grep("x|X",Taxa$Invertebrate)]<-"Invertebrate"
              Taxa$Plant[grep("x|X",Taxa$Plant)]<-"Plant"
              Taxa$Microbe[grep("x|X",Taxa$Microbe)]<-"Microbe"
              Taxa$Else[grep("x|X",Taxa$Else)]<-"Else"
              
              Taxa$Vertebrate[Taxa$Vertebrate==""]<-NA
              Taxa$Invertebrate[Taxa$Invertebrate==""]<-NA
              Taxa$Plant[Taxa$Plant==""]<-NA
              Taxa$Microbe[Taxa$Microbe==""]<-NA
              Taxa$Else[Taxa$Else==""]<-NA

ConceptBridiging<-
                left_join(ConceptBridiging,Taxa)

                ConceptBridiging<-ConceptBridiging[1:63,]

people<-strsplit(ConceptBridiging$Who,"and ")
          people<-
          lapply(people,function(x)
            data.frame(Responsible_1<-x[1],
                       Responsible_2<-x[2],
                       Responsible_3<-x[3],stringsAsFactors = F))
          
          people<-do.call(rbind, people)
          names(people)<-c("Responsible_1","Responsible_2","Responsible_3")
          people$Responsible_1<-trimws(people$Responsible_1)
          people$Responsible_2<-trimws(people$Responsible_2)
          people$Responsible_3<-trimws(people$Responsible_3)

concepts_SW<-strsplit(ConceptBridiging$EcologicalConcept_Scheiner_Willig,"AND")
              concepts_SW<-
                lapply(concepts_SW,function(x)
                  data.frame(Theory_1<-x[1],
                             Theory_2<-x[2],stringsAsFactors = F
                             ))
              
              concepts_SW<-do.call(rbind, concepts_SW)
              names(concepts_SW)<-c("Theory_1","Thoery_2")

              concepts_SW$Theory_1[which(concepts_SW$Theory_1=="Niche theory")]<-"Niche (Coexistence) Theory"
              concepts_SW$Thoery_2[grep("Niche",concepts_SW$Thoery_2)]<-"Niche (Coexistence) Theory"
              concepts_SW$Thoery_2[grep("Food",concepts_SW$Thoery_2)]<-"Food web theory"

concepts_V<-strsplit(ConceptBridiging$EcologicalProceses_Vellend,"AND")
            concepts_V<-
              lapply(concepts_V,function(x)
                data.frame(P_1<-x[1],
                           P_2<-x[2],stringsAsFactors = F
                ))
            
            concepts_V<-do.call(rbind, concepts_V)
            names(concepts_V)<-c("Process_1","Process_2")
            concepts_V$Process_2[grep(" Drift",concepts_V$Process_2)]<-"Drift"
            concepts_V$Process_1[grep("Selection ",concepts_V$Process_1)]<-"Selection"

ConceptBridiging<-cbind(ConceptBridiging,concepts_SW,concepts_V,people)

                ConceptBridiging<-
                  gather(ConceptBridiging,key=Theory_rank,value=Theory,Theory_1:Thoery_2,na.rm = T)
                
                ConceptBridiging<-
                  gather(ConceptBridiging,key=Process_rank,value=Community_Process,Process_1:Process_2,na.rm = T)
                
                ConceptBridiging<-
                  gather(ConceptBridiging,key=Organism_rank,value=Organism,Vertebrate:Else,na.rm = T)
                
                ConceptBridiging<-
                  gather(ConceptBridiging,key=Person_rank,value=Person,Responsible_1:Responsible_3,na.rm = T)
                
                ConceptBridiging$Theory_rank<-NULL
                ConceptBridiging$Process_rank<-NULL
                ConceptBridiging$Organism_rank<-NULL
                ConceptBridiging$Person_rank<-NULL

                #originalConceptBridging<-ConceptBridiging


#############################################################################################
######Collapsible trees visualization (however we agreed on Feb 2019 not to focuse on this###
#ones any more)##############################################################################
                
BIBS_projects_as_Vellend<-
ConceptBridiging%>%
  filter(!is.na(Community_Process))%>%
  filter(!is.na(Working.hypothesis..prediction.))%>%
  collapsibleTreeSummary(
    #ConceptBridiging[-which(is.na(ConceptBridiging$Working.hypothesis..prediction.)),],
    hierarchy = c("Community_Process", 
                  #"General.hypothesis..long." ,
                  "Working.hypothesis..prediction.",
                  "Person"),
    root ="Community ecology process as Vellend ",
    attribute = "leafCount",
    nodeSize = "leafCount",
    width = 1500,
    height = 1500,
    fontSize = 15,
    zoomable = T,
    tooltip = T,
    collapsed = TRUE
  )


BIBS_projects_as_Scheiner_Willig<-
  ConceptBridiging%>%
  filter(!is.na(Theory))%>%
  filter(!is.na(Working.hypothesis..prediction.))%>%
  collapsibleTreeSummary(
    #ConceptBridiging[-which(is.na(ConceptBridiging$Working.hypothesis..prediction.)),],
    hierarchy = c("Theory", 
                  #"General.hypothesis..long." ,
                  "Working.hypothesis..prediction.",
                  "Person"),
    root ="Ecological Theory as Scheiner & Willig ",
    attribute = "leafCount",
    nodeSize = "leafCount",
    width = 1500,
    height = 1500,
    fontSize = 12,
    zoomable = T,
    tooltip = T,
    collapsed = TRUE
  )
#############################################################################################
#################################NETWORK GRAPHS##############################################
#############################################################################################

#Transforming the table into the right format
ConceptsMatrixFormat<-
                    ConceptBridiging%>%
                      group_by(Person,Theory) %>%
                      summarise(weight = n()) %>% 
                      ungroup()%>%
                      spread(Theory,weight);
                      ConceptsMatrixFormat[is.na(ConceptsMatrixFormat)]<-0;
                      ConceptsMatrixFormat<-as.matrix(ConceptsMatrixFormat)
                      
                      row.names(ConceptsMatrixFormat)<-ConceptsMatrixFormat[,1]
                      ConceptsMatrixFormat<-ConceptsMatrixFormat[,-1]

ConceptsMatrixFormat_net<-graph_from_incidence_matrix(ConceptsMatrixFormat)
                          V(ConceptsMatrixFormat_net)$color <- c("orange", "steel blue")[V(ConceptsMatrixFormat_net)$type+1]
                          V(ConceptsMatrixFormat_net)$shape <- c("circle", "square")[V(ConceptsMatrixFormat_net)$type+1]
                          V(ConceptsMatrixFormat_net$label.cex = 2)
                          
                          
                          
                          ######Plotting the network###############
                          plot(ConceptsMatrixFormat_net,vertex.label.cex=1)
                          #With tkplot I can interactively change the position and aspects for this network graph
                          tkplot(ConceptsMatrixFormat_net,vertex.label.cex=2.5)

#Here I am simplying the network by only having one type of data (either the people or the 
#concepts)

ConceptsMatrixFormat_net.bp <- bipartite.projection(ConceptsMatrixFormat_net) 
                              plot(ConceptsMatrixFormat_net.bp$proj1)

                      

