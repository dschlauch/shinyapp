library(shiny)
library(sjPlot)
library(ggplot2)
library(reshape2)
library(igraph)

data(MisLinks)
data(MisNodes)

shinyServer(function(input, output) {
    validLeagueDF <- readRDS("./data/validLeagueDF.rdata")
    masterData <- readRDS("./data/masterData.rds")
    masterData[,1] <- as.character(masterData[,1])
    masterData[,5] <- as.numeric(as.character(masterData[,5]))
    masterData[,6] <- as.numeric(as.character(masterData[,6]))
    
    getFullLinks <- function(){
        html.raw<-htmlTreeParse(
            "http://www.socialbostonsports.com/leagues/dodgeball?state=LIVE",
            useInternalNodes=T
        )      
        html.parse<-xpathApply(html.raw, "//a/@href")
        html.parse2<-xpathApply(html.raw, "//h2", xmlValue)
        links <- as.character(unlist(html.parse))
        linknames <- as.character(unlist(html.parse2))
        links <- links[grep("standings",links)]
        linknames <- linknames[grep("Dodgeball",linknames)]
        fulllinks <- paste("http://www.socialbostonsports.com",links,sep="")
        names(fulllinks) <- linknames
        fulllinks    
    }
    getArchiveLinks <- function(){
        archiveLinks <- as.list(as.character(validLeagueDF$validLeagueIDs))
        names(archiveLinks) <- validLeagueDF$leaguenames
        rev(archiveLinks)    
    }
    output$searchtype <- renderUI({
        if(input$radio==1)
            return(selectInput("teamselect", label = h3("Select season"), choices = as.list(getFullLinks()), selected = 1))
        else if(input$radio==2)
            return(textInput("individualselect", label = h3("Enter Baller"), value = "SBS username..."))
        else if(input$radio==4)
            return(selectInput("seasonArchive", label = h3("Select season"), choices = getArchiveLinks(), selected = 5))
        else if(input$radio==5)
            return(list(sliderInput("minSeasons", "Minimum Seasons:", min = 1, max = 40, value = 20),
                        textInput("individualNet", label = h3("Add/Highlight Baller"), value = "SBS username...")))
        else return(NULL)
    })
    output$chart1 <- renderUI({
        if(input$radio==1){
            return(htmlOutput("table"))}
        else if(input$radio==2){
            return(htmlOutput("teams"))}
        else if(input$radio==3){
            return(htmlOutput("overall"))}
        else if(input$radio==4){
            return(htmlOutput("archivetable"))}
        else if(input$radio==5){
            return(NULL)
        }
    })
    output$chart2 <- renderUI({
        if(input$radio==1){
            return(plotOutput("hist"))}
        else if(input$radio==2){
            return(plotOutput("wins"))}
        else if(input$radio==4){
            return(plotOutput("archivehist"))}
        else{
            return(NULL)
        }
    })
    output$NetChart <- renderUI({
        if(input$radio==5){
            return(plotOutput("network"))
        }
    })
    output$force <- renderForceNetwork({
        minSeasonsPlayed <- input$minSeasons
        addPlayer <- input$individualNet
        playerTeam <- data.frame(cbind(masterData$Player,masterData$TeamID,as.numeric(1.0)))
        playersPass <- table(masterData$Player)
        playersPass <- c(names(playersPass[playersPass >= minSeasonsPlayed]), addPlayer)
        playerTeam <- playerTeam[masterData$Player%in%playersPass,]
        playerTeam[,3] <- as.numeric(playerTeam[,3])
        playerTeamSpread <- dcast(playerTeam, X1 ~ X2, value.var='X3',fill=0)
        rownames(playerTeamSpread) <- playerTeamSpread[,1]
        playerTeamSpread <- as.matrix(playerTeamSpread[,-1])
        adjMatrix <- playerTeamSpread %*% t(playerTeamSpread)
        diag(adjMatrix) <- 0
        
        require(igraph)
        filterEdges <- rownames(adjMatrix)!=addPlayer
        adjMatrix[filterEdges,filterEdges][adjMatrix[filterEdges,filterEdges]<3]<-0
        pairs <- melt(adjMatrix)
        playernames <- levels(factor(pairs[,1]))
        pairs[,1] <- as.numeric(factor(pairs[,1]))-1
        pairs[,2] <- as.numeric(factor(pairs[,2]))-1
        pairs <- pairs[pairs[,3]>0,]
        colnames(pairs) <- c("source","target","value")
        nodes <- data.frame('name'=playernames,"group"=1)
        
        forceNetwork(Links = pairs, Nodes = nodes, Source = "source",
                     Target = "target", Value = "value", NodeID = "name",
                     Group = "group", opacity = .8)
    })
    
  output$hist <- renderPlot({
      standingsHistogram(input$teamselect)
  })
  output$archivehist <- renderPlot({
      standingsHistogram(paste("http://www.socialbostonsports.com/leagues/",input$seasonArchive,"/standings",sep=""))
  })
  output$network <- renderPlot({
      minSeasonsPlayed <- input$minSeasons
      addPlayer <- input$individualNet
      playerTeam <- data.frame(cbind(masterData$Player,masterData$TeamID,as.numeric(1.0)))
      playersPass <- table(masterData$Player)
      playersPass <- c(names(playersPass[playersPass >= minSeasonsPlayed]), addPlayer)
      playerTeam <- playerTeam[masterData$Player%in%playersPass,]
      playerTeam[,3] <- as.numeric(playerTeam[,3])
      playerTeamSpread <- dcast(playerTeam, X1 ~ X2, value.var='X3',fill=0)
      rownames(playerTeamSpread) <- playerTeamSpread[,1]
      playerTeamSpread <- as.matrix(playerTeamSpread[,-1])
      adjMatrix <- playerTeamSpread %*% t(playerTeamSpread)
      diag(adjMatrix) <- 0
      
      require(igraph)
      filterEdges <- rownames(adjMatrix)!=addPlayer
      adjMatrix[filterEdges,filterEdges][adjMatrix[filterEdges,filterEdges]<3]<-0
      playerNet <-graph.adjacency(adjMatrix)
      V(playerNet)$color<-ifelse(V(playerNet)$name==addPlayer, 'red', 'blue')
      P <- rbind(cbind(which(filterEdges),which(!filterEdges)),cbind(which(!filterEdges),which(filterEdges)))
      E(playerNet)$color <-'grey'
      E(playerNet)[inc(V(playerNet)$name==addPlayer)]$color<-'red'
      plot.igraph(playerNet, vertex.size=0, edge.arrow.size=0)
  },height=800)
  
  standingsHistogram <- function(link){
      html.raw<-htmlTreeParse(link, useInternalNodes=T)
      print(link)
      html.parse<-xpathApply(html.raw, "//td", xmlValue)
      unlistedRes <- unlist(html.parse)
      if(length(unlistedRes)<6)
          return(NULL)
      table <- matrix(unlistedRes,nrow=6)
      table.df <- data.frame(t(matrix(as.numeric(table[-1,]),nrow=nrow(table[-1,]))))
      table.df$Team <- table[1,]
      
      notHolder <- !grepl("SBS Holder|Court|TBD|SBSholder|SBSHolder|SBS HOLDER|SBS holder", table.df$Team)
      table.df <- table.df[notHolder,]
      
      colnames(table.df)[4] <- "Wins"
      table.df <- table.df[order(-table.df[,4]),]
      
      ggplot(table.df, aes(x=Team,y=Wins)) +
          geom_bar(stat="identity",fill="red") +
          scale_x_discrete(limits = rev(table.df$Team)) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          coord_flip()  
  }
  
  output$wins <- renderPlot({
      teams.df <- masterData[masterData$Player==input$individualselect, c(1,2,3,5,6)]
      ggplot(teams.df, aes(x=LeagueID,y=TeamWins)) +
          geom_bar(stat="identity",fill="red") +
          scale_x_discrete(labels = teams.df$TeamName) +
          ylim(0,60) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  standingsTable <- function(link){
      html.raw<-htmlTreeParse(link, useInternalNodes=T)
      html.parse<-xpathApply(html.raw, "//td", xmlValue)
      table <- matrix(unlist(html.parse),nrow=6)
      table.df <- data.frame(t(matrix(as.numeric(table[-1,]),nrow=nrow(table[-1,]))))
      table.df$Team <- table[1,]
      colnames(table.df)[4] <- "Wins"
      table.df <- table.df[order(-table.df[,4]),]
      total.games <- sum(table.df$Wins)/nrow(table.df)
      display.table <- data.frame("Wins"=table.df$Wins)
      rownames(display.table) <- table.df$Team
      notHolder <- !grepl("SBS Holder|Court|TBD|SBSholder|SBSHolder|SBS HOLDER|SBS holder", rownames(display.table))
      display.table <- display.table[notHolder,,drop=F]
      numGames <- 2*sum(display.table$Wins)/nrow(display.table)
      if(numGames<70&numGames>45){
          numGames <- 60
      }

      display.table$Losses <- round(numGames - display.table$Wins,0)
      display.table$PCT <- round(display.table$Wins / (display.table$Wins + display.table$Losses),3)
      display.table$GB <- round(max(display.table$Wins) - display.table$Wins,0)
      html.table <- sjt.df(display.table, altr.row.col=TRUE, string.var = "Team Name",
                           orderColumn="Losses", describe=FALSE, no.output=T, hideProgressBar=T)
      html.table$output.complete
  }
  output$archivetable <- renderText({
      standingsTable(paste("http://www.socialbostonsports.com/leagues/",input$seasonArchive,"/standings",sep=""))
  })
  output$table <- renderText({
      html.raw<-htmlTreeParse(input$teamselect, useInternalNodes=T)
      html.parse<-xpathApply(html.raw, "//td", xmlValue)
      table <- matrix(unlist(html.parse),nrow=6)
      table.df <- data.frame(t(matrix(as.numeric(table[-1,]),nrow=nrow(table[-1,]))))
      table.df$Team <- table[1,]
      colnames(table.df)[4] <- "Wins"
      table.df <- table.df[order(-table.df[,4]),]
      total.games <- sum(table.df$Wins)/nrow(table.df)
      display.table <- data.frame("Wins"=table.df$Wins)
      rownames(display.table) <- table.df$Team
      notHolder <- !grepl("SBS Holder|Court|TBD|SBSholder|SBSHolder|SBS HOLDER|SBS holder", rownames(display.table))
      display.table <- display.table[notHolder,,drop=F]
      display.table$Losses <- round(2*sum(display.table$Wins)/nrow(display.table)-display.table$Wins,0)
      display.table$PCT <- round(display.table$Wins / (display.table$Wins + display.table$Losses),3)
      display.table$GB <- round(max(display.table$Wins) - display.table$Wins,0)
      html.table <- sjt.df(display.table, altr.row.col=TRUE, string.var = "Team Name",
             orderColumn="Losses", describe=FALSE, no.output=T, hideProgressBar=T)
      html.table$output.complete
  })
    output$teams <- renderText({
        print(input$individualselect)
        teams.df <- masterData[masterData$Player==input$individualselect, c(2,3,5,6)]
        html.table <- sjt.df(teams.df, altr.row.col=TRUE, string.var = "Seasons", describe=FALSE, no.output=T,show.rownames = FALSE, hideProgressBar=T)
        paste(h3("Overall record:",sum(teams.df$TeamWins),"-",sum(teams.df$TeamLosses)), html.table$output.complete, sep="")
    })
    output$overall <- renderText({
        playerSeasons <- rev(sort(table(masterData$Player)))
        playerSeasons <- playerSeasons[playerSeasons>3]
        playerOveralldf <- cbind(data.frame(t(sapply(names(playerSeasons),function(player){
            playerSeasons <- masterData[masterData$Player==player,]
            winSum <- sum(playerSeasons$TeamWins)
            loseSum <- sum(playerSeasons$TeamLosses)
            diff <- paste(ifelse(winSum>loseSum,"+",""),(winSum-loseSum),sep="")
            return(c(winSum,loseSum,diff))
        }))),playerSeasons)
        playerOveralldf$X1 <- as.numeric(as.character(playerOveralldf$X1))
        playerOveralldf$X2 <- as.numeric(as.character(playerOveralldf$X2))
        playerOveralldf$X3 <- as.character(playerOveralldf$X3)
#         playerOveralldf$playerSeasons <- as.numeric(playerOveralldf$playerSeasons)
#         playerOveralldf <- cbind(rownames(playerOveralldf),playerOveralldf)
        colnames(playerOveralldf) <- c("Wins","Losses","+/-","Seasons")
        html.table <- sjt.df(playerOveralldf[,c(4,1,2,3)], altr.row.col=TRUE, string.var = "Player", describe=FALSE, no.output=T,show.rownames = T, hideProgressBar=T)
        html.table$output.complete
  })
})
