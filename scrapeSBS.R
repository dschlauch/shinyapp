library(XML)
library(RCurl)


# colnames(masterData) <- c('LeagueID', 'LeagueName', 'TeamName', 'TeamID', 'TeamWins', 'Player')


leagueIDs <- 1:50000

htmlRes <- sapply(leagueIDs,function(leagueID){
    cat('.')
    getURL(paste("http://www.socialbostonsports.com/leagues/",leagueID,"/standings",sep=""))
})



leaguenamesL <- lapply(htmlRes, function(webpage){
    if(nchar(webpage)==0){
        return(NA)
    }
    html.raw <-htmlTreeParse(webpage, useInternalNodes=T)
    leaguename <- xpathApply(html.raw, "//h1[@id='page-title']",xmlValue)
    if (length(leaguename)>0){
        return(strsplit(leaguename[[1]],"(\n|\t|\r)+")[[1]][2])
    }
    return(NA)
})
leaguenames <- unlist(leaguenamesL)
validLeague <- !is.na(unlist(leaguenames))
validLeagueIDs <- leagueIDs[validLeague]
leaguenames <- leaguenames[validLeague]
dballLeague <- grepl("Dodgeball",leaguenames)
leaguenames <- leaguenames[dballLeague]
validLeagueIDs <- validLeagueIDs[dballLeague]
validLeagueDF <- data.frame(cbind(validLeagueIDs,leaguenames))

saveRDS(validLeagueDF,"validLeagueDF.rdata")

# Go to each league standings page
htmlLeaguePages <- sapply(validLeagueDF$validLeagueIDs, function(leagueID){
    cat('.')
    webpage <- getURL(paste("http://www.socialbostonsports.com/leagues/",leagueID,"/standings",sep=""))
    htmlTreeParse(webpage, useInternalNodes=T)
})

numLeagues <- length(htmlLeaguePages)
if(numLeagues<1){
    stop
}

masterData <- data.frame('LeagueID'=NA, 'LeagueName'=NA, 'TeamName'=NA, 'TeamID'=NA, 'TeamWins'=NA, 'TeamLosses'=NA, 'Player'=NA)
for(leagueIndex in 1:numLeagues){
    cat('.')
    # Get wins for each team
    html.parse<-xpathApply(htmlLeaguePages[[leagueIndex]], "//td", xmlValue)
    if(length(html.parse)==0){
        next
    }
    table <- matrix(unlist(html.parse),nrow=6)
    table.df <- data.frame(t(matrix(as.numeric(table[-1,]),nrow=nrow(table[-1,]))))
    table.df$Team <- table[1,]
    colnames(table.df)[4] <- "Wins"
    if(sum(table.df[,4])<100){
        next
    }
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
    
    # Get links to each of the teams
    teamLinks <- xpathApply(htmlLeaguePages[[leagueIndex]], "//td[@class='main-col team']/a/@href")[notHolder]
    
    # Go to each team page
    if(length(teamLinks)>0){
        
        
        for(x in 1:length(teamLinks)){
            webpage <- getURL(paste("http://www.socialbostonsports.com", teamLinks[x], sep=""))
            htmlRawTeampage <- htmlTreeParse(webpage, useInternalNodes=T)
            teamID <- strsplit(as.character(teamLinks[x]),"/")[[1]][5]
            # Get a list of each player on those teams
            playername <- gsub("/members/","",as.character(xpathApply(htmlRawTeampage, "//dt[@class='thumb']/a/@href")))
            
            # rbind(LeagueID, LeagueName, TeamName, TeamID, TeamWins, Player)
            
            for(player in playername){
                masterData <- rbind(masterData,c(validLeagueIDs[leagueIndex], leaguenames[leagueIndex], rownames(display.table)[x], teamID, display.table$Wins[x], display.table$Losses[x], player))
            }
            
        }
    }
}
masterData <- masterData[-1,]
saveRDS(masterData,file="./data/masterData.rds")

library(reshape2)
minSeasonsPlayed <- 13
playerTeam <- data.frame(cbind(masterData$Player,masterData$TeamID,as.numeric(1.0)))
playersPass <- table(masterData$Player)
playersPass <- playersPass[playersPass >=minSeasonsPlayed]
playerTeam <- playerTeam[masterData$Player%in%names(playersPass),]
playerTeam[,3]<-as.numeric(playerTeam[,3])
playerTeamSpread <- dcast(playerTeam, X1 ~ X2, value.var='X3',fill=0)
rownames(playerTeamSpread) <- playerTeamSpread[,1]
playerTeamSpread <- as.matrix(playerTeamSpread[,-1])
adjMatrix <- playerTeamSpread %*% t(playerTeamSpread)
diag(adjMatrix) <- 0

# require(qgraph)
require(igraph)
# qgraph(adjMatrix)
adjMatrix[adjMatrix<3]<-0
# adjMatrix <- adjMatrix/3
playerNet <-graph.adjacency(adjMatrix)
plot.igraph(playerNet,vertex.size=2,edge.arrow.size=0)


###
# Regression for wins
minSeasonsPlayed <- 13
teamIDs  <-masterData$TeamID
nonDups <- !duplicated(teamIDs)
teamWins <- masterData$TeamWins[nonDups]
names(teamWins) <- teamIDs[nonDups]
playerTeam <- data.frame(cbind(masterData$Player,masterData$TeamID,as.numeric(1.0)))
playersPass <- table(masterData$Player)
playersPass <- playersPass[playersPass >=minSeasonsPlayed]
playerTeam <- playerTeam[masterData$Player%in%names(playersPass),]
playerTeam[,3]<-as.numeric(playerTeam[,3])
playerTeamSpread <- dcast(playerTeam, X1 ~ X2, value.var='X3',fill=0)
rownames(playerTeamSpread) <- playerTeamSpread[,1]
playerTeamSpread <- as.matrix(playerTeamSpread[,-1])

colnames(playerTeamSpread) <- teamWins[colnames(playerTeamSpread)]
wins <- colnames(playerTeamSpread)
players <- t(playerTeamSpread)
lmFit <- lm(wins ~ players)
summary(lmFit)$coef[order(-summary(lmFit)$coef[,1]),]
