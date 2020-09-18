Project 1 Vignette
================
Rachel Keller
September 18, 2020

Table of Contents
=================

-   Required Packages
-   Functions
    -   Franchise Endpoint
    -   Franchise Team Totals
    -   Franchise Season Records
    -   Franchise Goalie Records
    -   Franchise Skater Records
    -   Teams
    -   Wrapper
-   Exploratory Data Analysis
    -   NHL Analysis
    -   Washington Capitals Analysis

Requred Packages
================

The required packages for this vignette are `httr`, `jsonlite`,
`tidyverse`, and `knitr`. We will load in these packages.

    library(httr)
    library(jsonlite)
    library(tidyverse)
    library(knitr)

Functions
=========

In this section, we will create the functions that will contact the NHL
Records API and NHL Stats API, and return parsed data for certain
endpoints.

Franchise Endpoint
------------------

The function below will return the franchise endpoint from the NHL
Records API, given either the team name or team number/id. Variables
returned include id, firstSeasonId and lastSeasonId and name of every
team in the history of the NHL.

    getFranchise <- function(teamName = NA, teamNumber = NA){ 
      # Generate URL to contact API
      baseURLNHLRecords <- "https://records.nhl.com/site/api" 
      tableName <- "franchise"
      fullURL <- paste0(baseURLNHLRecords, "/", tableName)
      # Create get request
      get <- GET(fullURL)
      txt <- content(get, "text", encoding = "UTF-8")
      json <- fromJSON(txt, flatten = TRUE)
      if(is.na(teamName) & is.na(teamNumber)){
        # If no specified team return full data set
        result <- json$data
      }
      else if (is.na(teamName)){
        # If given teamNumber return filtered data set
        result <- json$data %>% mutate(fullName = paste(teamPlaceName, teamCommonName, sep = " ")) %>% filter(id == teamNumber)
      }
      else {
        # If given teamName return filtered data set
        result <- json$data %>% mutate(fullName = paste(teamPlaceName, teamCommonName, sep = " ")) %>% filter(fullName == teamName)
      }
      return(result)
    }

Franchise Team Totals
---------------------

The function below will return the franchise team totals endpoint from
the NHL Records API, given either the team name or team number/id.
Information returned from this endpoint include total stats for every
franchise such as roadTies, roadWins, etc.

    getFranchiseTotals <- function(teamName = NA, teamNumber = NA){
      # Generate URL to contact API
      baseURLNHLRecords <- "https://records.nhl.com/site/api"
      tableName <- "franchise-team-totals "
      fullURL <- paste0(baseURLNHLRecords, "/", tableName)
      # Create get request
      get <- GET(fullURL)
      txt <- content(get, "text", encoding = "UTF-8")
      json <- fromJSON(txt, flatten = TRUE)
      if(is.na(teamName) & is.na(teamNumber)){
        # If no specified team return full data set
        result <- json$data
      }
      else if (is.na(teamName)){
        # If given teamNumber return filtered data set
        result <- json$data %>% filter(franchiseId == teamNumber)
      }
      else {
        # If given teamName return filtered data set
        teamNameValue <- teamName
        result <- json$data %>% filter(teamName == teamNameValue)
      }
      return(result)
    }

Franchise Season Records
------------------------

The function below will return the franchise season records endpoint
from the NHL Records API, given either the team name or team number/id.
This endpoint drills down into season records for a specific franchise,
so a team must be specified for this endpoint.

    getFranchiseSeasonRecords <- function(teamName = NA, teamNumber = NA){
      baseURLNHLRecords <- "https://records.nhl.com/site/api"
      tableName <- "franchise-season-records?cayenneExp=franchiseId="
      if(is.na(teamName) & is.na(teamNumber)){
        # Endpoint needs a franchise to deliver results
        result <- "Error, need to specify a franchise ID or name. "
      }
      else if (is.na(teamName)){
        # Create URL to contact API
        id <- teamNumber
        fullURL <- paste0(baseURLNHLRecords, "/", tableName, id)
        # Create get request
        get <- GET(fullURL)
        txt <- content(get, "text", encoding = "UTF-8")
        json <- fromJSON(txt, flatten = TRUE)
        result <- json$data
      }
      else {
        # Look up id using getFranchise
        teamInfo <- getFranchise(teamName = teamName)
        id <- teamInfo$id
        # Create URL to contact API
        fullURL <- paste0(baseURLNHLRecords, "/", tableName, id)
        # Create get request
        get <- GET(fullURL)
        txt <- content(get, "text", encoding = "UTF-8")
        json <- fromJSON(txt, flatten = TRUE)
        result <- json$data
      }
      return(result)
    }

Franchise Goalie Records
------------------------

The function below will return the goalie records endpoint from the NHL
Records API, given either the team name or team number/id. This endpoint
returns goalie records for the specified franchise.

    getGoalieRecords <- function(teamName = NA, teamNumber = NA){
      baseURLNHLRecords <- "https://records.nhl.com/site/api"
      tableName <- "franchise-goalie-records?cayenneExp=franchiseId="
      if(is.na(teamName) & is.na(teamNumber)){
        # Endpoint needs a franchise to deliver results
        result <- "Error, need to specify a franchise ID or name. "
      }
      else if (is.na(teamName)){
        # Create URL to contact API
        id <- teamNumber
        fullURL <- paste0(baseURLNHLRecords, "/", tableName, id)
        # Create get request
        get <- GET(fullURL)
        txt <- content(get, "text", encoding = "UTF-8")
        json <- fromJSON(txt, flatten = TRUE)
        result <- json$data
      }
      else {
        # Look up id using getFranchise
        teamInfo <- getFranchise(teamName = teamName)
        id <- teamInfo$id
        # Create URL to contact API
        fullURL <- paste0(baseURLNHLRecords, "/", tableName, id)
        # Create get request
        get <- GET(fullURL)
        txt <- content(get, "text", encoding = "UTF-8")
        json <- fromJSON(txt, flatten = TRUE)
        result <- json$data
      }
      return(result)
    }

Franchise Skater Records
------------------------

The function below will return the skater records endpoint from the NHL
Records API, given either the team name or team number/id. This endpoint
returns skater records for the specified franchise.

    getSkaterRecords <- function(teamName = NA, teamNumber = NA){
      # Endpoint needs a franchise to deliver results
      baseURLNHLRecords <- "https://records.nhl.com/site/api"
      tableName <- "franchise-skater-records?cayenneExp=franchiseId="
      if(is.na(teamName) & is.na(teamNumber)){
        result <- "Error, need to specify a franchise ID or name. "
      }
      else if (is.na(teamName)){
        # Create URL to contact API
        id <- teamNumber
        fullURL <- paste0(baseURLNHLRecords, "/", tableName, id)
        # Create get request
        get <- GET(fullURL)
        txt <- content(get, "text", encoding = "UTF-8")
        json <- fromJSON(txt, flatten = TRUE)
        result <- json$data
      }
      else {
        # Look up id using getFranchise
        teamInfo <- getFranchise(teamName = teamName)
        id <- teamInfo$id
        # Create URL to contact API
        fullURL <- paste0(baseURLNHLRecords, "/", tableName, id)
        # Create get request
        get <- GET(fullURL)
        txt <- content(get, "text", encoding = "UTF-8")
        json <- fromJSON(txt, flatten = TRUE)
        result <- json$data
      }
      return(result)
    }

Teams
-----

The function below will return the teams endpoint from the NHL Stats
API, given either the team name or team number/id. The endpoint includes
arguments for eight modifiers.

    getTeams <- function(teamName = NA, teamNumber = NA, modifier = NA, modifierValue = NA){
      baseURLNHLStats <- "https://statsapi.web.nhl.com/api/v1"
      tableName <- "teams"
      # Create URL given modifier input
      switch(modifier,
                 "Team Roster"={
                   modifierURL <- "?expand=team.roster"
                 }, "Person Names"={
                   modifierURL <- "?expand=person.names"
                 }, "Next Schedule"={
                   modifierURL <- "?expand=team.schedule.next"
                 }, "Previous Schedule"={
                   modifierURL <- "?expand=team.schedule.previous"
                 }, "Team Stats"={
                   modifierURL <- "?expand=team.stats"
                 }, "Season Roster"={
                   modifierURL <- paste0("?expand=team.roster&season=", modifierValue)
                 }, "Multiple Teams"={
                   modifierURL <- "?teamId="
                 }, "Single Season Stats"={
                   modifierURL <- "?stats=statsSingleSeasonPlayoffs"
                 })
      if(is.na(teamName) & is.na(teamNumber)){
        fullURL <- paste0(baseURLNHLStats, "/", tableName)
        # Create get request and return full data set
        get <- GET(fullURL)
        txt <- content(get, "text")
        json <- fromJSON(txt, flatten=TRUE)
        result <- json$teams
      }
      else if (is.na(teamName)){
        # Contact full data set
        fullURL <- paste0(baseURLNHLStats, "/", tableName)
        get <- GET(fullURL)
        txt <- content(get, "text")
        json <- fromJSON(txt, flatten=TRUE)
        if (is.na(modifier)){
          # If no modifier return filtered version of full data set
          result <- json$teams %>% filter(franchiseId == teamNumber)
        } else {
          # Get id from full data set
          getId <- json$teams %>% filter(franchiseId == teamNumber)
          id <- getId$id
          URLWithModifier <- paste0(baseURLNHLStats, "/", tableName, "/", id, modifierURL)
          # Create get request
          getModifier <- GET(URLWithModifier)
          text <- content(getModifier, "text")
          jsonFile <- fromJSON(text, flatten=TRUE)
          # Return output depending on modifier value
          if (modifier == "Team Roster"){
            result <- jsonFile$teams$roster.roster
          } else if (modifier == "Previous Schedule"){
            result <- jsonFile$teams$previousGameSchedule.dates
          } else if (modifier == "Team Stats"){
            result <- jsonFile$teams$teamStats
          } else if (modifier == "Season Roster"){
            result <- jsonFile$teams$roster.roster
          }
          else {
            # Return full data set
            result <- jsonFile$teams
          }
        }
      }
      else {
        # Contact full data set
        fullURL <- paste0(baseURLNHLStats, "/", tableName)
        get <- GET(fullURL)
        txt <- content(get, "text")
        json <- fromJSON(txt, flatten=TRUE)
        teamNameValue <- teamName
        if (is.na(modifier)){
          # If no modifier return filtered version of full data set
          result <- json$teams %>% filter(name == teamNameValue)
        } else {
          # Get id from full data set
          getId <- json$teams %>% filter(name == teamNameValue)
          id <- getId$id
          URLWithModifier <- paste0(baseURLNHLStats, "/", tableName, "/", id, modifierURL)
          # Create get request
          getModifier <- GET(URLWithModifier)
          text <- content(getModifier, "text")
          jsonFile <- fromJSON(text, flatten=TRUE)
          # Return output depending on modifier value
          if (modifier == "Team Roster"){
            result <- jsonFile$teams$roster.roster
          } else if (modifier == "Previous Schedule"){
            result <- jsonFile$teams$previousGameSchedule.dates
          } else if (modifier == "Team Stats"){
            result <- jsonFile$teams$teamStats
          } else if (modifier == "Season Roster"){
            result <- jsonFile$teams$roster.roster
          }
          else {
            # Return full data set
            result <- jsonFile$teams
          }
        }
      }
      return(result)
    }

Wrapper
-------

Now, we will create our wrapper function. The user can select their
desired endpoint.

    getEndpoint <- function(endpoint, teamName = NA, teamNumber = NA, modifier = NA, modifierValue = NA){
      # Select function depending on endpoint desired
      if(endpoint == "franchise"){
        result <- getFranchise(teamName, teamNumber)
      } else if (endpoint == "franchise totals"){
        result <- getFranchiseTotals(teamName, teamNumber)
      } else if (endpoint == "franchise season records"){
        result <- getFranchiseSeasonRecords(teamName, teamNumber)
      } else if (endpoint == "goalie records"){
        result <- getGoalieRecords(teamName, teamNumber)
      } else if (endpoint == "skater records"){
        result <- getSkaterRecords(teamName, teamNumber)
      } else if (endpoint == "teams"){
        result <- getTeams(teamName, teamNumber, modifier, modifierValue)
      } else {
        # If no or incorrect endpoint specified
        result <- "Error, incorrect endpoint."
      }
      return(result)
    }

Exploratory Data Analysis
=========================

Now that we have all of functions created to contact various endpoints
in our APIs, we will explore our data. We’ll begin by doing some
analysis for the overall NHL, then dive into analysis of the Washington
Capitals, my personal team. For both analyses, we will take a look at a
few measures that could indicate strong performance.

NHL Analysis
------------

First, we will look at a contingency table to get an idea about the
overall structure of the NHL.

    overallStructure <- getEndpoint("teams")
    kable(table(overallStructure$division.name, overallStructure$conference.name))

|              | Eastern | Western |
|--------------|--------:|--------:|
| Atlantic     |       8 |       0 |
| Central      |       0 |       7 |
| Metropolitan |       8 |       0 |
| Pacific      |       0 |       8 |

We can see that there are 16 active teams in the Eastern conceference, 8
in the Atlantic division and 8 in the Metropolitan division. And there
are 15 active teams in the Western conference, 7 in the Central division
and 8 in the Pacific division.

As standings are determined by points, we will create numeric summaries
with the overall points by team, division, and conference.

    conferences <- getEndpoint("teams") %>% rename(teamId = id)
    points <- getEndpoint("franchise totals") %>% filter(gameTypeId == 2)
    conferencesPoints <- right_join(conferences, points, by = "teamId")
    conferencesPoints %>% group_by(name) %>% summarise(pointsEarned = points) %>% filter(!is.na(name))

    ## # A tibble: 31 x 2
    ## # Groups:   name [31]
    ##    name                  pointsEarned
    ##    <chr>                        <int>
    ##  1 Anaheim Ducks                 2224
    ##  2 Arizona Coyotes                434
    ##  3 Boston Bruins                 7391
    ##  4 Buffalo Sabres                4149
    ##  5 Calgary Flames                3360
    ##  6 Carolina Hurricanes           1834
    ##  7 Chicago Blackhawks            6556
    ##  8 Colorado Avalanche            2175
    ##  9 Columbus Blue Jackets         1500
    ## 10 Dallas Stars                  2395
    ## # ... with 21 more rows

    conferencesPoints %>% group_by(division.name) %>% summarise(sumPointsEarned = sum(points)) %>% filter(!is.na(division.name))

    ## # A tibble: 4 x 2
    ##   division.name sumPointsEarned
    ##   <chr>                   <int>
    ## 1 Atlantic                39126
    ## 2 Central                 19811
    ## 3 Metropolitan            29742
    ## 4 Pacific                 19822

    conferencesPoints %>% group_by(conference.name) %>% summarise(sumPointsEarned = sum(points)) %>% filter(!is.na(conference.name))

    ## # A tibble: 2 x 2
    ##   conference.name sumPointsEarned
    ##   <chr>                     <int>
    ## 1 Eastern                   68868
    ## 2 Western                   39633

We can see that the Montréal Canadiens have the most amount of points
earned over the entire history of the NHL at 7899. Also, the Atlantic
division and Eastern conferences have the largest amount of points. The
Canadiens belong to the Eastern conference and Atlantic division, so
this partly explains why that division and conference have the largest
amount of points.

Now that we have looked at points as a whole, we will look at the
average points per game in regular season by team, division, and
conference.

    conferencesPoints %>% mutate(avgPoints = points / gamesPlayed) %>% group_by(name) %>% summarise(avg = mean(avgPoints)) %>% filter(!is.na(name))

    ## # A tibble: 31 x 2
    ##    name                    avg
    ##    <chr>                 <dbl>
    ##  1 Anaheim Ducks         1.08 
    ##  2 Arizona Coyotes       0.904
    ##  3 Boston Bruins         1.12 
    ##  4 Buffalo Sabres        1.07 
    ##  5 Calgary Flames        1.08 
    ##  6 Carolina Hurricanes   1.04 
    ##  7 Chicago Blackhawks    1.01 
    ##  8 Colorado Avalanche    1.13 
    ##  9 Columbus Blue Jackets 0.992
    ## 10 Dallas Stars          1.17 
    ## # ... with 21 more rows

    conferencesPoints %>% mutate(avgPoints = points / gamesPlayed) %>% filter(!is.na(name)) %>% group_by(division.name) %>% summarise(avg = mean(avgPoints), med = median(avgPoints), var = var(avgPoints))

    ## # A tibble: 4 x 4
    ##   division.name   avg   med     var
    ##   <chr>         <dbl> <dbl>   <dbl>
    ## 1 Atlantic       1.06  1.05 0.00384
    ## 2 Central        1.10  1.11 0.00259
    ## 3 Metropolitan   1.05  1.04 0.00224
    ## 4 Pacific        1.05  1.05 0.00923

    conferencesPoints %>% mutate(avgPoints = points / gamesPlayed) %>% filter(!is.na(name)) %>% group_by(conference.name) %>% summarise(avg = mean(avgPoints), med = median(avgPoints), var = var(avgPoints))

    ## # A tibble: 2 x 4
    ##   conference.name   avg   med     var
    ##   <chr>           <dbl> <dbl>   <dbl>
    ## 1 Eastern          1.06  1.04 0.00287
    ## 2 Western          1.07  1.08 0.00651

The Vegas Golden Knights have the highest average of points per game.
Also, the Cental division and Western conference have the highest per
game point average. We can see that median of the Cental division and
median of the Western conference is greater than the average, which
suggests that the data is skewed left. The Pacific division has a
noticably higher spread. And the Western conference also has a higher
spread than the Eastern conference.

Now that we have done some analysis on points, we will begin to look at
the some metrics that may imdicate good performance, such as the number
of wins overall. First, we will create a bar graph of home wins by
conference.

    teamInfo <- getEndpoint("teams") %>% rename(teamId = id)
    totals <- getEndpoint("franchise totals")
    teamInfoTotals <- right_join(teamInfo, totals, by = "teamId") %>% filter(!is.na(name), !is.na(conference.name))
    g <- ggplot(teamInfoTotals, aes(name, homeWins, fill = conference.name))
    g + geom_bar(stat = "identity") + facet_grid(~conference.name) + coord_flip() + labs(y = "Number of Wins at Home", x = "Team Name", title = "Bar Plot of Home Wins by Conference", fill = "Conference")

![](Project-1_files/figure-gfm/unnamed-chunk-12-1.png)<!-- --> In the
Eastern conference, the Montréal Canadiens have the largest number of
wins at home. And in the Western conference, the Chicago Blackhawks have
the largest number of home wins.

Staying on the topic of wins, we will now look at a bar plot of road
wins by conference.

    g <- ggplot(teamInfoTotals, aes(name, roadWins, fill = conference.name))
    g + geom_bar(stat = "identity") + facet_grid(~conference.name) + coord_flip() + labs(y = "Number of Wins on the Road", x = "Team Name", title = "Bar Plot of Road Wins by Conference", fill = "Conference")

![](Project-1_files/figure-gfm/unnamed-chunk-13-1.png)<!-- --> Not
surprisingly, the Montréal Canadiens have the largest number of wins on
the road for the Eastern conference. Also, the Chicago Blackhawks have
the largest number of road wins for the Western conference.

The last performance metric we will analyze are shootouts. We will take
a look at the distribution of the proportion of shootouts won, for both
regular and post season.

    shootout <- getEndpoint("franchise totals") %>% mutate(propShootouts = shootoutWins / (shootoutWins + shootoutLosses))
    g <- ggplot(shootout, aes(X = teamName, y = propShootouts, fill = teamName))
    g + geom_boxplot() + labs(x = "Team", y = "Proportion of Successful Shootouts", title = "Bar Plot of Shootouts")

![](Project-1_files/figure-gfm/unnamed-chunk-14-1.png)<!-- --> We can
see that the vast majority of teams hover around the 0.5 mark. However,
4 teams have distributions that span from 0.5 to 1, and from 0 to 0.5.
These teams, the Washington Capitals, Tampa Bay Lighting, St. Louis
Blues, and Dallas Stars have either won or lost all of their shootouts
in the post season.

Washington Capitals Analysis
----------------------------

Much like the analysis of the NHL, we will begin by creating some
contingency tables to gain some insight as to the layout of the
Capitals.

First, we will create a contingency table of the skater history.

    skaterHistory <- getEndpoint("skater records", teamName = "Washington Capitals")
    kable(table(Active = skaterHistory$activePlayer))

| Active | Freq |
|:-------|-----:|
| FALSE  |  457 |
| TRUE   |   52 |

There are currently 52 active skaters for the Washington Capitals.

And now we will create a similar contingency table of the goalie
history.

    goalieHistory <- getEndpoint("goalie records", teamName = "Washington Capitals")
    kable(table(Active = goalieHistory$activePlayer))

| Active | Freq |
|:-------|-----:|
| FALSE  |   27 |
| TRUE   |    3 |

There are currently 3 active goalies on the team.

We will create another contingency table that further drills down into
the positions of the current roster.

    currentRoster <- getEndpoint("teams", teamName = "Washington Capitals", modifier = "Team Roster")
    kable(table(Position = currentRoster[[1]][["position.name"]]))

| Position   | Freq |
|:-----------|-----:|
| Center     |    9 |
| Defenseman |   10 |
| Goalie     |    4 |
| Left Wing  |    6 |
| Right Wing |    5 |

The position with the most players is defensemen, followed by center and
left wing.

Now we will start to dig into the details of the players, first by
seeing how much experience they have. To do this, we will create a
histogram of the seasons a player has been with the Capitals, for both
skaters and goalies.

    skaterSeasons <- getEndpoint("skater records", teamName = "Washington Capitals") %>% mutate(skater = "Yes")
    goalieSeasons <- getEndpoint("goalie records", teamName = "Washington Capitals") %>% mutate(skater = "No")
    joinedSeasons <- full_join(skaterSeasons, goalieSeasons)
    g <- ggplot(joinedSeasons, aes(x = seasons, fill = skater))
    g + geom_histogram(binwidth = 3) + facet_wrap(~skater) + labs(x = "Number of Seasons", y = "Number of Players", title = "Distribution of Seasons for Players", fill = "Skater")

![](Project-1_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

Both goalie and skater distributions appear to be right skewed. There
are very few players who have a long career with the Capitals.

Players with more experience should also perform better. We will test
this by looking at boxplots of points by season earned by each skater
and goalie.

    g <- ggplot(joinedSeasons, aes(x = seasons, y = points, group = seasons))
    g + geom_boxplot(color = "blue") + labs(x = "Number of Seasons", y = "Points Earned", title = "Distribution of Points by Seasons")

![](Project-1_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

There is a steady upward trend. As players gain more experience and play
for more seasons, the points they earn are consistently higher. Or
alternatively, players who no longer earn higher points are drafted to
another team and/or no longer play for the NHL.

Finally, we will look at the points earned for skaters, both active and
inactive, in one season vs. one game, to see if the Capitals are
improving in their choice of skaters over time.

    seasonPoints <- getEndpoint("skater records", teamName = "Washington Capitals")
    g <- ggplot(seasonPoints, aes(x = mostPointsOneSeason, y = mostPointsOneGame, color = activePlayer))
    g + geom_jitter() + labs(x = "Most Points Earned in One Season", y = "Most Points in One Game", title = "Points Earned in One Season and One Game", color = "Active Player")

![](Project-1_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

The trend of both active and nonactive players appears to be very
similar. So the Capitals are not necessarily improving their choice of
skaters over time.

Now that we have looked at skaters, we will begin looking at some stats
that may represent a solid goalie. We will look at a scatterplot of
shots against one vs. the saves in one game, for both active and
inactive goalies.

    seasons <- getEndpoint("goalie records", teamName = "Washington Capitals")
    g <- ggplot(seasons, aes(x = mostShotsAgainstOneGame, y = mostSavesOneGame, color = activePlayer))
    g + geom_jitter() + labs(x = "Most Shots Against One", y = "Most Saves in One Game", title = "Shots Against One and Save in a Game", color = "Active Player")

![](Project-1_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

There is a general upward trend in the scatterplot. The current goalie
for the Capitals seems to fall right in line with goalies of the past.

The last statistics we will take a look at are penalty minutes. To do
this, we will create a boxplot of penalty minutes by the number of
seasons a player has played for the Washington Capitals. .

    g <- ggplot(joinedSeasons, aes(x = seasons, y = penaltyMinutes, group = seasons))
    g + geom_boxplot(color = "blue") + labs(x = "Number of Seasons", y = "Penalty Minutes", title = "Distribution of Penalties by Seasons")

![](Project-1_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

There is a clear trend here, the longer a player has stuck around, the
the longer they will end up sitting in the penalty box.
