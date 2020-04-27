#Load in packages I'll need

library(shiny)
library(tidyverse)
library(rvest)
library(xml2)
library(readxl)
library(janitor)
library(ggrepel)
library(ggimage)
library(RCurl)
library(gt)
library(broom)
options(scipen=999)

# read in data
Wins <- read_excel("raw-data/wins.xlsx") %>%
    filter(!is.na(ABBR)) %>%
    pivot_longer(-ABBR, names_to = "year", values_to = "Wins")

Battingaverage <- read_excel("raw-data/MLBbattingaverage.xlsx") %>%
    select (-Name) %>%
    pivot_longer(-ABBR, names_to = "year", values_to = "Battingaverage") 

Errors <- read_excel("raw-data/MLBerrors.xlsx") %>%
    select (-Team) %>%
    pivot_longer(-ABBR, names_to = "year", values_to = "Errors") 

Runs <- read_excel("raw-data/MLBRuns.xlsx") %>%
    select (-Name) %>%
    pivot_longer(-ABBR, names_to = "year", values_to = "Runs") 

Strikeouts_pitchers <- read_excel("raw-data/strikeoutsper9.xlsx") %>%
    select (-Name) %>%
    pivot_longer(-ABBR, names_to = "year", values_to = "Strikeouts_pitchers") 

Strikeouts_hitters <- read_excel("raw-data/strikeouts_hitters.xlsx") %>%
    select (-Name) %>%
    pivot_longer(-ABBR, names_to = "year", values_to = "Strikeouts_hitters") 

Attendance <- read_excel("raw-data/MLBattendance.xlsx") %>%
    select(-Name) %>%
    select(-max) %>%
    pivot_longer(-ABBR, names_to = "year", values_to = "Attendance") 

Walks <- read_excel("raw-data/Walks.xlsx") %>%
    pivot_longer(-ABBR, names_to = "year", values_to = "Walks") 

StolenBases <- read_excel("raw-data/StolenBases.xlsx") %>%
    pivot_longer(-ABBR, names_to = "year", values_to = "StolenBases") 

AllStars <- read_excel("raw-data/allstars.xlsx") %>%
    pivot_longer(-ABBR, names_to = "year", values_to = "AllStars") 

CityPopulation <- read_excel("raw-data/CityPopulation.xlsx") %>%
    select(-City) %>%
    pivot_longer(-ABBR, names_to = "year", values_to = "CityPopulation") 

Payroll <- read_excel("raw-data/Payroll.xlsx") %>%
    pivot_longer(-ABBR, names_to = "year", values_to = "Payroll") 

HomeRuns_pitchers <- read_excel("raw-data/HomeRuns.xlsx") %>%
    pivot_longer(-ABBR, names_to = "year", values_to = "HomeRuns_pitchers") 

HomeRuns_hitters<- read_excel("raw-data/HomeRunsHitters.xlsx") %>%
    pivot_longer(-ABBR, names_to = "year", values_to = "HomeRuns_hitters")

ERA <- read_excel("raw-data/ERA.xlsx") %>%
    pivot_longer(-ABBR, names_to = "year", values_to = "ERA") 

mlblogos <- read_excel("raw-data/mlblogosfinal.xlsx")
   
# join data on abbreviation and year
joined <- Wins %>%
    full_join (Battingaverage, by = c("ABBR" = "ABBR", "year" = "year")) %>%
    full_join(Errors, by = c("ABBR" = "ABBR", "year" = "year")) %>%
    full_join(Runs, by = c("ABBR" = "ABBR", "year" = "year")) %>%
    full_join(Strikeouts_pitchers, by = c("ABBR" = "ABBR", "year" = "year")) %>%
    full_join(Attendance, by = c("ABBR" = "ABBR", "year" = "year")) %>%
    full_join(Strikeouts_hitters, by = c("ABBR" = "ABBR", "year" = "year")) %>%
    full_join(Walks, by = c("ABBR" = "ABBR", "year" = "year")) %>%
    full_join(StolenBases, by = c("ABBR" = "ABBR", "year" = "year")) %>%
    full_join(AllStars, by = c("ABBR" = "ABBR", "year" = "year")) %>%
    full_join(CityPopulation, by = c("ABBR" = "ABBR", "year" = "year")) %>%
    full_join(Payroll, by = c("ABBR" = "ABBR", "year" = "year")) %>%
    full_join(HomeRuns_pitchers, by = c("ABBR" = "ABBR", "year" = "year")) %>%
    full_join(HomeRuns_hitters, by = c("ABBR" = "ABBR", "year" = "year")) %>%
    full_join(ERA, by = c("ABBR" = "ABBR", "year" = "year")) %>%
    full_join(mlblogos, by = c("ABBR" = "ABBR"))
    

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    navbarPage(
        "Analysis of MLB statistics",
        tabPanel(
            title = "Introduction",
            h3("Why Baseball?"),
            p("For many people, baseball is seen as a boring sport. It's too long, 
              too boring, and there isn't enough action. I, however, find every intracy of 
              baseball fascinating. The extra inning games, the heartbreaking losses, 
              and the jaw-dropping victories are unforgettable. The perfect games, no
              hitters, grand slams are remarkable, and I've been an avid fantasy 
              baseball player since 2007. A large part of what makes baseball so amazing is the statistics. 
              In other sports, there's the
              main stats: points, assists, field goals, etc. In baseball, there's almost
              an infinite number of ways to compare teams, compare players, and compare statistics."),
              p("What I hope to do in this project is looking into just some of the statistics that 
              factor into why one team wins and another doesn't, on a macro-scale."),
            p("Here is just one video showing why I love baseball."),
            br(),
            # Include link to embedded video
            HTML('<iframe width="1120" height="630" src="https://www.youtube.com/embed/lRTUIBVfLP4" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
            ),
        tabPanel(
            # Explain thought process behind project
            title = "Methodology",
            h3("By Scott Mahon"),
            p("This project aims to understand how certain statistics in the MLB
               influence a team's total number of wins per season. While there 
               are many stats I could've chosen, I tried to include stats I was
               most curious about or thought might have a particularly interesting
               relationship with the numbers of wins"),
            h3("Methodology"),
            p("To begin, I collected data from various websites and scraped the 
               data into R. A lot of the data was not accessible in premade data sets,
              so for almost all of the data, I scraped it into Excel files and exported
              the data as an .XLSX into R. There are many different dependent variables,
              which I explore by comparing them to the number of wins, a standard way
              to measure a team's success. The data is taken from 2010-2019, in an effort
              to follow modern baseball trends rather since the game and the way it 
              is played is constantly evolving."),
            h4("Explanation of Variables"),
            # Explain Meaning of variables
            p("Wins - Number of wins per season"),
            p("Batting Average - Average Batting Average per season"),
            p("Runs - Average number of runs scores per game "),
            p("Errors - Average number of errors per game "),
            p("Strikeouts (pitchers) - Average number of strikeouts by pitchers per
              9 innings pitched"),
            p("Strikeouts (hitters) - Average number of strikeouts by hitters per
              game"),
            p("Attendance - Average percent of capacity filled per season"),
            p("Walks - Average number of walks by hitters per game"),
            p("Stolen Bases - Average number of stolen bases per game"),
            p("All-Stars - Number of all-stars per team"),
            p("HomeRuns_pitchers - Average Number of home runs allowed
              by pitching staff per 9 innings"),
            p("HomeRuns_hitters - Average Number of home runs hit per game"),
            p("ERA - Average team ERA per season"),
            p("City Population - Population in the city most associated with team"),
            p("Payroll - Total Payroll at end of season")
        ),
    # Sidebar with a slider input for number of bins 
    tabPanel(
        title = "Stats",
        # create drop down menu
            selectInput("stat", "Choose a statistic", 
                    choices = c("Battingaverage",
                                   "Errors",
                                   "Runs",
                                   "Strikeouts_pitchers", 
                                   "Attendance", 
                                   "Strikeouts_hitters",
                                   "Walks", 
                                   "StolenBases",
                                   "AllStars", 
                                   "HomeRuns_pitchers",
                                   "HomeRuns_hitters",
                                   "ERA")),
            p("Select Year"),
        # create slider bar 
            sliderInput("year","Year", min = 2010, max = 2019, value = 2019, 
                        sep = ""),
        # output plot that corresponds to user input
            mainPanel(plotOutput("statistic")),
        ),
    tabPanel(
        # Explain meaning of data and trends
        title = "Analysis",
        h3("How do we analyze this data?"),
        p("In order to analyze the data, I create a linear regression model that
           compares wins to any selected stat. In essence, this shows how much 
           increasing the dependent varibale x (such as battingaverage, runs, etc.)
           will have on the total number of wins. Usually, linear regressions show 
           how a one unit increase in the dependent variable will impact the 
           independent variable (wins). However, since each MLB metric is measured 
           in an entirely different unit, the model would be essentially useless. For instance,
           this model would compare increasing the average season attendance by 1 person
           with increasing the average number of runs by 1 run per game. Obviously,
           it isn't accurate to compare these variables. Instead, we can scale all 
           of the data, which essentially uses standard deviations to make all of the
           results comparable. This table shows the results of running the linear model on each of 
           the dependent variables on the number of wins."),
        br(),
        # produce output gt table 
        sidebarPanel(tableOutput("correlation")),
        br(), 
        p("The most important takeaway from this table is comparing variables to each 
          other. After offsetting our data (which means we take each variable into
          account with the other), we can see that ERA and wins are the two most 
          strongly correlated with wins. Meaning that scoring runs and giving up less runs
          are the best ways to increase the team's number of wins. "),
        p("The way we specifically interpret these variables, is by saying, a one standard 
        deviation increase in the dependent variable will lead to the estimated
        standard deviation increase in wins. While we know that correlation does
           not mean causation, we can make educated guesses about these results. 
           For starters, it makes sense that runs seems to be the highest correlated
           variable to number of wins; if a team is scoring more often, odds are they 
           are winning more often. 
           Looking into the number of all-stars, we may think that 
           this variable increase a team's number of wins, but if we think about
           this more, we can probably conclude that the reverse is true. A team that
           wins more is more likely to have a lot of all-stars per team. This is a prime 
           example of correlation not meaning causation. 
           We also see that stolen bases has a very weak correlation. This means
           that increasing the number of stolen bases virtually has no impact on the 
           number of wins a team will have. We can understand the other variables by a 
           similar logic. If the estimate is negative, after offsetting, for instance errors, the relationship
           is negative, meaning that increasing the number of errors will decrease the 
           number of wins, which makes sense with our intuition.t We can use a similar 
          logic to interpret the remaining statistics."),
        p("In conclusion, we can see that at the end of the day, scoring more runs
          and giving up less runs is ultimately the best way to ensure wins; this is
          a metric that has been confirmed by sabermetics, a form of data analysis
          used to analyze the MLB. Scoring more runs, and giving up less runs, are 
          obviously influenced by a variety of variables, many of which I looked
          at including runs, homers, ERA, strikeouts, etc. We can see which of the variables
          have a stronger correlation than others.")
    ),
    tabPanel(
        title = "Big Market Teams",
        selectInput("bigmarket", "Choose a statistic", 
                    choices = c("Payroll", "CityPopulation")),
        p("Select Year"),
        sliderInput("year1","Year", min = 2010, max = 2019, value = 2019, 
                    sep = ""),
        mainPanel(plotOutput("bigmarketteams")),
        sidebarPanel(tableOutput("bigmarketteamsgt")),mainPanel(p("One common sentiment amongst MLB fans is that larger market teams, ie.
          teams that are located in bigger cities, attract more fans, and overall
          have more money to afford higher salries players inherently are more
          successful. Particularly, people estimate this, in addition to how
          'likable' a city is to players, will impact the success of a team. This
          hypothesis makes sense: teams in cities with more people will be able
          draw more fans, sell more gear, and get more money, which they can use to
          buy better players. We can see the results of this model using a linear
          regression to compare wins agains the size of the city and the total
          payroll for each team. While it is difficult to absolutely say that this correlation
          is a result of causation, I think this is definitely an indication that payroll
          plays a roll into the likelihood for a team to do well."))
    ),
    tabPanel(
        # provide user the option to look at specific teams' stats
        title = "By Team",
        h4("If you are interested in exploring how a team has performed for a 
           specific variable, this section will allow you to see the fluctuations
           by year."),
        # create drop down meny
        selectInput("ABBR", "Choose a team", 
                    choices = c("ARI","ATL", "BAL", "BOS", "CHC", "CHW", "CIN", 
                                "CLE", "COL", "DET", "HOU", "KAN", "LAA", "LAD",
                                "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", "PHI","PIT", 
                                "SDP", "SEA", "SFG", "STL", "TBR","TEX","TOR", "WSN")),
        # create another drop down menu
        selectInput("stat1", "Choose a statistic", 
                    choices = c("Battingaverage", "Errors", "Runs",
                                "Strikeouts_pitchers", "Attendance", "Wins",
                                "Strikeouts_hitters", "Walks", "StolenBases",
                                "AllStars", "CityPopulation", "Payroll", 
                                "HomeRuns_pitchers","HomeRuns_hitters", "ERA"
                    )),
        # output plot based on user input
        mainPanel(plotOutput("team"))),
    tabPanel(
        # Include information about myself and sourcing 
        title = "About",
        h3("Contact:"),
        p("Scott Mahon"),
        p("Harvard College Class of 2022"),
        p("Project for Gov 1005 by David Kane"),
        p("Email: scottmahon@college.harvard.edu"),
        p("GitHub: scottmahon"),
        h3("About Me"),
        p("My name is Scott Mahon, and I am a current Sophomore at Harvard College
          studying applied math focusing in economics and plan on getting a secondary  in computer
          science. I am originally from the suburbs of Chicago, and love travelling, 
          playing instruments, and watching baseball in my free time."),
        h3("Sources"),
        p("https://www.teamrankings.com/mlb/stats/"),
        p("https://www.usatoday.com/sports/mlb/salaries/"),
        p("https://www.macrotrends.net/cities/"),
        p("https://www.baseball-reference.com/leagues/MLB/index.shtml"),
        p("https://www.baseball-almanac.com/baseball_attendance.shtml")
    )
    ))
#title = paste("x in title$year")

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$statistic <- renderPlot({
        # use user input to adjust plot accordingly, based on year and stat input
        joined %>%
            filter (year == input$year) %>%
            ggplot(aes_string(x = input$stat, y = "Wins")) +
            # include logos that match each team respectively 
            geom_image(aes(image = URL), size = 0.05, asp = 2) +
            labs(x = input$stat, y = "Wins") +
            # create line of best fit 
            geom_smooth(method =lm, se = FALSE, color = "black") +
            theme_classic()
    })
    
    output$bigmarketteams <- renderPlot({
        joined %>%
            filter (year == input$year1) %>%
            ggplot(aes_string(x = input$bigmarket, y = "Wins")) +
            geom_image(aes(image = URL), size = 0.05, asp = 2) +
            labs(x = input$bigmarket, y = "Wins") +
            geom_smooth(method =lm, se = FALSE, color = "black") +
            theme_classic()
    })
    
    output$correlation <- renderTable({
        joined2 <- joined %>%
            # use scale to put dependent variables on similar scale to compare
            # create linear model to see correlations and compare variables
        lm(scale(Wins) ~ scale(Battingaverage) + scale(Errors) + 
               scale(Attendance) + scale(Runs) + scale(Strikeouts_pitchers) +
               scale(Strikeouts_hitters) + scale(Walks) + 
               scale(StolenBases) + scale(AllStars) + scale(HomeRuns_hitters) 
           + scale(HomeRuns_pitchers) + scale(ERA),
           data = .) %>%
        tidy(conf.int = TRUE) %>%
        select(term, estimate) %>%
        rename(
            "Variable" = term,
            "Estimate" = estimate) %>%
            mutate_if(is.numeric, round, 4) %>%
        gt()
    })
    
    output$bigmarketteamsgt <- renderTable({
        joined3 <- joined %>%
            # Use lm and scale to find correlations and compare
            lm(scale(Wins) ~ scale(CityPopulation) + scale(Payroll),
               data = .) %>%
            tidy(conf.int = TRUE) %>%
            select(term, estimate) %>%
            rename(
                "Variable" = term,
                "Estimate" = estimate) %>%
            mutate_if(is.numeric, round, 4) %>%
            gt()
    })
    
    output$team <- renderPlot({
        joined %>%
            # creative plot of count vs stats by year for input stat1
            filter(ABBR == input$ABBR) %>%
            ggplot(aes_string(x = "year", y = input$stat1)) +
            geom_point() + theme_classic() 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
