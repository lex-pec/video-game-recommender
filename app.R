library(shiny)
library(data.table)
library(tidyverse)
library(tidyr)
library(stringr)
library(dplyr)
library(e1071)

games <- read.table("data_game.csv", fileEncoding = "UTF-8", header = TRUE)
user <- read.table("data_user.csv", fileEncoding = "UTF-8", header = TRUE)
names(user)[names(user)=="Mark"]<-"mean"

ui <- navbarPage("Do you wanna play a game?",
                 tabPanel("Random Game",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("platform", label = "Выберите платформу, на которой будете играть:",
                                          choices = unique(as.character(games$Platform)), selected = "PC"),
                              hr(),
                              selectInput("genre", label = "Выберите жанр игры:",
                                          choices = c("Action","General","First.Person","Shooter",
                                                      "Traditional","Role.Playing","Adventure","Arcade","Sci.Fi",
                                                      "Fantasy","Open.World","Strategy","Miscellaneous","Historic",
                                                      "2D","Tactical","Third.Person","Real.Time","Sports",
                                                      "Racing","Platformer","Modern","Action.Adventure","Team"), selected = "Action"),
                              hr(),
                              radioButtons("type", label = "Хотите играть один или с друзьями?",
                                           choices = c("Я волк одиночка", "Хочу играть с друзьями"))),
                            mainPanel(
                              titlePanel("Попробуйте сыграть в эти игры:"),
                              tableOutput("random_table")
                            ))),
                 tabPanel("Special for ME",
                          fluidPage(
                            titlePanel("Оцените предложенные игры, чтобы получить рекомендацию"),
                            fluidRow(
                              column(4, h3("Выберите платформу"),
                                     wellPanel(
                                       selectInput("platform1", label = "",
                                                   choices = unique(as.character(games$Platform)), selected = "PC"))),
                              column(4, h3("Играете один?"),
                                     wellPanel(
                                       radioButtons("type1", label = "",
                                                    choices = c("Я волк одиночка", "Хочу играть с друзьями")))),
                              column(4, h3("Сколько рекомендовать игр?"),
                                     wellPanel(
                                       sliderInput("number", label = "",
                                                   min=2, max=10, value=5)))),
                            hr(),
                            fluidRow(
                              column(4, h3("Выберите жанр игры:"),
                                     wellPanel(
                                       selectInput("genre1", label = "",
                                                   choices = c("Action","General","First.Person","Shooter",
                                                               "Traditional","Role.Playing","Adventure","Arcade","Sci.Fi",
                                                               "Fantasy","Open.World","Strategy","Miscellaneous","Historic",
                                                               "2D","Tactical","Third.Person","Real.Time","Sports",
                                                               "Racing","Platformer","Modern","Action.Adventure","Team"), selected = "Action"),
                                       selectInput("genre2", label = "",
                                                   choices = c("Action","General","First.Person","Shooter",
                                                               "Traditional","Role.Playing","Adventure","Arcade","Sci.Fi",
                                                               "Fantasy","Open.World","Strategy","Miscellaneous","Historic",
                                                               "2D","Tactical","Third.Person","Real.Time","Sports",
                                                               "Racing","Platformer","Modern","Action.Adventure","Team"), selected = "General"),
                                       selectInput("genre3", label = "",
                                                   choices = c("Action","General","First.Person","Shooter",
                                                               "Traditional","Role.Playing","Adventure","Arcade","Sci.Fi",
                                                               "Fantasy","Open.World","Strategy","Miscellaneous","Historic",
                                                               "2D","Tactical","Third.Person","Real.Time","Sports",
                                                               "Racing","Platformer","Modern","Action.Adventure","Team"), selected = "First.Person"),
                                       selectInput("genre4", label = "",
                                                   choices = c("Action","General","First.Person","Shooter",
                                                               "Traditional","Role.Playing","Adventure","Arcade","Sci.Fi",
                                                               "Fantasy","Open.World","Strategy","Miscellaneous","Historic",
                                                               "2D","Tactical","Third.Person","Real.Time","Sports",
                                                               "Racing","Platformer","Modern","Action.Adventure","Team"), selected = "Shooter"),
                                       selectInput("genre5", label = "",
                                                   choices = c("Action","General","First.Person","Shooter",
                                                               "Traditional","Role.Playing","Adventure","Arcade","Sci.Fi",
                                                               "Fantasy","Open.World","Strategy","Miscellaneous","Historic",
                                                               "2D","Tactical","Third.Person","Real.Time","Sports",
                                                               "Racing","Platformer","Modern","Action.Adventure","Team"), selected = "Racing")
                                     )),
                              column(4, h3("Выберите игры:"),
                                     wellPanel(
                                       uiOutput("game_select1"),
                                       uiOutput("game_select2"),
                                       uiOutput("game_select3"),
                                       uiOutput("game_select4"),
                                       uiOutput("game_select5")
                                     )),
                              column(4, h3("Поставьте им оценку:"),
                                     wellPanel(
                                       sliderInput("mark1","",
                                                   min = 1, max = 10, value = 1),
                                       sliderInput("mark2","",
                                                   min = 1, max = 10, value = 1),
                                       sliderInput("mark3","",
                                                   min = 1, max = 10, value = 1),
                                       sliderInput("mark4","",
                                                   min = 1, max = 10, value = 1),
                                       sliderInput("mark5","",
                                                   min = 1, max = 10, value = 1)
                                     ))),
                            hr(),
                            h3("<- Рекомендация в консоле! ;("),
                            tableOutput("norandom_table")
                          )))
######################################################

######################################################

server <- function(input, output) {
  ####################RANDOM####################
  filtered_random <- reactive({games %>% 
      filter(Platform %in% input$platform) %>% 
      filter(get(input$genre)==1) %>% 
      filter(Single_player==ifelse(input$type=="Я волк одиночка", 1 , 0)) %>% 
      select(Title, Metascore, Avg_Userscore) %>% 
      top_n(5)
  })
  output$random_table <- renderTable({filtered_random()})
  
  ################RECOMMENDATION################
  set.seed(123)
  
  ###Filtration for buttons
  filtered_rec1 <- reactive({games %>% filter(Platform %in% input$platform1) %>% 
      filter(Single_player==ifelse(input$type1=="Я волк одиночка", 1 , 0)) %>%
      filter(get(input$genre1)==1) %>% 
      select(-Platform,-Single_player)})
  filtered_rec2 <- reactive({games %>% filter(Platform %in% input$platform1) %>% 
      filter(Single_player==ifelse(input$type1=="Я волк одиночка", 1 , 0)) %>% 
      filter(get(input$genre2)==1) %>% 
      select(-Platform,-Single_player)})
  filtered_rec3 <- reactive({games %>% filter(Platform %in% input$platform1) %>% 
      filter(Single_player==ifelse(input$type1=="Я волк одиночка", 1 , 0)) %>% 
      filter(get(input$genre3)==1) %>% 
      select(-Platform,-Single_player)})
  filtered_rec4 <- reactive({games %>% filter(Platform %in% input$platform1) %>% 
      filter(Single_player==ifelse(input$type1=="Я волк одиночка", 1 , 0)) %>% 
      filter(get(input$genre4)==1) %>% 
      select(-Platform,-Single_player)})
  filtered_rec5 <- reactive({games %>% filter(Platform %in% input$platform1) %>% 
      filter(Single_player==ifelse(input$type1=="Я волк одиночка", 1 , 0)) %>% 
      filter(get(input$genre5)==1) %>% 
      select(-Platform,-Single_player)})
  
  ###UI buttons output
  output$game_select1 <- renderUI({
    filtered1<-filtered_rec1()
    selectInput("input_game1", "",
                choices = filtered1$Title,
                selected = sample(filtered1$Title,1))})  
  output$game_select2 <- renderUI({
    filtered2<-filtered_rec2() 
    selectInput("input_game2", "",
                choices = filtered2$Title,
                selected = sample(filtered2$Title,1))})
  output$game_select3 <- renderUI({
    filtered3<-filtered_rec3() 
    selectInput("input_game3", "",
                choices = filtered3$Title,
                selected = sample(filtered3$Title,1))})
  output$game_select4 <- renderUI({
    filtered4<-filtered_rec4()
    selectInput("input_game4", "",
                choices = filtered4$Title,
                selected = sample(filtered4$Title,1))})
  output$game_select5 <- renderUI({
    filtered5<-filtered_rec5() 
    selectInput("input_game5", "",
                choices = filtered5$Title,
                selected = sample(filtered5$Title,1))})
  
  ###Filtration for recommendation
  games_for_rec <- reactive({
    games %>% 
      filter(Platform %in% input$platform1) %>% 
      filter(Single_player==ifelse(input$type1=="Я волк одиночка", 1 , 0)) %>% 
      select(-Metascore,-Avg_Userscore,-Platform,-Single_player)
  })
  
  user_for_rec <- reactive({
    games_for <- games_for_rec()
    usergame <- c(input$input_game1,input$input_game2,input$input_game3,input$input_game4,input$input_game5)
    gid <- games_for$gid[match(usergame, games_for$Title)]
    mean <- as.integer(c(input$mark1, input$mark2, input$mark3, input$mark4, input$mark5))
    user_id <- max(user$uid)+1
    uid <- as.integer(c(user_id,user_id,user_id,user_id,user_id))
    user_all <- data.frame(gid,uid,mean)
    user <- full_join(user,user_all)
    user
  })
  
  ###Recommendation
  suggestion <- reactive({
    clusterGames<-function(gameinfo){
      set.seed(123)
      i<-1
      #уберём пока идентификационный номер и название игры
      gameinfo<-as.data.frame(gameinfo)[,c(-1,-2)]
      repeat {
        set.seed(123)
        #построим 2 kmean модели, начнём с 2 и 3 кластеров и будем повторять, пока dss < 0.2
        i <- i + 1
        gameCluster<-cmeans(gameinfo,i,100,verbose=TRUE,method="cmeans",m=2)
        gameCluster2<-cmeans(gameinfo,i+1,100,verbose=TRUE,method="cmeans",m=2)
        #decision criterion
        dss<-((gameCluster$withinerror-gameCluster2$withinerror)/gameCluster$withinerror)
        #exit if dss < 0.2
        if (dss < 0.01) break
      }
      return(gameCluster)
    }
    
    getUserInfo<-function(dat,id){
      #отбираем пользователя, указанного в функции, при этом созраняем колонки gid и
      #rating в gamedata
      a<-subset(dat, uid==id,select=c(gid, mean))
      # создаём колонку с кластерами
      cluster<-0
      activeUser <- data.frame( a[order(a$gid),] ,cluster)
      return(activeUser)
    }
    
    setUserGameCluster<-function(gameCluster, activeUser){
      # создадим временный датасет в котором свяжем полученные кластеры с gids игр
      df1<- data.frame(cbind(gameinfo$gid, clusterNum = gameCluster$cluster))
      names(df1)<-c("game_id", "cluster")
      #свяжем кластеры с пользователем
      activeUser$cluster<-df1[match(activeUser$gid, df1$game_id),2]
      return(activeUser)
    }
    
    getMeanClusterRating<-function(movieCluster, activeUser){
      #aggregate() нужна чтоб определить средние значения оценок для каждого кластера в оригинальном датасете
      like<-aggregate(activeUser$mean, by=list(cluster=activeUser$cluster), mean)
      #здесь чуть другой подход: Если максимальная средняя оценка ниже трёх функция создаёт дамми со значением 0
      if(max(like$x)<3){
        like<-as.vector(0)
        #в другом случае она выводит количество кластеров максимальной средней величины
      } else{
        like<-as.vector(t(max(subset(like, x>=3, select=cluster))))
      }
      return(like)
    }
    
    getGoodGames<-function(like, gameCluster, gameinfo){
      # ещё один временный датасет необходимый для получения списка всех игр и их кластеров
      df1<- data.frame(cbind(gameinfo$gid, clusterNum = gameCluster$cluster))
      names(df1)<-c("game_id", "cluster")
      #если переменная like имеет значение 0, то она выбирает рандомно 100 игр
      if(like==0){
        recommend<-gameinfo[sample.int(n = dim(gameinfo)[1], size = 100), 1]
      }
      # в другом случае выбираются все фильмы из кластера с максимальным значением средних оценок
      else{
        recommend<-as.vector(t(subset(df1, cluster==like, select=game_id)))
      }
      return(recommend)
    }
    
    getRecommendedGames<-function(gameinfo, gamedata, uid){
      # вызываем все созданные функции
      gameCluster<-clusterGames(gameinfo)
      activeUser<-getUserInfo(gamedata, uid)
      activeUser<-setUserGameCluster(gameCluster, activeUser)
      like<-getMeanClusterRating(gameCluster, activeUser)
      recommend<-getGoodGames(like, gameCluster, gameinfo)
      # выбираем только игры, в которые ещё не играл пользователь
      recommend<-recommend[-activeUser$gid]
      # добавим название игры
      gametitle<-gameinfo[match(recommend,gameinfo$gid),2]
      recommend<-data.frame(recommend,gametitle)
      return(recommend)
    }
    
    suggestGames<-function(gameinfo, gamedata, uid, no_games){
      #получаем рекомендацию
      suggestions = getRecommendedGames(gameinfo, gamedata, uid)
      #выбираем необходимое количество рекомендаций
      suggestions = suggestions[1:no_games,]
      #отображаем предложения без названий строк или колонок
      write.table(suggestions[2], row.names = FALSE, col.names = FALSE)
    }
    gameinfo <- games_for_rec()
    gamedata <- user_for_rec()
    suggest <- data.frame(suggestGames(gameinfo, gamedata, max(gamedata$uid), input$number))
    suggest
  })
  
  ###Final table
  output$norandom_table <- renderTable({
    suggestion()
  })
}

shinyApp(ui = ui, server = server)