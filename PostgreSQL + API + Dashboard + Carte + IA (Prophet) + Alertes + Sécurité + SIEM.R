#####################################################################
# SYSTEME EPIDEMIOLOGIQUE ULTRA-PRO (TYPE OMS)
# R + PostgreSQL + API + IA + CARTE + ALERTES + SECURITE
#####################################################################

# ==============================
# 1. LIBRAIRIES
# ==============================
packages <- c(
  "DBI","RPostgres","httr","jsonlite",
  "tidyverse","lubridate","zoo",
  "shiny","shinydashboard","plotly",
  "leaflet","prophet","openssl"
)

installed <- packages %in% installed.packages()
if(any(!installed)) install.packages(packages[!installed])

library(DBI); library(RPostgres)
library(httr); library(jsonlite)
library(tidyverse); library(lubridate); library(zoo)
library(shiny); library(shinydashboard); library(plotly)
library(leaflet); library(prophet); library(openssl)

# ==============================
# 2. SECURITE (HASH MOT DE PASSE)
# ==============================
hash_password <- function(pwd){
  sha256(pwd)
}

# Exemple utilisateur
users <- data.frame(
  username = "admin",
  password = hash_password("admin123")
)

auth_user <- function(user, pwd){
  hashed <- hash_password(pwd)
  any(users$username == user & users$password == hashed)
}

# ==============================
# 3. CONNEXION POSTGRESQL
# ==============================
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "epidemiologie",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "password"
)

# ==============================
# 4. TABLE
# ==============================
dbExecute(con, "
CREATE TABLE IF NOT EXISTS cas_epidemio (
  id SERIAL PRIMARY KEY,
  date DATE,
  region TEXT,
  lat FLOAT,
  lon FLOAT,
  cas INTEGER,
  deces INTEGER,
  gueris INTEGER
);
")

# ==============================
# 5. API (SIMULATION / REEL)
# ==============================
url <- "https://disease.sh/v3/covid-19/historical/all?lastdays=60"
res <- GET(url)

data_api <- fromJSON(content(res, "text"))

df <- data.frame(
  date = as.Date(names(data_api$cases), "%m/%d/%y"),
  cas = as.numeric(data_api$cases),
  deces = as.numeric(data_api$deaths),
  gueris = as.numeric(data_api$recovered),
  region = "Goma",
  lat = -1.6788,
  lon = 29.2228
)

# ==============================
# 6. INSERTION DB
# ==============================
dbWriteTable(con, "cas_epidemio", df, append = TRUE, row.names = FALSE)

# ==============================
# 7. LECTURE + FEATURES
# ==============================
data <- dbGetQuery(con, "SELECT * FROM cas_epidemio")

data <- data %>%
  mutate(
    taux_mortalite = deces / cas,
    rolling = rollmean(cas, 7, fill = NA),
    anomalie = ifelse(cas > rolling * 1.5, 1, 0)
  )

# ==============================
# 8. IA PREDICTIVE (PROPHET)
# ==============================
df_prophet <- data %>% select(ds = date, y = cas)

model <- prophet(df_prophet)

future <- make_future_dataframe(model, periods = 30)
forecast <- predict(model, future)

# ==============================
# 9. ALERTES AUTOMATIQUES
# ==============================
if(sum(data$anomalie, na.rm = TRUE) > 5){
  cat("ALERTE CRITIQUE: Epidemie detectee\n")
  
  # Exemple Telegram
  # POST("https://api.telegram.org/botTOKEN/sendMessage",
  #      body = list(chat_id="ID", text="Alerte epidemie"))
}

# ==============================
# 10. DASHBOARD PRO
# ==============================
ui <- dashboardPage(
  dashboardHeader(title = "OMS - Surveillance Epidemiologique"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName="dash"),
      menuItem("Carte", tabName="map"),
      menuItem("IA Prediction", tabName="ia")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # KPI + GRAPHIQUES
      tabItem(tabName="dash",
              fluidRow(
                valueBoxOutput("cas"),
                valueBoxOutput("deces"),
                valueBoxOutput("alerte")
              ),
              fluidRow(
                box(plotlyOutput("courbe"), width=12)
              )
      ),
      
      # CARTE
      tabItem(tabName="map",
              leafletOutput("map", height=600)
      ),
      
      # IA
      tabItem(tabName="ia",
              plotOutput("forecast")
      )
    )
  )
)

server <- function(input, output){
  
  output$cas <- renderValueBox({
    valueBox(sum(data$cas), "Cas", color="blue")
  })
  
  output$deces <- renderValueBox({
    valueBox(sum(data$deces), "Décès", color="red")
  })
  
  output$alerte <- renderValueBox({
    valueBox(sum(data$anomalie), "Alertes", color="yellow")
  })
  
  output$courbe <- renderPlotly({
    p <- ggplot(data, aes(x=date, y=cas)) + geom_line()
    ggplotly(p)
  })
  
  output$map <- renderLeaflet({
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(~lon, ~lat,
                       radius = ~cas/100,
                       popup = ~paste("Cas:", cas))
  })
  
  output$forecast <- renderPlot({
    plot(model, forecast)
  })
}

# Lancer
# shinyApp(ui, server)

# ==============================
# 11. SIEM SANTE (LOG SIMULATION)
# ==============================
logs <- data.frame(
  time = Sys.time(),
  type = "Epidemie",
  severity = ifelse(data$anomalie == 1, "HIGH", "LOW")
)

print(logs)

# ==============================
# 12. FERMETURE DB
# ==============================
dbDisconnect(con)


