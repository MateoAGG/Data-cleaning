# Solow Growth Model Shiny App con PWT para Ecuador y China
# Basado en Solow (1956) y datos pre-cargados (pwt1001)
install.packages("reshape2")
library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)

# 0. Cargar datos PWT pre-descargados ----
pwt1001 <- read_excel("C:/Users/magarciagu/Downloads/pwt1001.xlsx", sheet = "Data") %>%
  mutate(
    k_eff_emp = ck / emp,
    y_eff_emp = cgdpo / emp,
    period = year - min(year, na.rm = TRUE)
  ) %>%
  filter(country %in% c("Ecuador", "China"))

# 1. Funciones del modelo per effective worker ----
f_per_eff <- function(k, alpha) k^alpha
update_k_eff <- function(k, s, n, g, d, alpha) (s * f_per_eff(k, alpha) + (1 - d) * k) / ((1 + n) * (1 + g))
compute_steady_state <- function(s, n, g, d, alpha) {
  denom <- n + g + d + n * g
  (s / denom)^(1 / (1 - alpha))
}
simulate_solow <- function(s, n, d, g, alpha, k0, periods) {
  k <- numeric(periods + 1); y_e <- numeric(periods + 1);
  k[1] <- k0; y_e[1] <- f_per_eff(k0, alpha)
  for (t in seq_len(periods)) {
    k[t+1] <- update_k_eff(k[t], s, n, g, d, alpha)
    y_e[t+1] <- f_per_eff(k[t+1], alpha)
  }
  data.frame(period = 0:periods, k_eff = k, y_eff = y_e)
}
generate_phase <- function(s, n, g, d, alpha, k_max) {
  k_vals <- seq(0, k_max, length.out = 200)
  data.frame(
    k = k_vals,
    investment = s * f_per_eff(k_vals, alpha),
    replacement = (n + g + d + n * g) * k_vals
  )
}

# 2. UI ----
ui <- fluidPage(
  titlePanel("Modelo de Solow: Ecuador vs China"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "País:", choices = c("Ecuador","China")),
      numericInput("s",     "Tasa de ahorro (s)",           value = 0.2, min = 0, max = 1, step = 0.01),
      numericInput("n",     "Crec. poblacional (n)",        value = 0.01, min = 0, max = 1, step = 0.001),
      numericInput("d",     "Depreciación (δ)",             value = 0.05, min = 0, max = 1, step = 0.001),
      numericInput("g",     "Progreso tecnológico (g)",     value = 0.02, min = 0, max = 1, step = 0.001),
      numericInput("alpha", "Elasticidad del capital (α)",  value = 0.3, min = 0, max = 1, step = 0.01),
      numericInput("k0",    "k₀ teórico (capital per eff)", value = 1,   min = 0.001, max = 100, step = 0.1),
      numericInput("periods","Períodos a simular",           value = 50,  min = 1, max = 200, step = 1),
      checkboxInput("showEmpirical","Comparar con datos PWT", value = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Serie Temporal", plotOutput("timePlot", height = "400px")),
        tabPanel("Espacio de Fases", plotOutput("phasePlot", height = "400px")),
        tabPanel("Consumo vs k", plotOutput("consumptionPlot", height = "400px"))
      )
    )
  )
)

# 3. Server ----
server <- function(input, output) {
  # Datos empíricos filtrados
  empData <- reactive({ pwt1001 %>% filter(country == input$country) })
  # Simulación teórica
  simData <- reactive({ simulate_solow(input$s, input$n, input$d, input$g, input$alpha, input$k0, input$periods) })
  
  # Serie Temporal
  output$timePlot <- renderPlot({
    df_th <- simData()
    p <- ggplot(df_th, aes(x = period)) +
      geom_line(aes(y = k_eff, color = 'k teórico')) +
      geom_line(aes(y = y_eff, color = 'y teórico'))
    if (input$showEmpirical) {
      df_emp <- empData()
      p <- p +
        geom_point(data = df_emp, aes(x = period, y = k_eff_emp, shape = 'k empírico')) +
        geom_point(data = df_emp, aes(x = period, y = y_eff_emp, shape = 'y empírico'))
    }
    p + labs(title = paste('Serie Temporal:', input$country), x = 'Período', y = 'Valor') +
      scale_color_manual('', values = c('k teórico'='blue','y teórico'='red')) +
      scale_shape_manual('', values = c('k empírico'=16,'y empírico'=17)) + theme_minimal()
  })
  
  # Espacio de Fases con flechas
  output$phasePlot <- renderPlot({
    df_th <- simData()
    pd <- generate_phase(input$s, input$n, input$g, input$d, input$alpha, max(df_th$k_eff)*1.2)
    ggplot(pd, aes(x = k)) +
      geom_path(aes(y = investment), linetype='dashed', arrow=arrow(type='open', length=unit(0.1,'inches')))+
      geom_path(aes(y = replacement), linetype='dotted', arrow=arrow(type='open', length=unit(0.1,'inches')))+
      labs(title=paste('Espacio de Fases:', input$country), x='k', y='Flujo') + theme_minimal()
  })
  
  # Consumo vs k
  output$consumptionPlot <- renderPlot({
    pd <- generate_phase(input$s, input$n, input$g, input$d, input$alpha, max(simData()$k_eff)*1.2)
    pd$cons <- (1-input$s)*f_per_eff(pd$k, input$alpha)
    ggplot(pd, aes(x=k, y=cons)) + geom_line() +
      labs(title=paste('Consumo per Effective Worker:', input$country), x='k', y='Consumo') + theme_minimal()
  })
}

# 4. Ejecutar la App ----
shinyApp(ui = ui, server = server)





# # first try -------------------------------------------------------------
# 1. Funciones del modelo per effective worker ----
f_per_eff <- function(k, alpha) {
  k^alpha
}

update_k_eff <- function(k, s, n, g, d, alpha) {
  (s * f_per_eff(k, alpha) + (1 - d) * k) / ((1 + n) * (1 + g))
}

compute_steady_state <- function(s, n, g, d, alpha) {
  denom <- n + g + d + n * g
  (s / denom)^(1 / (1 - alpha))
}

simulate_solow <- function(s, n, d, g, alpha, k0, periods) {
  k <- numeric(periods + 1)
  y_e <- numeric(periods + 1)
  A  <- numeric(periods + 1)
  k[1] <- k0
  A[1] <- 1
  y_e[1] <- f_per_eff(k0, alpha)
  for (t in seq_len(periods)) {
    k[t + 1] <- update_k_eff(k[t], s, n, g, d, alpha)
    A[t + 1] <- A[t] * (1 + g)
    y_e[t + 1] <- f_per_eff(k[t + 1], alpha)
  }
  time_df <- data.frame(period = 0:periods, k_eff = k, y_eff = y_e, tech = A)
  return(time_df)
}

# Generate phase diagram data ----
generate_phase <- function(s, n, g, d, alpha, k_max) {
  k_vals <- seq(0, k_max, length.out = 200)
  inv  <- s * f_per_eff(k_vals, alpha)
  repl <- (n + g + d + n * g) * k_vals
  data.frame(k = k_vals, investment = inv, replacement = repl)
}

# 2. UI ----
ui <- fluidPage(
  titlePanel("Modelo de Solow por Effective Worker (1956)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("s",     "Tasa de ahorro (s)",             value = 0.2, min = 0, max = 1,   step = 0.01),
      numericInput("n",     "Crecimiento poblacional (n)",    value = 0.01, min = 0, max = 1,   step = 0.001),
      numericInput("d",     "Depreciación (δ)",               value = 0.05, min = 0, max = 1,   step = 0.001),
      numericInput("g",     "Progreso tecnológico (g)",       value = 0.02, min = 0, max = 1,   step = 0.001),
      numericInput("alpha", "Elasticidad del capital (α)",    value = 0.3, min = 0, max = 1,   step = 0.01),
      numericInput("k0",    "Capital per effective worker k₀", value = 1,    min = 0.001, max = 100, step = 0.1),
      numericInput("periods","Períodos a simular",             value = 100,  min = 1,   max = 1000, step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Serie Temporal", plotOutput("timePlot", height = "400px")),
        tabPanel("Espacio de Fases", plotOutput("phasePlot", height = "400px")),
        tabPanel("Consumo vs k", plotOutput("consumptionPlot", height = "400px"))
      )
    )
  )
)

# 3. Server ----
server <- function(input, output) {
  sim <- reactive({
    simulate_solow(s     = input$s,
                   n     = input$n,
                   d     = input$d,
                   g     = input$g,
                   alpha = input$alpha,
                   k0    = input$k0,
                   periods = input$periods)
  })
  
  output$timePlot <- renderPlot({
    df <- sim()
    k_star <- compute_steady_state(input$s, input$n, input$g, input$d, input$alpha)
    ggplot(df, aes(x = period)) +
      geom_line(aes(y = k_eff, color = 'k per effective worker'), size = 1) +
      geom_line(aes(y = y_eff, color = 'y per effective worker'), size = 1) +
      geom_hline(yintercept = k_star, linetype = 'longdash', color = 'black') +
      annotate("text", x = max(df$period) * 0.7, y = k_star, label = 'k*', vjust = -1) +
      labs(title = 'Serie Temporal con Estado Estacionario', x = 'Período', y = 'Valor', color = '') +
      theme_minimal()
  })
  
  output$phasePlot <- renderPlot({
    df <- sim()
    k_star <- compute_steady_state(input$s, input$n, input$g, input$d, input$alpha)
    pd <- generate_phase(input$s, input$n, input$g, input$d, input$alpha, max(df$k_eff) * 1.2)
    ggplot(pd, aes(x = k)) +
      geom_line(aes(y = investment), linetype = 'dashed', size = 1, color = 'blue') +
      geom_line(aes(y = replacement), linetype = 'dotted', size = 1, color = 'red') +
      geom_point(aes(x = k_star, y = input$s * f_per_eff(k_star, input$alpha)), color = 'black', size = 3) +
      labs(title = 'Espacio de Fases con Estado Estacionario', x = 'k', y = 'Flujo') +
      theme_minimal()
  })
  
  output$consumptionPlot <- renderPlot({
    df <- sim()
    pd <- generate_phase(input$s, input$n, input$g, input$d, input$alpha, max(df$k_eff) * 1.2)
    pd$consumption <- (1 - input$s) * f_per_eff(pd$k, input$alpha)
    k_star <- compute_steady_state(input$s, input$n, input$g, input$d, input$alpha)
    ggplot(pd, aes(x = k, y = consumption)) +
      geom_line(size = 1, color = 'green') +
      geom_vline(xintercept = k_star, linetype = 'longdash') +
      annotate('text', x = k_star, y = max(pd$consumption) * 0.8, label = 'k*', angle = 90, vjust = -0.5) +
      labs(title = 'Consumo per Effective Worker vs k', x = 'k', y = 'Consumo') +
      theme_minimal()
  })
}

# 4. Ejecutar la App ----
shinyApp(ui = ui, server = server)

