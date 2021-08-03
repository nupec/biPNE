library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(lubridate)

Meta1  <- readr::read_rds("data/Meta1.rds")

ui <- dashboardPage(
  dashboardHeader(title = HTML("PNE-Municípios"),
                  disable = FALSE,
                  titleWidth  = 550),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Meta 1",  tabName = "meta1", icon = icon("fas fa-chevron-right"))
      # menuItem("Meta 2",  tabName = "meta2", icon = icon("fas fa-chevron-right")),
      # menuItem("Meta 3",  tabName = "meta3", icon = icon("fas fa-chevron-right")),
      # menuItem("Meta 4",  tabName = "meta4", icon = icon("fas fa-chevron-right")),
      # menuItem("Meta 5",  tabName = "meta5", icon = icon("fas fa-chevron-right")),
      # menuItem("Meta 6",  tabName = "meta6", icon = icon("fas fa-chevron-right")),
      # menuItem("Meta 7",  tabName = "meta7", icon = icon("fas fa-chevron-right")),
      # menuItem("Meta 8",  tabName = "meta8", icon = icon("fas fa-chevron-right")),
      # menuItem("Meta 9",  tabName = "meta9", icon = icon("fas fa-chevron-right")),
      # menuItem("Meta 10", tabName = "meta10", icon = icon("fas fa-chevron-right"))
    )
  ),
  dashboardBody(
   tabItems(
      #Pagina 1
      tabItem(
        tabName = "meta1",
        fluidRow(
          column(
            width = 12,
            h1("Meta 1: Universalizar, até 2016, a educação infantil na pré-escola para as crianças
de 4 (quatro) a 5 (cinco) anos de idade e ampliar a oferta de educação infantil em
creches, de forma a atender, no mínimo, 50% (cinquenta por cento) das crianças de
até 3 (três) anos até o final da vigência deste PNE",
    style = "font-size: 25px; color:Black")
    )

),
    hr(style = "border-top: 1px solid black;"), # tag horizontal row style para mudar o css
    br(),

# Graficos ----------------------------------------------------------------
    fluidRow(
        box(
          width = 12,
          fluidRow(
            column(
              width = 4,
              selectInput(
                "reg", "Apenas Região Norte",
                choices = "Norte",
                selected = "Norte"
               )
              ),
            column(
              width = 4,
              selectInput(
                "uf", "Selecione um estado",
                choices = Meta1 |> pull(nome_uf) |> unique()
                |> sort(),
                selected = "Amazonas"
              )
            ),

          column(
            width = 4,
            selectInput(
              "municipio",
              "Selecione o município",
              choices = "",
              selected = "Carregando..."
                )
              ),
              br(),
              box(
                  width = 4,
                  title = "Indicador 1B: Matrícula em Creche (0 a 3 anos)",
                  solidHeader = TRUE, #cabeçalho colorido
                  status = "primary",  # para mudar a cor igual do anterior
                  shinycssloaders::withSpinner(
                    plotOutput("graf_indicador1b")

                )
              ),
              br(),
              box(
                width = 4,
                title = "Indicador 1A: Matrícula em Pré-Escola (4 e 5 anos)",
                solidHeader = TRUE, #cabeçalho colorido
                status = "primary",  # para mudar a cor igual do anterior
                shinycssloaders::withSpinner(
                  plotOutput("graf_indicador1a")

                )
              ),
          br(),
          box(
            width = 4,
            title = "Meta: Matrícula de 0 e 5 anos",
            solidHeader = TRUE, #cabeçalho colorido
            status = "primary",  # para mudar a cor igual do anterior
            shinycssloaders::withSpinner(
              plotOutput("graf_meta1")

            )
          )
            )
          ),
        # tabelas -----------------------------------------------------------------

        tabItem(
          br(),
          fluidRow(
            infoBoxOutput("num_pop0a3",
                          width = 3),
            infoBoxOutput("num_mat0a3",
                          width = 3),
            infoBoxOutput("num_pop4e5",
                          width = 3),
            infoBoxOutput("num_mat4e5",
                          width = 3)
          ),

        )
        ,

        )
      )
    )
  )
)


server <- function(input, output, session) {

  Meta1  <- readr::read_rds("data/Meta1.rds")



  # Necessário

  observe({
    municipios <- Meta1 |>
      filter(nome_uf == isolate(input$uf)) |>
      pull("nome_municipio") |>
      unique() |> sort()

    updateSelectInput(
      session,
      "municipio",
      choices = municipios
    )
  })


# Graficos ----------------------------------------------------------------

  # Gráfico 1B
  output$graf_indicador1b <- renderPlot({

    Meta1 |> filter(nome_uf == isolate(input$uf),
                    nome_municipio == input$municipio) |>
      ggplot() +
      geom_col(aes(x = ano, y = indice1b, fill = indice1b)) +
      coord_cartesian(ylim = c(0, 1)) +
      # labs(
      #   x = "ano",
      #   y = "índice 1a",
      #   color = "índice 1b",
      #   title = "Plano Nacional de Educação",
      #   subtitle = "Índice 1B"
      # )  +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
    }  )

  # Gráfico 1A

  output$graf_indicador1a <- renderPlot({

    Meta1 |> filter(nome_uf == isolate(input$uf),
                    nome_municipio == input$municipio) |>
      ggplot() +
      geom_col(aes(x = ano, y = indice1a, fill = indice1a)) +
      coord_cartesian(ylim = c(0, 1)) +
      # labs(
      #   x = "ano",
      #   y = "índice 1a",
      #   color = "índice 1b",
      #   title = "Plano Nacional de Educação",
      #   subtitle = "Índice 1B"
      # )  +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
  }  )

  # Meta 1

  output$graf_meta1 <- renderPlot({

    Meta1 |> filter(nome_uf == isolate(input$uf),
                    nome_municipio == input$municipio) |>
      ggplot() +
      geom_col(aes(x = ano, y = meta1, fill = meta1)) +
      coord_cartesian(ylim = c(0, 1)) +
      # labs(
      #   x = "ano",
      #   y = "índice 1a",
      #   color = "índice 1b",
      #   title = "Plano Nacional de Educação",
      #   subtitle = "Índice 1B"
      # )  +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
  }  )


  # Tabelas -----------------------------------------------------------------

  ## Creche
  # output$num_pop0a3 <- renderInfoBox({
  #   pop0a3 <- Meta1 |> filter(nome_uf == input$uf,
  #                             nome_municipio == input$municipio) |>
  #     dplyr::summarise(
  #       pop0a3 = format(sum(Meta1$popFaixa0a3, na.rm = T),
  #                       big.mark=".")
  #     )
  #
  #   #   format(sum(Meta1$popFaixa0a3, na.rm = T),
  #   #                  big.mark=".")
  #   infoBox(
  #     title = "População: 0 a 3 anos - Região Norte",
  #     value = pop0a3,
  #     icon = icon("baby"),
  #     fill = TRUE,
  #     color = "teal"
  #   )
  # })

  # red, yellow, aqua, blue, light-blue, green, navy, teal,
  # olive, lime, orange, fuchsia, purple, maroon, black.

  # output$num_mat0a3 <- renderInfoBox({
  #   mat0a3 <- format(sum(Meta1$qtdeMatCreche, na.rm = T),
  #                    big.mark=".")
  #   infoBox(
  #     title = "Matrículas em creches - Região Norte",
  #     value = mat0a3,
  #     icon = icon("baby"),
  #     fill = TRUE,
  #     color = "teal"
  #   )
  # })


  ## Pré-escola
  # output$num_pop4e5 <- renderInfoBox({
  #   pop4e5 <- format(sum(Meta1$popFaixa4e5,na.rm = T),
  #                    big.mark=".")
  #   infoBox(
  #     title = "População: 4 e 5 anos- Região Norte",
  #     value = pop4e5,
  #     icon = icon("child"),
  #     fill = TRUE,
  #     color = "blue"
  #   )
  # })

  # output$num_mat4e5 <- renderInfoBox({
  #   mat4e5 <- format(sum(Meta1$qtdeMatPre, na.rm = T),
  #                    big.mark=".")
  #   infoBox(
  #     title = "Matrícula Pré-Escola - Região Norte",
  #     value = mat4e5,
  #     icon = icon("child"),
  #     fill = TRUE,
  #     color = "blue"
  #   )
  # }
  # )

}

shinyApp(ui, server)
