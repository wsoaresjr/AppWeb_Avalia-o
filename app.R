library(shiny)
library(shinydashboard)
library(shinymanager)
library(dplyr)

banco_itens <- read.csv2("banco_itens.csv", header = TRUE, sep = ";", dec = ".", fileEncoding="latin1")

# MODULO DE AUTENTICACAO
acesso <- read.csv2("Dados/Cod_Acesso.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, fileEncoding="latin1")
credentials <- acesso

# MODULO UI
ui <- fluidPage(
  
  auth_ui(
    id = "auth",
    lan = use_language("pt-BR"),
    # Imagem Logotipo
    tags_top =
      tags$div(
        tags$h3("PLATAFORMA DE AVALIAÇÃO ONLINE", style = "align:center"), # Entre aspas é possível colocar um título
        tags$h4("", style = "align:center"), # Entre aspas é possível colocar um título
        tags$img(
          src = "https://exploratoriadigital.com.br/wp-content/uploads/2024/02/Exploratoria-Digital_Logo.png", width = 200
        )
      ),
    
    # add information on bottom ?
    tags_bottom = tags$div(
      tags$p(
        "Havendo algum problema, por favor, contate o ",
        tags$a(
          #href = "mailto:suporte@exploratoriadigital.com.br?Subject=Problema na Plataforma de Avaliação Online",
          href = "https://app.pipefy.com/public/form/LkU8njSn",
          target="_top", "Administrador"
        )
      )
    ),
    
    # Background
    background  = "linear-gradient(rgba(0, 0, 0, 0),
            rgba(0, 0, 0, 0)),
            url('https://exploratoriadigital.com.br/wp-content/uploads/2024/02/Fundo_Cinza-1536x864.png');"
  ),
  
  # RESULTADO DA AUTENTICACAO
  verbatimTextOutput(outputId = "res_auth"),
  
  # FIM MODULO DE AUTENTICACAO
  
  
  
  
  # Atualização no módulo UI
  #ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "Plataforma de Avaliação Online", titleWidth = 450),
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        menuItem("MENU 1", tabName = "home"),
        menuItem("MENU 2", tabName = "resultados"),
        menuItem("Modelagem", tabName = "modelagem"), # Adicionado menu "Modelagem"
        actionButton("action_logout", "Sair")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "modelagem",
                selectInput("codDescritor", "Escolha o Código do Descritor:", choices = NULL),
                uiOutput("codItemUI"), # UI dinâmico para seleção de CodItem
                textOutput("descritorTexto"),
                htmlOutput("itemContent") # Para exibir o conteúdo do RMarkdown
        )
      )
    )
  )
)

# Atualização no módulo Server
server <- function(input, output, session) {
  
  # SERVER - MODULO DE AUTENTICACAO 
  auth <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials(credentials)
  )
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(auth) ## <---- this line print which user is logged in
  })
  
  # the following line is just an example how to use auth$user in a different
  # reactive
  user_data <- reactive({
    auth$user
  })
  
  nome_data <- reactive({
    auth$user_info$nome
  })
  
  grupo_data <- reactive({
    auth$user_info$grupo
  })
  
  
  
  
  # call the new reactive in a render function
  output$Login <- renderText({
    paste0("Olá!  Bem vindo(a),  ", nome_data(),"!")
  })
  
  
  # SAIDA DO APLICATIVO
  observeEvent(input$action_logout, {
    session$reload()
  }) 
  
  # Carregando os dados do arquivo CSV
  banco_itens <- read.csv2("banco_itens.csv", header = TRUE, sep = ";", dec = ".", fileEncoding="latin1")
  
  # Preenchendo choices para CodDescritor
  observe({
    updateSelectInput(session, "codDescritor", choices = unique(banco_itens$CodDescritor))
  })
  
  # UI dinâmico para CodItem baseado no CodDescritor selecionado
  output$codItemUI <- renderUI({
    selectedDescritor <- input$codDescritor
    if (!is.null(selectedDescritor)) {
      filteredItems <- banco_itens %>% filter(CodDescritor == selectedDescritor)
      selectInput("codItem", "Escolha o Código do Item:", choices = filteredItems$CodItem)
    }
  })
  
  # Exibir texto do Descritor
  output$descritorTexto <- renderText({
    if (!is.null(input$codDescritor)) {
      selectedDescritorText <- banco_itens %>% 
        filter(CodDescritor == input$codDescritor) %>% 
        pull(Descritor) %>% 
        unique()
      selectedDescritorText
    }
  })
  
  # Exibir conteúdo do item selecionado
  output$itemContent <- renderUI({
    req(input$codItem) # Garante que um item foi selecionado
    # Supondo que o nome do arquivo RMarkdown seja baseado no CodItem, por exemplo, "M0001.Rmd"
    # Aqui, você precisaria adaptar a lógica para encontrar e renderizar o conteúdo do arquivo RMarkdown.
    # Esta é uma estrutura básica, a implementação exata depende de como os Rmds são renderizados em sua aplicação
    itemFilePath <- paste0("Itens/", input$codItem, ".Rmd")
    # Você pode usar, por exemplo, rmarkdown::render para processar o arquivo e depois incluir o output
    # ou usar alguma função customizada para exibir o conteúdo diretamente no Shiny.
    tags$iframe(style="width:100%; height:400px;", src=itemFilePath) # Exemplo genérico
  })
  
  # Observa o evento de logout
  observeEvent(input$action_logout, {
    session$reload()
  })
}

# Executando o aplicativo
shinyApp(ui, server)

