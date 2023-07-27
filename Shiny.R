library(shiny)
library(DT)
ui <- fluidPage(
  titlePanel("Tabelador listas UnB"),
  sidebarLayout(
    sidebarPanel(
      selectInput('Processo', 'Selecione o processo seletivo', selected = 'PAS', choices = c('PAS', 'ENEM', 'VESTIBULAR')),
      textInput('lista', 'Digite a lista'),
      textInput("inscri", "digite os números de incrição que desja retirar", placeholder = "Ex: 14569870, 25406987, 45001455"),
      actionButton('Tabelar', "Tabelar"),
      actionButton("instru", "Instruções")),
    mainPanel(
      DT::DTOutput('dados'),
      textOutput("teste"))
  ))

server <- function(input, output, session){
  m <- NULL
  rodar <- eventReactive(input$Tabelar, {
    if (input$Processo == 'PAS') {
      
      a <- c(input$lista)
      b <- unlist(strsplit(a, "/ "))
      c <- matrix(unlist(strsplit(b, ",")), byrow = TRUE, ncol = 22)
      dados <- data.frame(c)
      
      names(dados)[names(dados) == 'X1'] <- 'Nº'
      names(dados)[names(dados) == 'X2'] <- 'Nome'
      names(dados)[names(dados) == 'X3'] <- 'EB_p1_PAS_1'
      names(dados)[names(dados) == 'X4'] <- 'EB_p2_PAS_1'
      names(dados)[names(dados) == 'X5'] <- 'Redacao_PAS_1'
      names(dados)[names(dados) == 'X6'] <- 'EB_p1_PAS_2'
      names(dados)[names(dados) == 'X7'] <- 'EB_p2_PAS_2'
      names(dados)[names(dados) == 'X8'] <- 'Redacao_PAS_2'
      names(dados)[names(dados) == 'X9'] <- 'EB_p1_PAS_3'
      names(dados)[names(dados) == 'X10'] <- 'EB_p2_PAS_3'
      names(dados)[names(dados) == 'X11'] <- 'Redacao_PAS_3'
      names(dados)[names(dados) == 'X12'] <- 'AF'
      names(dados)[names(dados) == 'X13'] <- 'U'
      names(dados)[names(dados) == 'X14'] <- 'CN'
      names(dados)[names(dados) == 'X15'] <- 'PPIm'
      names(dados)[names(dados) == 'X16'] <- 'PPImd'
      names(dados)[names(dados) == 'X17'] <- 'NPPIm'
      names(dados)[names(dados) == 'X18'] <- 'NPPImd'
      names(dados)[names(dados) == 'X19'] <- 'PPIM'
      names(dados)[names(dados) == 'X20'] <- 'PPIMd'
      names(dados)[names(dados) == 'X21'] <- 'NPPIM'
      names(dados)[names(dados) == 'X22'] <- 'NPPIMd'
      
      dados$Nº <- as.integer(dados$Nº)
      dados$EB_p1_PAS_1 <- as.numeric(dados$EB_p1_PAS_1)
      dados$EB_p2_PAS_1 <- as.numeric(dados$EB_p2_PAS_1)
      dados$Redacao_PAS_1 <- as.numeric(dados$Redacao_PAS_1)
      dados$EB_p1_PAS_2 <- as.numeric(dados$EB_p1_PAS_2)
      dados$EB_p2_PAS_2 <- as.numeric(dados$EB_p2_PAS_2)
      dados$Redacao_PAS_2 <- as.numeric(dados$Redacao_PAS_2)
      dados$EB_p1_PAS_3 <- as.numeric(dados$EB_p1_PAS_3)
      dados$EB_p2_PAS_3 <- as.numeric(dados$EB_p2_PAS_3)
      dados$Redacao_PAS_3 <- as.numeric(dados$Redacao_PAS_3)
      dados$AF <- as.numeric(dados$AF)
      dados$U <- as.integer(dados$U)
      dados$CN <- as.integer(dados$CN)
      dados$PPIm <- as.integer(dados$PPIm)
      dados$PPImd <- as.integer(dados$PPImd)
      dados$NPPIm <- as.integer(dados$NPPIm)
      dados$NPPImd <- as.integer(dados$NPPImd)
      dados$PPIM <- as.integer(dados$PPIM)
      dados$PPIMd <- as.integer(dados$PPIMd)
      dados$NPPIM <- as.integer(dados$NPPIM)
      dados$NPPIMd <- as.integer(dados$NPPIMd)
      
    } else if (input$Processo == 'ENEM') {
      a <- c(input$lista)
      b <- unlist(strsplit(a, "/ "))
      c <- matrix(unlist(strsplit(b, ",")), byrow = TRUE, ncol = 13)
      dados <- data.frame(c)
      
      names(dados)[names(dados) == 'X1'] <- 'Nº'
      names(dados)[names(dados) == 'X2'] <- 'Nome'
      names(dados)[names(dados) == 'X3'] <- 'Nota'
      names(dados)[names(dados) == 'X4'] <- 'U'
      names(dados)[names(dados) == 'X5'] <- 'CN'
      names(dados)[names(dados) == 'X6'] <- 'PPIm'
      names(dados)[names(dados) == 'X7'] <- 'PPImd'
      names(dados)[names(dados) == 'X8'] <- 'NPPIm'
      names(dados)[names(dados) == 'X9'] <- 'NPPImd'
      names(dados)[names(dados) == 'X10'] <- 'PPIM'
      names(dados)[names(dados) == 'X11'] <- 'PPIMd'
      names(dados)[names(dados) == 'X12'] <- 'NPPIM'
      names(dados)[names(dados) == 'X13'] <- 'NPPIMd'
      
      dados$Nº <- as.integer(dados$Nº)
      dados$Nota <- as.numeric(dados$Nota)
      dados$U <- as.integer(dados$U)
      dados$CN <- as.integer(dados$CN)
      dados$PPIm <- as.integer(dados$PPIm)
      dados$PPImd <- as.integer(dados$PPImd)
      dados$NPPIm <- as.integer(dados$NPPIm)
      dados$NPPImd <- as.integer(dados$NPPImd)
      dados$PPIM <- as.integer(dados$PPIM)
      dados$PPIMd <- as.integer(dados$PPIMd)
      dados$NPPIM <- as.integer(dados$NPPIM)
      dados$NPPIMd <- as.integer(dados$NPPIMd)
      
      
    } else if (input$Processo == 'VESTIBULAR') {
      a <- c(input$lista)
      b <- unlist(strsplit(a, "/ "))
      c <- matrix(unlist(strsplit(b, ",")), byrow = TRUE, ncol = 17)
      dados <- data.frame(c)
      
      names(dados)[names(dados) == 'X1'] <- 'Nº'
      names(dados)[names(dados) == 'X2'] <- 'Nome'
      names(dados)[names(dados) == 'X3'] <- 'P1'
      names(dados)[names(dados) == 'X4'] <- 'P2'
      names(dados)[names(dados) == 'X5'] <- 'P3'
      names(dados)[names(dados) == 'X6'] <- 'PR'
      names(dados)[names(dados) == 'X7'] <- 'AF'
      names(dados)[names(dados) == 'X8'] <- 'U'
      names(dados)[names(dados) == 'X9'] <- 'CN'
      names(dados)[names(dados) == 'X10'] <- 'PPIm'
      names(dados)[names(dados) == 'X11'] <- 'PPImd'
      names(dados)[names(dados) == 'X12'] <- 'NPPIm'
      names(dados)[names(dados) == 'X13'] <- 'NPPImd'
      names(dados)[names(dados) == 'X14'] <- 'PPIM'
      names(dados)[names(dados) == 'X15'] <- 'PPIMd'
      names(dados)[names(dados) == 'X16'] <- 'NPPIM'
      names(dados)[names(dados) == 'X17'] <- 'NPPIMd'
      
      dados$Nº <- as.integer(dados$Nº)
      dados$P1 <- as.numeric(dados$P1)
      dados$P2 <- as.numeric(dados$P2)
      dados$P3 <- as.numeric(dados$P3)
      dados$PR <- as.numeric(dados$PR)
      dados$AF <- as.numeric(dados$AF)
      dados$U <- as.integer(dados$U)
      dados$CN <- as.integer(dados$CN)
      dados$PPIm <- as.integer(dados$PPIm)
      dados$PPImd <- as.integer(dados$PPImd)
      dados$NPPIm <- as.integer(dados$NPPIm)
      dados$NPPImd <- as.integer(dados$NPPImd)
      dados$PPIM <- as.integer(dados$PPIM)
      dados$PPIMd <- as.integer(dados$PPIMd)
      dados$NPPIM <- as.integer(dados$NPPIM)
      dados$NPPIMd <- as.integer(dados$NPPIMd)
    }
    
    if (any(is.na(dados$Nº))){
      dados <- NULL
      showNotification("Veja se escolheu o processo seletivo correto e se as entradas foram copiadas corretamente.", type = "error")
    } else if (input$inscri != ""){  
      n <- unlist(strsplit(input$inscri, ","))
      z <- as.integer(n)
      v <- c()
      for (i in 1:length(z)) {
        v <- c(v, as.integer(row.names(dados[dados$Nº == z[i],])))
      }
      dados <- dados[-v, ]
    }
    dados
  })
  
  output$dados <- DT::renderDT({
    dados <- rodar()
    DT::datatable(dados)
  })
  
  observeEvent(input$instru,{
    showModal(modalDialog(title = "Instruções", tags$div("Você deve primeiro selecionar o processo seletivo correto, e em seguida copiar a lista do curso que deseja ver diretamente do pdf que o cebraspe publica na página do processo e depois clicar em tabelar, e se desejar retirar alguém da lista é nescessário colocar o número de inscrição separando por vírgula por exemplo: 19888445, 13336654, 41525356. E clicar em tabelar novamente.", 
                                                         tags$p(), "O foco do programa é para que o vestibulando possa acompanhar o processo e saber se ainda têm chance dito que quando um dos nomes chamados não faz o registro academico uma nova chamada é feita (Não existe um número de chamadas definido, veja os anos anteriores para ter ideia de quantas são feitas.) e aquela vaga vai para o candidato da próxima colocação seguindo respectivamente a cota.", 
                                                         tags$p(), "As cotas podem descer quando não existe mais candidato na cota seguindo a seguinte ordem:", 
                                                         tags$p(), "PPImd -> PPIm -> NPPImd -> NPPIm -> PPIMd -> PPIM -> NPPIMd -> NPPIM -> U",  
                                                         tags$p(), "Observações:", 
                                                         tags$p(), "a cota para negros não faz parte das cotas de escolas públicas então a vaga remanescente vai direto para a universal.", 
                                                         tags$p(), "Para saber a nota de corte do respectivo sistema, basta conferir na lista da primeira chamada os convocados e verificar quem foi o último a passar no sistema respectivo, a nota do último convocado é a nota de corte do curso no ano respectivo.")
    ))
  })
}
shinyApp(ui = ui, server = server)

