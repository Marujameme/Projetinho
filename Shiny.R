library(shiny)
library(DT)
help(input)
ui <- fluidPage(
  titlePanel("Tabelador listas UnB"),
  sidebarLayout(
  sidebarPanel(
    selectInput('Processo', 'Selecione o processo seletivo', selected = 'PAS', choices = c('PAS', 'ENEM', 'VESTIBULAR')),
    textInput('lista', 'Digite a lista'),
    actionButton('Tabelar', "Tabelar")),
  mainPanel(
    DT::DTOutput('dados'))
  ))

server <- function(input, output, session){
  rodar <- eventReactive(input$Tabelar, {
    if (input$Processo == 'PAS') {
      char2 <- c(input$lista)
      
      char3<-  (strsplit(char2, "/ "))
      char4 <- unlist(strsplit(char3[[1]], ", "))
      
      c_vetor_PAS <- function(x, i){
        vetor <- c(1:(length(x)/22)) 
        j = 1
        while(i <= length(x)) {
          vetor[j] <- x[i]
          i = i + 22
          j = j + 1
        } 
        return(vetor)
      }
      Nº <- c_vetor_PAS(char4, 1)
      Nome <- c_vetor_PAS(char4, 2)
      EB_p1_PAS_1 <- as.numeric(c_vetor_PAS(char4, 3))
      EB_p2_PAS_1 <- as.numeric(c_vetor_PAS(char4, 4))
      Redacao_PAS_1  <- as.numeric(c_vetor_PAS(char4, 5))
      EB_p1_PAS_2 <- as.numeric(c_vetor_PAS(char4, 6))
      EB_p2_PAS_2  <- as.numeric(c_vetor_PAS(char4, 7))
      Redacao_PAS_2 <- as.numeric(c_vetor_PAS(char4, 8))
      EB_p1_PAS_3 <- as.numeric(c_vetor_PAS(char4, 9))
      EB_p2_PAS_3 <- as.numeric(c_vetor_PAS(char4, 10))
      Redacao_PAS_3 <- as.numeric(c_vetor_PAS(char4, 11))
      AF <- as.numeric(c_vetor_PAS(char4, 12))
      U <- as.integer(c_vetor_PAS(char4, 13))
      CN <- as.integer(c_vetor_PAS(char4, 14))
      PPIm <- as.integer(c_vetor_PAS(char4, 15))
      PPImd <- as.integer(c_vetor_PAS(char4, 16))
      NPPIm <- as.integer(c_vetor_PAS(char4, 17))
      NPPImd <- as.integer(c_vetor_PAS(char4, 18))
      PPIM <- as.integer(c_vetor_PAS(char4, 19))
      PPIMd <- as.integer(c_vetor_PAS(char4, 20))
      NPPIM <- as.integer(c_vetor_PAS(char4, 21))
      NPPIMd <- as.integer(c_vetor_PAS(char4, 22))
      dados <- data.frame(Nº, Nome, EB_p1_PAS_1, EB_p2_PAS_1, Redacao_PAS_1, EB_p1_PAS_2, EB_p2_PAS_2, Redacao_PAS_2, 
                          EB_p1_PAS_3, EB_p2_PAS_3, Redacao_PAS_3, AF, U, CN, PPIm, PPImd, NPPIm, NPPImd, PPIM, PPIMd, NPPIM, NPPIMd)
      DT::datatable(dados)
    } else if (input$Processo == 'ENEM') {
      char2 <- c(input$lista)
      
      char3<-  (strsplit(char2, "/ "))
      char4 <- unlist(strsplit(char3[[1]], ", "))
      
      c_vetor_ENEM <- function(x, i){
        vetor <- c(1:(length(x)/13)) 
        j = 1
        while(i <= length(x)) {
          vetor[j] <- x[i]
          i = i + 13
          j = j + 1
        } 
        return(vetor)
      }
      Nº <- c_vetor_ENEM(char4, 1)
      Nome <- c_vetor_ENEM(char4, 2)
      Nota <- as.numeric(c_vetor_ENEM(char4, 3))
      U <- as.integer(c_vetor_ENEM(char4, 4))
      CN <- as.integer(c_vetor_ENEM(char4, 5))
      PPIm <- as.integer(c_vetor_ENEM(char4, 6))
      PPImd <- as.integer(c_vetor_ENEM(char4, 7))
      NPPIm <- as.integer(c_vetor_ENEM(char4, 8))
      NPPImd <- as.integer(c_vetor_ENEM(char4, 9))
      PPIM <- as.integer(c_vetor_ENEM(char4, 10))
      PPIMd <- as.integer(c_vetor_ENEM(char4, 11))
      NPPIM <- as.integer(c_vetor_ENEM(char4, 12))
      NPPIMd <- as.integer(c_vetor_ENEM(char4, 13))
      dados <- data.frame(Nº, Nome, Nota, U, CN, PPIm, PPImd, NPPIm, NPPImd, PPIM, PPIMd, NPPIM, NPPIMd)
      DT::datatable(dados)
    } else if (input$Processo == 'VESTIBULAR') {
      char2 <- c(input$lista)
      
      char3<-  (strsplit(char2, "/ "))
      char4 <- unlist(strsplit(char3[[1]], ", "))
      
      c_vetor_VEST <- function(x, i){
        vetor <- c(1:(length(x)/17)) 
        j = 1
        while(i <= length(x)) {
          vetor[j] <- x[i]
          i = i + 17
          j = j + 1
        } 
        return(vetor)
      }
      Nº <- c_vetor_VEST(char4, 1)
      Nome <- c_vetor_VEST(char4, 2)
      P1 <- as.numeric(c_vetor_VEST(char4, 3))
      P2 <- as.numeric(c_vetor_VEST(char4, 4))
      P3 <- as.numeric(c_vetor_VEST(char4, 5))
      PR <- as.numeric(c_vetor_VEST(char4, 6))
      AF  <- as.numeric(c_vetor_VEST(char4, 7))
      U <- as.integer(c_vetor_VEST(char4, 8))
      CN <- as.integer(c_vetor_VEST(char4, 9))
      PPIm <- as.integer(c_vetor_VEST(char4, 10))
      PPImd <- as.integer(c_vetor_VEST(char4, 11))
      NPPIm <- as.integer(c_vetor_VEST(char4, 12))
      NPPImd <- as.integer(c_vetor_VEST(char4, 13))
      PPIM <- as.integer(c_vetor_VEST(char4, 14))
      PPIMd <- as.integer(c_vetor_VEST(char4, 15))
      NPPIM <- as.integer(c_vetor_VEST(char4, 16))
      NPPIMd <- as.integer(c_vetor_VEST(char4, 17))
      dados <- data.frame(Nº, Nome, P1, P2, P3, PR, AF, U, CN, PPIm, PPImd, NPPIm, NPPImd, PPIM, PPIMd, NPPIM, NPPIMd)
      DT::datatable(dados)
    }
     })
  output$dados <- DT::renderDT({
    rodar()
  })
}
shinyApp(ui = ui, server = server)

