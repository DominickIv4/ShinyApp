library(ggplot2)
library(shiny)
library(shinythemes)
library(plotly)
library(colourpicker)
library(moments)
library(data.table)
library(e1071)  
library(shinyBS)
library(tools)

about = tabPanel(
    icon=icon("info"),lib="font-awesome",
    title = "About app",
    titlePanel("About my app"),
    strong("This app was created for the purpose of completing the course KMA/PREZR."),
    br(),
    "@ Dominick Ivan, 2022"
)

nic = "not selected"

main = tabPanel(
    icon=icon("calculator"), lib = "font-awesome",
    title = "Main page",
    actionButton("show", strong("Click here!"),icon=icon("arrow-right"),class="btn btn-info", lib = "font-awesome"),
    titlePanel("Main page"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Import .csv file", accept = ".csv",
                      buttonLabel = "Look for..."),
            fluidRow(
                bsButton("warn",label="Warning!", style="warning",class="btn-sm" ),
                bsTooltip(id = "warn", title = "If only one variable is selected, a histogram will be drawn instead of a ggplot in the first window!", placement = "right")
            ),
            selectInput("var_1", "Variable no.1", choices = c(nic)),
            selectInput("var_2", "Variable no.2", choices = c(nic)),
            colourInput("color", 'Colour of the main graph', value="red"),
            br(),
            actionButton("run", "Run it!", icon = icon("sync"), class = "btn btn-danger btn-lg btn-block"),
            br(),
            downloadButton("download","Download",class="btn btn-success" )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    title = "Graphic output",
                    plotOutput("plot_1")
                ),
                tabPanel(
                    title="Boxplots",
                    fluidRow(
                        splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_2"), plotOutput("plot_4"))
                    )
                ),
                tabPanel(
                    title="Linear regression",
                    plotOutput("plot_3"),
                    verbatimTextOutput("modelSummary")
                ),
                tabPanel(
                    title="Density",
                    fluidRow(
                        splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_5"), plotOutput("plot_6"))
                    )
                ),
                tabPanel(
                    title = "Statistical summary",
                    fluidRow(
                        column(width = 6, em(strong(textOutput("var1_nadpis")))),
                        column(width = 6, em(strong(textOutput("var2_nadpis"))))
                    ),
                    fluidRow(
                        splitLayout(cellWidths=c("50%" ,"50%"), em(tableOutput("var1_blok")),em(tableOutput("var2_blok"))
                        ))
                )
            )
        )
    )
)


reg=function(data_load, var_1, var_2){
    thematic::thematic_shiny()
    ggplot(data = data_load,
           aes_string(x = var_1, y = var_2)) +
        geom_point()+
        geom_smooth(method='lm',se=FALSE)+labs(title="Linear regression")+
        theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 
}



input_tab = function(data_load, var){
    if(var != nic){
        options(OutDec=",")
        val = data_load[,get(var)]
        val=val[!is.na(val)]
        velicina = c("mean", "min.value","1Q","median","3Q", "max.value",
                     "dispersion", "standard deviation","kurtosis","skewness")
        hodnota = c(round(mean(val),2), round(min(val),2),
                    round(quantile(val, 0.25),2), round(median(val),2),round(quantile(val,0.75),2) ,
                    round(max(val),2),var(val), sd(val),kurtosis(val),skewness(val))
        data.table(velicina, hodnota)
    }
}


############################################################
ui <- navbarPage(
    title = "Statistical app",
    theme = shinytheme('flatly'),
    position=c("static-top"),
    collapsible = TRUE,
    main,
    about
)

############################################################
server <- function(input, output){
    
    data_load <- reactive({
        req(input$file1)
        fread(input$file1$datapath,fill=TRUE)
        if (file_ext(input$file1$name) != "csv"){
            showModal(modalDialog(strong("You have chosen the wrong file type!"), easyClose=TRUE, footer = modalButton("Zatvoriť")))
        }
        else if(file_ext(input$file1$name) == "csv"){
            req(input$file1)
            fread(input$file1$datapath,fill=TRUE)
        }
    })
    
    
    
    observeEvent(data_load(),{
        choices <- c(nic,names(data_load()))
        updateSelectInput(inputId = "var_1", choices = choices)
        updateSelectInput(inputId = "var_2", choices = choices)
    })
    
    observeEvent(input$show, {
        showModal(modalDialog(
            title = "Important anouncement!",
            "Thank you for visiting my page!",
            easyClose = TRUE,
            fade=TRUE,
            footer=modalButton("Close")
        ))
    })
    
    var_1 = eventReactive(input$run,input$var_1)
    var_2 = eventReactive(input$run,input$var_2)
    color=eventReactive(input$run,input$color)
    
    # plot 1
    
    plot_1 <- eventReactive(input$run,{
        if(var_1() != nic & var_2() != nic){
            ggplot(data = data_load(),
                   aes_string(x = input$var_1, y = input$var_2)) +
                geom_point(col=input$color)
        }
        else if(var_1() != nic & var_2() == nic){
            ggplot(data = data_load(),
                   aes_string(x = input$var_1)) +
                geom_histogram(color="black", fill=input$color,bins=10)
        }
        else if(var_1() == nic & var_2() != nic){
            ggplot(data = data_load(),
                   aes_string(x = input$var_2)) +
                geom_histogram(color="black", fill=input$color,bins=10)
        }
        else if(var_1() == nic & var_2() == nic){
            showModal(modalDialog(strong("You have not chosen any variable!"),easyClose = TRUE))
        }
    })
    
    output$plot_1 <- renderPlot(plot_1())
    
    # plot2
    
    plot_2 <- eventReactive(input$run,{
        ggplot(data_load(),
               aes_string(x =  factor(0), y = input$var_1)) +
            geom_boxplot()+labs(x=input$var_1,y="y")+ theme(
                axis.text.x = element_blank())
    })
    
    output$plot_2 <- renderPlot(plot_2())
    
    plot_4 <- eventReactive(input$run,{
        ggplot(data_load(),
               aes_string(x = factor(0), y = input$var_2)) +
            geom_boxplot()+labs(x=input$var_1,y="y")+ theme(
                axis.text.x = element_blank())
    })
    
    output$plot_4 <- renderPlot(plot_4())
    
    
    
    ############################################################################### 
    # plot3
    
    
    plot_3 <- eventReactive(input$run,{
        reg(data_load(), var_1(), var_2())
    })
    
    output$plot_3 <-  renderPlot(plot_3())
    ############################################################################### 
    #lm summary (output for regression)
    
    formula = reactive({
        vysv = paste(c(input$var_2))
        as.formula(paste(input$var_1, " ~ ", vysv))
    })
    
    
    model = eventReactive(input$run, {
        lm(formula(), data = data_load())
    })
    
    
    output$modelSummary = renderPrint({
        summary(model())
    })
    
    ############################################################################### 
    # plot5 (density no.1)
    plot_5 = eventReactive(input$run,{
        ggplot(data_load(),
               aes_string(x = input$var_1)) +
            geom_density(color="darkblue", fill="lightblue")+labs(y="density")
    })
    
    output$plot_5 = renderPlot(plot_5())
    
    # plot6 (denisty no.2)
    
    plot_6 <- eventReactive(input$run,{
        ggplot(data_load(),
               aes_string(x = input$var_2)) +
            geom_density(color="red", fill="lightblue")+labs(y="density")
    })
    
    output$plot_6 = renderPlot(plot_6())
    
    
    # statistical summary
    #a) for the first variable
    
    output$var1_nadpis = renderText(paste("Var no.1:",var_1()))
    
    var1_blok <- eventReactive(input$run,{
        input_tab(data_load(), var_1())
    })
    
    output$var1_blok = renderTable(var1_blok(),bordered=TRUE,striped=TRUE)
    
    #b) for the seconf variable
    output$var2_nadpis = renderText(paste("Var. no.2:",var_2()))
    
    var2_blok <- eventReactive(input$run,{
        input_tab(data_load(), var_2())
    })
    
    output$var2_blok = renderTable(var2_blok(),bordered = TRUE, striped = TRUE)
    
    # download button
    
    output$download = downloadHandler( 
        filename = function() { 
            paste0(input$file1, ".csv")
        },
        content = function(file) {
            vroom::vroom_write(data_load(), file) 
        }
    )
    
    
    
}

shinyApp(ui = ui, server = server)

