
library(shiny)
library(ggplot2)
library(Hmisc)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mini Chatbot"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Choose CSV File"),
            textInput("caption", "Do you want basic stats", "Ask Here..."),
            textInput("plot", "Do you want to plot", "Ask Here ...")
        ),

        # Show a plot of the generated distribution
        
        mainPanel(
            tabsetPanel(type = 'tabs',
                        tabPanel("Basic Analysis",
                                 verbatimTextOutput("value"),
                                 tableOutput('table'),
                                 verbatimTextOutput("basic_stats"),
                                 plotOutput("all_plots")),
                        tabPanel("Regression",
                                 verbatimTextOutput("value_reg")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    file_read_fun = reactive({
        validate(
            need(input$file != "", " ")
        )
        name <- input$file
        full_file = read.csv(name$datapath)
    })
    
    plot_fun <- reactive({
        #dataset
        data_set = file_read_fun()
        
        inp_plot = unlist(strsplit(input$plot," "))
        inp_plot_x= unlist(strsplit(input$plot,"x as "))
        inp_plot_x_final = unlist(strsplit(inp_plot_x[2],' '))[1]
        inp_plot_color = unlist(strsplit(inp_plot_x[2],'color as '))
        inp_plot_color_final = unlist(strsplit(inp_plot_color[2],' '))[1]
        inp_plot_fill = unlist(strsplit(inp_plot_x[2],'fill as '))
        inp_plot_fill_final = unlist(strsplit(inp_plot_fill[2],' '))[1]
               
        col_names = unlist(names(data_set))
        
        for (one in inp_plot){
            if(one == "histogram" || one == "hist"){
                inp_plot_binw = unlist(strsplit(inp_plot_x[2],'binwidth= '))
                inp_plot_binw_final = as.numeric(unlist(strsplit(inp_plot_binw[2],' '))[1])
                
                g = ggplot(data_set,aes_string(x = inp_plot_x_final))+geom_histogram()+theme_minimal()
                if(!is.na(inp_plot_binw_final)){
                    g = ggplot(data_set,aes_string(x = inp_plot_x_final))+geom_histogram(binwidth = inp_plot_binw_final) +theme_minimal()
                    if(!is.na(inp_plot_color_final)){
                        g = ggplot(data_set,aes_string(x = inp_plot_x_final))+geom_histogram(color = inp_plot_color_final,binwidth = inp_plot_binw_final) +theme_minimal()
                        
                    }
                    if(!is.na(inp_plot_fill_final)){
                        g = ggplot(data_set,aes_string(x = inp_plot_x_final))+geom_histogram(fill = inp_plot_fill_final,binwidth = inp_plot_binw_final) +theme_minimal()
                    }
                }
                if(!is.na(inp_plot_color_final)){
                    g = ggplot(data_set,aes_string(x = inp_plot_x_final))+geom_histogram(color = inp_plot_color_final) +theme_minimal()
                    
                    if(!is.na(inp_plot_binw_final)){
                        g = ggplot(data_set,aes_string(x = inp_plot_x_final))+geom_histogram(binwidth = inp_plot_binw_final,color = inp_plot_color_final) +theme_minimal()
                        
                    }
                    if(!is.na(inp_plot_fill_final)){
                        g = ggplot(data_set,aes_string(x = inp_plot_x_final))+geom_histogram(fill = inp_plot_fill_final,color = inp_plot_color_final) +theme_minimal()
                    }
                }
                if(!is.na(inp_plot_fill_final)){
                    g = ggplot(data_set,aes_string(x = inp_plot_x_final))+geom_histogram(fill = inp_plot_fill_final) +theme_minimal()
                    if(!is.na(inp_plot_binw_final)){
                        g = ggplot(data_set,aes_string(x = inp_plot_x_final))+geom_histogram(binwidth = inp_plot_binw_final,fill = inp_plot_fill_final) +theme_minimal()
                        
                    }
                    if(!is.na(inp_plot_color_final)){
                        g = ggplot(data_set,aes_string(x = inp_plot_x_final))+geom_histogram(color = inp_plot_color_final,fill = inp_plot_fill_final) +theme_minimal()
                        
                    }
                }
                if(!is.na(inp_plot_fill_final) & !is.na(inp_plot_binw_final) & !is.na(inp_plot_color_final)){
                    g = ggplot(data_set,aes_string(x = inp_plot_x_final))+geom_histogram(binwidth = inp_plot_binw_final,color = inp_plot_color_final,fill = inp_plot_fill_final) +theme_minimal()
                }
            }
            if(one == 'xlimit'){
                inp_plot_x_lim= unlist(strsplit(input$plot,"xlimit as "))
                inp_plot_x_lim_final_first = as.numeric(unlist(strsplit(inp_plot_x_lim[2],","))[1])
                inp_plot_x_lim_final_last = as.numeric(unlist(strsplit(inp_plot_x_lim[2],","))[2])
                g = g+xlim (inp_plot_x_lim_final_first,inp_plot_x_lim_final_last)
            }
        }
        print(g)
    })
    
    basic_stats_fun <- reactive({
        
        data_set = file_read_fun()
        column_named = unlist(colnames(data_set))
        total_words = unlist(strsplit(input$caption," "))
        for (words_f in total_words ){
            if (words_f == 'describe'){
                stat_data = describe(data_set)
            }
            for (words_f in total_words ){
                for (col_Word in column_named){
                    if(words_f == col_Word){
                        stat_data_temp = data_set[col_Word]
                        stat_data = describe(stat_data_temp)
                    }
                }
            }
        }
        stat_data
    })
    
    reg_fun = reactive({
        data_set = file_read_fun()
        column_named = unlist(colnames(data_set))
        total_words = unlist(strsplit(input$caption," "))
        for (words_f in total_words ){
            if(words_f == 'split'){
                set.seed(123)
                
                total_words_sec = unlist(strsplit(input$caption,"train="))
                reg_train_final = as.numeric(unlist(strsplit(total_words_sec[2]," "))[1])/100
                reg_test = unlist(strsplit(total_words_sec[2],'test='))
                reg_test_final = as.numeric(unlist(strsplit(reg_test[2]," "))[1])/100
                
                sample_size <- floor(reg_train_final * nrow(data_set))
                train_ind <- sample(seq_len(nrow(data_set)), size = sample_size)
                train <- data_set[train_ind, ]
                test <- data_set[-train_ind, ]
                
                reg_y = unlist(strsplit(input$caption,"y as "))
                reg_y_final = unlist(strsplit(reg_y[2]," "))[1]
                reg_x = unlist(strsplit(input$caption,"x as "))
                reg_x_final = unlist(strsplit(reg_x[2]," "))[1]
                
                linearMod <- lm(as.formula(paste(reg_y_final,'~',reg_x_final)),data = train )
                
            }
        }
        print(linearMod)
    })

    output$value <- renderText({ input$caption })
    output$table <- renderTable(
        head(file_read_fun())
    )
    #output$mean_table <- renderText({ sprintf("Mean is %f",mean_fun()) })
    #output$max_table <- renderText({ sprintf("Max is %f",max_fun()) })
    output$basic_stats <- renderPrint({ basic_stats_fun() })
    
    output$all_plots = renderPlot({
        plot_fun()
    })
    output$value_reg = renderPrint({ reg_fun( ) })
}

# Run the application 
shinyApp(ui = ui, server = server)
