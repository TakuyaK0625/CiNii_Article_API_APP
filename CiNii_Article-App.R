# ----------------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(dplyr)
library(DT)
library(purrr)
library(plotly)
library(stringr)
library(data.table)


# ----------------------------------------------------------------------------------

gakkai <- fread("学会.csv", stringsAsFactors = F)
univ <- fread("university.csv", stringsAsFactors = F)
Area <- gakkai[, unique(Area)]

Group <- list(旧帝大 = univ[旧帝大 == 1, paste(Name_jp, " OR ",  Name_en, sep = "")],
                 旧六医大 = univ[旧六医大 == 1, paste(Name_jp, " OR ",  Name_en, sep = "")],
                 新八医大 = univ[新八医大 == 1, paste(Name_jp, " OR ",  Name_en, sep = "")],
                 NISTEP_G1 = univ[NISTEP == "G1", paste(Name_jp, " OR ",  Name_en, sep = "")],
                 NISTEP_G2 = univ[NISTEP == "G2", paste(Name_jp, " OR ",  Name_en, sep = "")],
                 NISTEP_G3 = univ[NISTEP == "G3", paste(Name_jp, " OR ",  Name_en, sep = "")],
                 国立財務_A = univ[国立財務 == "A", paste(Name_jp, " OR ",  Name_en, sep = "")],
                 国立財務_B = univ[国立財務 == "B", paste(Name_jp, " OR ",  Name_en, sep = "")],
                 国立財務_C = univ[国立財務 == "C", paste(Name_jp, " OR ",  Name_en, sep = "")],
                 国立財務_D = univ[国立財務 == "D", paste(Name_jp, " OR ",  Name_en, sep = "")],
                 国立財務_E = univ[国立財務 == "E", paste(Name_jp, " OR ",  Name_en, sep = "")],
                 国立財務_F = univ[国立財務 == "F", paste(Name_jp, " OR ",  Name_en, sep = "")],
                 国立財務_G = univ[国立財務 == "G", paste(Name_jp, " OR ",  Name_en, sep = "")],
                 国立財務_H = univ[国立財務 == "H", paste(Name_jp, " OR ",  Name_en, sep = "")]
)

# ----------------------------------------------------------------------------------


ui <- dashboardPage(
    dashboardHeader(title = "CiNii Article Analyzer"),
    dashboardSidebar(
        sliderInput("year", "Year", min = 2010, max = 2020, value = 2015:2019, step = 1),
        selectInput("group", "Group", choices = c(c("---", names(Group)))),
        textInput("univ", "University"),
        checkboxGroupInput("area", "Area", Area),
        actionButton("submit", "Apply Filter")
    ),
    dashboardBody(
        plotlyOutput("plot"),
        br(),
        dataTableOutput("table"),
        downloadButton("download", "Download")
    )
)


# ----------------------------------------------------------------------------------

server <- function(input, output) {
    
    observeEvent(input$submit, {
        
        # -- 検索パラミタ --
        year_from <- input$year[1]
        year_to <- input$year[2]
        publisher <- gakkai %>% filter(Area %in% input$area) %>% .$Name %>% unique %>% 
            split(1:ceiling(length(.)/30)) %>%
            map(function(x) paste0(x, collapse = " OR ")) %>% unlist
        univs <- c(Group[[input$group]], input$univ) %>% unique  %>%.[which(. != "")]
        
        
        
        # -- ループ開始 --
        withProgress(message = "Now processing...", value = 0, {    
            
            # DFの初期化
            DF <- data.frame()
            
            # 大学名でループ
            for(i in univs){
                
                # 出版社でループ
                for(j in publisher){
                    
                    # -- 検索結果総数を取得
                    res <- GET("http://ci.nii.ac.jp/opensearch/search?", 
                               query = list(affiliation = i, 
                                            year_from = year_from, 
                                            year_to = year_to,
                                            publisher = j,
                                            format = "json", 
                                            count = 1, 
                                            start = 1,
                                            appid = "LuljKukSRnqJ4rDk5s5E"))
                    
                    res.json <- content(res, as = "text") %>% fromJSON()
                    totalResults <- res.json$`@graph`$`opensearch:totalResults` %>% as.integer
                    
                    # -- 検索結果総数がゼロの場合はスキップ
                    if(totalResults == 0) next
                    
                    # -- ページごとにループ
                    start <- 0:floor(totalResults/200) * 200 + 1
                    
                    for(k in start){
                        res <- GET("http://ci.nii.ac.jp/opensearch/search?", 
                                   query = list(affiliation = i, 
                                                year_from = year_from, 
                                                year_to = year_to,
                                                publisher = j,
                                                format = "json", 
                                                count = 200, 
                                                start = k,
                                                appid = "LuljKukSRnqJ4rDk5s5E"))
                        
                        res.json <- content(res, as = "text") %>% fromJSON()
                        df <- res.json$`@graph`$items[[1]] 
                        df <- df %>% mutate(著者 = map_chr(`dc:creator`, function(x){paste(x$`@value`, collapse = ", ") %>% unlist}))
                        df <- df %>% mutate(大学名 = str_replace(i, " OR.+$", "")) %>%
                            select(著者, 論文タイトル = title, 資料名 = `prism:publicationName`, 出版社 = `dc:publisher`, 
                                     出版日 = `dc:date`, リンク = `@id`, 大学名)
                        
                        DF <- rbind(DF, df)
                        Sys.sleep(0.5)
                        
                        } # ページループ終わり
                    
                    } # 出版社ループ終わり
                
                incProgress(1/length(univs))
                
                }　# 大学名ループ終わり
            
            })　# プログレスバー終わり　
        
        DF <- unique(DF)
        
        # Bar plot
        output$plot <- renderPlotly({
            DF %>% group_by(大学名) %>% summarize(N = n()) %>%
                mutate(大学名 = str_replace_all(大学名, "/", "")) %>%
                plot_ly(x = ~reorder(大学名, -N), y = ~N, type = "bar") %>%
                layout(yaxis = list(title = 'Count'), xaxis = list(title = ""))
            })
        
        # 集計表
        output$table <- renderDataTable({
            DF %>% mutate(大学名 = str_replace_all(大学名, "/", "")) %>%
            datatable(rownames = F)
            })
        
        # 集計表のダウンロード
        output$download <- downloadHandler(
            filename = "CiNii_Summary.csv",
            content = function(file) {
                write.csv(DF %>% mutate(大学名 = str_replace_all(大学名, "/", "")), file, row.names = FALSE)
            }
        )
        
    }) # observeEventの終わり
       
}

shinyApp(ui, server)