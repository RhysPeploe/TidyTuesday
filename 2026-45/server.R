library(tidytuesdayR)
library(DT)

function(input, output, session){
  dataset = reactive(tt_load(2026, week = 45)$fiction_rankings|> 
    mutate(tier_rank = str_sub(tier_label, end = 7),
           tier_rank = factor(tier_rank, levels = c("Tier 00",
                                                    "Tier 04",
                                                    "Tier 03",
                                                    "Tier 02",
                                                    "Tier 01")))) 
  
  output$filter_text = renderText("Filtered Table")
  
  output$table_all = renderDT(dataset())
  
  output$table_filter = renderDT({
    if(input$franchaise_opt == "All Franchises"){
      dataset()
    } else{
      dataset() |> 
        filter(franchise %in% input$franchaise_opt)
    }
    })
  
  output$bp = renderPlotly({
    if(input$franchaise_opt == "All Franchises"){
      plot_ly(dataset(), x = ~citations, y = ~tier_rank,
              type = "box", boxpoints = "all", jitter = 0.7,colors = "Set3", color = ~tier_rank,
              text = ~paste(str_to_title(str_replace_all(entry_id, "-", " ")), "<br>", franchise))|> 
        layout(
          xaxis = list(title = "Number of Citations"),
          yaxis = list(title = "Tier")
        )
    } else{
      dataset() |> 
        filter(franchise %in% input$franchaise_opt) |> 
        plot_ly(x = ~citations, y = ~tier_rank,
                type = "box", boxpoints = "all", jitter = 0.7,colors = "Set3", color = ~tier_rank,
                text = ~paste(str_to_title(str_replace_all(entry_id, "-", " ")), "<br>", franchise)) |> 
        layout(
          xaxis = list(title = "Number of Citations"),
          yaxis = list(title = "Tier")
        )
    }
  })
  
  # 
  # output$boxplot = renderPlot({
  #   ggplot(dataset(), aes(x = citations, y = tier_rank)) +
  #     geom_boxplot() +
  #     theme_minimal() +
  #     theme(axis.text = element_text(size = 14),
  #           axis.title = element_text(size = 16)) +
  #     labs(x = "Number of Citations",
  #          y = "Tier")
  # }, height = 880, res = 96)
  # 
  # output$plotly_box = renderPlotly({
  #   plot_ly(dataset(), x = ~citations, y = ~tier_rank,
  #           type = "box", boxpoints = "all", jitter = 0.7,
  #           text = ~paste(str_to_title(str_replace_all(entry_id, "-", " ")), "<br>", franchise))
  # })
  
  # output$input_select = renderText("Showing rows for: ", observe(input$franchaise_opt))
  
  # observeEvent(
  #   if(input$franchaise_opt == "All Franchises"){
  #     output$table = renderDT({
  #       datatable(
  #         dataset(),
  #         filter = "top",
  #         selection = "multiple",
  #         options = list(
  #           pageLength = 10,
  #           autoWidth = TRUE
  #         )
  #       )
  #     },
  #     server = TRUE)
  #   } else {
  #     output$table = renderDT({
  #       datatable(
  #         dataset() |>
  #           filter(franchaise %in% input$franchaise_opt),
  #         filter = "top",
  #         selection = "multiple"
  #       )
  #     },
  #     server = TRUE)
  #   }
  # )
  
  # observeEvent(input$franchaise,{
  #   filtered_data = dataset() |> 
  #     filter(franchaise %in% franchaise_opt)
  # })
  
  # observeEvent(input$select_all, {
  #   
  #   selected_rows = input$franchaise
  #   
  #   proxy = dataTableProxy("table") |> 
  #     filter(franchaise %in% selected_rows)
  #   
  # })
  # 
  # observeEvent(input$clear_selection,{
  #   proxy = dataset()
  # })
}