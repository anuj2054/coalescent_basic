library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("pop_size", "Population Size:", value = 100),
      numericInput("num_gen", "Number of Generations:", value = 10),
      numericInput("mutation_rate", "Mutation Rate:", value = 0.01),
      numericInput("selection_prop", "Selection Proportion:", value = 0.5),
      actionButton("simulate_btn", "Simulate")
    ),
    mainPanel(
      plotOutput("genealogy_plot"),
      tableOutput("lineage_table"),
      plotOutput("mutation_plot"),
      plotOutput("selection_plot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$simulate_btn, {
    pop_size <- input$pop_size
    num_gen <- input$num_gen
    mutation_rate <- input$mutation_rate
    selection_prop <- input$selection_prop
    
    genealogy <- simulate_coalescent(pop_size, num_gen)
    genealogy <- introduce_mutations(genealogy, mutation_rate)
    genealogy <- apply_selection(genealogy, selection_prop)
    
    output$genealogy_plot <- renderPlot({
      ggplot(data = genealogy, aes(x = Generation, y = Lineage, color = Selected)) +
        geom_segment(aes(xend = Generation + 1, yend = Parent)) +
        scale_color_manual(values = c("gray", "black")) +
        labs(x = "Generation", y = "Lineage") +
        theme_minimal()
    })
    
    output$lineage_table <- renderTable({
      genealogy %>%
        group_by(Lineage) %>%
        summarise(FirstGeneration = min(Generation), LastGeneration = max(Generation))
    })
    
    output$mutation_plot <- renderPlot({
      mutation_counts <- genealogy %>%
        filter(!is.na(Parent)) %>%
        group_by(Generation) %>%
        summarise(Mutations = n())
      
      ggplot(data = mutation_counts, aes(x = Generation, y = Mutations)) +
        geom_bar(stat = "identity", fill = "skyblue", color = "black") +
        labs(x = "Generation", y = "Mutation Count") +
        theme_minimal()
    })
    
    output$selection_plot <- renderPlot({
      selection_counts <- genealogy %>%
        filter(Selected) %>%
        group_by(Generation) %>%
        summarise(Selections = n())
      
      ggplot(data = selection_counts, aes(x = Generation, y = Selections)) +
        geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
        labs(x = "Generation", y = "Selection Count") +
        theme_minimal()
    })
  })
}

simulate_coalescent <- function(pop_size, num_gen) {
  genealogy <- data.frame(Generation = 1, Lineage = 1, Parent = NA, Selected = FALSE)
  
  for (gen in 2:num_gen) {
    lineage <- 1:pop_size
    parent <- sample(lineage, size = pop_size, replace = TRUE)
    
    genealogy <- rbind(genealogy, data.frame(Generation = gen, Lineage = lineage, Parent = parent, Selected = FALSE))
  }
  
  return(genealogy)
}

introduce_mutations <- function(genealogy, mutation_rate) {
  num_mutations <- rpois(nrow(genealogy), lambda = mutation_rate)
  
  for (i in seq_along(num_mutations)) {
    if (num_mutations[i] > 0) {
      mutation_lineages <- sample(genealogy$Lineage, size = num_mutations[i])
      genealogy <- rbind(genealogy, data.frame(Generation = genealogy$Generation[i],
                                               Lineage = mutation_lineages,
                                               Parent = genealogy$Parent[i],
                                               Selected = FALSE))
    }
  }
  
  return(genealogy)
}

apply_selection <- function(genealogy, selection_prop) {
  last_generation <- max(genealogy$Generation)
  
  for (gen in last_generation:2) {
    generation <- genealogy[genealogy$Generation == gen, ]
    num_selected <- round(nrow(generation) * selection_prop)
    selected_lineages <- sample(generation$Lineage, size = num_selected)
    
    genealogy[genealogy$Generation == gen & genealogy$Lineage %in% selected_lineages, "Selected"] <- TRUE
  }
  
  return(genealogy)
}

shinyApp(ui = ui, server = server)
