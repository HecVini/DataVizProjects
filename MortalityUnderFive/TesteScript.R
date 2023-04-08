library(plotly)

# Create a dummy plot
fig <- plot_ly(
  x = 1:10,
  y = runif(10, 0, 50),
  type = 'scatter',
  mode = 'lines'
)

# Add vertical line annotation at y=25
fig <- fig %>% add_annotations(
  x = 0,
  y = 25,
  xref = "x",
  yref = "y",
  showarrow = FALSE,
  text = "Acceptable level",
  font = list(size = 16, color = "black"),
  ax = 0,
  ay = -50,
  bordercolor = "black",
  borderwidth = 2
)

# Customize layout
fig <- fig %>% layout(
  yaxis = list(range = c(0, 50), title = "Mortality Rate"),
  xaxis = list(title = "Year"),
  title = list(text = "Child Under 5 Mortality Rate"),
  margin = list(l = 50, r = 50, b = 50, t = 80),
  plot_bgcolor = "#ffffff",
  paper_bgcolor = "#ffffff"
)

# Show plot
fig







library(plotly)
library(shiny)

# create example data
x <- seq(1, 10, by = 0.1)
y1 <- sin(x)
y2 <- cos(x)
y3 <- tan(x)

# create plotly plot
p <- plot_ly() %>%
  add_lines(x = x, y = y1, name = "Sin") %>%
  add_lines(x = x, y = y2, name = "Cos") %>%
  add_lines(x = x, y = y3, name = "Tan")
p
# define function to update plot attributes when hovering over trace
update_plot <- function(data, hover_data) {
  if (is.null(hover_data)) {
    # if not hovering over any trace, set all line widths to 1
    data <- data %>% plotly::style(line = list(width = 1))
  } else {
    # if hovering over a trace, highlight that trace and underlight the others
    trace_index <- hover_data$curveNumber + 1
    data <- data %>% plotly::style(
      line = list(
        width = c(rep(1, trace_index - 1), 3, rep(1, length(data$x) - trace_index)),
        color = c(rep("gray", trace_index - 1), "red", rep("gray", length(data$x) - trace_index))
      )
    )
  }
  return(data)
}

# create plotly proxy object to allow for updating plot attributes
proxy <- plotlyProxy("plot", session = shiny::getDefaultReactiveDomain())

# define observer function that updates plotly plot based on hover events
observe({
  hover_data <- event_data("plotly_hover")
  data <- p$data
  data <- update_plot(data, hover_data)
  proxy %>% plotlyProxyInvoke("restyle", list(data), list(list()))
})

# display plotly plot
p


library(plotly)

# Create data for the chart
year <- c("2000", "2019")
deaths <- c(5, 10)
data <- data.frame(year, deaths)

# Create the plot
plot <- plot_ly(data, x = ~year, y = ~deaths, type = 'scatter', mode = 'markers+lines') %>%
  
  # Add a circle marker at 2019 data point
  add_markers(x = ~year[2], y = ~deaths[2], 
              marker = list(symbol = "circle", size = 15, color = "red"),
              showlegend = FALSE) %>%
  
  # Add an arrow to the circle marker
  add_annotations(x = ~year[2], y = ~deaths[2],
                  ax = 50, ay = -50,
                  text = "In 2019, 10 million children under 5 died. In 2000, it was 5 million",
                  arrowhead = 2, arrowsize = 1.5, arrowwidth = 2) %>%
  
  # Add title and axis labels
  layout(title = "Child Mortality", xaxis = list(title = "Year"), yaxis = list(title = "Deaths (millions)"))

# Display the plot
plot

