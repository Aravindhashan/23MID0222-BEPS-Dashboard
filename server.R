# server.R – Reactive logic, chart rendering, insights
# CSI 3005 | Advanced Data Visualization Techniques | VIT

server <- function(input, output, session) {

  # ── Shared plotly config (no mode-bar clutter) ───────────────────────────────
  cfg <- list(displayModeBar = TRUE, displaylogo = FALSE,
              modeBarButtonsToRemove = list("lasso2d","select2d","autoScale2d"))

  # ── Helper: scale age to marker size (5–18 px) ─────────────────────────────────
  scales_age <- function(age) {
    lo <- 18; hi <- 90
    min_sz <- 6; max_sz <- 18
    pmin(pmax(min_sz + (age - lo) / (hi - lo) * (max_sz - min_sz), min_sz), max_sz)
  }

  # ╔══════════════════════════════════════════════════════════════════════════╗
  # ║  CLICK-TO-FILTER STATE                                                   ║
  # ╚══════════════════════════════════════════════════════════════════════════╝
  click_party  <- reactiveVal(NULL)   # party name or NULL
  click_age_grp <- reactiveVal(NULL)  # age-group string or NULL

  # Reset click state when sidebar filters change
  observeEvent(
    list(input$gender_f, input$age_group_f, input$knowledge_f, input$europe_f),
    { click_party(NULL); click_age_grp(NULL) },
    ignoreInit = TRUE
  )

  # ── bar-chart click (chart1 – vote count bar) ───────────────────────────────
  observeEvent(event_data("plotly_click", source = "bar_vote"), {
    d <- event_data("plotly_click", source = "bar_vote")
    if (!is.null(d)) {
      parties <- c("Labour","Conservative","Liberal Democrat")
      idx <- d$pointNumber + 1
      if (idx >= 1 && idx <= length(parties)) {
        if (identical(click_party(), parties[idx]))
          click_party(NULL)
        else
          click_party(parties[idx])
      }
    }
  })

  # ── stacked-bar click (chart_stacked100 – age group bar) ───────────────────
  observeEvent(event_data("plotly_click", source = "bar_age"), {
    d <- event_data("plotly_click", source = "bar_age")
    if (!is.null(d)) {
      ag <- d$x
      if (!is.null(ag)) {
        if (identical(click_age_grp(), ag))
          click_age_grp(NULL)
        else
          click_age_grp(ag)
      }
    }
  })

  # ╔══════════════════════════════════════════════════════════════════════════╗
  # ║  FILTERED DATASET  (reactive – drives every chart)                       ║
  # ╚══════════════════════════════════════════════════════════════════════════╝
  fdata <- reactive({
    d <- beps

    # Sidebar filters
    if (input$gender_f != "All")
      d <- d %>% filter(gender == input$gender_f)
    if (length(input$age_group_f) > 0 && length(input$age_group_f) < 4)
      d <- d %>% filter(as.character(age_group) %in% input$age_group_f)
    if (input$knowledge_f != "All")
      d <- d %>% filter(as.character(political.knowledge) == input$knowledge_f)
    d <- d %>% filter(Europe >= input$europe_f[1], Europe <= input$europe_f[2])

    # Click-to-filter (party)
    if (!is.null(click_party()))
      d <- d %>% filter(as.character(vote) == click_party())

    # Click-to-filter (age group from stacked bar)
    if (!is.null(click_age_grp()))
      d <- d %>% filter(as.character(age_group) == click_age_grp())

    d
  })

  # ── Reset button ─────────────────────────────────────────────────────────────
  observeEvent(input$reset, {
    updateSelectInput(session, "gender_f",    selected = "All")
    updateCheckboxGroupInput(session, "age_group_f", selected = age_group_levels)
    updateSelectInput(session, "knowledge_f", selected = "All")
    updateSliderInput(session,   "europe_f",  value = c(1, 11))
    click_party(NULL)
    click_age_grp(NULL)
  })

  # ╔══════════════════════════════════════════════════════════════════════════╗
  # ║  VALUE BOXES                                                              ║
  # ╚══════════════════════════════════════════════════════════════════════════╝
  output$box1 <- renderValueBox({
    valueBox(
      formatC(nrow(fdata()), big.mark = ","),
      "Respondents", icon = icon("users"), color = "navy"
    )
  })
  output$box2 <- renderValueBox({
    p <- round(sum(fdata()$vote == "Labour", na.rm = TRUE) / nrow(fdata()) * 100, 1)
    valueBox(paste0(p, "%"), "Labour Share", icon = icon("check"), color = "red")
  })
  output$box3 <- renderValueBox({
    p <- round(sum(fdata()$vote == "Conservative", na.rm = TRUE) / nrow(fdata()) * 100, 1)
    valueBox(paste0(p, "%"), "Conservative Share", icon = icon("check"), color = "blue")
  })
  output$box4 <- renderValueBox({
    p <- round(mean(fdata()$age, na.rm = TRUE), 1)
    valueBox(p, "Avg Age", icon = icon("birthday-cake"), color = "green")
  })

  # ╔══════════════════════════════════════════════════════════════════════════╗
  # ║  CHART 1 – Vote Count Bar  (click-to-filter source)                      ║
  # ╚══════════════════════════════════════════════════════════════════════════╝
  output$chart1 <- renderPlotly({
    d <- fdata() %>%
      count(vote) %>%
      mutate(
        pct   = round(n / sum(n) * 100, 1),
        color = party_colors[as.character(vote)],
        tip   = paste0("<b>", vote, "</b><br>",
                       "Count: ", n, "<br>",
                       "Share: ", pct, "%")
      )

    plot_ly(
      d, x = ~vote, y = ~n,
      type        = "bar",
      marker      = list(color = d$color,
                         line  = list(color = "#fff", width = 1.5)),
      text        = ~tip,
      hoverinfo   = "text",
      source      = "bar_vote"
    ) %>%
      layout(
        xaxis   = list(title = ""),
        yaxis   = list(title = "Count"),
        margin  = list(t = 10),
        plot_bgcolor  = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(cfg)
  })

  # ╔══════════════════════════════════════════════════════════════════════════╗
  # ║  DONUT – Vote Share (Overview)                                            ║
  # ╚══════════════════════════════════════════════════════════════════════════╝
  output$chart_donut <- renderPlotly({
    d <- fdata() %>%
      count(vote) %>%
      mutate(pct = round(n / sum(n) * 100, 1))

    plot_ly(
      d,
      labels  = ~vote, values = ~n,
      type    = "pie", hole = 0.52,
      marker  = list(
        colors = party_colors[as.character(d$vote)],
        line   = list(color = "#fff", width = 2)
      ),
      textinfo   = "label+percent",
      hovertemplate = paste0(
        "<b>%{label}</b><br>",
        "Count: %{value}<br>",
        "Share: %{percent}<extra></extra>"
      )
    ) %>%
      layout(
        showlegend    = TRUE,
        legend        = list(orientation = "v", x = 1.02, y = .5),
        margin        = list(t = 10, b = 10, l = 10, r = 10),
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(cfg)
  })

  # ╔══════════════════════════════════════════════════════════════════════════╗
  # ║  CHART 2 – Vote by Gender                                                 ║
  # ╚══════════════════════════════════════════════════════════════════════════╝
  output$chart2 <- renderPlotly({
    d <- fdata() %>%
      count(vote, gender) %>%
      group_by(gender) %>%
      mutate(pct = round(n / sum(n) * 100, 1)) %>%
      ungroup()

    plot_ly(
      d, x = ~gender, y = ~n,
      color     = ~vote,
      colors    = party_colors,
      type      = "bar",
      text      = ~paste0("<b>", vote, "</b> (", gender, ")<br>",
                          "Count: ", n, "<br>Share: ", pct, "%"),
      hoverinfo = "text"
    ) %>%
      layout(
        barmode       = "group",
        xaxis         = list(title = "Gender"),
        yaxis         = list(title = "Count"),
        legend        = list(orientation = "h", y = -0.2),
        margin        = list(t = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(cfg)
  })

  # ╔══════════════════════════════════════════════════════════════════════════╗
  # ║  CHART STACKED 100% – Vote Share by Age Group  (click-to-filter source)  ║
  # ╚══════════════════════════════════════════════════════════════════════════╝
  output$chart_stacked100 <- renderPlotly({
    d <- fdata() %>%
      count(age_group, vote) %>%
      group_by(age_group) %>%
      mutate(
        pct  = round(n / sum(n) * 100, 1),
        tot  = sum(n),
        tip  = paste0("<b>", vote, "</b><br>Age Group: ", age_group,
                      "<br>Share: ", pct, "%<br>Count: ", n,
                      "<br>Group Total: ", tot)
      ) %>%
      ungroup()

    parties <- c("Labour","Conservative","Liberal Democrat")
    p <- plot_ly(source = "bar_age")
    for (pty in parties) {
      dd <- d %>% filter(vote == pty)
      p <- p %>% add_trace(
        x         = ~age_group, y = ~pct,
        data      = dd,
        type      = "bar",
        name      = pty,
        marker    = list(color = party_colors[pty]),
        text      = ~tip,
        hoverinfo = "text"
      )
    }
    p %>%
      layout(
        barmode       = "stack",
        xaxis         = list(title = "Age Group",
                             categoryorder = "array",
                             categoryarray = age_group_levels),
        yaxis         = list(title = "Vote Share (%)", ticksuffix = "%",
                             range = c(0, 100)),
        legend        = list(orientation = "h", y = -0.2),
        margin        = list(t = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(cfg)
  })

  # ╔══════════════════════════════════════════════════════════════════════════╗
  # ║  CHART 3 – Age vs Vote scatter                                            ║
  # ╚══════════════════════════════════════════════════════════════════════════╝
  output$chart3 <- renderPlotly({
    d <- fdata() %>%
      group_by(age, vote) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(tip = paste0("<b>", vote, "</b><br>Age: ", age,
                          "<br>Count: ", n))

    plot_ly(
      d, x = ~age, y = ~n,
      color     = ~vote,
      colors    = party_colors,
      type      = "scatter", mode = "markers",
      marker    = list(opacity = 0.75, size = 8),
      text      = ~tip,
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis         = list(title = "Age"),
        yaxis         = list(title = "Count"),
        legend        = list(orientation = "h", y = -0.2),
        margin        = list(t = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(cfg)
  })

  # ╔══════════════════════════════════════════════════════════════════════════╗
  # ║  CHART 4 – Age distribution box                                           ║
  # ╚══════════════════════════════════════════════════════════════════════════╝
  output$chart4 <- renderPlotly({
    d <- fdata()
    plot_ly(
      d, x = ~vote, y = ~age,
      color     = ~vote,
      colors    = party_colors,
      type      = "box",
      boxmean   = "sd",
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "Median Age: %{median}<br>",
        "Q1: %{q1} | Q3: %{q3}<extra></extra>"
      )
    ) %>%
      layout(
        xaxis         = list(title = "Party"),
        yaxis         = list(title = "Age"),
        showlegend    = FALSE,
        margin        = list(t = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(cfg)
  })

  # ╔══════════════════════════════════════════════════════════════════════════╗
  # ║  CHART 5 – Economic perception box (Economy tab)                          ║
  # ╚══════════════════════════════════════════════════════════════════════════╝
  output$chart5 <- renderPlotly({
    plot_ly(
      fdata(), x = ~vote, y = ~economic.cond.national,
      color     = ~vote,
      colors    = party_colors,
      type      = "box",
      boxmean   = TRUE,
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "National Econ (median): %{median}<br>",
        "Mean: %{mean}<extra></extra>"
      )
    ) %>%
      layout(
        xaxis         = list(title = "Party"),
        yaxis         = list(title = "National Economic Perception (1–5)"),
        showlegend    = FALSE,
        margin        = list(t = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(cfg)
  })

  # ╔══════════════════════════════════════════════════════════════════════════╗
  # ║  HEATMAP – Avg Econ Perception (National + Household) by Vote             ║
  # ╚══════════════════════════════════════════════════════════════════════════╝
  output$chart_heatmap <- renderPlotly({
    d <- fdata() %>%
      group_by(vote) %>%
      summarise(
        National  = round(mean(economic.cond.national,  na.rm = TRUE), 2),
        Household = round(mean(economic.cond.household, na.rm = TRUE), 2),
        n         = n(),
        avg_age   = round(mean(age, na.rm = TRUE), 1),
        .groups   = "drop"
      )

    # Build z matrix: rows = metric, cols = party
    parties  <- as.character(d$vote)
    z_matrix <- rbind(d$National, d$Household)
    text_mat <- matrix(
      paste0(round(z_matrix, 2), "\n(n=", rep(d$n, each = 2),
             ", avg age=", rep(d$avg_age, each = 2), ")"),
      nrow = 2
    )

    plot_ly(
      x         = parties,
      y         = c("National Economy", "Household Economy"),
      z         = z_matrix,
      text      = text_mat,
      type      = "heatmap",
      colorscale = list(
        list(0.0, "#fff9f0"),
        list(0.5, "#FF9F00"),
        list(1.0, "#E4002B")
      ),
      hovertemplate = paste0(
        "<b>%{y}</b> → <b>%{x}</b><br>",
        "Avg Score: %{z:.2f}<br>",
        "%{text}<extra></extra>"
      ),
      zmin = 1, zmax = 5,
      showscale = TRUE
    ) %>%
      layout(
        xaxis         = list(title = "Party"),
        yaxis         = list(title = ""),
        margin        = list(t = 30, b = 60),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(cfg)
  })

  # ╔══════════════════════════════════════════════════════════════════════════╗
  # ║  CHART 6 – National vs Household bubble                                   ║
  # ╚══════════════════════════════════════════════════════════════════════════╝
  output$chart6 <- renderPlotly({
    d <- fdata() %>%
      group_by(vote) %>%
      summarise(
        National  = round(mean(economic.cond.national,  na.rm = TRUE), 2),
        Household = round(mean(economic.cond.household, na.rm = TRUE), 2),
        n         = n(),
        avg_age   = round(mean(age, na.rm = TRUE), 1),
        .groups   = "drop"
      ) %>%
      mutate(tip = paste0("<b>", vote, "</b><br>",
                          "National: ",  National,  "<br>",
                          "Household: ", Household, "<br>",
                          "Count: ",     n,         "<br>",
                          "Avg Age: ",   avg_age))

    plot_ly(
      d, x = ~National, y = ~Household,
      color     = ~vote,
      colors    = party_colors,
      type      = "scatter", mode = "markers+text",
      marker    = list(size = 40, opacity = 0.85,
                       sizemode = "diameter"),
      text      = ~vote,
      textposition = "middle center",
      textfont  = list(color = "#fff", size = 11, family = "Arial Black"),
      hovertext = ~tip,
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis         = list(title = "Avg National Economic Perception",
                             range = c(1, 5)),
        yaxis         = list(title = "Avg Household Economic Perception",
                             range = c(1, 5)),
        showlegend    = FALSE,
        margin        = list(t = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(cfg)
  })

  # ╔══════════════════════════════════════════════════════════════════════════╗
  # ║  BLAIR vs EUROPE SCATTER + TREND (Leaders tab)                            ║
  # ╚══════════════════════════════════════════════════════════════════════════╝
  output$chart_scatter_blair <- renderPlotly({
    d <- fdata() %>%
      filter(!is.na(Blair), !is.na(Europe)) %>%
      mutate(
        tip = paste0("<b>", vote, "</b><br>",
                     "Blair Rating: ", Blair, "<br>",
                     "Europe Score: ", Europe, "<br>",
                     "Age: ", age)
      )

    p <- plot_ly()

    # One trace per party
    for (pty in c("Labour", "Conservative", "Liberal Democrat")) {
      dd <- d %>% filter(vote == pty)
      if (nrow(dd) < 2) next

      p <- p %>% add_trace(
        data      = dd,
        x         = ~Blair, y = ~Europe,
        type      = "scatter", mode = "markers",
        name      = pty,
        marker    = list(
          color   = party_colors[pty],
          size    = ~scales_age(age),
          opacity = 0.65,
          line    = list(color = "#fff", width = 0.8)
        ),
        text      = ~tip,
        hoverinfo = "text"
      )

      # Smooth trend line via LOESS
      if (nrow(dd) >= 5) {
        lo <- tryCatch(
          loess(Europe ~ Blair, data = dd, span = 0.8),
          error = function(e) NULL
        )
        if (!is.null(lo)) {
          x_seq <- seq(min(dd$Blair), max(dd$Blair), length.out = 60)
          y_hat <- predict(lo, newdata = data.frame(Blair = x_seq))
          p <- p %>% add_trace(
            x         = x_seq, y = y_hat,
            type      = "scatter", mode = "lines",
            name      = paste(pty, "trend"),
            line      = list(color = party_colors[pty], dash = "dot", width = 2),
            hoverinfo = "skip",
            showlegend = FALSE
          )
        }
      }
    }

    p %>%
      layout(
        xaxis         = list(title = "Blair Rating (1 = low, 5 = high)"),
        yaxis         = list(title = "Europe Score (1 = pro-EU, 11 = anti-EU)"),
        legend        = list(orientation = "h", y = -0.15),
        margin        = list(t = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(cfg)
  })

  # ╔══════════════════════════════════════════════════════════════════════════╗
  # ║  CHART 7 – Leader ratings grouped bar                                     ║
  # ╚══════════════════════════════════════════════════════════════════════════╝
  output$chart7 <- renderPlotly({
    d <- fdata() %>%
      group_by(vote) %>%
      summarise(
        Blair   = round(mean(Blair,   na.rm = TRUE), 2),
        Hague   = round(mean(Hague,   na.rm = TRUE), 2),
        Kennedy = round(mean(Kennedy, na.rm = TRUE), 2),
        n       = n(),
        avg_age = round(mean(age, na.rm = TRUE), 1),
        .groups = "drop"
      )

    make_tip <- function(leader, val, d) {
      paste0("<b>", leader, "</b> rated by <b>", d$vote, "</b><br>",
             "Avg Rating: ", val, "<br>",
             "Group size: ", d$n, "<br>",
             "Avg Age: ", d$avg_age)
    }

    plot_ly() %>%
      add_trace(x = ~d$vote, y = ~d$Blair,   name = "Blair",
                type = "bar",
                marker    = list(color = "#E4002B"),
                text      = make_tip("Blair",   d$Blair,   d),
                hoverinfo = "text") %>%
      add_trace(x = ~d$vote, y = ~d$Hague,   name = "Hague",
                type = "bar",
                marker    = list(color = "#0033A0"),
                text      = make_tip("Hague",   d$Hague,   d),
                hoverinfo = "text") %>%
      add_trace(x = ~d$vote, y = ~d$Kennedy, name = "Kennedy",
                type = "bar",
                marker    = list(color = "#FF9F00"),
                text      = make_tip("Kennedy", d$Kennedy, d),
                hoverinfo = "text") %>%
      layout(
        barmode       = "group",
        xaxis         = list(title = "Party"),
        yaxis         = list(title = "Avg Rating (1–5)"),
        legend        = list(orientation = "h", y = -0.2),
        margin        = list(t = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(cfg)
  })

  # ╔══════════════════════════════════════════════════════════════════════════╗
  # ║  INSIGHTS TAB – rich HTML report                                          ║
  # ╚══════════════════════════════════════════════════════════════════════════╝
  output$insights <- renderUI({
    df <- fdata()
    n  <- nrow(df)
    if (n == 0) return(HTML("<p>No data matches current filters.</p>"))

    # ── summary stats ──────────────────────────────────────────────────────────
    vote_share <- df %>%
      count(vote) %>%
      mutate(pct = round(n / sum(n) * 100, 1))

    best_party  <- as.character(vote_share$vote[which.max(vote_share$n)])
    worst_party <- as.character(vote_share$vote[which.min(vote_share$n)])
    best_pct    <- vote_share$pct[which.max(vote_share$n)]
    worst_pct   <- vote_share$pct[which.min(vote_share$n)]

    age_by_party <- df %>%
      group_by(vote) %>%
      summarise(avg = round(mean(age, na.rm = TRUE), 1), .groups = "drop")

    oldest_pty <- as.character(age_by_party$vote[which.max(age_by_party$avg)])
    youngest_pty <- as.character(age_by_party$vote[which.min(age_by_party$avg)])

    econ_by_party <- df %>%
      group_by(vote) %>%
      summarise(
        nat = round(mean(economic.cond.national,  na.rm = TRUE), 2),
        hh  = round(mean(economic.cond.household, na.rm = TRUE), 2),
        .groups = "drop"
      )

    ag_share <- df %>%
      count(age_group, vote) %>%
      group_by(age_group) %>%
      mutate(pct = round(n / sum(n) * 100, 1)) %>%
      ungroup()

    young_labour <- ag_share %>%
      filter(age_group == "18-29", vote == "Labour") %>%
      pull(pct)
    young_labour <- ifelse(length(young_labour) == 0, NA, young_labour)

    old_tory <- ag_share %>%
      filter(age_group == "65+", vote == "Conservative") %>%
      pull(pct)
    old_tory <- ifelse(length(old_tory) == 0, NA, old_tory)

    blair_corr <- tryCatch(
      round(cor(df$Blair, df$Europe, use = "complete.obs"), 3),
      error = function(e) NA
    )

    male_labour_pct <- df %>%
      filter(gender == "male") %>%
      count(vote) %>%
      mutate(p = round(n / sum(n) * 100, 1)) %>%
      filter(vote == "Labour") %>%
      pull(p)
    male_labour_pct <- ifelse(length(male_labour_pct) == 0, NA, male_labour_pct)

    female_labour_pct <- df %>%
      filter(gender == "female") %>%
      count(vote) %>%
      mutate(p = round(n / sum(n) * 100, 1)) %>%
      filter(vote == "Labour") %>%
      pull(p)
    female_labour_pct <- ifelse(length(female_labour_pct) == 0, NA, female_labour_pct)

    # ── helpers ────────────────────────────────────────────────────────────────
    badge <- function(party) {
      cls <- switch(party,
        "Labour"           = "badge-labour",
        "Conservative"     = "badge-tory",
        "Liberal Democrat" = "badge-libdem",
        ""
      )
      paste0('<span class="', cls, '">', party, '</span>')
    }

    # ── HTML output ─────────────────────────────────────────────────────────── 
    HTML(paste0('
<style>
.insight-card  { background:#fff; border-radius:10px; padding:18px 22px;
                 margin-bottom:16px; box-shadow:0 2px 8px rgba(0,0,0,.10); }
.insight-card h4 { margin-top:0; font-size:15px; font-weight:700;
                   border-left:4px solid #E4002B; padding-left:10px; }
.stat-grid     { display:grid; grid-template-columns:repeat(auto-fill,minmax(155px,1fr));
                 gap:10px; margin-top:12px; }
.stat-tile     { background:#f8f9fa; border-radius:8px; padding:12px 14px; text-align:center; }
.stat-tile .val{ font-size:22px; font-weight:700; color:#1a1a2e; }
.stat-tile .lbl{ font-size:11px; color:#666; text-transform:uppercase; letter-spacing:.5px; }
.anomaly-box   { background:#fff8e1; border:1px solid #FFD700;
                 border-radius:8px; padding:12px 16px; margin-top:10px; }
.badge-labour  { background:#E4002B; color:#fff; padding:2px 8px;
                 border-radius:12px; font-size:11px; }
.badge-tory    { background:#0033A0; color:#fff; padding:2px 8px;
                 border-radius:12px; font-size:11px; }
.badge-libdem  { background:#FF9F00; color:#fff; padding:2px 8px;
                 border-radius:12px; font-size:11px; }
</style>

<h3 style="color:#1a1a2e;border-bottom:3px solid #E4002B;padding-bottom:8px;margin-bottom:18px;">
  BEPS 2001 Election Dashboard — Analytics Report
</h3>

<!-- ── SNAPSHOT ──────────────────────────────────────────────── -->
<div class="insight-card">
  <h4>Dataset Snapshot (current filters)</h4>
  <div class="stat-grid">
    <div class="stat-tile"><div class="val">', formatC(n, big.mark=","), '</div>
      <div class="lbl">Respondents</div></div>
    <div class="stat-tile"><div class="val">', round(mean(df$age, na.rm=TRUE), 1), '</div>
      <div class="lbl">Avg Age</div></div>
    <div class="stat-tile"><div class="val">', round(mean(df$economic.cond.national, na.rm=TRUE), 2), '</div>
      <div class="lbl">Avg National Econ</div></div>
    <div class="stat-tile"><div class="val">', round(mean(df$economic.cond.household, na.rm=TRUE), 2), '</div>
      <div class="lbl">Avg Household Econ</div></div>
    <div class="stat-tile"><div class="val">', round(mean(df$Blair, na.rm=TRUE), 2), '</div>
      <div class="lbl">Avg Blair Rating</div></div>
    <div class="stat-tile"><div class="val">', round(mean(df$Europe, na.rm=TRUE), 2), '</div>
      <div class="lbl">Avg Europe Score</div></div>
  </div>
</div>

<!-- ── BEST & WORST ──────────────────────────────────────────── -->
<div class="insight-card">
  <h4>Best & Worst Performing Parties</h4>
  <p>
    <b>Best performing:</b> ', badge(best_party), '
    with <b>', best_pct, '%</b> vote share under current filters.<br>
    <b>Worst performing:</b> ', badge(worst_party), '
    with only <b>', worst_pct, '%</b> vote share.
  </p>
  <table style="width:100%;border-collapse:collapse;font-size:13px;margin-top:10px;">
    <thead>
      <tr style="background:#f0f2f5;">
        <th style="padding:7px 12px;text-align:left;">Party</th>
        <th style="padding:7px 12px;">Count</th>
        <th style="padding:7px 12px;">Share</th>
        <th style="padding:7px 12px;">Avg National Econ</th>
        <th style="padding:7px 12px;">Avg Household Econ</th>
      </tr>
    </thead>
    <tbody>',
    paste(sapply(1:nrow(vote_share), function(i) {
      pty <- as.character(vote_share$vote[i])
      en  <- econ_by_party %>% filter(vote == pty)
      nat <- if (nrow(en) > 0) en$nat else "—"
      hh  <- if (nrow(en) > 0) en$hh  else "—"
      paste0(
        '<tr style="border-bottom:1px solid #eee;">',
        '<td style="padding:7px 12px;">', badge(pty), '</td>',
        '<td style="padding:7px 12px;text-align:center;">', vote_share$n[i], '</td>',
        '<td style="padding:7px 12px;text-align:center;"><b>', vote_share$pct[i], '%</b></td>',
        '<td style="padding:7px 12px;text-align:center;">', nat, '</td>',
        '<td style="padding:7px 12px;text-align:center;">', hh,  '</td>',
        '</tr>'
      )
    }), collapse = ""),
  '  </tbody>
  </table>
</div>

<!-- ── AGE POLARISATION ──────────────────────────────────────── -->
<div class="insight-card">
  <h4>Age Polarisation</h4>
  <p>
    The electorate shows clear <b>age-based political polarisation</b>.
    ', badge("Conservative"), ' supporters skew older
    (avg: <b>', age_by_party$avg[age_by_party$vote == "Conservative"], '</b> yrs),
    while ', badge("Labour"), ' and ', badge("Liberal Democrat"), ' attract
    younger voters.<br><br>
    Among <b>18–29 year-olds</b>, Labour receives
    <b>', ifelse(is.na(young_labour), "—", paste0(young_labour, "%")), '</b>
    of the vote.<br>
    Among <b>65+</b>, Conservatives hold
    <b>', ifelse(is.na(old_tory), "—", paste0(old_tory, "%")), '</b>.
  </p>
  <div class="stat-grid">',
  paste(sapply(1:nrow(age_by_party), function(i) {
    pty <- as.character(age_by_party$vote[i])
    paste0('<div class="stat-tile"><div class="val">', age_by_party$avg[i], '</div>',
           '<div class="lbl">Avg Age<br>', pty, '</div></div>')
  }), collapse = ""),
  '</div>
</div>

<!-- ── ECONOMY & VOTING ──────────────────────────────────────── -->
<div class="insight-card">
  <h4>Economic Perception & Voting Behaviour</h4>
  <p>
    ', badge("Labour"), ' voters perceive both national and household economic conditions
    most positively — consistent with rewarding the incumbent government (Blair 1997–2001).<br>
    ', badge("Conservative"), ' voters rate national economic conditions most negatively,
    reflecting opposition sentiment.<br>
    ', badge("Liberal Democrat"), ' occupy an intermediate position on national economy but rate household
    conditions relatively low.
  </p>
  <p style="margin-top:8px;font-size:12px;color:#666;">
    Scale: 1 = much worse, 5 = much better
  </p>
</div>

<!-- ── LEADER BIAS ───────────────────────────────────────────── -->
<div class="insight-card">
  <h4>Leader Approval Bias</h4>
  <p>
    As expected, voters rate their own party leader highest.
    ', badge("Labour"), ' voters give Blair an average of
    <b>', df %>% filter(vote=="Labour") %>% summarise(m=round(mean(Blair,na.rm=T),2)) %>% pull(m), '</b> / 5.<br>
    The <b>Blair–Europe correlation</b> is
    <b>', ifelse(is.na(blair_corr), "—", blair_corr), '</b> —
    ', if (!is.na(blair_corr) && blair_corr < 0)
        "suggesting that stronger Blair supporters lean more pro-European."
      else if (!is.na(blair_corr) && blair_corr > 0)
        "suggesting that Blair approval is slightly associated with Euro-scepticism in this sample."
      else "insufficient data to determine direction.",
  '
  </p>
</div>

<!-- ── GENDER INSIGHT ────────────────────────────────────────── -->
<div class="insight-card">
  <h4>Gender & Voting</h4>
  <p>
    Labour support among <b>male</b> respondents:
    <b>', ifelse(is.na(male_labour_pct), "—", paste0(male_labour_pct, "%")), '</b><br>
    Labour support among <b>female</b> respondents:
    <b>', ifelse(is.na(female_labour_pct), "—", paste0(female_labour_pct, "%")), '</b><br><br>
    The gender gap in UK politics was relatively narrow in 2001,
    though women slightly favoured Labour at this election.
  </p>
</div>

<!-- ── ANOMALIES ─────────────────────────────────────────────── -->
<div class="insight-card">
  <h4>Anomalies & Noteworthy Observations</h4>
  <div class="anomaly-box">
    <ul style="margin:0;padding-left:18px;font-size:13px;">
      <li><b>Hague over-rated by Labour voters:</b>
          Labour supporters rate William Hague
          (<b>', df %>% filter(vote=="Labour") %>% summarise(m=round(mean(Hague,na.rm=T),2)) %>% pull(m), '</b>)
          somewhat highly for an opposition leader — possibly protest sentiment.</li>
      <li><b>Liberal Democrat crossover:</b>
          Lib Dem voters rate Kennedy and Blair almost equally,
          reflecting the party\'s centre-ground appeal.</li>
      <li><b>Low political knowledge:</b>
          A notable share of respondents score 0–1 on political knowledge yet still express
          clear vote intentions — suggesting instinctive or identity-based voting.</li>
      <li><b>Europe scale distribution:</b>
          The Europe variable clusters toward the middle (4–7), indicating most respondents
          hold moderate Eurosceptic views regardless of party.</li>
    </ul>
  </div>
</div>

<p style="font-size:11px;color:#999;text-align:right;margin-top:10px;">
  CSI 3005 | Advanced Data Visualization Techniques | VIT | BEPS 2001 Dataset
</p>
    '))
  })

} # /server
