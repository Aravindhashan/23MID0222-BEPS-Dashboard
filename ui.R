# ui.R – Dashboard layout
# CSI 3005 | Advanced Data Visualization Techniques | VIT

source("global.R")

# ── Custom CSS ─────────────────────────────────────────────────────────────────
custom_css <- tags$head(tags$style(HTML("
  /* ── Sidebar ── */
  .main-sidebar { background: #1a1a2e !important; }
  .sidebar-menu > li > a { color: #e0e0e0 !important; font-size: 13px; }
  .sidebar-menu > li.active > a,
  .sidebar-menu > li > a:hover { color: #FFD700 !important; background: #16213e !important; }

  /* ── Header ── */
  .main-header .logo { background: #16213e !important; color: #FFD700 !important;
                        font-weight: 700; font-size: 16px; }
  .main-header .navbar { background: #16213e !important; }
  .main-header .navbar .sidebar-toggle { color: #FFD700 !important; }

  /* ── Body ── */
  .content-wrapper, .right-side { background: #f0f2f5; }
  .box { border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,.12); }
  .box-header { background: #f8f9fa; border-radius: 8px 8px 0 0; }
  .box-title { font-weight: 600; color: #1a1a2e; font-size: 14px; }

  /* ── Value boxes ── */
  .small-box { border-radius: 8px !important; }
  .small-box h3 { font-size: 28px; }

  /* ── Insights tab ── */
  .insight-card { background:#fff; border-radius:10px; padding:18px 22px;
                  margin-bottom:16px; box-shadow:0 2px 8px rgba(0,0,0,.10); }
  .insight-card h4 { margin-top:0; font-size:15px; font-weight:700;
                      border-left:4px solid #E4002B; padding-left:10px; }
  .stat-grid { display:grid; grid-template-columns: repeat(auto-fill, minmax(160px,1fr));
               gap:10px; margin-top:10px; }
  .stat-tile { background:#f8f9fa; border-radius:8px; padding:12px 16px;
               text-align:center; }
  .stat-tile .val { font-size:22px; font-weight:700; color:#1a1a2e; }
  .stat-tile .lbl { font-size:11px; color:#666; text-transform:uppercase;
                    letter-spacing:.5px; margin-top:2px; }
  .badge-labour   { background:#E4002B; color:#fff; padding:2px 8px;
                    border-radius:12px; font-size:11px; }
  .badge-tory     { background:#0033A0; color:#fff; padding:2px 8px;
                    border-radius:12px; font-size:11px; }
  .badge-libdem   { background:#FF9F00; color:#fff; padding:2px 8px;
                    border-radius:12px; font-size:11px; }
  .anomaly-box    { background:#fff8e1; border:1px solid #FFD700;
                    border-radius:8px; padding:12px 16px; }

  /* ── Sidebar filter labels ── */
  .sidebar-label { color:#aaa; font-size:11px; text-transform:uppercase;
                   letter-spacing:.5px; padding:6px 15px 2px; }
  .sidebar hr { border-color:#333; }

  /* ── Reset button ── */
  #reset { width:85%; margin:8px auto; display:block;
           background:#E4002B; border:none; color:#fff;
           border-radius:6px; padding:7px; font-size:12px; font-weight:600; }
  #reset:hover { background:#b8001f; }
")))

# ── Dashboard ─────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",

  # ── Header ──────────────────────────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://i.imgur.com/PB6VQGK.png",
               height = "22px", style = "margin-right:6px;"),
      "23MID0222 - BEPS 2001 Dashboard"
    )
  ),

  # ── Sidebar ─────────────────────────────────────────────────────────────────
  dashboardSidebar(
    custom_css,
    sidebarMenu(
      id = "active_tab",
      menuItem("Overview",      tabName = "overview"),
      menuItem("Age Analysis",  tabName = "age"),
      menuItem("Economy",       tabName = "economy"),
      menuItem("Leaders",       tabName = "leaders"),
      menuItem("Insights",      tabName = "insights")
    ),
    tags$div(class = "sidebar-label", "FILTERS"),
    hr(),
    selectInput("gender_f",    "Gender:",
                choices = c("All", "male", "female"), selected = "All"),
    checkboxGroupInput(
      "age_group_f", "Age Group:",
      choices  = age_group_levels,
      selected = age_group_levels,
      inline   = FALSE
    ),
    selectInput("knowledge_f", "Pol. Knowledge:",
                choices = c("All", "0", "1", "2", "3"), selected = "All"),
    sliderInput("europe_f", "Europe Score:", 1, 11, c(1, 11), step = 1),
    actionButton("reset", "↺  Reset Filters")
  ),

  # ── Body ────────────────────────────────────────────────────────────────────
  dashboardBody(
    tabItems(

      # ── TAB 1 : Overview ──────────────────────────────────────────────────
      tabItem("overview",
        fluidRow(
          valueBoxOutput("box1", width = 3),
          valueBoxOutput("box2", width = 3),
          valueBoxOutput("box3", width = 3),
          valueBoxOutput("box4", width = 3)
        ),
        fluidRow(
          box(
            title       = "Vote Share Distribution",
            status      = "primary", solidHeader = TRUE,
            width       = 6,
            plotlyOutput("chart_donut", height = "320px")
          ),
          box(
            title       = "Vote Count by Party",
            status      = "primary", solidHeader = TRUE,
            width       = 6,
            plotlyOutput("chart1", height = "320px")
          )
        ),
        fluidRow(
          box(
            title       = "Vote by Gender",
            status      = "primary", solidHeader = TRUE,
            width       = 12,
            plotlyOutput("chart2", height = "300px")
          )
        )
      ),

      # ── TAB 2 : Age Analysis ─────────────────────────────────────────────
      tabItem("age",
        fluidRow(
          box(
            title       = "100% Stacked Bar – Vote Share by Age Group",
            status      = "warning", solidHeader = TRUE,
            width       = 12,
            plotlyOutput("chart_stacked100", height = "350px")
          )
        ),
        fluidRow(
          box(
            title       = "Age Distribution by Party (Box Plot)",
            status      = "warning", solidHeader = TRUE,
            width       = 6,
            plotlyOutput("chart4", height = "320px")
          ),
          box(
            title       = "Age vs Vote (Scatter)",
            status      = "warning", solidHeader = TRUE,
            width       = 6,
            plotlyOutput("chart3", height = "320px")
          )
        )
      ),

      # ── TAB 3 : Economy ──────────────────────────────────────────────────
      tabItem("economy",
        fluidRow(
          box(
            title       = "Economic Perception Heatmap (by Party)",
            status      = "success", solidHeader = TRUE,
            width       = 12,
            plotlyOutput("chart_heatmap", height = "380px")
          )
        ),
        fluidRow(
          box(
            title       = "National vs Household Economic Perception",
            status      = "success", solidHeader = TRUE,
            width       = 6,
            plotlyOutput("chart6", height = "320px")
          ),
          box(
            title       = "Economic Perception by Party (Box)",
            status      = "success", solidHeader = TRUE,
            width       = 6,
            plotlyOutput("chart5", height = "320px")
          )
        )
      ),

      # ── TAB 4 : Leaders ──────────────────────────────────────────────────
      tabItem("leaders",
        fluidRow(
          box(
            title       = "Blair vs Europe Attitude (Scatter + Trend)",
            status      = "danger", solidHeader = TRUE,
            width       = 12,
            plotlyOutput("chart_scatter_blair", height = "420px")
          )
        ),
        fluidRow(
          box(
            title       = "Average Leader Ratings by Vote",
            status      = "danger", solidHeader = TRUE,
            width       = 12,
            plotlyOutput("chart7", height = "320px")
          )
        )
      ),

      # ── TAB 5 : Insights ─────────────────────────────────────────────────
      tabItem("insights",
        fluidRow(
          box(
            title       = "Dashboard Insights & Analytics Report",
            status      = "primary", solidHeader = TRUE,
            width       = 12,
            uiOutput("insights")
          )
        )
      )
    ) # /tabItems
  )   # /dashboardBody
)     # /dashboardPage
