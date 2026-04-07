# BEPS 2001 Election Analysis Dashboard
### CSI 3005 — Advanced Data Visualization Techniques 

---

## 🚀 LIVE DASHBOARD

> **[▶ CLICK HERE TO OPEN THE LIVE DASHBOARD](https://aravindhashan23mid0222.shinyapps.io/beps-advanced-data-viz-dashboard/)**

---

##  Project Overview

An interactive **R Shiny** dashboard built on the **British Election Panel Study (BEPS) 2001** dataset.  
The dashboard demonstrates advanced data-visualization techniques.

| Item | Detail |
|------|--------|
| **Dataset** | BEPS 2001 — 1,525 UK respondents |
| **Framework** | R + Shiny + shinydashboard + Plotly |
| **Charts** | 10+ interactive Plotly charts |
| **Filters** | Gender · Age Group · Political Knowledge · Europe Score |

---

##  Dashboard Tabs

| Tab | Visualizations |
|-----|----------------|
| ** Overview** | Donut (vote share) · Bar chart (count) · Grouped bar (by gender) · 4 value boxes |
| ** Age Analysis** | 100% Stacked bar by age group · Box plot · Scatter (age vs vote) |
| ** Economy** | Heatmap (avg econ perception) · Bubble plot · Box plot |
| ** Leaders** | Blair vs Europe scatter + trend lines · Grouped bar (leader ratings) |
| ** Insights** | Full HTML analytics report: age polarisation, economy impact, leader bias, anomalies |



##  Setup & Run Locally

```r
# Install dependencies (run once)
install.packages(c("shiny", "shinydashboard", "plotly", "dplyr", "tidyr"))

# Clone / unzip the project, then:
shiny::runApp(".")
```

---

##  File Structure

```
BEPS-Advanced-Data-Viz-Dashboard/
├── app.R          # Entry point – sources ui.R + server.R
├── ui.R           # Dashboard layout, CSS, all tab definitions
├── server.R       # Reactive logic, all chart rendering, insights
├── global.R       # Data load, preprocessing, shared globals
├── BEPS.csv       # Raw dataset (1,525 rows × 10 variables)
└── README.md      # This file
```

---

## 👤 Author

**Om Aravindhashan S** | **23MID0222** | VIT  
Course: CSI 3005 — Advanced Data Visualization Techniques  
Submitted: April 2026
