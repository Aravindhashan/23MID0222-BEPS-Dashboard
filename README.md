# BEPS 2001 Election Analysis Dashboard
### CSI 3005 — Advanced Data Visualization Techniques | VIT

---

## 🚀 LIVE DASHBOARD

> **[▶ CLICK HERE TO OPEN THE LIVE DASHBOARD](https://YOUR-SHINYAPPS-USERNAME.shinyapps.io/BEPS-Dashboard/)**
>
> *(Replace the URL above with your deployed shinyapps.io / Posit Cloud link before submission)*

---

## 📋 Project Overview

An interactive **R Shiny** dashboard built on the **British Election Panel Study (BEPS) 2001** dataset.  
The dashboard demonstrates advanced data-visualization techniques as required by **CSI 3005** at VIT.

| Item | Detail |
|------|--------|
| **Dataset** | BEPS 2001 — 1,525 UK respondents |
| **Framework** | R + Shiny + shinydashboard + Plotly |
| **Charts** | 10+ interactive Plotly charts |
| **Filters** | Gender · Age Group · Political Knowledge · Europe Score |

---

## 🗂️ Dashboard Tabs

| Tab | Visualizations |
|-----|----------------|
| **📊 Overview** | Donut (vote share) · Bar chart (count) · Grouped bar (by gender) · 4 value boxes |
| **👥 Age Analysis** | 100% Stacked bar by age group · Box plot · Scatter (age vs vote) |
| **💷 Economy** | Heatmap (avg econ perception) · Bubble plot · Box plot |
| **🎙️ Leaders** | Blair vs Europe scatter + trend lines · Grouped bar (leader ratings) |
| **💡 Insights** | Full HTML analytics report: age polarisation, economy impact, leader bias, anomalies |

---

## ✅ Assignment Feature Checklist

- [x] **Time-series / Trend** — 100% stacked bar by age group (age polarisation over cohorts)
- [x] **Overall Vote Share** — Donut chart in Overview tab
- [x] **Relationship between variables** — Blair vs Europe scatter with LOESS trend line, coloured by vote, sized by age
- [x] **Heatmap** — Avg National + Household economic perception by party
- [x] **Age Group Filter** — Multi-select checkboxes (18-29, 30-49, 50-64, 65+) in sidebar
- [x] **All filters reactive** — Gender, Knowledge, Europe, Age Group update every chart instantly
- [x] **Click-to-filter** — Click a bar in the vote-count chart or stacked-age chart → filters the whole dashboard
- [x] **Insights Tab** — Age polarisation · Economy impact · Leader bias · Gender gap · Anomalies; rich formatted HTML
- [x] **Party colours** — Exact hex codes used throughout (#E4002B, #0033A0, #FF9F00)
- [x] **Custom tooltips** — Every Plotly chart shows %, count, avg age, group size
- [x] **Reset button** — Resets all sidebar filters and click-state in one click

---

## 🛠️ Setup & Run Locally

```r
# Install dependencies (run once)
install.packages(c("shiny", "shinydashboard", "plotly", "dplyr", "tidyr"))

# Clone / unzip the project, then:
shiny::runApp(".")
```

---

## 📁 File Structure

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

**[Your Name]** | **[Registration Number]** | VIT  
Course: CSI 3005 — Advanced Data Visualization Techniques  
Submitted: April 2026
