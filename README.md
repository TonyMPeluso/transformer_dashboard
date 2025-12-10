# 🚘 Transformer Dashboard — ZEV Charging Impacts on Transformer Substations

Interactive R Shiny dashboard to explore how Zero Emission Vehicle (ZEV) adoption and charging behaviour impact transformer substations in Ontario cities.

Using projected ZEV ownership by service area and flexible charging-behaviour assumptions, the dashboard simulates transformer loading over time and helps identify where grid stress may emerge as EV adoption accelerates.

## 🌐 Live Demo

👉 Transformer Dashboard for Toronto
https://tonympeluso.shinyapps.io/ZEV_CT_Forecast_Simulator_Tor/

💡 Tip: Click anywhere on the peak load time-series chart to update the map of transformer stations for that moment in time.

## 🧭 Overview

ZEV adoption introduces new evening and overnight loads that may cluster spatially. This dashboard links:
- Projected ZEV ownership to 2046 based on historic ZEV registrations, population projections and modelled ZEV adoption rates
- Charging behaviour (Level 1 vs Level 2, evening vs non-evening charging)
- Service-area-level load aggregation
- Transformer-level peak load trajectories
- Interactive geospatial visualization

The result is a planning tool for utilities, municipalities, and analysts evaluating EV-driven infrastructure needs.

## ✨ Key Features
### 🔋 ZEV Adoption & Charging Behaviour Controls
- Year selection: view adoption and loads in 2025 → 2046
- Slider: Share of Level 1 charging
- Slider: Share of charging done in the evening
  - Helps explore how customer behaviour shifts peak loads

### 📈 Transformer Peak Load Trajectories
- Time-series of transformer station peaks over the selected year
- Shows how peak load intensity shifts as ZEV numbers grow
- Highlights potential overload trends

### 🗺️ Interactive Transformer Map
- Clicking the time-series chart updates the map to the selected time
- Displays:
  - Transformer station locations
  - Load magnitude (colour or size)
  - Spatial distribution of ZEV-related transformer stress

### 🎯 Designed for Utility Planning
- Identify hotspot substations where EV charging impacts accumulate
- Understand how evening-heavy charging amplifies peak loads
- Evaluate the spatial and temporal evolution of electrification impacts

### 🛠️ Technology
- R Shiny for interactive UI
- cancensus for census + service area inputs
- sf, dplyr for spatial and data processing
- leaflet for mapping
- ggplot2 / plotly for charts

✔️ Runs in plain R — no special configuration needed.
Only requirement: user must supply their own cancensus API key.

## 📦 Project Structure
```
transformer_dashboard/
├── app_dashboard.R             # Main Shiny app
├── 00_read_and_write_*.R       # Data download + preparation
├── 01_read_and_write_*.R       # ZEV projections + transformer loading inputs
├── data/
│   ├── zev_projected.rds
│   ├── transformer_geometries.rds
│   ├── ...
├── www/                        # (Optional) assets (images/css)
├── README.md
└── .gitignore
```


(Update filenames if they differ in your repo.)

## 📥 Installation & Setup
### 1️⃣ Clone the repository
```
git clone https://github.com/TonyMPeluso/transformer_dashboard.git
cd transformer_dashboard
```
### 2️⃣ Install required R packages
```
install.packages(c("shiny", "cancensus", "sf", "dplyr", "leaflet", "ggplot2", "plotly"))
```

### 3️⃣ Set your cancensus API key

Create one at: https://mountainmath.github.io/cancensus/

Then in R:
```
cancensus::set_cancensus_api_key("YOUR_KEY")
```

Or store it in .Renviron:
```
CANCENSUS_API_KEY=YOUR_KEY
```

### 4️⃣ Run the pipeline + dashboard
source("00_read_and_write_*.R")
source("01_read_and_write_*.R")
shiny::runApp("app_dashboard.R")

##🔍 How to Use the Dashboard
 
Choose the analysis year (e.g., 2030, 2035, 2046)

Adjust charging behaviour:

Level 1 vs Level 2 charging mix

Share of charging done in the evening

View peak transformer loads over time

Click the chart → map updates for that time point

Explore spatial patterns:

Where charging demand concentrates

Which transformers face the highest evening peaks

How risk evolves across years and behaviours

## 🧪 Use Cases
### For Utilities
- Identify transformers most at risk of overload as ZEV adoption grows
- Evaluate the benefit of shifting charging behaviour or managed charging

### For Municipalities / Planners
- Understand electrification pressures at the neighbourhood level
- Plan infrastructure upgrades earlier and more efficiently

### For Researchers
- Demonstrate time-series ↔ spatial linkages in electrification impacts

## 🧩 Related Projects (same portfolio)
- Feeder-Level Winter Peak Microsimulation
- Transformer Thermal Aging (Monte Carlo)
- Investment Optimization Dashboard

Together, these form a comprehensive suite for distribution planning under electrification.

## 📄 License

MIT License (see LICENCE.txt)

## 👤 Author

Tony Peluso, PhD Energy Modelling & Grid Analytics — Montreal, QC
📧 tonympeluso@gmail.com 🔗 GitHub: https://github.com/TonyMPeluso 🔗 LinkedIn: https://www.linkedin.com/in/tony-peluso-phd
