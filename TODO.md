---
output:
  word_document: default
  html_document: default
---

<!-- 
PROMPT: Help me come up with a planning document TODO.md in the next 2 weeks to 
complete the contract with analyses and deliverables described in 
@contract.md 
(created from: `cd '~/My Drive/contracts/ricei '25'; pandoc -f docx -t markdown -o contract.md contract.docx`), 
given existing work from @goa.qmd, the Shiny app @app_company/ and the 
previously referenced report at @index.qmd. I will need to intersect whale 
densities with ship presence using available parameters from ship on weight and 
length in a way as to inform on minimizing impacts and report out for specific 
subareas.
-->

# Rice's Whale Ship-Strike Risk Analysis

**NRDC Contract - Statement of Work Implementation Plan**

## Overview

This document outlines the tasks to analyze ship-strike risk reduction for Rice's whale (*Balaenoptera ricei*) using 2023 AIS data and Litz et al. (2022) whale densities. The analysis supports NRDC outreach to 16 major shipping companies regarding voluntary slowdown initiatives.

## Data Inventory

| Data | Location | Records | Notes |
|------|----------|---------|-------|
| 2023 AIS traffic | `goa.duckdb` ship_cell | 13.8M | speed, distance, hours by cell/date |
| Ship registry | `goa.duckdb` ship | 32,838 | `length_m`, `weight_gt`, `operator` |
| 16 companies | `goa.duckdb` company | 16 | Already mapped to operators |
| Whale densities | `data/whales.geojson` + `whales.csv` | - | Litz 2022, hexagonal grid |
| 7 subareas | `data/crithab_subareas.geojson` | 7 | Fairways/port approaches |
| Critical habitat | `data/nmfs_crit_hab.geojson` | - | 100-400m depth zone |
| Study area | `data/goa.geojson` | - | U.S. Gulf of Mexico EEZ |

## Risk Model

**Primary approach:** Conn/Silber 2013 speed-based risk factors

| Speed Bin | Risk Factor | Rationale |
|-----------|-------------|-----------|
| ≤10 kt | 1.0 | Baseline (slowdown target) |
| >10-12 kt | 1.6 | Midpoint 11 kt |
| >12-15 kt | 5.4 | Midpoint 13.5 kt |
| >15 kt | 10.8 | Conservative at 15 kt |

**Future enhancement:** Blondin/Garrison 2025 multifactor model (integrates vessel size, whale avoidance) if papers become available.

---

## Phase 1: Data Preparation

### 1.1 Speed Binning
- [ ] Create speed bin categories in ship_cell data
  - Bin 1: ≤10 kt (risk factor 1.0)
  - Bin 2: >10-12 kt (risk factor 1.6)
  - Bin 3: >12-15 kt (risk factor 5.4)
  - Bin 4: >15 kt (risk factor 10.8)

### 1.2 Spatial Intersections
- [ ] Intersect whale hexagon grid with ship_cell grid
- [ ] Intersect results with critical habitat boundary
- [ ] Intersect results with 7 subareas
- [ ] Generate lookup table: cell_id -> subarea_id mapping

### 1.3 Company Fleet Mapping
- [ ] Verify company-operator lookup mappings
- [ ] Join company fleet data to ship_cell via operator lookup
- [ ] Generate per-company traffic summaries

---

## Phase 2: Risk Calculations

### 2.1 Core Risk Functions
- [ ] `calc_base_risk()` - whale density × traffic (no speed weighting)
- [ ] `calc_speed_weighted_risk()` - apply Conn/Silber factors
- [ ] `calc_risk_reduction()` - compare baseline vs slowdown scenarios

### 2.2 Baseline Risk by Area
Calculate for each geographic unit:
- [ ] Study area (U.S. Gulf of Mexico EEZ)
- [ ] Critical habitat (100-400m depth)
- [ ] Each of 7 subareas (fairways/port approaches)

### 2.3 Company-Specific Risk
- [ ] Per-company baseline risk in each area
- [ ] Percentage contribution to total risk by company

---

## Phase 3: Risk Reduction Scenarios

### 3.1 Universal Slowdown (All vessels ≤10 kt)
- [ ] Recalculate risk assuming all speeds ≤10 kt
- [ ] Compare to baseline for absolute reduction
- [ ] Calculate percentage reduction by area

### 3.2 Subarea 4 Rerouting
- [ ] Use 1000m depth whale density instead of subarea 4 density
- [ ] Calculate risk reduction from avoidance vs slowdown
- [ ] Compare rerouting vs slowdown benefits

### 3.3 Nighttime Scheduling (if data available)
- [ ] Check if day/night transit data exists
- [ ] If available, calculate daytime-only transit risk reduction

### 3.4 Per-Company Slowdown Benefits
- [ ] Risk reduction if company X slows to ≤10 kt
- [ ] Relative to company's own baseline (not all vessels)
- [ ] By area: study area, critical habitat, each subarea

---

## Phase 4: Deliverables

### D1: Maps
- [ ] **D1a.** Rice's whale densities with critical habitat + subareas overlay
- [ ] **D1b.** 2023 vessel traffic (all vessels, all speeds)
- [ ] **D1c.** Strike risk (all vessels, all speeds)

### D2: Whale Density Table
- [ ] Whale densities summarized by:
  - Study area (U.S. Gulf of Mexico)
  - Critical habitat area

### D3: Risk Table (No Speed Weighting)
- [ ] Total strike risk by area (whale × traffic, unweighted)
- [ ] Percentage contribution to total by each area

### D4: Risk Table (Speed-Weighted)
- [ ] Total strike risk by area (Conn/Silber weighted)
- [ ] Percentage contribution to total by each area

### D5: Risk Reduction Table
- [ ] Risk reduction from universal slowdown (≤10 kt)
- [ ] Contribution of each area to total reduction
- [ ] Subarea 4 rerouting alternative
- [ ] Nighttime scheduling (if data available)

### D6: Graphics
- [ ] Map with risk reduction % overlaid on subareas
- [ ] Pie chart of risk distribution by area

### D7: Company-Specific Tables
For each of 16 companies:
- [ ] Baseline risk (by company fleet, by area)
- [ ] Risk reduction from slowdown (relative to company total)

### D8: Company Graphics
- [ ] Per-company risk reduction charts
- [ ] Comparison across companies

---

## 16 Focal Companies

1. CMA CGM
2. Stolt Tankers
3. MSC
4. Maersk
5. Hapag-Lloyd
6. Carnival Cruise Line
7. Royal Caribbean Cruises Ltd
8. OKEE Maritime GmbH
9. MOL Chemical Tankers Pte Ltd
10. TUICruises
11. Oldendorff Carriers
12. Cargill
13. Rudolf Schepers Reederei GmbH
14. Rudolf A Oetker A/S & Co KG
15. Seabulk Tankers Inc
16. Crowley Petroleum Services Inc

---

## File Structure

```
ricei/
├── TODO.md                      # This planning document
├── analysis/
│   └── risk_analysis.qmd        # Main analysis notebook
├── scripts/
│   └── risk_functions.R         # Risk calculation functions
├── data/
│   ├── goa.duckdb              # AIS + ship data (external)
│   ├── whales.geojson          # Whale density hexagons
│   ├── crithab_subareas.geojson # 7 subareas
│   ├── nmfs_crit_hab.geojson   # Critical habitat boundary
│   └── goa.geojson             # Study area boundary
└── app_company/                 # Existing Shiny app
```

---

## Verification Checklist

- [ ] Sum of subarea risks equals critical habitat total
- [ ] Risk reduction percentages consistent across methods
- [ ] Company risk totals sum to fleet-wide totals
- [ ] Traffic patterns match existing app_company outputs
- [ ] Spatial intersections cover full study area without gaps

---

## References

- Best (unpub.) - Previous ship-strike risk analysis
- Benioff (unpub.) - 2023 AIS analysis, company fleets, subareas
- Conn & Silber 2013 - Speed-based risk factors
- Litz et al. 2022 - Rice's whale density model
- NMFS 2020 - Service spatial risk analysis
- NMFS 2023 - Critical habitat proposal
