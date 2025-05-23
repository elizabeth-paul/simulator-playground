
# Load required package
library(shiny)
library(bslib)
library(janitor)
library(bsicons)
library(tidyverse)
library(plotly)
library(scales)
library(viridis)
library(DT)
library(markdown)

bw_primary <- c("#6D1E4A", # 1 plum
                "#007786", # 2 teal
                "#0D525A", # 3 dark green
                "#212B46", # 4 navy
                "#5A6675", # 5 grey
                "#F0DEC1") # 6 cream

bw_secondary <- c("#FFC762", # 1 yellow
                  "#FFB653", # 2 orange
                  "#BEC6CE", # 3 light grey
                  "#2E1A4A", # 4 deep purple
                  "#7EA2D1", # 5 soft blue
                  "#CAD3FB", # 6 lavender
                  "#9CD4EA", # 7 sky
                  "#FFA497") # 8 peach

# load dist data
demo_dist <- read_rds("data/traditional_lea_23_24.rds") 


# shiny server ---------
server <- function(input, output, session) {

observeEvent(input$apply_scenario, {

  if (input$scenario == "Default") {
    updateSliderInput(session, "base_amt", value = 8000)
    updateSliderInput(session, "ed_weight", value = 25)
    updateSliderInput(session, "conc_pov_weight", value = 5)
    updateSliderInput(session, "conc_pov_eligible", value = c(20, 60))
    
    updateSliderInput(session, "sped_weight_tier_i", value = 20)
    updateSliderInput(session, "sped_weight_tier_ii", value = 50)
    updateSliderInput(session, "sped_weight_tier_iii", value = 100)
    
    updateSliderInput(session, "el_weight", value = 15)
    updateSliderInput(session, "conc_el_type", value = "Tiered")
    updateSliderInput(session, "conc_el_weight_tiered", value = 3)
    updateSliderInput(session, "conc_el_eligible_tier", value = 9)
    
    updateSliderInput(session, "rural_weight", value = 5)
    updateSliderInput(session, "rural_elig", value = c(1, 15))
    
    updateSliderInput(session, "charter_weight", value = 2)
    
  } else if (input$scenario == "Scenario 1: Higher Poverty Weights") {
    updateSliderInput(session, "base_amt", value = 8000)
    updateSliderInput(session, "ed_weight", value = 35)
    updateSliderInput(session, "conc_pov_weight", value = 7)
    updateSliderInput(session, "conc_pov_eligible", value = c(40, 80))
    
    updateSliderInput(session, "sped_weight_tier_i", value = 20)
    updateSliderInput(session, "sped_weight_tier_ii", value = 50)
    updateSliderInput(session, "sped_weight_tier_iii", value = 100)
    
    updateSliderInput(session, "el_weight", value = 15)
    updateSliderInput(session, "conc_el_type", value = "Tiered")
    updateSliderInput(session, "conc_el_weight_tiered", value = 3)
    updateSliderInput(session, "conc_el_eligible_tier", value = 9)
    
    updateSliderInput(session, "rural_weight", value = 5)
    updateSliderInput(session, "rural_elig", value = c(1, 15))
    
    updateSliderInput(session, "charter_weight", value = 2)
    
  } else if (input$scenario == "Scenario 2: Higher Base Amount") {
    updateSliderInput(session, "base_amt", value = 9000)
    updateSliderInput(session, "ed_weight", value = 25)
    updateSliderInput(session, "conc_pov_weight", value = 5)
    updateSliderInput(session, "conc_pov_eligible", value = c(20, 60))
    
    updateSliderInput(session, "sped_weight_tier_i", value = 20)
    updateSliderInput(session, "sped_weight_tier_ii", value = 50)
    updateSliderInput(session, "sped_weight_tier_iii", value = 100)
    
    updateSliderInput(session, "el_weight", value = 15)
    updateSliderInput(session, "conc_el_type", value = "Tiered")
    updateSliderInput(session, "conc_el_weight_tiered", value = 3)
    updateSliderInput(session, "conc_el_eligible_tier", value = 9)
    
    updateSliderInput(session, "rural_weight", value = 5)
    updateSliderInput(session, "rural_elig", value = c(1, 15))
    
    updateSliderInput(session, "charter_weight", value = 2)
    
  } else if (input$scenario == "Scenario 3: Higher SPED and EL Weights") {
    updateSliderInput(session, "base_amt", value = 8000)
    updateSliderInput(session, "ed_weight", value = 25)
    updateSliderInput(session, "conc_pov_weight", value = 5)
    updateSliderInput(session, "conc_pov_eligible", value = c(20, 60))
    
    updateSliderInput(session, "sped_weight_tier_i", value = 25)
    updateSliderInput(session, "sped_weight_tier_ii", value = 75)
    updateSliderInput(session, "sped_weight_tier_iii", value = 115)
    
    updateSliderInput(session, "el_weight", value = 20)
    updateSliderInput(session, "conc_el_type", value = "Escalating")
    updateSliderInput(session, "conc_el_weight", value = 5)
    updateSliderInput(session, "conc_el_eligible", value = c(5, 15))
    
    updateSliderInput(session, "rural_weight", value = 5)
    updateSliderInput(session, "rural_elig", value = c(1, 15))
    
    updateSliderInput(session, "charter_weight", value = 2)
    
  } 

})


  # calculate weighted funding -----------
  demo_model <- reactive({
    
    demo_dist |> 
      
      # calculate weighted funding inputs
      mutate(weight_base = input$base_amt,
             ed_wt = input$ed_weight / 100, 
             conc_pov_max = input$conc_pov_weight / 100 , 
             conc_pov_elig_min = input$conc_pov_eligible[1] / 100,
             conc_pov_elig_max = input$conc_pov_eligible[2] / 100,
             conc_pov_adj = case_when(ed_pct <= conc_pov_elig_min ~ 0,
                                      ed_pct >= conc_pov_elig_max ~ 1,
                                      TRUE ~ (ed_pct - conc_pov_elig_min) / 
                                        (conc_pov_elig_max - conc_pov_elig_min)
                                      
             ),
             conc_pov_net_wt = conc_pov_max * conc_pov_adj,
             sped_tier_i_wt = input$sped_weight_tier_i / 100, 
             sped_tier_ii_wt =input$sped_weight_tier_ii / 100,
             sped_tier_iii_wt = input$sped_weight_tier_iii / 100, 
             sped_tier_i_adm = case_when(input$sld_tier == 1 ~ swd_sld, TRUE ~ 0) +
               case_when(input$sli_tier == 1 ~ swd_sli, TRUE ~ 0) +
               case_when(input$ohi_tier == 1 ~ swd_ohi, TRUE ~ 0) +
               case_when(input$aut_tier == 1 ~ swd_aut, TRUE ~ 0) +
               case_when(input$id_tier == 1 ~ swd_id, TRUE ~ 0) +
               case_when(input$dd_tier == 1 ~ swd_dd, TRUE ~ 0) +
               case_when(input$md_tier == 1 ~ swd_md, TRUE ~ 0) +
               case_when(input$ed_tier == 1 ~ swd_ed, TRUE ~ 0) +
               case_when(input$hi_tier == 1 ~ swd_hi, TRUE ~ 0) +
               case_when(input$oi_tier == 1 ~ swd_oi, TRUE ~ 0) +
               case_when(input$vi_tier == 1 ~ swd_vi, TRUE ~ 0) +
               case_when(input$db_tier == 1 ~ swd_db, TRUE ~ 0) +
               case_when(input$tbi_tier == 1 ~ swd_tbi, TRUE ~ 0) , 
             sped_tier_ii_adm = case_when(input$sld_tier == 2 ~ swd_sld, TRUE ~ 0) +
               case_when(input$sli_tier == 2 ~ swd_sli, TRUE ~ 0) +
               case_when(input$ohi_tier == 2 ~ swd_ohi, TRUE ~ 0) +
               case_when(input$aut_tier == 2 ~ swd_aut, TRUE ~ 0) +
               case_when(input$id_tier == 2 ~ swd_id, TRUE ~ 0) +
               case_when(input$dd_tier == 2 ~ swd_dd, TRUE ~ 0) +
               case_when(input$md_tier == 2 ~ swd_md, TRUE ~ 0) +
               case_when(input$ed_tier == 2 ~ swd_ed, TRUE ~ 0) +
               case_when(input$hi_tier == 2 ~ swd_hi, TRUE ~ 0) +
               case_when(input$oi_tier == 2 ~ swd_oi, TRUE ~ 0) +
               case_when(input$vi_tier == 2 ~ swd_vi, TRUE ~ 0) +
               case_when(input$db_tier == 2 ~ swd_db, TRUE ~ 0) +
               case_when(input$tbi_tier == 2 ~ swd_tbi, TRUE ~ 0) , 
             sped_tier_iii_adm = case_when(input$sld_tier == 3 ~ swd_sld, TRUE ~ 0) +
               case_when(input$sli_tier == 3 ~ swd_sli, TRUE ~ 0) +
               case_when(input$ohi_tier == 3 ~ swd_ohi, TRUE ~ 0) +
               case_when(input$aut_tier == 3 ~ swd_aut, TRUE ~ 0) +
               case_when(input$id_tier == 3 ~ swd_id, TRUE ~ 0) +
               case_when(input$dd_tier == 3 ~ swd_dd, TRUE ~ 0) +
               case_when(input$md_tier == 3 ~ swd_md, TRUE ~ 0) +
               case_when(input$ed_tier == 3 ~ swd_ed, TRUE ~ 0) +
               case_when(input$hi_tier == 3 ~ swd_hi, TRUE ~ 0) +
               case_when(input$oi_tier == 3 ~ swd_oi, TRUE ~ 0) +
               case_when(input$vi_tier == 3 ~ swd_vi, TRUE ~ 0) +
               case_when(input$db_tier == 3 ~ swd_db, TRUE ~ 0) +
               case_when(input$tbi_tier == 3 ~ swd_tbi, TRUE ~ 0)  ,
             
             sped_tier_i_pct_calc = sped_tier_i_adm / adm, 
             sped_tier_ii_pct_calc = sped_tier_ii_adm / adm, 
             sped_tier_iii_pct_calc = sped_tier_iii_adm / adm, 
             
             el_wt_raw = input$el_weight / 100,
             conc_el_max = input$conc_el_weight / 100,
             conc_el_elig_min = input$conc_el_eligible[1] / 100,
             conc_el_elig_max = input$conc_el_eligible[2] / 100,
             conc_el_adj = case_when(el_pct <= conc_el_elig_min ~ 0,
                                      el_pct >= conc_el_elig_max ~ 1,
                                      TRUE ~ (el_pct - conc_el_elig_min) / 
                                        (conc_el_elig_max - conc_el_elig_min)
                                      
             ),
             conc_el_tiered_wt = input$conc_el_weight_tiered / 100,
             conc_el_tiered_elig = input$conc_el_eligible_tier / 100,
             conc_el_tiered_calc = case_when(el_pct >= conc_el_tiered_elig ~ conc_el_tiered_wt,
                                            TRUE ~ 0),
             conc_el_type = input$conc_el_type,
             el_wt = case_when(conc_el_type == "Escalating" ~  (conc_el_max * conc_el_adj) + el_wt_raw,
                               conc_el_type == "Tiered" ~ conc_el_tiered_calc + el_wt_raw),
             el_conc = el_wt - el_wt_raw,
             el_conc_adm = case_when(el_conc > 0 ~ el_adm,
                                     TRUE ~ 0),
             
             s_per_sq_mi = adm / sq_mi,
             
             rural_wt = input$rural_weight / 100,
             
             rural_wt_adj = case_when(s_per_sq_mi > input$rural_elig[2] ~ 0,
                                          s_per_sq_mi <= input$rural_elig[1] ~ 1,
                                          TRUE ~ (input$rural_elig[2] - s_per_sq_mi) / 
                                            (input$rural_elig[2] - input$rural_elig[1])),
             rural_net_wt = rural_wt * rural_wt_adj,
             
             charter_wt = input$charter_weight / 100,
             
             charter_cat = case_when(charter_status == "Charter" ~ 1,
                                        TRUE ~ 0),
             # hard-code Montgomery County conversion charter ADM
             charter_adm = case_when(dist_id == "051" ~ 1538.85,
                                     TRUE ~ adm * charter_cat)
             
             ) |> 
      # calculate weighted funding amounts
      mutate(base_total = weight_base * adm,
             
             poverty_weight_core = ed_adm * weight_base * ed_wt,
             poverty_weight_conc = ed_adm * weight_base * conc_pov_net_wt,
             poverty_weight_total = poverty_weight_core + poverty_weight_conc,
             

             sped_weight_tier_i_raw = sped_tier_i_adm * weight_base * sped_tier_i_wt,
             sped_weight_tier_ii_raw = sped_tier_ii_adm * weight_base * sped_tier_ii_wt,
             sped_weight_tier_iii_raw =  sped_tier_iii_adm * weight_base * sped_tier_iii_wt,
             
             sped_weight_tier_i = sped_weight_tier_i_raw,
             sped_weight_tier_ii =  sped_weight_tier_ii_raw,
             sped_weight_tier_iii = sped_weight_tier_iii_raw,
             
             
             sped_weight_total = sped_weight_tier_i + sped_weight_tier_ii +
               sped_weight_tier_iii,
             
             
             el_weight_base_raw = el_adm * weight_base * el_wt_raw,
             
             el_weight_base = el_weight_base_raw,
             
             el_weight_conc_raw = el_adm * weight_base * (el_wt - el_wt_raw),
             
             el_weight_conc = el_weight_base_raw,
             
             el_weight_total_raw = el_adm * weight_base * el_wt,
             
             el_weight_total = el_weight_total_raw,
             

             
            
             rural_weight_total = case_when(is.na(s_per_sq_mi) ~ 0,
                                            TRUE ~ adm * weight_base * rural_net_wt),
             rural_weight_diff = rural_weight_total,
             
             charter_weight_total_raw = charter_adm * charter_wt * weight_base,
             
             charter_weight_total = charter_weight_total_raw,
             

             
             weight_total = poverty_weight_total + sped_weight_total +
               el_weight_total +  rural_weight_total + charter_weight_total,
             weight_total_pp = weight_total / adm,
             formula_total = base_total + weight_total,
             formula_pp = formula_total / adm,
             current_pp = current_total / adm,
             total_diff = formula_total - current_total,
             pp_diff = total_diff / adm
             
             )
    
    
  })
  
  state_summary <- reactive({
    
    demo_model() |> summarise(
      base_base = mean(weight_base, na.rm = T),
      base_weight_pct = NA,
      base_adm = sum(adm, na.rm = T),
      base_weight_total = sum(base_total, na.rm = T),

      poverty_base = mean(weight_base, na.rm = T),
      poverty_weight_pct = mean(ed_wt, na.rm = T),
      poverty_adm = sum(ed_adm, na.rm = T),
      poverty_weight_total = sum(poverty_weight_total, na.rm = T),

      sped1_base = mean(weight_base, na.rm = T),
      sped2_base = mean(weight_base, na.rm = T),
      sped3_base = mean(weight_base, na.rm = T),
      sped1_weight_pct = mean(sped_tier_i_wt, na.rm = T),
      sped2_weight_pct = mean(sped_tier_ii_wt, na.rm = T),
      sped3_weight_pct = mean(sped_tier_iii_wt, na.rm = T),
      sped1_adm = sum(sped_tier_i_adm, na.rm = T),
      sped2_adm = sum(sped_tier_ii_adm, na.rm = T),
      sped3_adm = sum(sped_tier_iii_adm, na.rm = T),
      sped1_weight_total = sum(sped_weight_tier_i, na.rm = T),
      sped2_weight_total = sum(sped_weight_tier_ii, na.rm = T),
      sped3_weight_total = sum(sped_weight_tier_iii, na.rm = T),
      sped_weight_total = sum(sped_weight_total, na.rm = T),
                
      elraw_base = mean(weight_base, na.rm = T),
      elconc_base = mean(weight_base, na.rm = T),
      elraw_weight_pct = mean(el_wt_raw, na.rm = T),
      elconc_weight_pct = max(el_conc, na.rm = T),
      elraw_adm = sum(el_adm, na.rm = T),
      elconc_adm = sum(el_conc_adm, na.rm = T),
      elraw_weight_total = sum(el_weight_base, na.rm = T),
      elconc_weight_total = sum(el_weight_conc, na.rm = T),
      eltotal_weight_total = sum(el_weight_total, na.rm = T),

      charter_base = mean(weight_base, na.rm = T),
      charter_weight_pct = mean(charter_wt, na.rm = T),
      charter_adm = sum(charter_adm, na.rm = T),
      charter_weight_total = sum(charter_weight_total, na.rm = T),
      
      total_current = sum(current_total, na.rm = T)
                
      ) |>  
      mutate(total_weight_total = base_weight_total + poverty_weight_total + sped_weight_total +
               eltotal_weight_total + charter_weight_total,
            
             total_diff = total_weight_total - total_current
      ) |> 
      pivot_longer(everything()) |> 
      
      separate(name, sep = "_", 
               into = c("name", "cat"),
               extra = "merge") |> 
      pivot_wider(names_from = "cat", values_from = "value") |> 
      mutate(name = str_to_title(name),
             name = str_replace_all(name, "Sped1", "SPED, Tier I"),
             name = str_replace_all(name, "Sped2", "SPED, Tier II"),
             name = str_replace_all(name, "Sped3", "SPED, Tier III"),
             name = str_replace_all(name, "Sped", "SPED, Total"),
             
             name = str_replace_all(name, "Elraw", "EL, Standard"),
             name = str_replace_all(name, "Elconc", "EL, Concentrated"),
             name = str_replace_all(name, "Eltotal", "EL, Total")) |> 
      rename(`Funding Category` = name,
             `Base Per-Pupil` = base,
             `Weight %` = weight_pct,
             ADM = adm,
             `Model Funding` = weight_total,
             `Current Funding` = current,
             `Difference, Model vs. Current` = diff)
    
  })
  
  output$tbl_state_toplines <- renderDataTable({
    
    state_summary() |> 
      datatable(rownames = FALSE,
                options = list(paging = FALSE, 
                               scrollY = "700px", 
                               scrollX = TRUE,
                               scrollCollapse = TRUE,
                               dom = "t")) |> 
      formatRound("ADM", digits = 0) |> 
      formatCurrency(c("Model Funding", "Current Funding",
                       "Difference, Model vs. Current"),
                     digits = 0) |> 
      formatPercentage("Weight %", digits = 2) |> 
      formatCurrency("Base Per-Pupil",
                     digits = 2)
    
    
    
  })
  
  # dist comp table ------
  
  dist_comp <- reactive({
    demo_model() |> 
      select(
        district, adm, formula_total, current_total, total_diff,
        formula_pp, current_pp, pp_diff
      ) |> 
      rename(
        District = district,
        ADM = adm,
        `Model Total` = formula_total,
        `Current Total` = current_total,
        `Total Difference, Model vs. Current` = total_diff,
        `Model Per-Pupil` = formula_pp,
        `Current Per-Pupil` = current_pp,
        `Per-Pupil Difference, Model vs. Current` = pp_diff
      )
    
    
  })
  
  output$tbl_dist_comp <- renderDataTable({
    
    dist_comp() |> 
      datatable(rownames = FALSE,
                options = list(paging = FALSE, 
                               scrollY = "700px", 
                               scrollX = TRUE,
                               scrollCollapse = TRUE,
                               dom = "ft")) |> 
      formatRound("ADM", digits = 0) |> 
      formatCurrency(c("Model Total", "Current Total",
                       "Total Difference, Model vs. Current",
                       "Model Per-Pupil", "Current Per-Pupil",
                       "Per-Pupil Difference, Model vs. Current"),
                     digits = 0)
      
    
  })
  
  # dist detail tbl -------

  # Update district selection dropdown whenever demo_model() changes
  observe({
    # Get all unique district names from the current data
    district_choices <- sort(unique(demo_model()$district))
    
    # Store the current selection (if any)
    current_selection <- input$dist_detail_tbl
    
    # If the current selection isn't in the new choices, select the first district
    selected <- ifelse(current_selection %in% district_choices, 
                       current_selection, 
                       district_choices[1])
    
    # Update the dropdown with new choices
    updateSelectInput(session, 
                      "dist_detail_tbl", 
                      choices = district_choices,
                      selected = selected)
  }) 
  
  
    
  dist_detail <- reactive({
    
    demo_model() |> 
      filter(district == input$dist_detail_tbl) |> 
      summarise(
      base_base = mean(weight_base, na.rm = T),
      base_weight_pct = NA,
      base_adm = sum(adm, na.rm = T),
      base_weight_total = sum(base_total, na.rm = T),
      
      poverty_base = mean(weight_base, na.rm = T),
      poverty_weight_pct = mean(ed_wt, na.rm = T),
      poverty_adm = sum(ed_adm, na.rm = T),
      poverty_weight_total = sum(poverty_weight_total, na.rm = T),

      sped1_base = mean(weight_base, na.rm = T),
      sped2_base = mean(weight_base, na.rm = T),
      sped3_base = mean(weight_base, na.rm = T),
      sped1_weight_pct = mean(sped_tier_i_wt, na.rm = T),
      sped2_weight_pct = mean(sped_tier_ii_wt, na.rm = T),
      sped3_weight_pct = mean(sped_tier_iii_wt, na.rm = T),
      sped1_adm = sum(sped_tier_i_adm, na.rm = T),
      sped2_adm = sum(sped_tier_ii_adm, na.rm = T),
      sped3_adm = sum(sped_tier_iii_adm, na.rm = T),
      sped1_weight_total = sum(sped_weight_tier_i, na.rm = T),
      sped2_weight_total = sum(sped_weight_tier_ii, na.rm = T),
      sped3_weight_total = sum(sped_weight_tier_iii, na.rm = T),
      sped_weight_total = sum(sped_weight_total, na.rm = T),

      
      elraw_base = mean(weight_base, na.rm = T),
      elconc_base = mean(weight_base, na.rm = T),
      elraw_weight_pct = mean(el_wt_raw, na.rm = T),
      elconc_weight_pct = max(el_conc, na.rm = T),
      elraw_adm = sum(el_adm, na.rm = T),
      elconc_adm = sum(el_conc_adm, na.rm = T),
      elraw_weight_total = sum(el_weight_base, na.rm = T),
      elconc_weight_total = sum(el_weight_conc, na.rm = T),
      eltotal_weight_total = sum(el_weight_total, na.rm = T),


      
      charter_base = mean(weight_base, na.rm = T),
      charter_weight_pct = mean(charter_wt, na.rm = T),
      charter_adm = sum(charter_adm, na.rm = T),
      charter_weight_total = sum(charter_weight_total, na.rm = T),

      
      total_current = sum(current_total, na.rm = T)
      
    ) |>  
      mutate(total_weight_total = base_weight_total + poverty_weight_total + sped_weight_total +
               eltotal_weight_total + charter_weight_total,
             
             total_diff = total_weight_total - total_current
      ) |> 
      pivot_longer(everything()) |> 
      
      separate(name, sep = "_", 
               into = c("name", "cat"),
               extra = "merge") |> 
      pivot_wider(names_from = "cat", values_from = "value") |> 
      mutate(name = str_to_title(name),
             name = str_replace_all(name, "Sped1", "SPED, Tier I"),
             name = str_replace_all(name, "Sped2", "SPED, Tier II"),
             name = str_replace_all(name, "Sped3", "SPED, Tier III"),
             name = str_replace_all(name, "Sped", "SPED, Total"),
             
             name = str_replace_all(name, "Elraw", "EL, Standard"),
             name = str_replace_all(name, "Elconc", "EL, Concentrated"),
             name = str_replace_all(name, "Eltotal", "EL, Total")) |> 
      rename(`Funding Category` = name,
             `Base Per-Pupil` = base,
             `Weight %` = weight_pct,
             ADM = adm,
             `Model Funding` = weight_total,
             `Current Funding` = current,
             `Difference, Model vs. Current` = diff)
    
  })
  
  output$tbl_dist_detail <- renderDataTable({
    
    dist_detail() |> 
      datatable(rownames = FALSE,
                options = list(paging = FALSE, 
                               scrollY = "700px", 
                               scrollX = TRUE,
                               scrollCollapse = TRUE,
                               dom = "t")) |> 
      formatRound("ADM", digits = 0) |> 
      formatCurrency(c("Model Funding", "Current Funding",
                       "Difference, Model vs. Current"),
                     digits = 0) |> 
      formatPercentage("Weight %", digits = 2) |> 
      formatCurrency("Base Per-Pupil",
                     digits = 2)
    
    
  })
  
  # before after plot --------
  
  output$plt_before_comp <- renderPlotly({
    
    ggplotly(
      ggplot() +
        geom_abline(intercept = 0,
                    slope = 1,
                    color = "black") +
        geom_point(data = demo_model(),
                   aes(x = current_pp,
                       y = formula_pp,
                       size = adm, 
                       color = ed_pct,
                       text = paste0("District: ",
                                     district, "<br>",
                                     "ADM: ", 
                                     comma(adm, accuracy =1), "<br>",
                                     "ED %: ", 
                                     percent(ed_pct, accuracy = .1), "<br>",
                                     "Current PP: ",
                                     dollar(current_pp, accuracy = 1), "<br>",
                                     "Model PP: ", 
                                     dollar(formula_pp, accuracy = 1), "<br>",
                                     "Diff PP: " ,
                                     dollar(pp_diff, accuracy = 1))
                       ),
                 
                   alpha = .9) +
        scale_size_area(max_size = 10) +
        scale_color_viridis(label = label_percent()) +
        scale_x_continuous(labels = label_dollar()) +
        scale_y_continuous(labels = label_dollar()) +
        labs(x = "Current Per-Pupil Funding",
             y = "Modeled Per-Pupil Funding",
             color = "% Econ.\nDisadv.",
             title = "Current vs. Model Per-Pupil Funding") +
        theme_bw() +
        theme(text = element_text(family = "Avenir", size = 11),
              plot.caption = element_text(hjust = 0),
              plot.background = element_rect(fill = 'transparent', color = NA),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank())
      
      
      ,
      tooltip = "text"
    )
    
    
  })
  
  # ed comp plot --------
  
  output$plt_ed_comp <- renderPlotly({
    
    ggplotly(
      ggplot() +
        geom_point(data = demo_model(),
                   aes(x = ed_pct,
                       y = current_pp,
                       size = adm,
                       text = paste0("Current Funding", "<br>",
                                     "District: ",
                                     district, "<br>",
                                     "ADM: ", 
                                     comma(adm, accuracy =1), "<br>",
                                     "ED %: ", 
                                     percent(ed_pct, accuracy = .1), "<br>",
                                     "Current PP: ",
                                     dollar(current_pp, accuracy = 1), "<br>",
                                     "Model PP: ", 
                                     dollar(formula_pp, accuracy = 1), "<br>",
                                     "Diff PP: " ,
                                     dollar(pp_diff, accuracy = 1))
                       ),
                   color = "grey67",
                   alpha = .5) +
        geom_point(data = demo_model(),
                   aes(x = ed_pct,
                       y = formula_pp,
                       size = adm, 
                       text = paste0("Modeled Funding", "<br>",
                                     "District: ",
                                     district, "<br>",
                                     "ADM: ", 
                                     comma(adm, accuracy =1), "<br>",
                                     "ED %: ", 
                                     percent(ed_pct, accuracy = .1), "<br>",
                                     "Current PP: ",
                                     dollar(current_pp, accuracy = 1), "<br>",
                                     "Model PP: ", 
                                     dollar(formula_pp, accuracy = 1), "<br>",
                                     "Diff PP: " ,
                                     dollar(pp_diff, accuracy = 1))
                   ),
                   color = bw_primary[3],
                   alpha = .9) +
        scale_x_continuous(labels = label_percent()) +
        scale_y_continuous(labels = label_dollar()) +
        labs(x = "% Economic Disadvantage",
             y = "Model Per-Pupil Funding",
             title = "Economic Disadvantage vs. Model Per-Pupil Funding") +
        theme_bw() +
        theme(text = element_text(family = "Avenir", size = 11),
              plot.caption = element_text(hjust = 0),
              plot.background = element_rect(fill = 'transparent', color = NA),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank())
      
      
      ,
      tooltip = "text"
    )
    
    
  })
  
  # ed weight plot ----------
  
  pov_wt_df <- reactive({
    
    tibble(ed_pct = (0:100)/100) |> 
      
      mutate(ed_weight = input$ed_weight / 100,
             conc_pov_min = input$conc_pov_eligible[1] / 100,
             conc_pov_max = input$conc_pov_eligible[2] / 100,
             conc_pov_weight = input$conc_pov_weight / 100,
             conc_pov_adj = (ed_pct - conc_pov_min) / (conc_pov_max - conc_pov_min),
             ed_wt = case_when(ed_pct < conc_pov_min ~ ed_weight,
                               ed_pct >= conc_pov_max ~ (ed_weight + conc_pov_weight),
                               TRUE ~ ed_weight + (conc_pov_weight * conc_pov_adj)
             ))
    
  }) 
  
  output$plt_dir_cert <- renderPlotly({
    
    ggplotly(
      ggplot(pov_wt_df()) + 
        geom_line(aes(x = ed_pct, y = ed_wt, group = 1,
                      text = paste0("ED %: ", percent(ed_pct, accuracy = 1), "<br>",
                                    "Combined Weight: ", percent(ed_wt, accuracy = .1)))) + 
        scale_x_continuous(limits = c(0,1), labels = label_percent()) +
        scale_y_continuous(limits = c(0,(input$conc_pov_weight + input$ed_weight + 5) / 100 ), 
                           labels = label_percent()) +
        
        labs(x = "LEA Economic Disadvantage %",
             y = "Poverty Weight + Concentrated Poverty Weight",
             title = "Total Poverty Weight by LEA Poverty %") +
        
        theme_bw() +
        theme(text = element_text(family = "Avenir"))
      ,
      tooltip = "text")
    
  })
  
  output$plt_dir_cert_hist <- renderPlotly({
    
    ggplotly(
      ggplot(demo_model()) +
        geom_histogram(aes(x = ed_pct,
                           text = paste0(district,
                                         "<br>",
                                         "Econ. Disadv. %: ",
                                         percent(ed_pct, accuracy = .1)),
        ),
        binwidth = .1,
        center = .05) +
        scale_x_continuous(limits = c(0,1), labels = label_percent()) +
        theme_bw() +
        labs(x = "LEA Economic Disadvantange %",
             y = "Number of LEAs", 
             title = "LEA Economic Disadvantage Rates") +
        theme(text = element_text(family = "Avenir")),
      tooltip = "text")
    
  })
  
  # el weight plot -----------
  
  el_wt_df <- reactive({
    
    if(input$conc_el_type == "Tiered") {
      
      tibble(el_pct = (0:100)/100) |> 
        
        mutate(el_weight = input$el_weight / 100,
               conc_el_tiered_wt = input$conc_el_weight_tiered / 100,
               conc_el_tiered_elig = input$conc_el_eligible_tier / 100,
               el_wt = case_when(el_pct < conc_el_tiered_elig ~ el_weight,
                                 el_pct >= conc_el_tiered_elig ~ (el_weight + conc_el_tiered_wt))
               )
      
    } else {
      
      tibble(el_pct = (0:100)/100) |> 
        
        mutate(el_weight = input$el_weight / 100,
               conc_el_max = input$conc_el_weight / 100,
               conc_el_elig_min = input$conc_el_eligible[1] / 100,
               conc_el_elig_max = input$conc_el_eligible[2] / 100,
               conc_el_adj = case_when(el_pct <= conc_el_elig_min ~ 0,
                                       el_pct >= conc_el_elig_max ~ 1,
                                       TRUE ~ (el_pct - conc_el_elig_min) / 
                                         (conc_el_elig_max - conc_el_elig_min)
                                       
               ),
               el_wt = (conc_el_max * conc_el_adj) + el_weight
               
        )
      
    }
    
  })
  
  output$plt_el <- renderPlotly({
    
    if(input$conc_el_type == "Tiered") {
      ggplotly(
        ggplot(el_wt_df()) + 
          geom_line(aes(x = el_pct, y = el_wt, group = 1,
                        text = paste0("EL %: ", percent(el_pct, accuracy = 1), "<br>",
                                      "Combined Weight: ", percent(el_wt, accuracy = .1)))) + 
          scale_x_continuous(limits = c(0,1), labels = label_percent()) +
          scale_y_continuous(limits = c(0,(input$el_weight + input$conc_el_weight_tiered + 5) / 100), 
                             labels = label_percent()) +
          
          labs(x = "LEA English Learner %",
               y = "EL Weight + Tiered EL Weight",
               title = "Total EL Weight by LEA EL %") +
          
          theme_bw() +
          theme(text = element_text(family = "Avenir"))
        ,
        tooltip = "text")
      
    } else {
      
      ggplotly(
        ggplot(el_wt_df()) + 
          geom_line(aes(x = el_pct, y = el_wt, group = 1,
                        text = paste0("EL %: ", percent(el_pct, accuracy = 1), "<br>",
                                      "Combined Weight: ", percent(el_wt, accuracy = .1)))) + 
          scale_x_continuous(limits = c(0, .5), labels = label_percent()) +
          scale_y_continuous(limits = c(0,(input$el_weight + input$conc_el_weight + 5) / 100), 
                             labels = label_percent()) +
          
          labs(x = "LEA English Learner %",
               y = "EL Weight + Concentrated EL Weight",
               title = "Total EL Weight by LEA EL %") +
          
          theme_bw() +
          theme(text = element_text(family = "Avenir"))
        ,
        tooltip = "text")
    }
    
  })
  
  output$plt_el_hist <- renderPlotly({
    
    ggplotly(
      ggplot(demo_model()) +
        geom_histogram(aes(x = el_pct,
                           text = paste0(district,
                                         "<br>",
                                         "English Learner %: ",
                                         percent(el_pct, accuracy = .1)),
        ),
        binwidth = .05,
        center = .025) +
        scale_x_continuous(limits = c(0, .5), labels = label_percent()) +
        theme_bw() +
        labs(x = "LEA English Learner %",
             y = "Number of LEAs", 
             title = "LEA English Learner Rates") +
        theme(text = element_text(family = "Avenir")),
      tooltip = "text")
    
  })
  
  # rural weight plot ---------
  
  sparse_wt_df <- reactive({
    
    tibble(s_per_sq_mi = 0:40) |> 
      mutate(sparse_wt = input$rural_weight / 100,
             sparse_min = input$rural_elig[1],
             sparse_max = input$rural_elig[2],
             sparse_adj = (sparse_max - s_per_sq_mi) / (sparse_max - sparse_min)) |> 
      mutate(sparse_wt_calc = case_when(s_per_sq_mi <= sparse_min ~ sparse_wt,
                                        s_per_sq_mi > sparse_max ~ 0,
                                        TRUE ~ sparse_wt * sparse_adj))
    
    
  })
  
  output$plt_rural <- renderPlotly({
    ggplotly(
      ggplot(sparse_wt_df()) + 
        geom_line(aes(x = s_per_sq_mi, y = sparse_wt_calc, group = 1,
                      text = paste0("S Per Sq. Mi.: ", comma(s_per_sq_mi, accuracy = .1), "<br>",
                                    "Rural Weight: ", percent(sparse_wt_calc, accuracy = .1)))) + 
        # geom_segment(aes(x = 0, 
        #                  xend = sim_df()$conc_pov_min_pct, 
        #                  y = sim_df()$ed_weight, 
        #                  yend = sim_df()$ed_weight) ,
        #              color = "red") +
        # 
        # geom_segment(aes(x = .5, xend = .75, y = .15, yend = .5) ,
        #              color = "blue") +
        # 
        # geom_segment(aes(x = .75, xend = 1, y = .5, yend = .5) ,
        #              color = "grey45") +
        scale_x_continuous(limits = c(0,40)) +
        scale_y_continuous(limits = c(0,(input$capacity_weight + 5) / 100 ), 
                           labels = label_percent()) +
        
        labs(x = "LEA Students Per Square Mile",
             y = "Local Capacity Weight",
             title = "Local Capacity Weight, LEAs w/ <40 S Per. Sq. Mi.)") +
        
        theme_bw() +
        theme(text = element_text(family = "Avenir"))
      ,
      tooltip = "text")
    
  })
  
  output$plt_rural_hist <- renderPlotly({
    
    ggplotly(
      ggplot(demo_model()) +
        geom_histogram(aes(x = s_per_sq_mi,
                           text = paste0(district,
                                         "<br>",
                                         "Students per Sq. Mi: ",
                                         comma(s_per_sq_mi, accuracy = .1)),
        ),
        binwidth = 2,
        center = 1) +
        scale_x_continuous(limits = c(0,40)) +
        theme_bw() +
        labs(x = "LEA Students Per Square Mile",
             y = "Number of LEAs", 
             title = "District Sparsity, LEAs w/ <40 S Per. Sq. Mi.)") +
        theme(text = element_text(family = "Avenir")),
      tooltip = "text")
  })
  
  
  # right sidebar box data ----
  
  output$diff_total <- reactive({
    dollar(demo_model() |> 
             summarise(total_diff = sum(weight_total - current_total, na.rm = TRUE)) |> 
             pull(total_diff))
  })
  
  
  output$diff_pp <- reactive({
    dollar(demo_model() |> 
             summarise(total_diff = sum(weight_total - current_total, na.rm = TRUE),
                       adm = sum(adm)) |> 
             mutate(pp_diff = total_diff / adm) |> 
             pull(pp_diff))
  })
  
  # download full table -----
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste('demo-simulator-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      write_csv(demo_model(), file)
    }
    
  )
  
  output$dl_state_toplines <- downloadHandler(
    
    filename = function() {
      paste('demo-toplines-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      write_csv(state_summary(), file)
    }
    
  )

  
} # close server

