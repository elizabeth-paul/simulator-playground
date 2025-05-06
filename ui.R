library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(plotly)
library(DT)
library(markdown)
# library(sf)
# library(leaflet)


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


dist_options <- read_rds("data/demo_dist.rds") |> pull(district)

# input sidebar -------------
input_sidebar <- sidebar(
  width = 350,

  h4("Formula Inputs"),
  
  accordion(
    # open = FALSE,
    # multiple = FALSE,
    id = "sidebarAccordion",
    
    accordion_panel(
      
      "Scenario Options",
      selectInput("scenario",
                  label = "Update inputs to match following scenarios:",
                  choices = c("Default",
                              "Scenario 1: Higher Poverty Weights",
                              "Scenario 2: Higher Base Amount",
                              "Scenario 3: Higher SPED and EL Weights"),
                  selected = "Default") |> 
        tooltip("These scenarios update values for each input to project a particular scenario."),
      actionButton("apply_scenario", "Apply Scenario"),
      
    ),
    
    
    
    accordion_panel(
      "Base Amount",
      sliderInput("base_amt",
                  "Base Funding Per-Pupil:",
                  min =  7000, max = 8500,
                  value = 7100, 
                  step = 50,
                  pre = "$")
      
                       
      
    ),
    
    accordion_panel(
      "Poverty Weight",
      sliderInput("ed_weight",
                  label = "Poverty Weight",
                  min = 0, max = 20, 
                  step = 0.25, value = 5,
                  post = "%") |> 
        tooltip("Select weight for poverty"),
      
      sliderInput("conc_pov_weight", 
                  label = "Max. Concentrated Poverty Weight", 
                  min = 0, max = 10,
                  step = .25, value = 2,
                  post = "%") |> 
        tooltip("Select concentrated poverty weight"),
      
      sliderInput("conc_pov_eligible", 
                  label = "Concentrated Poverty Min/Max Weight Dir. Cert. % Reqirements", 
                  min = 0, max = 100,
                  step = 5, value = c(20, 60),
                  post = "%") |> 
        tooltip("Select range of concentrated poverty weight eligibility for districts, from % direct cert. when districts start being eligible for the weight (lower value) to the % direct cert. when districts recieve the maximum concentrated poverty weight (higher value)")
    ),
    
    accordion_panel(
      "Special Ed. Weights",
      sliderInput("sped_weight_tier_i", 
                  label = "Tier 1", 
                  min = 0, max = 25, 
                  step = 0.5, 
                  value = 2,
                  post = "%") |> 
        tooltip("Select Tier I special education weight"),
      
      sliderInput("sped_weight_tier_ii", 
                  label = "Tier 2", 
                  min = 0, max = 50, 
                  step = 1, 
                  value = 10,
                  post = "%") |> 
        tooltip("Select Tier II special education weight"),
      
      sliderInput("sped_weight_tier_iii", 
                  label = "Tier 3", 
                  min = 0, max = 150, 
                  step = 5, 
                  value = 75,
                  post = "%") |> 
        tooltip("Select Tier III special education weight"),
      accordion_panel(
        "Special Ed Tiers",
        open = FALSE,
        sliderInput("sld_tier",
                    label = "Specific Learning Disability",
                    min = 1, max = 3, 
                    value = 1),
        sliderInput("sli_tier",
                    label = "Speech/Language Impairment",
                    min = 1, max = 3, 
                    value = 1),
        sliderInput("ohi_tier",
                    label = "Other Health Impairment",
                    min = 1, max = 3, 
                    value = 1),
        
        sliderInput("aut_tier",
                    label = "Autism",
                    min = 1, max = 3, 
                    value = 2),
        sliderInput("id_tier",
                    label = "Intellectual Disability",
                    min = 1, max = 3, 
                    value = 2),
        
        sliderInput("dd_tier",
                    label = "Developmental Disability",
                    min = 1, max = 3, 
                    value = 2),
        sliderInput("md_tier",
                    label = "Multiple Disabilities",
                    min = 1, max = 3, 
                    value = 2),
        sliderInput("ed_tier",
                    label = "Emotional Disability",
                    min = 1, max = 3, 
                    value = 3),
        sliderInput("hi_tier",
                    label = "Hearing Impairment",
                    min = 1, max = 3, 
                    value = 3),
        sliderInput("oi_tier",
                    label = "Orthopedic Impairment",
                    min = 1, max = 3, 
                    value = 3),
        sliderInput("vi_tier",
                    label = "Visual Impairment",
                    min = 1, max = 3, 
                    value = 3),
        sliderInput("db_tier",
                    label = "Deaf-Blindness",
                    min = 1, max = 3, 
                    value = 3),
        sliderInput("tbi_tier",
                    label = "Traumatic Brain Injury",
                    min = 1, max = 3, 
                    value = 3)
        
      )
      
    ),
    
    accordion_panel(
      "English Learner Weight",

      sliderInput("el_weight", label = "EL weight", 
                  min = 0, max = 16, step = 1, 
                  value = 7, post = "%") |> 
        tooltip("Select EL weight"),
      
      radioButtons("conc_el_type", 
                   label = "Conc. EL Weight Type",
                   choices = c("Tiered", "Escalating"),
                   selected = "Tiered"),
      conditionalPanel(condition = "input.conc_el_type == 'Tiered'",
                       sliderInput("conc_el_weight_tiered", 
                                   label = "Max. Concentrated EL Weight", 
                                   min = 0, max = 10,
                                   step = 1, value = 3,
                                   post = "%") |> 
                         tooltip("Select concentrated EL weight"),
                       
                       sliderInput("conc_el_eligible_tier", 
                                   label = "Concentrated EL Min % Reqirements", 
                                   min = 0, max = 30,
                                   step = 1, value = 9,
                                   post = "%") |> 
                         tooltip("Select concentrated EL weight eligibility for districts to recieved a tiered concentrated EL weight")
                       
                       ),
      conditionalPanel(condition = "input.conc_el_type == 'Escalating'",
                       sliderInput("conc_el_weight", 
                                   label = "Max. Concentrated EL Weight", 
                                   min = 0, max = 10,
                                   step = 1, value = 4,
                                   post = "%") |> 
                         tooltip("Select concentrated EL weight"),
                       
                       sliderInput("conc_el_eligible", 
                                   label = "Concentrated EL Min/Max % Reqirements", 
                                   min = 0, max = 30,
                                   step = 1, value = c(5, 15),
                                   post = "%") |> 
                         tooltip("Select range of concentrated EL weight eligibility for districts, from EL % when districts start being eligible for the weight (lower value) to the EL % when districts recieve the maximum concentrated EL weight (higher value)")
                       
                       
      ),
      

    ),
    
    
    accordion_panel(
      "Rural Weight",
      
      sliderInput("rural_weight",
                  label = "Rural Weight",
                  min = 0, max = 10,
                  step = 1,
                  value = 3,
                  post = "%") |> 
        tooltip("Select range of maximum rural weight"),
      sliderInput("rural_elig",
                  label = "Rural Weight Eligibility, Students per Sq. Mi.",
                  min = 0, max = 20,
                  step = 1,
                  value = c(1, 10)) |> 
        tooltip("Select range of maximum rural weight eligibility. Lower value indicates the students per sq. mi. value below which districts would recieve the maximum rural weight; higher value indicates the students per sq. mi when districts would become eligible for the rural weight."),
      
    ),
    
    accordion_panel(
      "Charter Weight",
      sliderInput("charter_weight",
                  label = "Charter Weight",
                  min = 0, max = 10,
                  step = 1,
                  value = 2,
                  post = "%") |> 
        tooltip("Select range of charter weight")
      
      
    )
    
    
  ),



) # close input sidebar


# right sidebar: formula toplines and csv download -----------
right_sidebar_content <- sidebar(
  position = "right",
  width = 350,
  border_radius = FALSE,
  fillable = TRUE,
  class = "p-0",


  value_box(
    title = "Model Funding, Total Difference",
    value = textOutput("diff_total"),
    theme = value_box_theme(bg = bw_primary[2],
                            fg = "#ffffff"),
    showcase = bsicons::bs_icon("activity")
  ),
  value_box(
    title = "Model Funding, Per-Pupil Difference",
    value = textOutput("diff_pp"),
    theme = value_box_theme(bg = bw_primary[6],
                            fg = "000000"),
    showcase = bsicons::bs_icon("activity")
  ),
  # value_box(
  #   title = "Districts w/ Decreased Funding Per-Pupil",
  #   value = textOutput("dist_loss"),
  #   showcase = bsicons::bs_icon("graph-down-arrow"),
  #   theme = value_box_theme(bg = bw_primary[1],
  #                           fg = "#FFFFFF"),
  # 
  # ),

  downloadButton("download_data", "Download CSV")

)


# full ui ---------

shinyUI({
  
  tags$head(
    tags$style(
      HTML("
      .custom-pills .nav-pills .nav-link {
        /* Unselected pill style */
        color: #212B46;          /* Text color when unselected */
        background-color: #fff;  /* Background when unselected */
      }
      
      .custom-pills .nav-pills .nav-link.active {
        /* Selected pill style */
        color: #fff;             /* Text color when selected */
        background-color: #212B46;  /* Background when selected */
      }
      
      /* Dropdown item when active/selected */
    .dropdown-item.active,
    .dropdown-item:active {
      color: #ffffff;              /* Text color when active */
      background-color: #7EA2D1;   /* Background color when active */
    }

    /* Dropdown item on hover */
    .dropdown-item:hover {
      color: #333333;              /* Text color on hover */
      background-color: #7EA2D1;   /* Background color on hover */
    }
    
   

    ")
    )
  )
  
  page_navbar(
    title = div(
    style = "display: flex; align-items: center; position: relative; top: -3px; margin-right: 30px;", # Adjust top value as needed
    img(src = "primary_full-color.png", height = "30px", style = "margin-right: 10px;"),
    "School Funding Formula Simulator"
  ),
    # title = "School Funding Formula Simulator",
    theme = bs_theme() |> 
      bs_add_rules(
        "
        .nav-pills .nav-link {
          color:#212B46;          /* Text color when unselected */
          background-color: #fff;  /* Background when unselected */
        }
        
        .nav-pills .nav-link.active {
          color: #fff;             /* Text color when selected */
          background-color: #212B46;  /* Background when selected */
        }
        
        .nav-pills .nav-link:hover {
          background-color: #e9ecef;
          color: #212B46;
        }
        
         .nav-pane.active {
        height: 100%;
    }
      
    .navbar-header {
      height: 30px !important;
      min-height: 30px !important;
    }
    
    /* Ensure the logo and text are properly positioned */
    .navbar-brand img {
      display: inline-block;
      vertical-align: middle;
    }
        "
      ),  
    
    fillable = TRUE,
    nav_panel(
      "Model",
      page_fillable(
        layout_sidebar(
          sidebar = input_sidebar,
          layout_sidebar(
            sidebar = right_sidebar_content,
            border_radius = FALSE,
            fillable = TRUE,
            class = "p-0",
            
            
            navset_card_tab(
              
              #  nav_menu for Plots
              nav_menu(
                "Plots",
                # nav_panel(
                #   "Base and Weight Summary",
                #   plotlyOutput("plt_base_weights")
                # ),
                nav_panel(
                  "District Poverty % vs. Model PP",
                  plotlyOutput("plt_ed_comp") |> 
                    tooltip("Green points illustrate represent funding for districts in the modeled scenario; grey points illustrate current district funding.")
                ),
                nav_panel(
                  "Current PP vs. Model PP",
                  plotlyOutput("plt_before_comp") |> 
                    tooltip("Points appearing above the black line represent distircts that would see a funding increase in this model compared to current funding.")
                ),
                
              
                
                
                
              ),
              
              #  nav_menu for Tables
              nav_menu(
                "Tables",
                nav_panel(
                  "State Toplines",
                  fluidRow(
                    column(6, h3("State Toplines")),
                    column(6, downloadButton("dl_state_toplines", "Download State Toplines"))
                  ),
                  DTOutput("tbl_state_toplines")
                          ),
                nav_panel("District Comparison",
                          fluidRow(
                            column(6, h3("District Comparison")),
                            column(6, 
                                   downloadButton("dl_dist_comp", "Download District Comparison")),
                          ),
                          DTOutput("tbl_dist_comp")
                ),
                nav_panel("District Detail",
                          fluidRow(
                            column(6,
                                   h3("District Detail")),
                            column(6,
                                   selectInput("dist_detail_tbl", "Select District:",
                                               choices = dist_options))
                            
                          ),
                          DTOutput("tbl_dist_detail")
                ),
               
              ),
              
              # nav menu for weight detail
              nav_menu(
                "Escalating Weight Detail",
                nav_panel("Poverty Weights",
                          plotlyOutput("plt_dir_cert") |> 
                            tooltip("This plot illustrates the combined poverty weight for a given economic disadvantage rate."),
                          plotlyOutput("plt_dir_cert_hist") |>
                            tooltip("This plot illustrates the distribution of economic disadvantage rates across LEAs.")
                          ),
                nav_panel("English Learner Weights",
                          plotlyOutput("plt_el") |> 
                            tooltip("This plot illustrates the combined English Learner weight for a given English Learner rate."),
                          plotlyOutput("plt_el_hist") |> 
                            tooltip("This plot illustrates the distribution of English Learner rates across LEAs.")
                ),
                nav_panel("Rural Weight",
                          plotlyOutput("plt_rural") |> 
                            tooltip("This plot illustrates the rural weight by students per square mile."),
                          plotlyOutput("plt_rural_hist") |> 
                            tooltip("This plot illustrates the distribution of sparsity across LEAs.")
                )
                
              )
            ),
            
            border = FALSE
          ),
          border_radius = FALSE,
          fillable = TRUE,
          class = "p-0"
        )
      )
    ),
    
    nav_panel(
      "Notes",
      layout_columns(
        col_widths = c(8, 4),  # First column takes 6/12 (half), second takes 6/12
        includeMarkdown("data_notes.md"),
        # The second column can be empty or contain something else
        NULL
      )
      
      
    )
    
  
  )
  
})
