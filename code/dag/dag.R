# Required packages
pacman::p_load(DiagrammeR, 
               tidyverse, 
               DiagrammeRsvg, 
               rsvg, 
               xml2)

# Mental health outcomes

DAGMH <-DiagrammeR::grViz("
                  digraph{
                  graph[ranksep=0.2]
                  
                  node[shape=plaintext,fontname=Arial]
                  
                    Inv[label ='Time invariant confounders',shape=oval]
                    Var1[label='Intermediate \nconfounders T0',shape=box]
                    Var2[label='Intermediate \nconfounders T1',shape=box]
                    Age1[label='Age T0', shape=oval]
                    Age2[label='Age T1', shape=oval]
                    Emp1[label=<<B>Employment T1</B>>]
                    Emp2[label=<<B>Employment T2</B>>]
                    Inc1[label=<<B>Income T1</B>>]
                    Inc2[label=<<B>Income T2</B>>]
                    Dis1[label=<<B>Financial distress T1</B>>,fontcolor=deepskyblue4]
                    Dis2[label=<<B>Financial distress T2</B>>,fontcolor=deepskyblue4]
                    MH0[label='Mental Health T0']
                    MH1[label=<<B>Mental Health T1</B>>,fontcolor=darkorange2]
                    MH2[label=<<B>Mental Health T2</B>>,fontcolor=darkorange2]
                    
                  edge[minlen=2]
                    Inv->Var1->Inc1->MH1->Inc2
                    Inv->Var2->Inc2->MH2
                    Inv->MH0->MH1->MH2
                    Inv->Inc1->Var2
                    Inv->MH1
                    Inv->Inc2
                    Inv->Emp1
                    Inv->Emp2
                    Inv->MH2
                    Var1->Var2
                    Var1->MH1
                    Var1->Emp1
                    Var2->Emp2
                    MH0->Var1
                    MH0->Inc1
                    Inc1->Inc2
                    Inc1->Emp2
                    MH1->Var2
                    Var2->MH2
                    Emp1->Inc1
                    Emp2->Inc2
                    Emp1->Emp2
                    Emp1->MH1
                    Emp2->MH2
                    MH0->Emp1
                    MH1->Emp2
                    Age1->Emp1
                    Age1->Inc1
                    Age1->MH1
                    Age1->Var1
                    Age2->Emp2
                    Age2->Inc2
                    Age2->MH2
                    Age2->Var2
                    Emp1->Var2
                    Age1->Age2
                    Dis1->Dis2
                    Dis1->MH1
                    Dis1->Emp2
                    Emp1->Dis1
                    Inc1->Dis1
                    Dis2->MH2
                    Emp2->Dis2
                    Inc2->Dis2
                    Var1->Dis1
                    Dis1->Var2
                    Var2->Dis2
                    Inv->Dis1
                    Inv->Dis2
                    Age1->Dis1
                    Age2->Dis2
                    
                    
                    
                  {rank=min; Inv}
                  {rank=same; Age1; Age2}
                  {rank=same; Var1; Var2}
                  {rank=same; Emp1; Emp2}
                  {rank=same; Inc1; Inc2}
                  {rank=same; Dis1; Dis2}
                  {rank=max; MH0; MH1; MH2}
                  }
                  ")

# Export a DAG to PNG at journal-ready resolution.
# width_in: target figure width in inches; dpi: target DPI (default 300).
export_dag_png <- function(dag, filename, width_in = 7, dpi = 300) {
  DiagrammeRsvg::export_svg(dag) |>
    charToRaw() |>
    rsvg::rsvg_png(
      here::here("figs", "dag", filename),
      width = round(width_in * dpi)
    )
}

DAGMH

DAGMH_key <- DiagrammeR::grViz("
                  digraph{
                  graph[ranksep=0]
                  
                  node[shape=plaintext,fontname=Arial]
                  
                  Key1[label='Time invariant confounders: Gender, ethnicity, education',shape=box]
                  Key2[label='Intermediate confounders: Other measures of SEP, marital status, # dependents, physical health, location',shape=box]
                  Key3[label='Time-varying covariates: Age',shape=box]
                  
                  edge[minlen=2]
                  Key1->Key2->Key3 [style=invis]
                  
                  {rank=max; Key3}
                  }
                  ")

export_dag_png(DAGMH,     "HEED_FD_DAG_MH.png")
export_dag_png(DAGMH_key, "HEED_FD_DAG_MH_key.png", width_in = 3.5)

# Physical health outcomes

DAGPH <-DiagrammeR::grViz("
                digraph{
                  graph[ranksep=0.2]
                  
                  node[shape=plaintext,fontname=Arial]
                  
                    Inv[label ='Time invariant confounders',shape=oval]
                    Var1[label='Intermediate \nconfounders T0',shape=box]
                    Var2[label='Intermediate \nconfounders T1',shape=box]
                    Age1[label='Age T0', shape=oval]
                    Age2[label='Age T1', shape=oval]
                    Emp1[label=<<B>Employment T1</B>>]
                    Emp2[label=<<B>Employment T2</B>>]
                    Inc1[label=<<B>Income T1</B>>]
                    Inc2[label=<<B>Income T2</B>>]
                    Dis1[label=<<B>Financial distress T1</B>>,fontcolor=deepskyblue4]
                    Dis2[label=<<B>Financial distress T2</B>>,fontcolor=deepskyblue4]
                    PH0[label='Physical Health T0']
                    PH1[label=<<B>Physical Health T1</B>>,fontcolor=darkorange2]
                    PH2[label=<<B>Physical Health T2</B>>,fontcolor=darkorange2]
                    
                  edge[minlen=2]
                    Inv->Var1->Inc1->PH1->Inc2
                    Inv->Var2->Inc2->PH2
                    Inv->PH0->PH1->PH2
                    Inv->Inc1->Var2
                    Inv->PH1
                    Inv->Inc2
                    Inv->Emp1
                    Inv->Emp2
                    Inv->PH2
                    Var1->Var2
                    Var1->PH1
                    Var1->Emp1
                    Var2->Emp2
                    PH0->Var1
                    PH0->Inc1
                    Inc1->Inc2
                    Inc1->Emp2
                    PH1->Var2
                    Var2->PH2
                    Emp1->Inc1
                    Emp2->Inc2
                    Emp1->Emp2
                    Emp1->PH1
                    Emp2->PH2
                    PH0->Emp1
                    PH1->Emp2
                    Age1->Emp1
                    Age1->Inc1
                    Age1->PH1
                    Age1->Var1
                    Age2->Emp2
                    Age2->Inc2
                    Age2->PH2
                    Age2->Var2
                    Emp1->Var2
                    Age1->Age2
                    Dis1->Dis2
                    Dis1->PH1
                    Dis1->Emp2
                    Emp1->Dis1
                    Inc1->Dis1
                    Dis2->PH2
                    Emp2->Dis2
                    Inc2->Dis2
                    Var1->Dis1
                    Dis1->Var2
                    Var2->Dis2
                    Inv->Dis1
                    Inv->Dis2
                    Age1->Dis1
                    Age2->Dis2
                    
                    
                    
                  {rank=min; Inv}
                  {rank=same; Age1; Age2}
                  {rank=same; Var1; Var2}
                  {rank=same; Emp1; Emp2}
                  {rank=same; Inc1; Inc2}
                  {rank=same; Dis1; Dis2}
                  {rank=max; PH0; PH1; PH2}
                  }
                  ")

export_dag_png(DAGPH, "HEED_FD_DAG_PH.png")

DAGPH_key <- DiagrammeR::grViz("
                  digraph{
                  graph[ranksep=0]
                  
                  node[shape=plaintext,fontname=Arial]
                  
                  Key1[label='Time invariant confounders: Gender, ethnicity, education',shape=box]
                  Key2[label='Intermediate confounders: Other measures of SEP, marital status, # dependents, mental health, location',shape=box]
                  Key3[label='Time-varying covariates: Age',shape=box]
                  
                  edge[minlen=2]
                  Key1->Key2->Key3 [style=invis]
                  
                  {rank=max; Key3}
                  }
                  ")

export_dag_png(DAGPH_key, "HEED_FD_DAG_PH_key.png", width_in = 3.5)