clean_rais_2013 <- function(rais_data) {
  rais_data %>% 
    as.data.table() %>% 
    rename_rais_2012() %>% 
    make_rais_lazy() %>% 
    destring_rais_2012() %>% 
    trim_rais() %>% 
    CPF_rais() %>% 
    identificad_rais() %>% 
    radiccnpj_rais() %>% 
    empem3112_rais %>% 
    tpvinculo_rais() %>% 
    causadesli_rais() %>% 
    ocupacao94_rais_2012 %>%
    ocup2002_rais() %>% 
    tipoadm_rais() %>% 
    tiposal_rais() %>% 
    indalvara_rais %>% 
    indpat_rais() %>% 
    indsimples_rais() %>% 
    grinstrucao_rais() %>% 
    genero_rais_2012 %>% 
    nacionalidad_rais() %>% 
    tpdefic_rais() %>% 
    raca_cor_rais() %>% 
    tamestab_rais() %>% 
    tipoestbl_rais() %>% 
    commareplace_rais() %>% 
    natjuridica_rais() %>% 
    causafast_rais() %>% 
    diainiafmesiniaf_rais() %>%
    dtnascimento_rais() %>% 
    dtadmissao_rais() %>% 
    as_tibble() %>% 
    rais_assertclass() %>%
    reorder_rais_2013() 
}
