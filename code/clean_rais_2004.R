clean_rais_2004 <- function(rais_data) {
  rais_data %>% 
    as.data.table() %>% 
    rename_rais() %>% 
    make_rais_lazy() %>% 
    dropTIPOESTBID_rais() %>% 
    destring_rais_2004() %>% 
    trim_rais() %>% 
    CPF_rais() %>% 
    identificad_rais() %>% 
    radiccnpj_rais() %>% 
    empem3112_rais %>% 
    tpvinculo_rais() %>% 
    causadesli_rais() %>% 
    ocupacao94_rais_2005() %>% 
    ocup2002_rais() %>%
    tipoadm_rais() %>% 
    tiposal_rais() %>% 
    indalvara_rais %>% 
    indpat_rais() %>% 
    indsimples_rais() %>% 
    grinstrucao_rais() %>% 
    dtadmissao_rais() %>% 
    dtnascimento_rais() %>% 
    genero_rais_2004() %>%
    nacionalidad_rais() %>% 
    raca_cor_rais() %>% 
    tamestab_rais() %>% 
    tipoestbl_rais() %>% 
    commareplace_rais() %>% 
    natjuridica_rais() %>% 
    as_tibble() %>% 
    assertclass_rais() 
}
