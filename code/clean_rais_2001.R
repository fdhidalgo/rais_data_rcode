clean_rais_2001 <- function(rais_data) {
  rais_data %>% 
    as.data.table() %>% 
    rename_rais_2001() %>% 
    make_rais_lazy() %>%
    destring_rais_2001() %>%
    trim_rais_2001() %>% ###
    identificad_rais() %>%
    radiccnpj_rais() %>%
    empem3112_rais %>%
    tpvinculo_rais() %>% 
    causadesli_rais() %>%
    tipoadm_rais() %>%
    ocupacao94_rais_2005() %>%
    indalvara_rais %>%
    indpat_rais() %>%
    indsimples_rais() %>%
    grinstrucao_rais() %>%
    genero_rais_2004() %>%
    nacionalidad_rais() %>%
    tamestab_rais() %>%
    tipoestbl_rais() %>%
    commareplace_rais_2001() %>%
    natjuridica_rais() %>%
    ibgesubsetor_rais() %>% 
    as_tibble()  %>% 
    assertclass_rais()
}
