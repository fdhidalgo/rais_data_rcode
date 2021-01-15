clean_rais_2002 <- function(rais_data) {
  rais_data %>% 
    as.data.table() %>% 
    rename_rais_2002() %>% 
    make_rais_lazy() %>% 
    dropTIPOESTBID_rais() %>% 
    destring_rais_2002() %>% 
    trim_rais() %>% 
    CPF_rais() %>% 
    identificad_rais() %>%
    radiccnpj_rais() %>%
    empem3112_rais %>%
    tpvinculo_rais() %>% # Stata code changes from 2002 to 2003, but function seems to hold up without changing anything
    causadesli_rais() %>%
    ocupacao94_rais_2005() %>%
    tiposal_rais() %>%
    indalvara_rais %>%
    indpat_rais() %>%
    indsimples_rais() %>%
    grinstrucao_rais() %>%
    dtadmissao_rais() %>%
    dtnascimento_rais() %>%
    genero_rais_2004() %>%
    nacionalidad_rais() %>%
    tamestab_rais() %>%
    tipoestbl_rais() %>%
    commareplace_rais() %>%
    natjuridica_rais() %>%
    as_tibble() %>% 
    assertclass_rais()
}
