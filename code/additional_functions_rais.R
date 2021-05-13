#### Types of establishments 

establishment_rais <- function(rais_data) {
  rais_data %>%
    mutate(legal_nature = natjuridica) %>% 
    mutate(public = if_else(natjuridica %in% c("POD EXEC FED",
                                               "POD EXEC EST",
                                               "POD EXEC MUN",
                                               "POD LEG FED" ,
                                               "POD LEG EST" ,
                                               "POD LEG MUN" ,
                                               "POD JUD FED",
                                               "POD JUD EST",
                                               "AUTARQ FED",
                                               "AUTARQ EST",
                                               "AUTARQ MUN",
                                               "FUNDAC FED",
                                               "FUNDAC EST",
                                               "FUNDAC MUN",
                                               "ORG AUT FED",
                                               "ORG AUT EST",
                                               "ORG AUT MUN",
                                               "COM POLINAC",
                                               "ASSOC PUBLIC", 
                                               "Consórcio Público de Direito Privado",
                                               "Estado ou Distrito Federal",
                                               "Município",
                                               "Fundação Pública de Direito Privado Federal", 
                                               "Fundação Pública de Direito Privado Estadual ou do Distrito Federal",
                                               "Fundação Pública de Direito Privado Estadual ou do Distrito Federal",
                                               "Fundo Público da Administração Indireta Federal", 
                                               "Fundo Público da Administração Indireta Estadual ou do Distrito Federal", 
                                               "Fundo Público da Administração Indireta Municipal", 
                                               "Fundo Público da Administração Direta Federal", 
                                               "Fundo Público da Administração Direta Estadual ou do Distrito Federal", 
                                               "Fundo Público da Administração Direta Municipal", 
                                               "União") , 1, 0 )) %>% 
    mutate(public_federal = if_else(natjuridica %in% c("POD EXEC FED",
                                                       "POD LEG FED" ,
                                                       "POD JUD FED",
                                                       "AUTARQ FED",
                                                       "FUNDAC FED",
                                                       "ORG AUT FED",
                                                       "Fundação Pública de Direito Privado Federal", 
                                                       "Fundo Público da Administração Indireta Federal", 
                                                       "Fundo Público da Administração Direta Federal", 
                                                       "União" 
    ) , 1, 0 )) %>%
    mutate(public_state = if_else(natjuridica %in% c( "POD EXEC EST",
                                                      "POD LEG EST",
                                                      "POD JUD EST",
                                                      "AUTARQ EST",
                                                      "ORG AUT EST",
                                                      "Estado ou Distrito Federal",
                                                      "Fundação Pública de Direito Privado Estadual ou do Distrito Federal",
                                                      "Fundo Público da Administração Indireta Estadual ou do Distrito Federal", 
                                                      "Fundo Público da Administração Direta Estadual ou do Distrito Federal") , 1, 0 )) %>% 
    mutate(public_municipal = if_else(natjuridica %in% c( 
      "POD EXEC MUN",
      "POD LEG MUN",
      "AUTARQ MUN",
      "FUNDAC MUN",
      "ORG AUT MUN",
      "Fundação Pública de Direito Privado Estadual ou do Distrito Federal",
      "Fundo Público da Administração Indireta Municipal", 
      "Fundo Público da Administração Direta Municipal"
      
    ) , 1, 0 )) %>% 
    mutate(public_exec = if_else(natjuridica %in% c( 
      "POD EXEC FED",
      "POD EXEC EST",
      "POD EXEC MUN"
    ) , 1, 0 )) %>% 
    mutate(public_leg = if_else(natjuridica %in% c(
      "POD LEG FED",
      "POD LEG EST",
      "POD LEG MUN"
    ) , 1, 0 )) %>%
    mutate(public_jud = if_else(natjuridica %in% c( 
      "POD JUD FED",
      "POD JUD EST",
      "AUTARQ MUN"
    ) , 1, 0 )) %>% 
    mutate(company = if_else(natjuridica %in% c("EMP PUB",
                                                "SOC MISTA",
                                                "SA ABERTA",
                                                "SA FECH",
                                                "SOC QT LTDA" ,
                                                "SOC COLETV" ,
                                                "SOC COMD SM" ,
                                                "SOC COMD AC" ,
                                                "SOC CTA PAR" ,
                                                "FRM MER IND",
                                                "COOPERATIVA" ,
                                                "CONS EMPRES",
                                                "GRUP SOC",
                                                "FIL EMP EXT" ,
                                                "FIL ARG-BRA" ,
                                                "EMP DOM EXT" ,
                                                "FUN INVEST",
                                                "SOC SIMP PUR",
                                                "SOC SIMP LTD" ,
                                                "SOC SIMP COL",
                                                "SOC SIMP COM" ,
                                                "EMPR BINAC" ,
                                                "CONS EMPREG" ,
                                                "CONS SIMPLES" ,
                                                "Empresa Individual de Responsabilidade Ltda (De Natureza Empresária)" ,
                                                "Empresa Individual de Responsabilidade Ltda (De Natureza Simples)" ,
                                                "Sociedade Unipessoal de Advocacia" ,
                                                "Cooperativas de Consumo" 
    ) , 1, 0 )) %>% 
    mutate(company_public = if_else(natjuridica %in% c("EMP PUB"
    ) , 1, 0 )) %>% 
    mutate(nonprofit = if_else(natjuridica %in% c("CARTORIO",
                                                  "OUT FUND PR",
                                                  "SERV SOC AU",
                                                  "CONDOMIN",
                                                  "COM CONC",
                                                  "ENT MED ARB",
                                                  "ENT SOCIAL07",
                                                  "FIL FUN EXT",
                                                  "FUN DOM EXT",
                                                  "ORG RELIG",
                                                  "COMUN INDIG",
                                                  "FUNDO PRIVAD",
                                                  "Órgão de Direção Nacional de Partido Político", 
                                                  "Órgão de Direção Regional de Partido Político",
                                                  "Órgão de Direção Local de Partido Político",
                                                  "Comitê Financeiro de Partido Político", 
                                                  "Frente Plebiscitária ou Referendária", 
                                                  "Organização Social (Os)", 
                                                  "Demais Condomínios", 
                                                  "OUTR ORG" 
    ) , 1, 0 ))
}

#### inflation --- You will need to download the data and upload it as well as select the year you are doing it for. 
# haven::read_dta("~data_IBGE_IPCA_1994-2018.dta")

inflation_rais<- function(rais_data, inflation_dta, yearval) {
rais_data %>% 
  mutate(year = yearval) %>% 
  left_join(inflation_dta  %>% 
              filter(month == 12) %>% 
              select(year, index) %>% 
              mutate(aux_index_2018 = index[year==2018]) %>% 
              mutate(index_2018 = max(aux_index_2018)) %>% 
              mutate(price_index_b2018 = index /index_2018) %>% 
              select(year, price_index_b2018), by = "year") 

}

#### job type. Only for years greater than 1995

jobtype_rais <- function(rais_data, year=2000){
  if (year >= 1995) {
    rais_data %>% 
      mutate(job_public = if_else(tpvinculo %in% c("ESTATUTARIO", "ESTAT RGPS", "ESTAT N/EFET" ), 1,0)) %>% 
      mutate(job_public_com = if_else(tpvinculo %in% c("ESTAT N/EFET"),1,0 ))
  }
}
