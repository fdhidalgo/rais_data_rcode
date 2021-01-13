### rename rais [dif] ----
rename_rais_2005 <- function(rais_data) {
  newnames <- c("municipio", "clascnae95", "empem3112", "tpvinculo","causadesli", "mesdesli","indalvara","tipoadm", "tiposal","ocupacao94","grinstrucao", "sexotrabalhador","nacionalidad", "raca_cor", "portdefic", "tamestab","natjuridica", "indceivinc", "tipoestbl", "indpat", "indsimples", "dtadmissao","remmedia" ,"remmedr" ,"remdezembro" ,"remdezr" ,"tempempr" ,"horascontr","ultrem" ,"salcontr" ,"PIS" ,"dtnascimento" ,"numectps" ,"CPF" ,"ceivinc" ,"identificad", "radiccnpj", "TIPOESTBID","nome", "diadesli",   "ocup2002" )
  setnames(rais_data, new = newnames ) # replacing old names with new
}
rename_rais_2012 <- function(rais_data) {
  newnames <- c("municipio", "clascnae95", "empem3112", "tpvinculo", "causadesli", "mesdesli", "indalvara", "tipoadm", "tiposal", "ocupacao94", "grinstrucao", "sexotrabalhador", "nacionalidad", "raca_cor", "portdefic", "tamestab", "natjuridica", "indceivinc", "tipoestbl", "indpat", "indsimples", "dtadmissao", "remmedr", "remmedia", "remdezr", "remdezembro", "tempempr", "horascontr", "ultrem", "salcontr", "PIS", "numectps", "CPF", "ceivinc", "identificad", "radiccnpj", "nome", "ocup2002", "clascnae20", "sbclas20", "tpdefic", "causafast1", "diainiaf1", "mesiniaf1", "diafimaf1", "mesfimaf1", "causafast2", "diainiaf2", "mesiniaf2", "diafimaf2", "mesfimaf2", "causafast3", "diainiaf3", "mesiniaf3", "diafimaf3", "mesfimaf3", "qtdiasafas", "idade")
  setnames(rais_data, new = newnames ) # replacing old names with new
}
rename_rais_2014 <- function(rais_data) {
  newnames <- c("municipio", "clascnae95", "empem3112", "tpvinculo", "causadesli", "mesdesli", "indalvara", "tipoadm", "tiposal", "ocupacao94", "grinstrucao", "sexotrabalhador", "nacionalidad", "raca_cor", "portdefic", "tamestab", "natjuridica", "indceivinc", "tipoestbl", "indpat", "indsimples", "dtadmissao", "remmedr", "remmedia", "remdezr", "remdezembro", "tempempr", "horascontr", "ultrem", "salcontr", "PIS", "dtnascimento","numectps", "CPF", "ceivinc", "identificad", "radiccnpj", "nome", "ocup2002", "clascnae20", "sbclas20", "tpdefic", "causafast1", "diainiaf1", "mesiniaf1", "diafimaf1", "mesfimaf1", "causafast2", "diainiaf2", "mesiniaf2", "diafimaf2", "mesfimaf2", "causafast3", "diainiaf3", "mesiniaf3", "diafimaf3", "mesfimaf3", "qtdiasafas", "idade", "diadesli")
  setnames(rais_data, new = newnames ) # replacing old names with new
}

rename_rais_2015 <- function(rais_data) {
  newnames <- c("municipio", "clascnae95", "empem3112", "tpvinculo", "causadesli", "mesdesli", "indalvara", "tipoadm", "tiposal", "ocupacao94", "grinstrucao", "sexotrabalhador", "nacionalidad", "raca_cor", "portdefic", "tamestab", "natjuridica", "indceivinc", "tipoestbl", "indpat", "indsimples", "dtadmissao", "remmedr", "remmedia", "remdezr", "remdezembro", "tempempr", "horascontr", "ultrem", "salcontr", "PIS", "dtnascimento","numectps", "CPF", "ceivinc", "identificad", "radiccnpj", "nome", "ocup2002", "clascnae20", "sbclas20", "tpdefic", "causafast1", "diainiaf1", "mesiniaf1", "diafimaf1", "mesfimaf1", "causafast2", "diainiaf2", "mesiniaf2", "diafimaf2", "mesfimaf2", "causafast3", "diainiaf3", "mesiniaf3", "diafimaf3", "mesfimaf3", "qtdiasafas", "idade", "diadesli","ibgesubsetor","anochegbr","cepestab","muntrab","razaosocial","remjan","remfev","remmar","remabr","remmai","remjun","remjul","remago","remset","remout","remnov")
  setnames(rais_data, new = newnames ) # replacing old names with new
}


### make lazy [same] ----
make_rais_lazy <- function(rais_data) lazy_dt(rais_data, immutable = FALSE)

### drop TIPOESTBID [same] ----
dropTIPOESTBID_rais <- function(rais_data) {
  rais_data %>%
    select(everything(), -TIPOESTBID)
}
### destring vars [dif] ----
destring_rais_2005 <- function(rais_data) {
  var_need_destringing <- c("municipio", "tipoadm", "tpvinculo", "causadesli", "empem3112", "mesdesli", "grinstrucao", "tamestab", "tipoestbl", "horascontr", "indceivinc", "tiposal", "indalvara", "indpat", "indsimples", "portdefic", "raca_cor")
  rais_data %>%
    mutate_at(var_need_destringing, as.numeric)
}
destring_rais_2012 <- function(rais_data) {
  var_need_destringing <- c("municipio", "tipoadm", "tpvinculo", "causadesli", "empem3112", "mesdesli", "idade", "grinstrucao", "sexotrabalhador", "tamestab", "tipoestbl", "horascontr", "indceivinc", "tiposal", "indalvara", "indpat", "indsimples", "portdefic", "tpdefic", "raca_cor", "qtdiasafas", "causafast1","causafast2","causafast3",
                            "clascnae95")
  rais_data %>%
    mutate_at(var_need_destringing, as.numeric)
}
destring_rais_2014 <- function(rais_data) {
  var_need_destringing <- c("municipio", "tipoadm", "tpvinculo", "causadesli", "empem3112", "mesdesli", "diadesli","idade", "grinstrucao", "sexotrabalhador", "tamestab", "tipoestbl", "horascontr", "indceivinc", "tiposal", "indalvara", "indpat", "indsimples", "portdefic", "tpdefic", "raca_cor", "qtdiasafas", "causafast1","causafast2","causafast3",
                            "clascnae95")
  rais_data %>%
    mutate_at(var_need_destringing, as.numeric)
}

destring_rais_2015 <- function(rais_data) {
  var_need_destringing <- c("municipio", "tipoadm", "tpvinculo", "causadesli", "empem3112", "mesdesli", "diadesli","idade", "grinstrucao", "sexotrabalhador", "tamestab", "tipoestbl", "horascontr", "indceivinc", "tiposal", "indalvara", "indpat", "indsimples", "portdefic", "tpdefic", "raca_cor", "qtdiasafas", "causafast1","causafast2","causafast3",
                            "clascnae95", "anochegbr")
  rais_data %>%
    mutate_at(var_need_destringing, as.numeric)
}

destring_rais_2016 <- function(rais_data) {
  var_need_destringing <- c("municipio", "tipoadm", "tpvinculo", "causadesli", "empem3112", "mesdesli", "diadesli","idade", "grinstrucao", "sexotrabalhador", "tamestab", "tipoestbl", "horascontr", "indceivinc", "tiposal", "indalvara", "indpat", "indsimples", "portdefic", "tpdefic", "raca_cor", "qtdiasafas", "causafast1","causafast2","causafast3", "anochegbr", "ibgesubsetor", "muntrab",
                            "clascnae95"  )
  rais_data %>%
    mutate_at(var_need_destringing, as.numeric)
}


### trim variables [dif] ----
trim_rais <- function(rais_data) {
  rais_data %>%
    mutate_at(c("PIS","CPF","nome", "identificad", "radiccnpj" ),str_trim)
}
### cpf [same] ----
CPF_rais <- function(rais_data) {
  rais_data %>%
    mutate(CPF = case_when(nchar(CPF) <= 5 ~ "",
                           nchar(CPF) == 6 ~ paste0("00000",CPF),
                           nchar(CPF) == 7 ~ paste0("0000",CPF),
                           nchar(CPF) == 8 ~ paste0("000",CPF),
                           nchar(CPF) == 9 ~ paste0("00",CPF),
                           nchar(CPF) == 10 ~ paste0("0",CPF),
                           TRUE ~ as.character(CPF)
    ))
}
### radiccnpj ----
radiccnpj_rais <- function(rais_data) {
  rais_data %>%
    mutate(radiccnpj = if_else(radiccnpj == "0", "",radiccnpj))
}
### identificad [same] ----
identificad_rais <- function(rais_data) {
  rais_data %>%
    mutate(identificad = case_when(nchar(identificad) == 3 ~ paste0("00000000000",identificad),
                                   nchar(identificad) == 4 ~ paste0("0000000000",identificad),
                                   nchar(identificad) == 5 ~ paste0("000000000",identificad),
                                   nchar(identificad) == 6 ~ paste0("00000000",identificad),
                                   nchar(identificad) == 7 ~ paste0("0000000",identificad),
                                   nchar(identificad) == 8 ~ paste0("000000",identificad),
                                   nchar(identificad) == 9 ~ paste0("00000",identificad),
                                   nchar(identificad) == 10 ~ paste0("0000",identificad),
                                   nchar(identificad) == 11 ~ paste0("000",identificad),
                                   nchar(identificad) == 12 ~ paste0("00",identificad),
                                   nchar(identificad) == 13 ~ paste0("0",identificad),
                                   identificad == 0 ~ "",
                                   TRUE ~ identificad
    ))
}
### empem3112 [same] ----
empem3112_rais <- function(rais_data) {
  rais_data %>%
    mutate(empem3112 = fct_recode(as_factor(empem3112), "Nao" = "0" ,  "Sim" = "1") )
}
### tpvinculo  ----
tpvinculo_rais <- function(rais_data) {
  rais_data %>%
    mutate(tpvinculo = fct_recode(as_factor(tpvinculo),
                                  "CLT U/PJ IND" = "10",
                                  "CLT U/PF IND" = "15",
                                  "CLT R/PJ IND" = "20",
                                  "CLT R/PF IND" = "25",
                                  "ESTATUTARIO" = "30",
                                  "ESTAT RGPS" = "31",
                                  "ESTAT N/EFET" = "35",
                                  "AVULSO" = "40",
                                  "TEMPORARIO" = "50",
                                  "APRENDIZ CONTR" = "55",
                                  "CLT U/PJ DET" = "60",
                                  "CLT U/PF DET" = "65",
                                  "CLT R/PJ DET" = "70",
                                  "CLT R/PF DET" = "75",
                                  "DIRETOR" = "80",
                                  "CONT PRZ DET" = "90",
                                  "CONT TMP DET" = "95",
                                  "CONT LEI EST" = "96",
                                  "CONT LEI MUN" = "97"
    ))
}
### causadesli  ----
causadesli_rais <- function(rais_data) {
  rais_data %>%
    mutate(causadesli = fct_recode(as_factor(causadesli),
                                   "Nao desligado"  = "0",
                                   "Rescisao com justa causa por iniciativa do empregador" = "10",
                                   "Rescisao sem justa causa por iniciativa do empregador" = "11",
                                   "Termino do contrato de trabalho" = "12",
                                   "Rescisao com justa causa por iniciativa do empregado (rescisao indireta)" = "20",
                                   "Rescisao sem justa causa por iniciativa do empregado" = "21",
                                   "Possui outro cargo" = "22",
                                   "Transferencia/movimentacao do empregado/servidor, com onus para a cedente" = "30",
                                   "Transferencia/movimentacao do empregado/servidor, sem onus para a cedente" = "31",
                                   "Readaptacao ou redistribuicao (especifico para servidor publico)" = "32",
                                   "Cessao" = "33",
                                   "Mudanca de regime trabalhista" = "40",
                                   "Reforma de militar para a reserva remunerada"  = "50",
                                   "Falecimento" = "60",
                                   "Falecimento decorrente de acidente do trabalho" = "62",
                                   "Falecimento decorrente de acidente do trabalho de trajeto (trajeto residencia-trabalho-residencia)" = "63",
                                   "Falecimento decorrente de doenca profissional" = "64",
                                   "Aposentadoria por tempo de servico, com rescisao contratual" = "70",
                                   "Aposentadoria por tempo de servico, sem rescisao contratual" = "71",
                                   "Aposentadoria por idade, com rescisao contratual" = "72",
                                   "Aposentadoria por invalidez, decorrente de acidente do trabalho" = "73",
                                   "Aposentadoria por invalidez, decorrente de doenca profissional" = "74",
                                   "Aposentadoria compulsoria" = "75",
                                   "Aposentadoria por invalidez, exceto a decorrente de doenca profissional ou acidente do trabalho" = "76",
                                   "Aposentadoria por idade, sem rescisao contratual" = "78",
                                   "Aposentadoria especial, com rescisao contratual" = "79",
                                   "Aposentadoria especial, sem rescisao contratual" = "80"
    ) )
}
### diadesli  ----
diadesli_rais <- function(rais_data) {
  rais_data %>%
    mutate(diadesli = as.numeric(recode(diadesli, "NAO DESL ANO" = "0")))

}
### ocupacao94  ----
ocupacao94_rais_2005 <- function(rais_data) {
  rais_data %>%
    mutate(ocupacao94 = str_replace_all(ocupacao94, "CBO", ""))%>%
    mutate(ocupacao94 = recode(ocupacao94, "000-1" = "")) %>%
    mutate(ocupacao94 = as.numeric(ocupacao94))
}
ocupacao94_rais_2012 <- function(rais_data) {
  rais_data %>%
    mutate(ocupacao94 = recode(ocupacao94, "000-1" = "")) %>%
    mutate(ocupacao94 = as.numeric(ocupacao94))
}
### ocup2002  ----
ocup2002_rais <- function(rais_data) {
  rais_data %>%
    mutate(ocup2002 = str_replace_all(ocup2002,"CBO",""))%>%
    mutate(ocup2002 = as.numeric(ocup2002))
}
### tipoadm  ----
tipoadm_rais <- function(rais_data) {
  rais_data %>%
    mutate(tipoadm = fct_recode(as_factor(tipoadm),
                                "PRIM EMPREGO" = "1",
                                "REEMPREGO" = "2" ,
                                "TRANSF C/ONUS" = "3",
                                "TRANSF S/ONUS" = "4",
                                "OUTROS" = "5",
                                "REINTEGRACAO" = "6",
                                "RECONDUCAO" = "7",
                                "REVERSAO" = "8",
                                "EXERC PROVISORIO" = "9",
                                "REQUISICAO" = "10",
                                "NAO ADMITIDO NO ANO" = "0"

    ) )
}
### tiposal  ----
tiposal_rais <- function(rais_data) {
  rais_data %>%
    mutate( tiposal = fct_recode(as_factor(tiposal),
                                 "Mensal" = "1",
                                 "Quinzenal" = "2",
                                 "Semanal" = "3",
                                 "Diario" = "4",
                                 "Horario" = "5",
                                 "Tarefa" = "6",
                                 "Outros" = "7"))
}
### indalvara  ----
indalvara_rais <- function(rais_data) {
  rais_data %>%
    mutate(indalvara = fct_recode(as_factor(indalvara), "Nao" = "1" ,  "Sim" = "2"))
}
### indpat ----
indpat_rais <- function(rais_data) {
  rais_data %>%
    mutate(indpat = fct_recode(as_factor(indpat), "Nao" = "0" ,  "Sim" = "1"))
}
### indsimples ----
indsimples_rais <- function(rais_data) {
  rais_data %>%
    mutate(indsimples = fct_recode(as_factor(indsimples), "Nao" = "0" ,  "Sim" = "1"))
}
### grinstrucao ----
grinstrucao_rais <- function(rais_data) {
  rais_data %>%
    mutate(grinstrucao = as.numeric(grinstrucao)) %>%
    mutate(grinstrucao = fct_recode(as_factor(grinstrucao),
                                    "Analfabeto" = "1" ,
                                    "Ate 5a Incompleto" = "2",
                                    "5a Completo" = "3",
                                    "6a a 9a Incompleto" = "4",
                                    "9a Completo" = "5",
                                    "Medio Incompleto" = "6",
                                    "Medio Completo" = "7",
                                    "Superior Incompleto" = "8",
                                    "Superior Completo" = "9",
                                    "Mestrado" = "10",
                                    "Doutorado" = "11"
    ))

}
### dtadmissao ----
dtadmissao_rais <- function(rais_data) {
  rais_data %>%
    mutate(dtadmissao = if_else(nchar(dtadmissao)==7, paste0("0",dtadmissao),dtadmissao) ) %>%
    mutate(dtadmissao = dmy(dtadmissao))
}

dtadmissao_rais_2011 <- function(rais_data) {
  rais_data %>%
    mutate(dtadmissao = if_else(nchar(dtadmissao)==7, paste0("0",dtadmissao),dtadmissao) ) %>%
    mutate(dtadmissao = ymd(dtadmissao))
}
### dtnascimento ----
dtnascimento_rais <- function(rais_data) {
  rais_data %>%
    mutate(dtnascimento = if_else(nchar(dtnascimento)==7, paste0("0",dtnascimento),dtnascimento))%>%
    mutate(dtnascimento = dmy(dtnascimento) )
}
### genero ----
genero_rais_2005 <- function(rais_data) {
  rais_data %>%
    mutate(genero	= as_factor(if_else(tolower(sexotrabalhador) == "masculino",  "Masculino","Feminino")))
}
genero_rais_2012 <- function(rais_data) {
  rais_data %>%
    mutate(genero	= as_factor(if_else(sexotrabalhador == 1,  "Masculino","Feminino")))
}
### nacionalidad ----
nacionalidad_rais <- function(rais_data) {
  rais_data %>%
    mutate(nacionalidad = fct_recode(as_factor(nacionalidad),
                                     "Brasileira" = "10",
                                     "Natur Bras" = "20",
                                     "Argentina" = "21",
                                     "Boliviana" = "22",
                                     "Chilena" = "23",
                                     "Paraguaia" = "24",
                                     "Uruguaia" = "25",
                                     "Venezuelano" = "26",
                                     "Colombiano" = "27",
                                     "Peruano" = "28",
                                     "Equatoriano" = "29",
                                     "Alem„" = "30",
                                     "Belga" = "31",
                                     "Britanica" = "32",
                                     "Canadense" = "34",
                                     "Espanhola" = "35",
                                     "Norte Americ." = "36",
                                     "Francesa" = "37",
                                     "Suica" = "38",
                                     "Italiana" = "39",
                                     "Haitiano" = "40",
                                     "Japonesa" = "41",
                                     "Chinesa" = "42",
                                     "Coreana" = "43",
                                     "Russo" = "44",
                                     "Portuguesa" = "45",
                                     "Paquistanes" = "46",
                                     "Indiano" = "47",
                                     "Out. Lat. Amer." = "48",
                                     "Outr. Asiatic." = "49",
                                     "Outras Nac." = "50",
                                     "Outros Europeus" = "51",
                                     "Angolano" = "60",
                                     "Congoles" = "61",
                                     "Sul-Africano" = "62",
                                     "Outros Africanos" = "70",
                                     "Outros" = "80"


    ))
}
### raca_cor ----
raca_cor_rais <- function(rais_data) {
  rais_data %>%
    mutate(raca_cor = fct_recode(as_factor(raca_cor),
                                 "Indigena" = "1",
                                 "Branca" = "2",
                                 "Preta" = "4",
                                 "Amarela" = "6",
                                 "Parda" = "8",
                                 "Nao Ident." = "9"
    ))
}
### tpdefic ----
tpdefic_rais <- function(rais_data) {
  rais_data %>%
    mutate(tpdefic = fct_recode(as_factor(tpdefic),
                                "Nao Defic." = "-1",
                                "Fisica" = "1",
                                "Auditiva" = "2",
                                "Visual" = "3",
                                "Mental" = "4",
                                "Multipla" = "5",
                                "Reabilitado" = "6"))
}
### tamestab ----
tamestab_rais <- function(rais_data) {
  rais_data %>%
    mutate(tamestab = case_when(
      tamestab == 1 ~ 0,
      tamestab == 2 ~ 1,
      tamestab == 3 ~ 2,
      tamestab == 4 ~ 3,
      tamestab == 5 ~ 4,
      tamestab == 6 ~ 5,
      tamestab == 7 ~ 6,
      tamestab == 8 ~ 7,
      tamestab == 9 ~ 8,
      tamestab == 10 ~ 9,
      TRUE ~ 0
    ))%>%  # partially collapsing tamestab into 10 parts from 11
    mutate(tamestab = fct_recode(as_factor(tamestab),
                                 "Zero" = "0",
                                 "Ate 4" = "1",
                                 "De 5 a 9" = "2",
                                 "De 10 a 19" = "3",
                                 "De 20 a 49" = "4",
                                 "De 50 a 99" = "5",
                                 "De 100 a 249" = "6",
                                 "De 250 a 499" = "7",
                                 "De 500 a 999" = "8",
                                 "1000 ou mais" = "9"
    ))
}
### tipoestbl ----
tipoestbl_rais <- function(rais_data) {
  rais_data %>%
    mutate(tipoestbl = if_else(tipoestbl==4, 9 , tipoestbl )) %>%
    mutate(tipoestbl = fct_recode(as_factor(tipoestbl),
                                  "CNPJ" = "1",
                                  "CEI" = "3",
                                  "Nao Identificado" = "9"
    ))
}
### commareplace ----
commareplace_rais <- function(rais_data) {
  var_need_subbing <- c("remdezembro", "remmedia", "remdezr", "remmedr", "tempempr", "salcontr", "ultrem")
  rais_data %>%
    mutate_at(var_need_subbing, ~ str_replace_all(. ,"," ,".")) %>% # changing commas to
    # periods and then shifting to numeric
    mutate_at(var_need_subbing, as.numeric)
}

commareplace_rais_2015 <- function(rais_data) {
  var_need_subbing <- c("remdezembro", "remmedia", "remdezr", "remmedr", "tempempr", "salcontr", "ultrem","remjan","remfev","remmar","remabr","remmai","remjun","remjul","remago","remset","remout","remnov"
)
  rais_data %>%
    mutate_at(var_need_subbing, ~ str_replace_all(. ,"," ,".")) %>% # changing commas to
    # periods and then shifting to numeric
    mutate_at(var_need_subbing, as.numeric)
}

### natjuridica ----
natjuridica_rais <- function(rais_data) {
  rais_data %>%
    mutate(natjuridica = as.numeric(natjuridica))%>%
    mutate(natjuridica = fct_recode(as_factor(natjuridica ),
                                    "POD EXEC FED" =  "1015",
                                    "POD EXEC EST" = "1023",
                                    "POD EXEC MUN" =  "1031",
                                    "POD LEG FED" = "1040",
                                    "POD LEG EST" = "1058",
                                    "POD LEG MUN" = "1066",
                                    "POD JUD FED" = "1074",
                                    "POD JUD EST" = "1082",
                                    "ORG AUT DPB" = "1090",
                                    "AUTARQ FED" = "1104",
                                    "AUTARQ EST" = "1112",
                                    "AUTARQ MUN" = "1120",
                                    "FUNDAC FED" = "1139",
                                    "FUNDAC EST" = "1147",
                                    "FUNDAC MUN" = "1155",
                                    "ORG AUT FED" = "1163",
                                    "ORG AUT EST" = "1171",
                                    "ORG AUT MUN" = "1180",
                                    "COM POLINAC" = "1198",
                                    "FUNDO PUBLIC" = "1201",
                                    "ASSOC PUBLIC" = "1210",
                                    "ADM PUB OUTR" = "2005",
                                    "EMP PUB" = "2011",
                                    "EMP PB SA CP" = "2020",
                                    "SOC MISTA" = "2038",
                                    "SA ABERTA" = "2046",
                                    "SA FECH" = "2054",
                                    "SOC QT LTDA" = "2062",
                                    "SOC COLETV" = "2070",
                                    "SOC COLETV07" = "2076",
                                    "SOC COMD SM" =  "2089",
                                    "SOC COMD AC" = "2097",
                                    "SOC CAP IND" = "2100",
                                    "SOC CIVIL" = "2119",
                                    "SOC CTA PAR" = "2127",
                                    "FRM MER IND" = "2135",
                                    "COOPERATIVA" = "2143",
                                    "CONS EMPRES" = "2151",
                                    "GRUP SOC" = "2160",
                                    "FIL EMP EXT" = "2178",
                                    "FIL ARG-BRA" = "2194",
                                    "ENT ITAIPU" = "2208",
                                    "EMP DOM EXT" = "2216",
                                    "FUN INVEST" = "2224",
                                    "SOC SIMP PUR" = "2232",
                                    "SOC SIMP LTD" = "2240",
                                    "SOC SIMP COL" = "2259",
                                    "SOC SIMP COM" = "2267",
                                    "EMPR BINAC" = "2275",
                                    "CONS EMPREG" = "2283",
                                    "CONS SIMPLES" = "2291",
                                    "OUTR ORG EMP" = "2992",
                                    "FUND REC PRV" = "3018",
                                    "ASSOCIACAO" = "3026",
                                    "CARTORIO" = "3034",
                                    "ORG SOCIAL" = "3042",
                                    "OSCIP" = "3050",
                                    "OUT FUND PR" = "3069",
                                    "SERV SOC AU" = "3077",
                                    "CONDOMIN" = "3085",
                                    "UNID EXEC" = "3093",
                                    "COM CONC" = "3107",
                                    "ENT MED ARB" = "3115",
                                    "PART POLIT" = "3123",
                                    "ENT SOCIAL" = "3130",
                                    "ENT SOCIAL07" = "3131",
                                    "FIL FUN EXT" = "3204",
                                    "FUN DOM EXT" = "3212",
                                    "ORG RELIG" = "3220",
                                    "COMUN INDIG" = "3239",
                                    "FUNDO PRIVAD" = "3247",
                                    "OUTR ORG" = "3999",
                                    "EMP IND IMO" = "4014",
                                    "SEG ESPEC" = "4022",
                                    "AUTONOMO" = "4030",
                                    "AUTON C/ EMPR" = "4049",
                                    "EMPDOR DOM" = "4057",
                                    "CCIVIL PFIS" = "4065",
                                    "EMPRESARIO" = "4073",
                                    "CONTR IND" = "4080",
                                    "CONTR IND07" = "4081",
                                    "CAN CARG POL" = "4090",
                                    "LEILOEIRO" = "4111",
                                    "OUTR ORG" = "4995",
                                    "ORG INTERN" = "5002",
                                    "ORG INTERNAC" = "5010",
                                    "REPR DIPL ES" = "5029",
                                    "OUT INST EXT" = "5037",
                                    "IGNORADO" = " -1"

    ))
}

### causafast ----
causafast_rais <- function(rais_data) {
  rais_data %>%
    mutate_at(vars(starts_with("causafast")), ~fct_recode(as_factor(. ),
                                                          "Nao se afastou" =  "-1",
                                                          "Acidente de trabalho tipico" = "10",
                                                          "Acidente do trabalho de trajeto" =  "20",
                                                          "Doenca relacionada ao trabalho" = "30",
                                                          "Doenca nao relacionada ao trabalho" = "40",
                                                          "Licenca maternidade" = "50",
                                                          "Servico militar obrigatorio" = "60",
                                                          "Licenca sem vencimento/remuneracao" = "70"
    ))


}
### diainiafmesiniaf ----
diainiafmesiniaf_rais <- function(rais_data) {
  rais_data %>%
    mutate_at(vars(starts_with(c("diainiaf", "mesiniaf"))), ~if_else(. == "IGNORADO", "-1", .)) %>%
    mutate_at(vars(starts_with(c("diainiaf", "mesiniaf"))), ~as.numeric(.)) %>%
    mutate_at(vars(starts_with(c("diainiaf", "mesiniaf"))),  ~fct_recode(as_factor(. ),
                                                                         "Nao se afastou" =  "-1"))
}

assertclass_rais <- function(rais_data){
  rais_data %>%
    mutate_at(if('PIS' %in% names(.)) 'PIS' else integer(0), as.character) %>%
    mutate_at(if('numectps' %in% names(.)) 'numectps' else integer(0), as.character) %>%
    mutate_at(if('CPF' %in% names(.)) 'CPF' else integer(0), as.character) %>%
    mutate_at(if('nome' %in% names(.)) 'nome' else integer(0), as.character) %>%
    mutate_at(if('identificad' %in% names(.)) 'identificad' else integer(0), as.character) %>%
    mutate_at(if('radiccnpj' %in% names(.)) 'radiccnpj' else integer(0), as.character) %>%
    mutate_at(if('tpvinculo' %in% names(.)) 'tpvinculo' else integer(0), as.factor) %>%
    mutate_at(if('empem3112' %in% names(.)) 'empem3112' else integer(0), as.factor) %>%
    mutate_at(if('tipoadm' %in% names(.))  'tipoadm' else integer(0), as.factor) %>%
    mutate_at(if('mesadmissao' %in% names(.))  'mesadmissao' else integer(0), as.numeric) %>%
    mutate_at(if('dtadmissao' %in% names(.))  'dtadmissao' else integer(0), as.Date) %>%
    mutate_at(if('causadesli' %in% names(.))  'causadesli' else integer(0), as.factor) %>%
    mutate_at(if('mesdesli' %in% names(.))  'mesdesli' else integer(0), as.numeric) %>%
    mutate_at(if('diadeslig' %in% names(.))  'diadeslig' else integer(0), as.numeric) %>%
    mutate_at(if('ocupacao94' %in% names(.))  'ocupacao94' else integer(0), as.character) %>%
    mutate_at(if('grinstrucao' %in% names(.))  'grinstrucao' else integer(0), as.factor) %>%
    mutate_at(if('genero' %in% names(.))  'genero' else integer(0), as.factor) %>%
    mutate_at(if('idade' %in% names(.))  'idade' else integer(0), as.numeric) %>%
    mutate_at(if('dtnascimento' %in% names(.))  'dtnascimento' else integer(0), as.Date) %>%
    mutate_at(if('faixaetaria' %in% names(.))  'faixaetaria' else integer(0), as.factor) %>%
    mutate_at(if('nacionalidad' %in% names(.))  'nacionalidad' else integer(0), as.factor) %>%
    mutate_at(if('raca_cor' %in% names(.))  'raca_cor' else integer(0), as.factor) %>%
    mutate_at(if('portdefic' %in% names(.))  'portdefic' else integer(0), as.numeric) %>%
    mutate_at(if('horascontr' %in% names(.))  'horascontr' else integer(0), as.numeric) %>%
    mutate_at(if('remdezembro' %in% names(.))  'remdezembro' else integer(0), as.numeric) %>%
    mutate_at(if('remmedia' %in% names(.))  'remmedia' else integer(0), as.numeric) %>%
    mutate_at(if('tempempr' %in% names(.))  'tempempr' else integer(0), as.numeric) %>%
    mutate_at(if('remdezr' %in% names(.))  'remdezr' else integer(0), as.numeric) %>%
    mutate_at(if('remmedr' %in% names(.))  'remmedr' else integer(0), as.numeric) %>%
    mutate_at(if('tiposal' %in% names(.))  'tiposal' else integer(0), as.factor) %>%
    mutate_at(if('salcontr' %in% names(.))  'salcontr' else integer(0), as.numeric) %>%
    mutate_at(if('ultrem' %in% names(.))  'ultrem' else integer(0), as.numeric) %>%
    mutate_at(if('ocup2002' %in% names(.))  'ocup2002' else integer(0), as.character) %>%
    mutate_at(if('tpdef' %in% names(.))  'tpdef' else integer(0), as.factor) %>%
    mutate_at(if('clascnae95' %in% names(.))  'clascnae95' else integer(0), as.character) %>%
    mutate_at(if('clascnae20' %in% names(.))  'clascnae20' else integer(0), as.character) %>%
    mutate_at(if('sbclas20' %in% names(.))  'sbclas20' else integer(0), as.character) %>%
    mutate_at(if('natjuridica' %in% names(.))  'natjuridica' else integer(0), as.factor) %>%
    mutate_at(if('tamestab' %in% names(.))  'tamestab' else integer(0), as.factor) %>%
    mutate_at(if('tipoestbl' %in% names(.))  'tipoestbl' else integer(0), as.factor) %>%
    mutate_at(if('ibgesubsetor' %in% names(.))  'ibgesubsetor' else integer(0), as.factor) %>%
    mutate_at(if('ibgeatividade' %in% names(.))  'ibgeatividade' else integer(0), as.factor) %>%
    mutate_at(if('indceivinc' %in% names(.))  'indceivinc' else integer(0), as.numeric) %>%
    mutate_at(if('ceivinc' %in% names(.))  'ceivinc' else integer(0), as.character) %>%
    mutate_at(if('indalvara' %in% names(.))  'indalvara' else integer(0), as.factor) %>%
    mutate_at(if('indpat' %in% names(.))  'indpat' else integer(0), as.factor) %>%
    mutate_at(if('indsimples' %in% names(.))  'indsimples' else integer(0),  as.factor) %>%
    mutate_at(if('causafast1' %in% names(.))  'causafast1' else integer(0), as.factor) %>%
    mutate_at(if('causafast2' %in% names(.))  'causafast2' else integer(0), as.factor) %>%
    mutate_at(if('causafast3' %in% names(.))  'causafast3' else integer(0), as.factor) %>%
    mutate_at(if('diainiaf1' %in% names(.))  'diainiaf1' else integer(0), as.factor) %>%
    mutate_at(if('diainiaf2' %in% names(.))  'diainiaf2' else integer(0), as.factor) %>%
    mutate_at(if('diainiaf3' %in% names(.))  'diainiaf3' else integer(0), as.factor) %>%
    mutate_at(if('diafimaf1' %in% names(.))  'diafimaf1' else integer(0), as.factor) %>%
    mutate_at(if('diafimaf2' %in% names(.))  'diafimaf2' else integer(0),as.factor) %>%
    mutate_at(if('diafimaf3' %in% names(.))  'diafimaf3' else integer(0), as.factor) %>%
    mutate_at(if('mesiniaf1' %in% names(.))  'mesiniaf1' else integer(0), as.factor) %>%
    mutate_at(if('mesiniaf2' %in% names(.))  'mesiniaf2' else integer(0), as.factor) %>%
    mutate_at(if('mesiniaf3' %in% names(.))  'mesiniaf3' else integer(0), as.factor) %>%
    mutate_at(if('mesfimaf1' %in% names(.))  'mesfimaf1' else integer(0), as.factor) %>%
    mutate_at(if('mesfimaf2' %in% names(.))  'mesfimaf2' else integer(0), as.factor) %>%
    mutate_at(if('mesfimaf3' %in% names(.))  'mesfimaf3' else integer(0), as.factor) %>%
    mutate_at(if('qtdiasafas' %in% names(.))  'qtdiasafas' else integer(0), as.numeric) %>%
    mutate_at(if('anochegbr' %in% names(.))  'anochegbr' else integer(0), as.numeric)
}

