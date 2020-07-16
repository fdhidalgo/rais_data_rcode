library(pacman)
p_load(dtplyr, data.table, ggplot2, janitor, stringr, lubridate, glue, tidyverse, parallel)

rm(list =ls())



clean_rais <- function(rais) {

  # renaming the variables to match the stata version
  newnames <- c("municipio", "clascnae95" ,"empem3112" ,"tpvinculo" ,"causadesli" ,"diadesli" ,"ocup2002" ,"mesdesli" ,"indalvara" ,"tipoadm" ,"tiposal" ,"ocupacao94" ,"grinstrucao" ,"sexotrabalhador" ,"nacionalidad" ,"raca_cor" ,"portdefic" ,"tamestab" ,"natjuridica" ,"indceivinc" ,"tipoestbl" ,"indpat" ,"indsimples" ,"dtadmissao" ,"remdezr" ,"remdezembro" ,"remmedr" ,"remmedia" ,"tempempr" ,"horascontr" ,"ultrem" ,"salcontr" ,"PIS" ,"dtnascimento" ,"numectps" ,"CPF" ,"ceivinc" ,"identificad" ,"radiccnpj" ,"nome")
  oldnames <- c("MUNICIPIO","CLASCNAE95" ,"EMPEM3112" ,"TPVINCULO" ,"CAUSADESLI" ,"DIADESL" ,"OCUP2002" ,"MESDESLIG" ,"INDALVARA" ,"TIPOADM" ,"TIPOSAL" ,"OCUPACAO94" ,"GRAUINSTR" ,"GENERO" ,"NACIONALIDAD" ,"RACA_COR" ,"PORTDEFIC" ,"TAMESTAB" ,"NATURJUR" ,"INDCEIVINC" ,"TIPOESTBL" ,"INDPAT" ,"INDSIMPLES" ,"DTADMISSAO" ,"REMDEZR" ,"REMDEZEMBRO" ,"REMMEDR" ,"REMMEDIA" ,"TEMPEMPR" ,"HORASCONTR" ,"ULTREM" ,"SALCONTR" ,"PIS" ,"DTNASCIMENT" ,"NUMECTPS" ,"CPF" ,"CEIVINC" ,"IDENTIFICAD" ,"RADICCNPJ" ,"NOME")

  names(rais) %<>%  # this gets rid of formatting differences for the old names
    str_remove_all("[//(//)]") %>%
    str_remove_all(fixed("$")) %>%
    str_remove_all("\\." )%>%
    str_remove_all("\\s")

  setnames(rais, old = oldnames, new = newnames ) # replacing old names with new


  # Making a lazy dataframe
  lazy <- lazy_dt(rais)

  # making vectors of variables that either need to be destrung later on or need character
  # character substitution
  var_need_destringing <- c("municipio", "tipoadm", "tpvinculo", "causadesli", "empem3112", "mesdesli", "grinstrucao", "tamestab", "tipoestbl", "horascontr", "indceivinc", "tiposal", "indalvara", "indpat", "indsimples", "portdefic", "raca_cor")
  var_need_subbing <- c("remdezembro", "remmedia", "remdezr", "remmedr", "tempempr", "salcontr", "ultrem")

  # this is where the code takes the dataset and changes it
  rais <- lazy %>%
    select(everything(), -TIPOESTBID) %>%  # dropping TIPOESTBID
    mutate_at(var_need_destringing, as.numeric)%>%  # destringing some variables
    mutate_at(c("PIS","CPF","nome", "identificad", "radiccnpj" ),str_trim)%>%
    mutate(CPF = case_when(nchar(CPF) <= 5 ~ "",
                           nchar(CPF) == 6 ~ paste0("00000",CPF),
                           nchar(CPF) == 7 ~ paste0("0000",CPF),
                           nchar(CPF) == 8 ~ paste0("000",CPF),
                           nchar(CPF) == 9 ~ paste0("00",CPF),
                           nchar(CPF) == 10 ~ paste0("0",CPF)
    )) %>%  # the above changes CPF
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
                                   identificad == 0 ~ ""
    ))%>%  # the above changes identificad

    mutate(empem3112 = fct_recode(as_factor(empem3112), "Nao" = "0" ,  "Sim" = "1") )%>%
    # above is recoding of empem3112
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
    ))%>%  # above is recoding of tpvinculo
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
    ) )%>%  # above is recoding of causadesli
    mutate(diadesli = as.numeric(recode(diadesli, "NAO DESL ANO" = "0"))) %>%
    # above is recoding of diadesli
    mutate(ocupacao94 = str_replace_all(ocupacao94, "CBO", ""))%>%
    mutate(ocupacao94 = recode(ocupacao94, "000-1" = "")) %>%
    mutate(ocupacao94 = as.numeric(ocupacao94)) %>%
    # above is recoding of ocupacao94
    mutate(ocup2002 = str_replace_all(ocup2002,"CBO",""))%>%
    mutate(ocup2002 = as.numeric(ocup2002))%>%
    # above is recoding of ocup2002
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

    ) ) %>%  # above is recoding of tipoadm
    mutate( tiposal = fct_recode(as_factor(tiposal),
                                 "Mensal" = "1",
                                 "Quinzenal" = "2",
                                 "Semanal" = "3",
                                 "Diario" = "4",
                                 "Horario" = "5",
                                 "Tarefa" = "6",
                                 "Outros" = "7")) %>%  # above is recoding of tiposa
    mutate(indalvara = fct_recode(as_factor(indalvara), "Nao" = "0" ,  "Sim" = "1"),
           indpat = fct_recode(as_factor(indpat), "Nao" = "0" ,  "Sim" = "1"),
           indsimples = fct_recode(as_factor(indsimples), "Nao" = "0" ,  "Sim" = "1")) %>%
    # above is recoding of indalvara, indpat, indsimples
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
    )) %>%
    # above is recoding of grinstrucao
    mutate(dtadmissao = if_else(nchar(dtadmissao) == 7, paste0("0", dtadmissao),dtadmissao),
           dtnascimento = if_else(nchar(dtnascimento) == 7, paste0("0", dtnascimento),dtnascimento)) %>%
    # above is recoding of dtadmissao, dtnascimento
    mutate(genero	= as_factor(if_else( tolower(sexotrabalhador) == "masculino",  "Masculino","Feminino")
    )) %>%  # creating gender variable
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


    )) %>%  # creating national id variable
    mutate(raca_cor = fct_recode(as_factor(raca_cor),
                                 "Indigena" = "1",
                                 "Branca" = "2",
                                 "Preta" = "4",
                                 "Amarela" = "6",
                                 "Parda" = "8",
                                 "Nao Ident." = "9"
    )) %>%  # recoding raca_cor
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
      tamestab == 10 ~ 9
    ))%>%  # partially collapsing tamestab into 10 parts from 11
    mutate(tamestab = fct_recode(as_factor(tamestab),
                                 "Zero" = "0",
                                 "Ate 4" = "1",
                                 "De 5 a 9" = "2",
                                 "De 20 a 49" = "4",
                                 "De 50 a 99" = "5",
                                 "De 100 a 249" = "6",
                                 "De 250 a 499" = "7",
                                 "De 500 a 999" = "8",
                                 "1000 ou mais" = "9"
    )) %>%  # now recoding tamestab
    mutate(tipoestbl = if_else(tipoestbl==4, 9 , tipoestbl )) %>%
    mutate(tipoestbl = fct_recode(as_factor(tipoestbl),
                                  "CNPJ" = "1",
                                  "CEI" = "3",
                                  "Nao Identificado" = "9"
    )) %>%  # recoding tipoestbl
    mutate_at(var_need_subbing, ~ str_replace_all(. ,"," ,".")) %>% # changing commas to
    # periods and then shifting to numeric
    mutate_at(var_need_subbing, as.numeric)%>%
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

    )) %>%  # above is recoding of national jurisdiction
    select(PIS, CPF, numectps, nome, identificad, radiccnpj, municipio,
           tpvinculo, empem3112, tipoadm, dtadmissao, causadesli, diadesli, mesdesli,
           ocupacao94,  ocup2002,  grinstrucao, genero, dtnascimento, nacionalidad,
           portdefic, raca_cor, remdezembro, remdezr, remmedia, remmedr, tempempr, tiposal,
           salcontr ,ultrem, horascontr, clascnae95, tamestab, natjuridica, tipoestbl, indceivinc,
           ceivinc, indalvara, indpat ,indsimples)  %>%  #this reorders the
    # dataframe and drops a duplicate column made in the process
    as.data.table()
}




