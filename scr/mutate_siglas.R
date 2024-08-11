

mutate_siglas <- function(df){
  df <- df |> 
  mutate(siglas = case_when(str_detect(str_to_lower(partido_politico), ".*part.*popul|pp") ~ "PP",
                            # P.P-E.U. nos interesa como PP
                            # listas conjuntas con el Partido Popular en las elecciones municipales y autonómicas de 2007
                            # en las elecciones generales de 2011 se presentó en coalición con el Partido Popular
                            str_detect(str_to_lower(partido_politico), "^part.*socialist|^socialist.*teruel") ~ "PSOE",
                            str_detect(str_to_lower(partido_politico), "^ciudadanos.*partido|^ciutadans") ~ "C's",
                            str_detect(str_to_lower(partido_politico), "vasco") ~ "PNV",
                            str_detect(str_to_lower(partido_politico), "galego") ~ "BNG",
                            str_detect(str_to_lower(partido_politico), "comprom") &
                              !str_detect(partido_politico,"^0-9")~ "COMPROMÍS", # para evitar COMPROMÍS 2
                            str_detect(str_to_lower(partido_politico), "^conver.*uni|^conver.*cata") ~ "CiU",
                            # El 19 de septiembre de 1978 [Convergencia Democrática de Cataluña] se coalizó con Unió Democràtica de Catalunya
                            # El 18 de junio de 2015 Convergència Democràtica de Catalunya anunció el fin de Convergència i Unió 
                            str_detect(str_to_upper(partido_politico), "VERDES|PODEM|EZKER|COMUNISTA|ZQUIERDA REPUBLICANA
                                     |VERDS|IZQUIERDA UNIDA|EN MAREA|UNIDAD POPULAR|EQUO") & # importante que el | esté en esta línea y no al final de la anterior
                              !str_detect(str_to_upper(stringi::stri_trans_general(partido_politico, id = "Latin-ASCII")), "MAS PAIS") ~ "UP",
                            partido_politico == "IU" ~ "UP",
                            str_detect(str_to_lower(partido_politico), "erc") |
                              str_detect(tolower(partido_politico), "esquerra republicana") ~ "ERC",
                            # FRONT PEL PAIS VALENCIA-ERC --> ERC
                            # https://www.vilaweb.cat/noticia/1142933/20000710/front-pais-valencia-fusionara-erc.pdf
                            str_detect(str_to_upper(partido_politico), "EH|EUSKO|ARALAR") | 
                              str_detect(str_to_upper(partido_politico), "EH|EUSKO|ARALAR")~ "EH - BILDU",
                            str_detect(str_to_upper(stringi::stri_trans_general(partido_politico, id = "Latin-ASCII")), "MAS PAIS") ~ "MÁS PAÍS",
                            str_detect(str_to_upper(partido_politico), "VOX") ~ "VOX",
                            TRUE ~ "Otros"))
  
  return(df)
}