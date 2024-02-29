# Cargar Librerias

library(readxl)
library(dplyr)
library(tidyr)

# Cargar archivo excel

datos <- read_excel("BLzm03_final.xlsx")

View(datos)

# Seleccionar solo las columnas relevantes
df <- datos %>% 
  select(ent,mun, cve_geo, cve_zm, nom_zm, cve_sec, cve_sub,
         ue, re, pb, va, fb, af, po, po_m, po_h, pre, pre_m, pre_h, pho, pho_m, pho_h,
         QLue, QLre, QLpb, QLva, QLfb, QLaf, QLpo, QLpo_m, QLpo_h, QLpre, QLpre_m, QLpre_h, QLpho, QLpho_m, QLpho_h,
         PRue, PRre, PRpb, PRva, PRfb, PRaf, PRpo, PRpo_m, PRpo_h, PRpre, PRpre_m, PRpre_h, PRpho, PRpho_m, PRpho_h,
         HHue, HHre, HHpb, HHva, HHfb, HHaf, HHpo, HHpo_m, HHpo_h, HHHHe, HHHHe_m, HHHHe_h, HHpho, HHpho_m, HHpho_h,
         IHHue, IHHre, IHHpb, IHHva, IHHfb, IHHaf, IHHpo, IHHpo_m, IHHpo_h, IHHIHHe, IHHIHHe_m, IHHIHHe_h, IHHpho, IHHpho_m, IHHpho_h)

# Obtener valores únicos de cve_geo

df_distinct <- df %>% distinct(cve_geo)

# Agrupar valores por cve_sub

df_wide <- df %>% 
  pivot_wider(names_from = cve_sub, values_from = c("ue", "af", "fb", "pb", "po", "re", "va", 
                                                    "QLue", "QLaf", "QLfb", "QLpb", "QLpo", "QLre", "QLva", 
                                                    "PRue", "PRaf", "PRfb", "PRpb", "PRpo", "PRre", "PRva",
                                                    "HHue", "HHaf", "HHfb", "HHpb", "HHpo", "HHre", "HHva",
                                                    "IHHue", "IHHaf", "IHHfb", "IHHpb", "IHHpo", "IHHre", "IHHva"), 
              names_glue = "{.value}_{cve_sub}")

# Unir valores únicos de cve_geo con datos agrupados por cve_sub

zm19 <- left_join(df_distinct, df_wide, by = "cve_geo")
View(zm19)

# Guardar como archivo xlsx

library(openxlsx)
write.xlsx(zm19, "zm19.xlsx")