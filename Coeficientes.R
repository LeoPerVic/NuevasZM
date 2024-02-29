# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

datos <- read_excel("Bases/SAIC_2003.xlsx")

# Crear vector subsec_mun

subsec_mun <- datos %>% group_by(cve_geo, cve_sub, cve_zm) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                                        po_h = sum(po_h, na.rm = TRUE),  
                                                                        po_m = sum(po_m, na.rm = TRUE), 
                                                                        re = sum(re, na.rm = TRUE), 
                                                                        pb = sum(pb, na.rm = TRUE), 
                                                                        pre = sum(pre, na.rm = TRUE),
                                                                        pre_h = sum(pre_h, na.rm = TRUE),  
                                                                        pre_m = sum(pre_m, na.rm = TRUE), 
                                                                        pho = sum(pho, na.rm = TRUE), 
                                                                        pho_h = sum(pho_h, na.rm = TRUE), 
                                                                        pho_m = sum(pho_m, na.rm = TRUE),
                                                                        va = sum(as.numeric(va), na.rm = TRUE),
                                                                        fb = sum(as.numeric(fb), na.rm = TRUE))

# Crear vector tot_mun

tot_mun <- datos %>% group_by(cve_geo, cve_zm) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                            po_h = sum(po_h, na.rm = TRUE),  
                                                            po_m = sum(po_m, na.rm = TRUE), 
                                                            re = sum(re, na.rm = TRUE), 
                                                            pb = sum(pb, na.rm = TRUE), 
                                                            pre = sum(pre, na.rm = TRUE),
                                                            pre_h = sum(pre_h, na.rm = TRUE),  
                                                            pre_m = sum(pre_m, na.rm = TRUE), 
                                                            pho = sum(pho, na.rm = TRUE), 
                                                            pho_h = sum(pho_h, na.rm = TRUE), 
                                                            pho_m = sum(pho_m, na.rm = TRUE),
                                                            va = sum(as.numeric(va), na.rm = TRUE),
                                                            fb = sum(as.numeric(fb), na.rm = TRUE))
View(tot_mun)

# Crear vector subsec_zm

subsec_zm <- datos %>% group_by(cve_zm, cve_sub) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                               po_h = sum(po_h, na.rm = TRUE),  
                                                               po_m = sum(po_m, na.rm = TRUE), 
                                                               re = sum(re, na.rm = TRUE), 
                                                               pb = sum(pb, na.rm = TRUE), 
                                                               pre = sum(pre, na.rm = TRUE),
                                                               pre_h = sum(pre_h, na.rm = TRUE),  
                                                               pre_m = sum(pre_m, na.rm = TRUE), 
                                                               pho = sum(pho, na.rm = TRUE), 
                                                               pho_h = sum(pho_h, na.rm = TRUE), 
                                                               pho_m = sum(pho_m, na.rm = TRUE),
                                                               va = sum(as.numeric(va), na.rm = TRUE),
                                                               fb = sum(as.numeric(fb), na.rm = TRUE))
View(subsec_zm)

# Crear vector tot_zm

tot_zm <- datos %>% group_by(cve_zm) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                   po_h = sum(po_h, na.rm = TRUE),  
                                                   po_m = sum(po_m, na.rm = TRUE), 
                                                   re = sum(re, na.rm = TRUE), 
                                                   pb = sum(pb, na.rm = TRUE), 
                                                   pre = sum(pre, na.rm = TRUE),
                                                   pre_h = sum(pre_h, na.rm = TRUE),  
                                                   pre_m = sum(pre_m, na.rm = TRUE), 
                                                   pho = sum(pho, na.rm = TRUE), 
                                                   pho_h = sum(pho_h, na.rm = TRUE), 
                                                   pho_m = sum(pho_m, na.rm = TRUE),
                                                   va = sum(as.numeric(va), na.rm = TRUE),
                                                   fb = sum(as.numeric(fb), na.rm = TRUE))




## Coeficiente de localización económica (QL)

# Numerador

subsec_mun_div <- left_join(subsec_mun, tot_mun, by = c("cve_geo" = "cve_geo", "cve_zm" = "cve_zm")) %>% 
  mutate(ue = ue.x/ue.y,
         po_h = po_h.x/po_h.y,
         po_m = po_m.x/po_m.y,
         re = re.x/re.y,
         pb = pb.x/pb.y,
         pre = pre.x/pre.y,
         pre_h = pre_h.x/pre_h.y,
         pre_m = pre_m.x/pre_m.y,
         pho = pho.x/pho.y,
         pho_h = pho_h.x/pho_h.y,
         pho_m = pho_m.x/pho_m.y,
         va = va.x/va.y,
         fb = fb.x/fb.y) %>% 
  select(-ue.x, -ue.y, -po_h.x, -po_h.y, -po_m.x, -po_m.y, -re.x, -re.y, -pb.x, -pb.y, -pre.x, -pre.y,
         -pre_h.x, -pre_h.y, -pre_m.x, -pre_m.y, -pho.x, -pho.y, -pho_h.x, -pho_h.y, -pho_m.x, -pho_m.y,
         -va.x, -va.y, -fb.x, -fb.y)


# Denominador

# Unir vectores subsec_zm y tot_zm por cve_zm

subsec_tot_zm <- left_join(subsec_zm, tot_zm, by = "cve_zm")

View(subsec_tot_zm)

# Dividir los valores de subsec_zm entre los valores de tot_zm por cve_zm

subsec_tot_zm_div <- subsec_tot_zm %>%
  mutate(ue.div = ue.x/ue.y,
         po_h.div = po_h.x/po_h.y,
         po_m.div = po_m.x/po_m.y,
         re.div = re.x/re.y,
         pb.div = pb.x/pb.y,
         pre.div = pre.x/pre.y,
         pre_h.div = pre_h.x/pre_h.y,
         pre_m.div = pre_m.x/pre_m.y,
         pho.div = pho.x/pho.y,
         pho_h.div = pho_h.x/pho_h.y,
         pho_m.div = pho_m.x/pho_m.y,
         va.div = va.x/va.y,
         fb.div = fb.x/fb.y) %>% 
  select(-ue.x, -ue.y, -po_h.x, -po_h.y, -po_m.x, -po_m.y, -re.x, -re.y, -pb.x, -pb.y, -pre.x, -pre.y,
         -pre_h.x, -pre_h.y, -pre_m.x, -pre_m.y, -pho.x, -pho.y, -pho_h.x, -pho_h.y, -pho_m.x, -pho_m.y,
         -va.x, -va.y, -fb.x, -fb.y)

View(subsec_tot_zm_div)

# Resultado final QL

# Unir subsec_mun_div y subsec_tot_zm_div por cve_zm y cve_sub

QL <- left_join(subsec_mun_div, subsec_tot_zm_div, by = c("cve_zm", "cve_sub"))

View(QL)

# Dividir cada variable de subsec_mun_div entre la variable correspondiente de subsec_tot_zm_div

QL <- QL %>% 
  mutate(QLue = ue / ue.div,
         QLpo_h = po_h / po_h.div,
         QLpo_m = po_m / po_m.div,
         QLre = re / re.div,
         QLpb = pb / pb.div,
         QLpre = pre / pre.div,
         QLpre_h = pre_h / pre_h.div,
         QLpre_m = pre_m / pre_m.div,
         QLpho = pho / pho.div,
         QLpho_h = pho_h / pho_h.div,
         QLpho_m = pho_m / pho_m.div,
         QLva = va / va.div,
         QLfb = fb / fb.div) %>% 
  select(-ue, -ue.div, -po_h, -po_h.div, -po_m, -po_m.div, -re, -re.div, -pb, -pb.div, 
         -pre, -pre.div, -pre_h, -pre_h.div, -pre_m, -pre_m.div, -pho, -pho.div, -pho_h, -pho_h.div, 
         -pho_m, -pho_m.div, -va, -va.div, -fb, -fb.div)

View(QL)

# Estimar coeficiente PR

PR <- subsec_mun %>% 
  left_join(subsec_zm, by = c("cve_sub", "cve_zm")) %>% 
  mutate(PRue = ue.x / ue.y,
         PRpo_h = po_h.x / po_h.y,
         PRpo_m = po_m.x / po_m.y,
         PRre = re.x / re.y,
         PRpb = pb.x / pb.y,
         PRpre = pre.x / pre.y,
         PRpre_h = pre_h.x / pre_h.y,
         PRpre_m = pre_m.x / pre_m.y,
         PRpho = pho.x / pho.y,
         PRpho_h = pho_h.x / pho_h.y,
         PRpho_m = pho_m.x / pho_m.y,
         PRva = va.x / va.y,
         PRfb = fb.x / fb.y) %>% 
  select(cve_geo, cve_sub, cve_zm, PRue, PRpo_h, PRpo_m, PRre, PRpb, PRpre, PRpre_h, PRpre_m, PRpho, PRpho_h, PRpho_m, PRva, PRfb)


View(PR)

# Estimar coeficiente HH

# Estimar la parte que se resta

resta <- tot_mun %>% 
  left_join(tot_zm, by = c("cve_zm")) %>% 
  mutate(Rue = ue.x / ue.y,
         Rpo_h = po_h.x / po_h.y,
         Rpo_m = po_m.x / po_m.y,
         Rre = re.x / re.y,
         Rpb = pb.x / pb.y,
         Rpre = pre.x / pre.y,
         Rpre_h = pre_h.x / pre_h.y,
         Rpre_m = pre_m.x / pre_m.y,
         Rpho = pho.x / pho.y,
         Rpho_h = pho_h.x / pho_h.y,
         Rpho_m = pho_m.x / pho_m.y,
         Rva = va.x / va.y,
         Rfb = fb.x / fb.y) %>% 
  select(cve_geo, cve_zm, Rue, Rpo_h, Rpo_m, Rre, Rpb, Rpre, Rpre_h, Rpre_m, Rpho, Rpho_h, Rpho_m, Rva, Rfb)


View(resta)
View(tot_mun)
View(tot_zm)
# Estimar HH

HH <- PR %>% 
  left_join(resta, by = c("cvegeo", "cve_zm")) %>% 
  mutate(HHue = PRue - Rue,
         HHpo_h = PRpo_h - Rpo_h,
         HHpo_m = PRpo_m - Rpo_m,
         HHre = PRre - Rre,
         HHpb = PRpb - Rpb,
         HHpre = PRpre - Rpre,
         HHpre_h = PRpre_h - Rpre_h,
         HHpre_m = PRpre_m - Rpre_m,
         HHpho = PRpho - Rpho,
         HHpho_h = PRpho_h - Rpho_h,
         HHpho_m = PRpho_m - Rpho_m,
         HHva = PRva - Rva,
         HHfb = PRfb - Rfb) %>% 
  select(cve_geo, cve_sub, cve_zm, HHue, HHpo_h, HHpo_m, HHre, HHpb, HHpre, HHpre_h, HHpre_m, HHpho, HHpho_h, HHpho_m, HHva, HHfb)


View(HH)

# Estimar IHH

IHH <- HH %>%
  mutate_at(vars(HHue, HHpo_h, HHpo_m, HHre, HHpb, HHpre, HHpre_h, HHpre_m, HHpho, HHpho_h, HHpho_m, HHva, HHfb), ~ 1 - .) %>%
  rename_with(~ paste0("IHH", gsub("HH", "", .)), starts_with("HH"))


View(IHH)

# Unir datos 

BLzm99_final <- left_join(datos, QL, by = c("cve_geo", "cve_sub", "cve_zm")) %>%
  left_join(PR, by = c("cve_geo", "cve_sub", "cve_zm")) %>%
  left_join(HH, by = c("cve_geo", "cve_sub", "cve_zm")) %>%
  left_join(IHH, by = c("cve_geo", "cve_sub", "cve_zm"))

View(BLzm99_final)

# Guardar archivo

library(openxlsx)

write.xlsx(BLzm99_final, "BLzm04_final.xlsx")



