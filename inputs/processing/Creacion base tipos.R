elsoc::load_elsoc('wide')

tip_par <- elsoc_wide_2016_2021[c("idencuesta", "c11_w01", "c11_w03", "c43_w05", "c08_02_w01", "c08_02_w02", "c08_02_w03", "c08_02_w04","c08_02_w05")]

#Crear variables donde 2 es si participa a veces, frecuentemente o muy frecuentemente.
tip_par$par_mov_w01<-1
tip_par$par_mov_w01[tip_par$c08_02_w01>=3]<-2
tip_par$par_mov_w02<-1
tip_par$par_mov_w02[tip_par$c08_02_w02>=3]<-2
tip_par$par_mov_w03<-1
tip_par$par_mov_w03[tip_par$c08_02_w03>=3]<-2
tip_par$par_mov_w04<-1
tip_par$par_mov_w04[tip_par$c08_02_w04>=3]<-2
tip_par$par_mov_w05<-1
tip_par$par_mov_w05[tip_par$c08_02_w05>=3]<-2

tip_par$c11_w01[tip_par$c11_w01<=0 | tip_par$c11_w01==3]<-NA
tip_par$c11_w03[tip_par$c11_w03<=0 | tip_par$c11_w03==3]<-NA
tip_par$c43_w05[tip_par$c43_w05<=0 | tip_par$c43_w05==3]<-NA

tip_par <- mutate(tip_par, c11_w01 = car::recode(tip_par$c11_w01, "1 = 2; 2 = 1"))
tip_par <- mutate(tip_par, c11_w03 = car::recode(tip_par$c11_w03, "1 = 2; 2 = 1"))
tip_par <- mutate(tip_par, c43_w05 = car::recode(tip_par$c43_w05, "1 = 2; 2 = 1"))

tip_par <- tip_par %>%
drop_na(c("c11_w01", "c11_w03", "c43_w05", "c08_02_w01", "c08_02_w02", "c08_02_w03", "c08_02_w04","c08_02_w05"))

var <- cbind(c11_w01, c11_w03, c43_w05, par_mov_w01, par_mov_w02, par_mov_w03, par_mov_w04, par_mov_w05) ~ 1

tipos_part4 <- poLCA(var, tip_par, nclass=4, na.rm = T, maxiter = 5000, nrep=10)
tip_par$class_part_4<- as.factor(tipos_part4$predclass)
tabpart_4<-table(tip_par$class_part_4)
tabpart_4
prop.table(tabpart_4)

# Guardar base especial
write.csv(tip_par, 
          file = file.path('..', 'participacion_tipos3.csv'), 
          row.names = F, fileEncoding = "UTF-8")


