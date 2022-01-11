program EGP_ajustado
	syntax anything , isco(varname numeric) selfemp(varname numeric) nsupervisa(varname numeric) from(string)
	tempvar egp11 situacion
	
	/// Generar EGP ///
	
	*** Paso 1: Generar clasificación EGP ***
	capture which iscogen
	if _rc != 0 {
	display as error "Se necesita funcion iscogen. Para instalar usar comando ssc install iscogen"
	exit
	}
	
	iscogen `egp11' = egp11(`isco' `selfemp' `nsupervisa') if `selfemp' != . & `nsupervisa' != ., from(`from')

	/// Aplicacion de ajuste de Solis-Boado ///
	
	* Realizado en base a sintaxis elaborada por Vicente Espinoza: *
				
	*** Paso 2: Generar variable `situacion', que conjunta sempl y supvis
	gen `situacion' = -1
	qui replace `situacion' = 1 if `selfemp' == 0 & `nsupervisa' == 0
	qui replace `situacion' = 2 if `selfemp' == 0 & `nsupervisa' >=  1 & `nsupervisa' <=  9
	qui replace `situacion' = 3 if `selfemp' == 0 & `nsupervisa' >=  10 & `nsupervisa' ! = .
	qui replace `situacion' = 4 if `selfemp' == 1 & `nsupervisa' == 0
	qui replace `situacion' = 5 if `selfemp' == 1 & `nsupervisa' >=  1 & `nsupervisa' <=  9
	qui replace `situacion' = 6 if `selfemp' == 1 & `nsupervisa' >=  10 & `nsupervisa' ! = .

	*** Paso 3: Modificaciones acordadas marzo 1 de 2012 (a partir de propuesta origi.l de 16 modificaciones Solís)

	** Dirigentes y administradores de org. especializadas y gerentes (grupos 1141 a 1319)

	* 1. Autónomos sin dependientes van a IVb
	qui replace `egp11' = 6 if `isco' >= 1141 & `isco' <= 1319 & `isco'! = 1221 & `isco'! = 1311 & `situacion' == 4

	* 2. Autónomos con menos de diez perso.s a su cargo van a IVa
	qui replace `egp11' = 5 if `isco' >= 1141 & `isco' <= 1319 & `isco'! = 1221 & `isco'! = 1311 & `situacion' == 5

	* 3. Dependientes con menos de diez dependientes pasan de 1 a 2
	qui replace `egp11' = 2 if `isco' >= 1141 & `isco' <= 1319 & `isco'! = 1221 & `isco'! = 1311 & `situacion' <= 2

	** Profesionales (gran grupo 2, 2111 a 2460)

	* 4. Inspectores de enseñanza y afines se da tratamiento igual que profesores de básico (<10 dep = clase II)
	qui replace `egp11' = 2 if `isco' >= 2351 & `isco' <= 2352 & `situacion' <= 2
	qui replace `egp11' = 2 if `isco' >= 2351 & `isco' <= 2352 & `situacion' == 4
	qui replace `egp11' = 2 if `isco' >= 2351 & `isco' <= 2352 & `situacion' == 5

	** Técnicos y profesionales de nivel medio (gran grupo 3)

	* 5. Curanderos y afines se reclasifican (en general se les remueve de clases superiores)
	qui replace `egp11' = 10 if `isco' >= 3241 & `isco' <= 3242 & `situacion' == 1
	qui replace `egp11' = 8 if `isco' >= 3241 & `isco' <= 3242 & `situacion' >= 2 & `situacion' <= 3
	qui replace `egp11' = 6 if `isco' >= 3241 & `isco' <= 3242 & `situacion' == 4
	qui replace `egp11' = 5 if `isco' >= 3241 & `isco' <= 3242 & `situacion' >= 5

	* 6. Maestros de básico son equiparados con grupos similares (2320 a 2359)
	qui replace `egp11' = 2 if `isco' >= 3310 & `isco' <= 3340 & (`situacion' == 1 | `situacion' == 4)

	* 7. No manuales de nivel medio (no técnicos) pasan a clase IIIa+b si no tienen dependientes
	qui replace `egp11' = 3 if `isco' == 3419 & `situacion' == 1
	qui replace `egp11' = 3 if `isco' >= 3429 & `isco' <= 3480 & `situacion' == 1

	* 8. No manuales de nivel medio (no técnicos) pasan a IVb si son independientes sin empleados
	qui replace `egp11' = 6 if `isco' == 3419 & `situacion' == 4
	qui replace `egp11' = 6 if `isco' >= 3429 & `isco' <= 3479 & `situacion' == 4
	qui replace `egp11' = 4 if `isco' == 3480

	  * 9. No manuales de nivel medio (no técnicos) pasan a IVa si son independientes con <10 empleados
	qui replace `egp11' = 5 if `isco' >= 3411 & `isco' <= 3480 & `situacion' == 5

	** Trabajadores en servicios perso.les y en ventas (Gran grupo 5)

	* 10. Revisores de transporte, Mayordomos, meseros, cuidados perso.les y otros semi-skilled en servicios perso.les son clasificados como de baja calificacion o supervisores manuales, dependiendo de su `situacion' laboral
	qui replace `egp11' = 10 if `isco' == 5111 & `situacion' == 1
	qui replace `egp11' = 8 if `isco' == 5111 & (`situacion' == 2 | `situacion' == 3)
	qui replace `egp11' = 10 if `isco' == 5111 & `situacion' == 4
	qui replace `egp11' = 8 if `isco' == 5111 & `situacion' >= 5

	qui replace `egp11' = 10 if (`isco' == 5121 | (`isco'>5123 & `isco' <= 5139)) & `situacion' == 1
	qui replace `egp11' = 8 if (`isco' == 5121 | (`isco'>5123 & `isco' <= 5139)) & (`situacion' == 2 | `situacion' == 3)
	qui replace `egp11' = 10 if (`isco' == 5121 | (`isco'>5123 & `isco' <= 5139)) & `situacion' == 4
	qui replace `egp11' = 8 if (`isco' == 5121 | (`isco'>5123 & `isco' <= 5139)) & `situacion' >= 5
	qui replace `egp11' = 9 if `isco' == 5132 & `situacion' >= 4

	qui replace `egp11' = 10 if `isco' >= 5142 & `isco' <= 5152 & `situacion' == 1
	qui replace `egp11' = 8 if `isco' >= 5142 & `isco' <= 5152 & (`situacion' == 2 | `situacion' == 3)
	qui replace `egp11' = 10 if `isco' >= 5142 & `isco' <= 5152 & `situacion' == 4
	qui replace `egp11' = 8 if `isco' >= 5142 & `isco' <= 5152 & `situacion' >= 5

	qui replace `egp11' = 10 if `isco' >= 5163 & `isco' <= 5169 & `situacion' == 1
	qui replace `egp11' = 8 if `isco' >= 5163 & `isco' <= 5169 & (`situacion' == 2 | `situacion' == 3)
	qui replace `egp11' = 10 if `isco' >= 5163 & `isco' <= 5169 & `situacion' == 4
	qui replace `egp11' = 8 if `isco' >= 5163 & `isco' <= 5169 & `situacion' >= 5

	** Agricultores y trabajadores agropecuarios (Gran grupo 6)

	* 11. Dependientes agricolas van a VIIb
	qui replace `egp11' = 11 if `isco' >= 6111 & `isco' <= 6210 & `situacion' <= 3

	** Operadores de instalaciones (gran grupo 8)

	* 12. Operadores de maquinaria en industria química equiparados a otros manuales calificados
	qui replace `egp11' = 9 if `isco' >= 8221 & `isco' <= 8229 & `situacion' == 1
	qui replace `egp11' = 8 if `isco' >= 8221 & `isco' <= 8229 & `situacion' >= 2 & `situacion' <= 3

	* 13. Operadores de maquinaria en cualquier industria pasan a 8 si tienen dependientes
	qui replace `egp11' = 8 if `isco' >= 8131 & `isco' <= 8143 & `situacion' >= 2 & `situacion' <= 3
	qui replace `egp11' = 8 if `isco' == 8212 & `situacion' >= 2 & `situacion' <= 3
	qui replace `egp11' = 8 if `isco' >= 8231 & `isco' <= 8324 & `situacion' >= 2 & `situacion' <= 3
	qui replace `egp11' = 8 if `isco' >= 8334 & `isco' <= 8340 & `situacion' >= 2 & `situacion' <= 3

	* 14. Operarios en grupos 8231 a 8279 empleados sin dependientes son clasificados como skilled
	qui replace `egp11' = 8 if `isco' >= 8231 & `isco' <= 8279 & `situacion' == 1

	** Trabajadores no calificados (gran grupo 9)

	* 15. Equiparamos criterio: todos (excepto agrícolas) a clase VIIa
	qui replace `egp11' = 8 if `isco' >= 9111 & `isco' <= 9113 & `situacion' >= 2 & `situacion' <= 3
	qui replace `egp11' = 5 if `isco' >= 9111 & `isco' <= 9113 & `situacion' >= 5 & `situacion' <= 6
	qui replace `egp11' = 10 if `isco' >= 9111 & `isco' <= 9113 & `situacion' == 1 & `situacion' == 4
	qui replace `egp11' = 10 if `isco' >= 9114 & `isco' <= 9162

	* Reasignación a VI asalariados sin dep en 7123, 7131, 7421 & 7432
	qui replace `egp11' = 9 if (`isco' == 7123 | `isco' == 7131 | `isco' == 7421 | `isco' == 7432) & `situacion' == 1

	*** Paso 4: Descreme de la pequeña burguesia (propuesta de descreme parcial de Jorrat y colegas, aceptada por el grupo en marzo 1. Puede haber descreme adicio.l en función de disponibilidad de información más fi. en cada país, en cuyo caso se debe especificar en u. breve nota metodológica

	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 5112
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 5161
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 5162
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7111
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7112
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7113
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7124
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7134
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7135
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7136
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7137
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7141
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7142
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7415
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7416
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7421
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7423
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7436
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8111
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8112
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8113
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8121
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8122
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8123
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8124
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8131
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8139
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8141
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8142
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8143
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8151
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8152
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8153
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8154
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8155
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8159
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8161
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8162
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8163
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8171
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8172
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8211
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8212
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8221
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8222
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8223
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8224
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8229
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8231
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8232
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8240
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8251
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8252
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8253
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8261
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8262
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8264
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8271
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8272
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8273
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8274
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8275
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8276
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8277
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8278
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8279
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8290
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8311
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8312
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8322
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8323
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8324
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8332
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8333
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8334
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8340
	qui replace `egp11' = 10 if `egp11' == 6 & `isco' == 3241
	qui replace `egp11' = 10 if `egp11' == 6 & `isco' == 3242
	qui replace `egp11' = 10 if `egp11' == 6 & `isco' == 7121
	qui replace `egp11' = 10 if `egp11' == 6 & `isco' == 7122
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7123
	qui replace `egp11' = 10 if `egp11' == 6 & `isco' == 7129
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7131
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7132
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7133
	qui replace `egp11' = 10 if `egp11' == 6 & `isco' == 7143
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7413
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7414
	qui replace `egp11' = 10 if `egp11' == 6 & `isco' == 7424
	qui replace `egp11' = 10 if `egp11' == 6 & `isco' == 7431
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 7432
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8281
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8282
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8283
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8284
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8285
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8286
	qui replace `egp11' = 10 if `egp11' == 6 & `isco' == 8321

	** mensaje Raul 12 marzo.
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8263
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8265
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8266
	qui replace `egp11' = 9 if `egp11' == 6 & `isco' == 8269

	* Eliminar casos perdidos
	qui replace `egp11' = . if `egp11' == 9999
	qui replace `egp11' = . if `egp11' == 9988
	qui replace `egp11' = . if `egp11' == 9955
	qui replace `egp11' = . if `egp11' == -1

	** variable final:
	gen `1' = `egp11'

end
	
