encode wojewodstwa, gen(wojewodstwa_num)
xtset wojewodstwa_num Data
gen przy_PKB = ln(PKB) - ln(L.PKB)
gen przy_produkcja_globalna = ln(produkcjaglobalna) - ln(L.produkcjaglobalna)
gen przy_zuzycie_posrednie = ln(zuzycieposrednie) - ln(L.zuzycieposrednie)
gen przy_Inwestycje = ln(Inwestycje) - ln(L.Inwestycje)
gen przy_wydatki_budzetowe = ln(wydatkibudzetowe) - ln(L.wydatkibudzetowe)
gen przy_konsumpcja = ln(konsumpcja) - ln(L.konsumpcja)
gen eksp= eksportnetto/(L.eksportnetto)
gen przy_eksp= ln(eksp)
xtreg przy_PKB przy_produkcja_globalna przy_zuzycie_posrednie, re mle
predict p_pred1, xb
xtreg przy_PKB przy_konsumpcja przy_Inwestycje przy_wydatki_budzetowe przy_eksp, re mle
predict p_pred2, xb
gen PKB_new= exp(p_pred1)*(L.PKB)
gen PKB_new2= exp(p_pred2)*(L.PKB)
summarize przy_produkcja_globalna przy_zuzycie_posrednie przy_Inwestycje przy_wydatki_budzetowe przy_konsumpcja przy_eksp, detail
summarize przy_PKB, detail
destring Data, replace
local vars
foreach var of varlist * {
    if inlist("`var'", "Data", "wojewodstwa") == 0 {
local vars `vars' `var'
    }
}
collapse (mean) `vars', by(Data)
gen szara_strefa= PKB_new-PKB_new2
gen szara_strefa_pr= szara_strefa/PKB
gen szara_strefa_pr_1= szara_strefa_pr*100
gen szara_strefa_pr_2= abs(szara_strefa_pr*100)
list Data PKB PKB_new PKB_new2 szara_strefa_pr_2 szara_strefa
 tsset Data
dfuller przy_PKB
dfuller przy_produkcja_globalna
dfuller przy_zuzycie_posrednie
dfuller przy_Inwestycje
dfuller przy_wydatki_budzetowe
dfuller przy_konsumpcja
dfuller przy_eksp
twoway (line PKB Data) (line PKB_new Data) (line PKB_new2 Data),title("PKB i predykcji PKB") ytitle("mln zł") xtitle("Rok")  legend(order(1 "Rzeczywisty PKB" 2 "PKB z Modelu I" 3 "PKB z Modelu II"))
gen roznica= produkcjaglobalna- zuzycieposrednie
twoway (line PKB Data) (line produkcjaglobalna Data) (line zuzycieposrednie Data)(line roznica Data),title("PKB i jego składniki") ytitle("mln zł") xtitle("Rok")  legend(order(1 "Rzeczywisty PKB" 2 "produkcja globalna" 3 "zużycie pośrednie" 4 "różnica Produkcja-Zużycie"))
twoway (line Inwestycje Data) (line wydatkibudzetowe Data)(line konsumpcja Data) (line eksportnetto Data),title("PKB i jego składniki") ytitle("zł") xtitle("Rok")  legend(order(1 "Inwestycje" 2 "Wydatki budżetowe" 3 "Konsumpcja" 4 "Eksport netto"))
gen szara_GUS = .
replace szara_GUS = 13.7 if _n == 4
replace szara_GUS = 13.7 if _n == 5
replace szara_GUS = 12.8 if _n == 6
replace szara_GUS = 11.8 if _n == 7
replace szara_GUS = 13.1 if _n == 8
replace szara_GUS = 12.6 if _n == 9
replace szara_GUS = 12.1 if _n == 10
replace szara_GUS = 14 if _n == 11
gen szara_EY = .
replace szara_EY = 18.9 if _n == 4
replace szara_EY = 17.4 if _n == 5
replace szara_EY = 15.8 if _n == 6
replace szara_EY = 14 if _n == 7
replace szara_EY = 14.4 if _n == 8
replace szara_EY = 14.6 if _n == 9
replace szara_EY = 14.2 if _n == 10
replace szara_EY = 14.1 if _n == 11
list Data szara_strefa_pr_2 szara_GUS szara_EY in 4/11
twoway (line szara_strefa_pr_2 Data in 4/11) (line szara_GUS Data in 4/11)(line szara_EY  Data in 4/11),title(" Udział szarej strefy według GUS, EY i własne badanie") ytitle("procent") xtitle("Rok")  legend(order(1 "Badanie własne" 2 "Szara strefa według GUS" 3 "Szara strefa według EY"))
twoway (line szara_strefa_pr_2 Data) ,title(" Udział‚ szarej strefy w PKB %") ytitle("procent") xtitle("Rok")



