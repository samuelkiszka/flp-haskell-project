## Adresa github repozitáře
- git@github.com:samuelkiszka/flp-haskell-project.git

## Zajímavé části řešení
- checkInterpreterResult
    - V této funkci bylo velice zajímavé mnohonásobné zanořování do bloků.
    - Zároveň jsem nikde nepřišel na to, zda je .out povinný a tedy když chybí, tak zda má tato funkce vracet nějakou chybu. Beru to tak, že pokud není, tak pouze porovnám výsledný a požadované výstupní kódy a vracím výsledek na základě toho.
- runDiffOnOutput
    - V této funkci dochází ke zjednodušení diff outputu. Samotný diff vrací 0 pro totožní, 1 pro rozdílné a >1 pro jiné chyby. Tyto jiné chyby jsou zanedbány a pro cokoli jiného než 0 se vrací DiffFail.
- filterTests
    - Tuto funkci jsem původně implementoval čistě na základě skládání polí, ale poté jsem dostal doporučení na užití partition, které to vcelku zjednodušilo a zelegantnilo.
- matchesCriterion
    - Funkce využívá další dvě pomocné funkce 
        - matchesAnyString - pouhé provolání matchesString pro každou hodnotu vstupního pole
        - matchesString - provádí samotné porovnání regexově nebo čistě porovnáním na základě vstupních parametrů
- parseHeaderLine
    - U všech stringových hodnot kromě popisku předpokládám, že prázdná hodnota za uvozujícím tagem je nevalidní.
- buildExitCodes
    - Vzhledem k tomu, že funkce nevrací žádné chyby, tak předpokládám, že když se objeví TestCaseType ParseOnly, tak header obsahujej parser návratové kódy a tedy vracím Just hodnoty. Obdobně u ExecuteOnly.

## Splnění bonusových úloh
- implementace matchování na základě regexů
    - Tento úkol vyžadoval pouze malé rozšíření původního kódu. Vyžadoval však zjištění způsobu přidání knihovny do projektu.

## Přidání dalších knihoven
- regex-tdfa - přidána za účelem splnění úkolu porovnávání vůči regexům

## Změny mimo funkce