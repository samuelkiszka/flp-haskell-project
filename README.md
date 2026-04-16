## Adresa github repozitáře
- git@github.com:samuelkiszka/flp-haskell-project.git

## Zajímavé části řešení
- checkInterpreterResult
    - V této funkci bylo velice zajímavé mnohonásobné zanořování do bloků.
    - Zároveň jsem nikde nepřišel na to, zda je .out povinný a tedy když chybí, tak zda má tato funkce vracet nějakou chybu. Beru to tak, že pokud není, tak pouze porovnám výsledný a požadované výstupní kódy a vracím výsledek na základě toho.
- runDiffOnOutput
    - V této funkci dochází ke zjednodušení diff outputu. Samotný diff vrací 0 pro totožní, 1 pro rozdílné a >1 pro jiné chyby. Tyto jiné chyby jsou zanedbány a pro cokoli jiného než 0 se vrací DiffFail.

## Splnění bonusových úloh

## Přidání dalších knihoven

## Změny mimo funkce