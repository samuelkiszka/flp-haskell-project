# Popis používání umělé inteligence při vypracovávání tohoto projektu
Používal jsem ChatGPT a to tím způsobem, že po prostudování jednotlivých souborů jsem poté jejich obsah hodil do chatu a nechal si ho ještě jednou pro úplnost vysvětlit.

Dále jsem poté pro jednotlivé soubory a implementace funkcí v nich zakládal nové chaty a opravoval si chyby, případně se doptával na důvody chyby. Snažil jsem se vždy dát jasně na jevo, že nechci vidět výslednou implementaci, ale pouze zhodnotit aktuální stav, případně popostrčit na správnou cestu.

V následujícím seznamu jsou odkazy na jednotlivé chaty týkající se úkolů v daných souborech.
- Discovery.hs
    - https://chatgpt.com/share/69dfeae3-be50-8325-8886-d7429ec94a88
- Executor.hs
    - https://chatgpt.com/share/69dffc15-3610-8333-99be-845b26f0c2f6
    - executeCombined
        - Kromě ChatGPT jsem jednou použil i vestavěný copilot, který mi prozradil, že parsRes v pomocné funkci není ve scope kvůli konstrukci do, a že jej tedy mám předat jako argument. (ChatGPT tvrdil opak)