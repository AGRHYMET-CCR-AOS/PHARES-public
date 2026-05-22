# Bulletin hydrologique automatique AGRHYMET CCR-AOS

Ce projet vise à automatiser la production quotidienne du bulletin hydrologique basé sur les sorties du système opérationnel FANFAR.

## Structure du projet

- `data/` : données brutes, traitées et référentielles
- `config/` : fichiers de configuration du bulletin et des règles de risque
- `R/` : scripts de téléchargement, traitement, cartographie et génération des textes
- `templates/` : modèles Quarto du bulletin
- `outputs/` : figures, tableaux et bulletins générés
- `logs/` : journaux d’exécution

## Script principal

Le script principal est :

```r
source('run_daily_bulletin.R')
```
