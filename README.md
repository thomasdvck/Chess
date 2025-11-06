# Myg Chess Game

## Installation

### Prérequis


- **Pharo** (version recommandée : Pharo 12)
- Accès à internet pour télécharger les dépendances


### Instructions d’installation

⚠️ l'image doit être en Pharo 12, tout nos tests on était effectués sur cette version de Pharo.

**Chargez le projet directement via Metacello**

* Accédez à **Browse > Playground** (ctrl+o+b).
* Ecrire et exécutez le code suivant :

```smalltalk
   Metacello new
     repository: 'github://thomasdvck/Chess:main';
     baseline: 'MygChess';
     onConflictUseLoaded;
     load.
````

> Cette commande installe automatiquement toutes les dépendances et le code du projet.


## Utilisation

### Démarrer une partie (nous n'avons pas modifier l'UI et l'UX nos katas étant sur du refactoring)

Dans le Playground, exécutez :

```smalltalk
board := MyChessGame freshGame.
board size: 800@600.
space := BlSpace new.
space root addChild: board.
space pulse.
space resizable: true.
space show.
```

### Interagir avec le jeu

*   **Sélection d’une case** : Cliquez sur une case pour la sélectionner.
*   **Déplacement d’une pièce** : Cliquez sur une pièce, puis sur la case de destination.
*   **Visualisation des mouvements** : Les cases cibles sont surlignées.
*   **Tests** : Les tests sont accessibles dans le dossier `Myg-Chess-Tests` et peuvent être lancés via l’interface de tests de Pharo.




## Organisation du code

	> Pour une meilleure visibilité écrire 'Chess' dans le filtre des packages.

*   **Code source** :
    *   Dossier : `Myg-Chess-Core`
*   **Tests** :
    *   Dossier : `Myg-Chess-Tests`
*   **Baseline** :
    *   Dossier : `baselineOfMygChess`


## Katas

Dans le cadre de ce projet, nous avons réalisé deux katas de refactoring sur le code existant du jeu d’échecs MygChess :

1.  **Remove nil checks**
    Objectif : supprimer toutes les vérifications explicites de `nil` dans le code, en utilisant le polymorphisme.
    Solution : création des classes `MyNilPiece` et `MyNilSquare` pour remplacer les `nil` par des objets neutres.

2.  **Refactor piece rendering**
    Objectif : simplifier la logique de rendu des pièces, en supprimant les conditions imbriquées.
    Solution : mise en place d’une méthode `renderPiece:` et d’une table de dispatch centralisée dans `MyPieceRenderingTable`.


###  Difficultés rencontrées et solutions

#### Pour le kata **Remove nil checks** :

*   **Comprendre l’impact des `nil` dans tout le code**
    Le plus difficile a été d’identifier tous les endroits où `nil` était utilisé. Nous avons dû bien comprendre comment les pièces et les cases étaient manipulées pour éviter de casser des comportements existants.

*   **Adapter les méthodes existantes**
    Certaines méthodes supposaient que des objets pouvaient être `nil`. Nous avons dû les modifier pour qu’elles fonctionnent avec les nouveaux objets `MyNilPiece` et `MyNilSquare`.La compréhension devait être totale pour être sur d'effectuer les bons changements.

*   **Maintenir la compatibilité**
    Le jeu devait continuer à fonctionner exactement comme avant. Nous avons donc fait des refactorings petit à petit, en testant à chaque étape pour éviter les régressions.

#### Pour le kata **Refactor piece rendering** :

*   **Première fois que nous écrivions une table de dispatch**
    C’était la première fois que nous écrivons une table de dispatch. Nous avons dû réfléchir à la meilleure façon de structurer les données pour qu’elles soient faciles à lire, et à maintenir.

*   **Supprimer les conditions sans perdre en clarté**
    Le code initial utilisait beaucoup de `ifTrue: [aBlock] ifFalse: [aBlock]` imbriqués. Il fallait les supprimer sans rendre le code plus complexe. La solution a été de centraliser la logique dans une table.


### Tests réalisés

*   **Type de tests** : uniquement des tests unitaires automatisés avec SUnit.
*   **Couverture** :
    *   Nouveaux tests pour `MyNilPiece` et `MyNilSquare`.
    *   Nouveaux tests de rendu pour chaque type de pièce et chaque combinaison de couleurs.
    *   Tests de non-régression (garder les anciens tests) pour garantir que le comportement du jeu reste inchangé.


## Design Decisions

#### Kata 1 : Remove nil checks
