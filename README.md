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

*   **Type de tests** : uniquement des tests unitaires automatisés.
*   **Couverture** :
    *   Nouveaux tests pour `MyNilPiece` et `MyNilSquare`.
    *   Nouveaux tests de rendu pour chaque type de pièce et chaque combinaison de couleurs.
    *   Tests de non-régression (garder les anciens tests) pour garantir que le comportement du jeu reste inchangé.


## Design Decisions

### Kata 1 : Remove nil checks


Le but de ce kata était de supprimer toutes les vérifications explicites de `nil` dans le code du jeu d’échecs MygChess. Dans le projet de base, l’absence de pièce ou de case était représentée par `nil`, ce qui obligeait à écrire des conditions du type :

```smalltalk
square := board at: 'e4'.
square ifNotNil: [ square contents doSomething ].
````

Ce genre de code est fragile, difficile à lire, et source d’erreurs. Notre objectif était de rendre le code plus robuste et plus orienté objet, en utilisant le polymorphisme.

#### Démarche entreprise

On a commencé par repérer tous les endroits où on utilisait nil dans le code, que ce soit pour les cases vides ou les mouvements hors du plateau. On s’est vite rendu compte que ça polluait pas mal de méthodes, et que ça compliquait la logique du jeu. Nous nous sommes rendu compte que :

*   Les cases vides étaient initialisées avec `contents: nil`.
*   Les accès hors du plateau retournaient `nil`.
*   Beaucoup de méthodes utilisaient `ifNil:` ou `isNil` pour gérer ces cas.

Maintenant, chaque case contient toujours une pièce (même si c’est une "fausse" pièce vide), et tous les accès hors du plateau retournent une vraie instance de MyNilSquare. Ça nous permet d’utiliser le polymorphisme et d’éviter les tests ifNil: partout.
La logique de certaines classes a été modifiée pour ne plus faire de nil chekcs ni d'implémenter `nil`

#### Exemple avant refactoring

```smalltalk
MyChessBoard >> at: coordinate
    ^ grid at: coordinate ifAbsent: nil

MyChessSquare >> contents: aPiece
    contents := aPiece.
    text := contents
        ifNil: [ color isBlack ifFalse: [ 'z' ] ifTrue: [ 'x' ] ]
        ifNotNil: [ contents renderPieceOn: self ].
```

#### Exemple après refactoring

Nous avons introduit deux nouvelles classes :

*   `MyNilPiece` : une sous-classe de `MyPiece` qui représente une pièce vide.
*   `MyNilSquare` : une sous-classe de `MyChessSquare` qui représente une case hors du plateau.

Le code devient :

```smalltalk
MyChessBoard >> at: coordinate
    ^ grid at: coordinate ifAbsent: [ MyNilSquare new ]

MyChessSquare >> contents: aPiece
    contents := aPiece.
    text := contents renderPieceOn: self.
```

Et lors de l’initialisation du plateau :

```smalltalk
MyChessSquare >> initialize
    ...
    contents := MyNilPiece new. "Au moins chaque square a une NilPiece pour ne pas avoir de nil"
    ...
```

Ceci est un exemple, d'autre changements ont été effectuée, ils sont disponibles dans les différents commits. Ils cherchent tous à supprimer des vérifications ou implémentations de nil.

### Pourquoi ce design ?

* **Pour éviter les bugs** : Plus de nil qui traîne, donc moins de risques d’erreurs inattendues.
* **Pour rendre le code plus lisible** : On n’a plus besoin de vérifier partout si une case ou une pièce est nil.
* **Pour profiter du polymorphisme** : Les objets neutres (MyNilPiece, MyNilSquare) répondent aux mêmes messages que les vrais objets, donc le code est plus uniforme.
* **Pour faciliter les évolutions** : Si on veut ajouter des règles ou des comportements, on n’a pas à se soucier des cas particuliers liés à nil.

### Notre priorités

*   Supprimer tous les nil checks pour atteindre l'objectif du kata.
*   Garantir un comportement neutre et cohérent pour les objets `MyNilPiece` et `MyNilSquare`.
*   Ne pas casser les méthodes existantes lors des éventuelles modification.
*   Ne pas changer la logique de jeux.

### Pourquoi cette partie est très testée ?

Ce changement impacte la logique centrale du jeu. Nous avons donc :

*   Adapté les tests existants pour qu’ils fonctionnent avec `MyNilPiece` et `MyNilSquare`.
*   Ajouté des tests pour vérifier que les mouvements hors du plateau retournent bien une `MyNilSquare`.
*   Vérifié que les cases vides contiennent une `MyNilPiece`.
*   Testé que les méthodes comme `hasPiece` ou `targetSquaresLegal:` fonctionnent correctement avec les objets neutres.
*   Ajouté des tests pour s’assurer que les méthodes directionnelles (`up`, `down`, `left`, `right`) sur une case hors plateau retournent toujours une `MyNilSquare`.

### Design patterns utilisé

Voici le design pattern utilisé :

*   **Null Object Pattern** 

    Nous avons utilisé ce pattern pour créer `MyNilPiece` et `MyNilSquare`. Ces objets héritent de leurs superclasses (`MyPiece` et `MyChessSquare`) et redéfinissent les méthodes avec un comportement neutre. 
    
Par exemple :

```smalltalk
MyNilPiece >> targetSquaresLegal: aBoolean
    ^ #()

MyNilSquare >> up
    ^ self
```

### Changements concrets dans le code

*   Les méthodes comme `hasPiece` passent de :

```smalltalk
hasPiece
    ^ contents isNil not
```

à :

```smalltalk
hasPiece
    ^ contents isPiece
```

*   Les méthodes directionnelles sur les cases comme `up`, `down` etc retournent toujours une case, jamais `nil`.
*   Les collections de pièces ou de cases ne contiennent plus de `nil`, mais des objets neutres.
*   Toutes les vérifications de nils ont été supprimé.

Ce refactoring nous permet de :

*   Éviter les bugs.
*   Rendre le code plus modulaire et plus facile à tester.
*   Faciliter l’ajout de nouvelles fonctionnalités sans devoir gérer des cas particuliers partout dans le code.
*   Rendre le code plus lisble.



### Kata 2 : Refactor piece rendering

Le but de ce kata était de simplifier la logique de rendu des pièces sur l’échiquier. Dans le projet de base, chaque type de pièce avait sa propre méthode de rendu, avec beaucoup de conditions imbriquées pour gérer la couleur de la pièce et la couleur de la case.

Voici la logique de rendu des pièces avant le refactor :

```smalltalk
MyChessSquare >> renderKnight: aPiece
    ^ aPiece isWhite
        ifFalse: [ color isBlack ifFalse: [ 'M' ] ifTrue: [ 'm' ] ]
        ifTrue: [ color isBlack ifFalse: [ 'N' ] ifTrue: [ 'n' ] ].

MyChessSquare >> renderBishop: aPiece
    ^ aPiece isWhite
        ifTrue: [ color isBlack ifFalse: [ 'B' ] ifTrue: [ 'b' ] ]
        ifFalse: [ color isBlack ifFalse: [ 'V' ] ifTrue: [ 'v' ] ]

... Pareil pour les autres types de pièce
````

On s'est aperçu que cette approche rendait le code très répétitif et compliqué à modifier.

#### Démarche entreprise

On a décidé de :

*   Créer une méthode `renderPiece:` dans `MyChessSquare`.
*   Mettre en place une table de dispatch (`MyPieceRenderingTable`) qui associe chaque type de pièce, couleur de pièce et couleur de case au bon caractère à afficher.
*   Utiliser le double dispatch : la pièce appelle `renderPieceOn:` sur la case, qui elle-même appelle `renderPiece:` en utilisant la table.

### Exemple après refactoring

Dans la pièce :

```smalltalk
MyPiece >> renderPieceOn: aSquare
    ^ aSquare renderPiece: self
```

Dans la case :

```smalltalk
MyChessSquare >> renderPiece: aPiece
    ^ MyPieceRenderingTable symbolFor: aPiece class pieceColor: aPiece color squareColor: self color
```

La méthode centrale est donc `symbolFor:pieceClass pieceColor:squareColor:`, qui va chercher le bon caractère dans la table de dispatch.

#### La table de dispatch

La table de dispatch est une structure imbriquée qui associe chaque type de pièce, chaque couleur de pièce et chaque couleur de case à un symbole précis :

```smalltalk
MyPieceRenderingTable class >> table
    table := Dictionary new.
    table at: MyKnight put: (Dictionary new
        at: #white put: (Dictionary new at: #white put: 'N'; at: #black put: 'n'; yourself);
        at: #black put: (Dictionary new at: #white put: 'M'; at: #black put: 'm'; yourself);
        yourself).
    table at: MyBishop put: (Dictionary new
        at: #white put: (Dictionary new at: #white put: 'B'; at: #black put: 'b'; yourself);
        at: #black put: (Dictionary new at: #white put: 'V'; at: #black put: 'v'; yourself);
        yourself).
    ... "Même approche pour les autres pièces du jeux existantes"
    ^ table
```

La méthode qui utilise cette table :

```smalltalk
MyPieceRenderingTable class >> symbolFor: pieceClass pieceColor: pieceColor squareColor: squareColor
    pieceKey := pieceColor isBlack ifTrue: [ #black ] ifFalse: [ #white ].
    squareKey := squareColor isBlack ifTrue: [ #black ] ifFalse: [ #white ].
    ^ ((self table at: pieceClass) at: pieceKey) at: squareKey
```

#### Pourquoi ce design ?

*   **Pour éviter la répétition** : On centralise toute la logique de rendu dans une seule table, donc plus besoin de dupliquer le code pour chaque pièce.
*   **Pour faciliter l’extension** : Si on veut ajouter une nouvelle pièce ou changer le rendu, il suffit de modifier la table.
*   **Pour rendre le code plus lisible** : La méthode `renderPiece:` est simple et claire, et la logique est séparée des conditions.
*   **Pour profiter du double dispatch** : La pièce délègue le rendu à la case, qui utilise la table pour choisir le bon caractère.

#### Nos priorités

*   Supprimer les conditions imbriquées et la duplication de code.
*   Centraliser la logique de rendu pour faciliter la maintenance.
*   Garder le comportement visuel du jeu identique à l’original.
*   Rendre le code extensible pour de futures évolutions.

#### Pourquoi cette partie est très testée ?

Le rendu des pièces est un aspect très visible du jeu. Une erreur ici se voit tout de suite à l’écran. On a donc :

*   Adapté les tests existants pour vérifier que chaque pièce s’affiche correctement selon sa couleur et la couleur de la case.
*   Ajouté des tests pour chaque combinaison possible (type de pièce, couleur de pièce, couleur de case).
*   Vérifié que le rendu des pièces neutres (`MyNilPiece`) fonctionne aussi avec la nouvelle logique.



#### Design patterns utilisés

Voici les deux design patterns utilisés pour ce kata de refactor :

*   **Table Dispatch** : Toute la logique de rendu est centralisée dans une table de correspondance.
*   **Double Dispatch** : La pièce appelle `renderPieceOn:` sur la case, qui appelle `renderPiece:` en utilisant la table.


#### Changements concrets dans le code

*   Suppression de toutes les méthodes de rendu spécifiques à chaque pièce (`renderBishop:`, `renderKnight:`, etc.) dans `MyChessSquare`.
*   Ajout d’une méthode générique `renderPiece:` qui utilise la table de dispatch.
*   Mise en place de la classe `MyPieceRenderingTable` pour gérer la correspondance entre type/couleur de pièce et couleur de case.
*   Modification et ajouts de tests pour s'assurer du bon fonctionnement du refactor.

Ce refactoring nous permet de :

*   Rendre le code plus modulaire et plus facile à maintenir.
*   Ajouter ou modifier le rendu d’une pièce sans toucher au reste du code.
*   Garantir que le comportement visuel du jeu reste identique, tout en simplifiant la logique interne.