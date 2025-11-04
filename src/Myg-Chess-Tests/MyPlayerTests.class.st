Class {
	#name : 'MyPlayerTests',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyPlayerTests >> testPlayerHaveGoodPieces [
	|game|
	game := MyChessGame freshGame.
	
self assert: (game whitePlayer pieces allSatisfy: [ :p | p isPiece and: [p color= Color white]]);
	  assert: (game blackPlayer pieces allSatisfy: [ :p | p isPiece and: [p color= Color black]]).
]
