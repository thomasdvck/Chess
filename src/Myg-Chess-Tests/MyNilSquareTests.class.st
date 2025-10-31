Class {
	#name : 'MyNilSquareTests',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyNilSquareTests >> testDirectionalNilSquare [

	| board square |
	board := MyChessBoard empty.

	square := board at: 'a8'.
	self assert: (square up isKindOf: MyNilSquare).
	self assert: (square left isKindOf: MyNilSquare).

	square := board at: 'h1'.
	self assert: (square down isKindOf: MyNilSquare).
	self assert: (square right isKindOf: MyNilSquare)
]

{ #category : 'tests' }
MyNilSquareTests >> testIsNilSquare [
 self assert: (MyNilSquare new isNilSquare )
]

{ #category : 'tests' }
MyNilSquareTests >> testNilSquareContentsNilPiece [
self assert:(MyNilSquare new contents isKindOf: MyNilPiece )
]
