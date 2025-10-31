Class {
	#name : 'MyNilSquareTests',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyNilSquareTests >> testIsNilSquare [
 self assert: (MyNilSquare new isNilSquare )
]

{ #category : 'tests' }
MyNilSquareTests >> testNilSquareContentsNilPiece [
self assert:(MyNilSquare new contents isKindOf: MyNilPiece )
]
