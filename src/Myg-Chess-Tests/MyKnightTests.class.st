Class {
	#name : 'MyKnightTests',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyKnightTests >> testTargetSquaresLegalNoNil [

	| board knight |
	board := MyChessBoard empty.
	knight := MyKnight white.
	board at: 'a1' put: knight.
	self assert: ((knight targetSquaresLegal: true) allSatisfy: [ :s |
			 s isNilSquare not ])
]
