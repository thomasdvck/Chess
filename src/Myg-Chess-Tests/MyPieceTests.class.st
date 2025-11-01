Class {
	#name : 'MyPieceTests',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyPieceTests >> testCollectSquareDontReturnNilSquare [

	| board bishop squares |
	board := MyChessBoard empty.
	bishop := MyBishop white.
	board at: 'e4' put: bishop.
	squares := bishop upRightDiagonal.
	self assert: (squares noneSatisfy: [ :s | s isNilSquare ])
]
