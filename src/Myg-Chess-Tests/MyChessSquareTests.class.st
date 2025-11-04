Class {
	#name : 'MyChessSquareTests',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyChessSquareTests >> testOutOfBoardReturnNilSquare [

	| board square |
	board := MyChessBoard empty.
	square := board at: 'a1'.
	self assert: (square + (0 @ -1) isKindOf: MyNilSquare)
]
