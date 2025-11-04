Class {
	#name : 'MyNillPieceTests',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyNillPieceTests >> testEmptySquareHasNilPiece [
	| square |
	
	square := MyChessBoard empty at: 'e4'.
	self assert: square hasPiece equals: false.
	self assert: (square contents isKindOf: MyNilPiece).
]

{ #category : 'tests' }
MyNillPieceTests >> testIsNotPiece [
	self assert: MyNilPiece new isPiece not
]

{ #category : 'tests' }
MyNillPieceTests >> testNullPieceNoMove [
	| NilPiece |
	NilPiece := MyNilPiece new.
	self assert: NilPiece targetSquares isEmpty.
	self assert: (NilPiece targetSquaresLegal: true) isEmpty.
	self assert: NilPiece attackingSquares isEmpty.
]
