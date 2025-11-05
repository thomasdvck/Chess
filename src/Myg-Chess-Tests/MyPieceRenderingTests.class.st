Class {
	#name : 'MyPieceRenderingTests',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyPieceRenderingTests >> testBishopRendering [ 
	| square piece |
	piece := MyBishop white.
	square := MyChessSquare color: Color white.
	self assert: (square renderBishop: piece) equals: 'B'.
	square := MyChessSquare color: Color black.
	self assert: (square renderBishop: piece) equals: 'b'.

	piece := MyBishop black.
	square := MyChessSquare color: Color white.
	self assert: (square renderBishop: piece) equals: 'V'.
	square := MyChessSquare color: Color black.
	self assert: (square renderBishop: piece) equals: 'v'.
]

{ #category : 'tests' }
MyPieceRenderingTests >> testKingRendering [ 
	| square piece |
	piece := MyKing white.
	square := MyChessSquare color: Color white.
	self assert: (square renderKing: piece) equals: 'K'.
	square := MyChessSquare color: Color black.
	self assert: (square renderKing: piece) equals: 'k'.

	piece := MyKing black.
	square := MyChessSquare color: Color white.
	self assert: (square renderKing: piece) equals: 'L'.
	square := MyChessSquare color: Color black.
	self assert: (square renderKing: piece) equals: 'l'.
]

{ #category : 'tests' }
MyPieceRenderingTests >> testNilPieceRendering [ 
	| square piece |
	piece := MyNilPiece new.

	square := MyChessSquare color: Color white.
	self assert: (piece renderPieceOn: square) equals: 'z'.

	square := MyChessSquare color: Color black.
	self assert: (piece renderPieceOn: square) equals: 'x'.
]

{ #category : 'tests' }
MyPieceRenderingTests >> testQueenRendering [ 
	| square piece |
	piece := MyQueen white.
	square := MyChessSquare color: Color white.
	self assert: (square renderQueen: piece) equals: 'Q'.
	square := MyChessSquare color: Color black.
	self assert: (square renderQueen: piece) equals: 'q'.

	piece := MyQueen black.
	square := MyChessSquare color: Color white.
	self assert: (square renderQueen: piece) equals: 'W'.
	square := MyChessSquare color: Color black.
	self assert: (square renderQueen: piece) equals: 'w'.
]

{ #category : 'tests' }
MyPieceRenderingTests >> testRookRendering [

	| square piece |
	piece := MyRook white.
	square := MyChessSquare color: Color white.
	self assert: (square renderRook: piece) equals: 'R'.
	square := MyChessSquare color: Color black.
	self assert: (square renderRook: piece) equals: 'r'.
	
	piece := MyRook black.
	square := MyChessSquare color: Color white.
	self assert: (square renderRook: piece) equals: 'T'.
	square := MyChessSquare color: Color black.
	self assert: (square renderRook: piece) equals: 't'.
]
