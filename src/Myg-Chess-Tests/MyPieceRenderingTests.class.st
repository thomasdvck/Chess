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
	self assert: (square renderPiece: piece) equals: 'B'.
	square := MyChessSquare color: Color black.
	self assert: (square renderPiece: piece) equals: 'b'.

	piece := MyBishop black.
	square := MyChessSquare color: Color white.
	self assert: (square renderPiece: piece) equals: 'V'.
	square := MyChessSquare color: Color black.
	self assert: (square renderPiece: piece) equals: 'v'
]

{ #category : 'tests' }
MyPieceRenderingTests >> testKingRendering [

	| square piece |
	piece := MyKing white.
	square := MyChessSquare color: Color white.
	self assert: (square renderPiece: piece) equals: 'K'.
	square := MyChessSquare color: Color black.
	self assert: (square renderPiece: piece) equals: 'k'.

	piece := MyKing black.
	square := MyChessSquare color: Color white.
	self assert: (square renderPiece: piece) equals: 'L'.
	square := MyChessSquare color: Color black.
	self assert: (square renderPiece: piece) equals: 'l'
]

{ #category : 'tests' }
MyPieceRenderingTests >> testKnightRendering [

	| square piece |
	piece := MyKnight white.
	square := MyChessSquare color: Color white.
	self assert: (square renderPiece: piece) equals: 'N'.
	square := MyChessSquare color: Color black.
	self assert: (square renderPiece: piece) equals: 'n'.

	piece := MyKnight black.
	square := MyChessSquare color: Color white.
	self assert: (square renderPiece: piece) equals: 'M'.
	square := MyChessSquare color: Color black.
	self assert: (square renderPiece: piece) equals: 'm'
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
	self assert: (square renderPiece: piece) equals: 'Q'.
	square := MyChessSquare color: Color black.
	self assert: (square renderPiece: piece) equals: 'q'.

	piece := MyQueen black.
	square := MyChessSquare color: Color white.
	self assert: (square renderPiece: piece) equals: 'W'.
	square := MyChessSquare color: Color black.
	self assert: (square renderPiece: piece) equals: 'w'
]

{ #category : 'tests' }
MyPieceRenderingTests >> testRookRendering [

	| square piece |
	piece := MyRook white.
	square := MyChessSquare color: Color white.
	self assert: (square renderPiece: piece) equals: 'R'.
	square := MyChessSquare color: Color black.
	self assert: (square renderPiece: piece) equals: 'r'.

	piece := MyRook black.
	square := MyChessSquare color: Color white.
	self assert: (square renderPiece: piece) equals: 'T'.
	square := MyChessSquare color: Color black.
	self assert: (square renderPiece: piece) equals: 't'
]
