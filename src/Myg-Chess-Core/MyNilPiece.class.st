Class {
	#name : 'MyNilPiece',
	#superclass : 'MyPiece',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'testing' }
MyNilPiece >> attackingSquares [

	^ #()
]

{ #category : 'testing' }
MyNilPiece >> isPiece [
	^ false
]

{ #category : 'testing' }
MyNilPiece >> renderPieceOn: aSquare [

	^ aSquare color isBlack
		  ifTrue: [ 'x' ]
		  ifFalse: [ 'z' ]
]

{ #category : 'testing' }
MyNilPiece >> targetSquares [

	^ #()
]

{ #category : 'rendering' }
MyNilPiece >> targetSquaresLegal: aBoolean [

	^ #()
]
