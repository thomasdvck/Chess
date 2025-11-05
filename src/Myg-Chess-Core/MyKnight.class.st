Class {
	#name : 'MyKnight',
	#superclass : 'MyPiece',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'accessing' }
MyKnight >> id [
	
	^ 'N'
]

{ #category : 'rendering' }
MyKnight >> renderPieceOn: aSquare [

	^ aSquare renderPiece: self
]

{ #category : 'rendering' }
MyKnight >> targetSquaresLegal: aBoolean [

	^ {
		  square right right up.
		  square right right down.
		  square left left up.
		  square left left down.
		  square up up left.
		  square up up right.
		  square down down left.
		  square down down right } select: [ :s |
		  s isNilSquare not and: [
			  s hasPiece not or: [ s contents color ~= color ] ] ]
]
