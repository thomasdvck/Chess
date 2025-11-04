Class {
	#name : 'MyKing',
	#superclass : 'MyPiece',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'as yet unclassified' }
MyKing >> attackingSquares [

	^ self basicTargetSquares
]

{ #category : 'rendering' }
MyKing >> basicTargetSquares [
	"The king can move one square on each direction including diagonals"

	^ {
		  (square right).
		  (square up right).
		  (square up).
		  (square up left).
		  (square left).
		  (square left down).
		  (square down).
		  (square down right) }
]

{ #category : 'accessing' }
MyKing >> id [
	
	^ 'K'
]

{ #category : 'testing' }
MyKing >> isCheckMated [
	"We check if the opponent pieces target all my potential movements and myself"

	| threatenedSquares |
	threatenedSquares := self opponentPieces flatCollect: [ :e |
		                     e attackingSquares ].

	^ self legalTargetSquares isEmpty and: [
		  threatenedSquares includes: self square ]
]

{ #category : 'testing' }
MyKing >> isInCheck [
	"We check if the opponent pieces target all my potential movements and myself"

	| threatenedSquares |
	threatenedSquares := self opponentPieces flatCollect: [ :e |
		                     e attackingSquares ].

	^ threatenedSquares includes: self square
]

{ #category : 'testing' }
MyKing >> isKing [
	
	^ true
]

{ #category : 'accessing' }
MyKing >> opponentPieces [

	^ self board pieces select: [ :e | e color = color negated  ] "toute case = 1 piece (MyPiece ou MyNilPiece)"
]

{ #category : 'rendering' }
MyKing >> renderPieceOn: aSquare [

	^ aSquare renderKing: self
]

{ #category : 'rendering' }
MyKing >> targetSquaresLegal: aBoolean [
	"The king cannot move to threatened squares"

	| threatenedSquares |
	threatenedSquares := self opponentPieces flatCollect: [ :e |
		                     e attackingSquares ].

	^ self basicTargetSquares select: [ :s |
		  s isNilSquare not and: [
			  (threatenedSquares includes: s) not and: [
				  s hasPiece not or: [ "The square has an opponent piece"
					  s contents color ~= color and: [ "Let's hope the piece is not defended"
						  threatenedSquares includes: s ] ] ] ] ]
]
