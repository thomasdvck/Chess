Class {
	#name : 'MyNilSquare',
	#superclass : 'MyChessSquare',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'testing' }
MyNilSquare >> contents [

	^ MyNilPiece new
]

{ #category : 'testing' }
MyNilSquare >> isNilSquare [

	^ true
]
