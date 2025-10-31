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
MyNilSquare >> down [

	^ self
]

{ #category : 'testing' }
MyNilSquare >> isNilSquare [

	^ true
]

{ #category : 'testing' }
MyNilSquare >> left [

	^ self
]

{ #category : 'testing' }
MyNilSquare >> right [

	^ self
]

{ #category : 'testing' }
MyNilSquare >> up [

	^ self
]
