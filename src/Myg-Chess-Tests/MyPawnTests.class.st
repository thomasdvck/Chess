Class {
	#name : 'MyPawnTests',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyPawnTests >> testTargetSquaresLegalNoNil [
|board pawn|
board := MyChessBoard empty.
    pawn := MyPawn white.
    board at: 'a8' put: pawn.
    self assert: ((pawn targetSquaresLegal: true) allSatisfy: [ :s | s isNilSquare not ]).

]
